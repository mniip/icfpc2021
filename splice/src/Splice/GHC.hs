module Splice.GHC
  ( spliceModGHC
  , runGHC
  , copyHostDyn
  ) where

import Control.Exception
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock
import qualified GHC.Paths
import System.IO.Unsafe

import qualified Language.Haskell.TH.Syntax as TH

import GHC
import GhcMonad
import TcRnMonad
import DynFlags
import HscTypes
import DriverPipeline
import Avail
import Module
import FieldLabel
import SrcLoc
import Name
import Bag
import FastString
import ByteCodeLink
import Linker
import TcSplice ()
import FileCleanup
import GHCi
import GHCi.RemoteTypes
import Outputable hiding ((<>))
import ErrUtils
import Panic

import Splice.Mod
import Splice.Reify

{-# NOINLINE moduleSeqVar #-}
moduleSeqVar :: IORef Int
moduleSeqVar = unsafePerformIO $ newIORef 0

newModuleName :: IO ModuleName
newModuleName = do
  modSeq <- atomicModifyIORef' moduleSeqVar (\x -> (x + 1, x))
  pure $ mkModuleName $ "Splice.#" <> show modSeq

modSrcLoc :: ModuleName -> RealSrcLoc
modSrcLoc modName = mkRealSrcLoc (moduleNameFS modName) 0 0

mkFakeModSummary :: DynFlags -> ModuleName -> Mod -> IO ModSummary
mkFakeModSummary dflags modName mod = do
  hsModule <- case reflectMod modName mod of
    Left errs -> throwGhcExceptionIO $ PprProgramError "TH conversion failed" errs
    Right hsModule -> pure hsModule
  now <- liftIO getCurrentTime
  temp_nonexistent <- newTempName dflags TFL_GhcSession mempty
  temp_hi <- newTempName dflags TFL_GhcSession $ hiSuf dflags
  temp_o <- newTempName dflags TFL_GhcSession $ objectSuf dflags
  let modSummary = ModSummary
        { ms_mod = mkModule mainUnitId modName
        , ms_hsc_src = HsSrcFile
        , ms_location = ModLocation
          { ml_hs_file = Just temp_nonexistent
          , ml_hi_file = temp_hi
          , ml_obj_file = temp_o
          , ml_hie_file = temp_nonexistent
          }
        , ms_hs_date = now
        , ms_obj_date = Nothing
        , ms_iface_date = Nothing
        , ms_hie_date = Nothing
        , ms_srcimps = []
        , ms_textual_imps = []
        , ms_parsed_mod = Just HsParsedModule
          { hpm_module = cL (srcLocSpan $ RealSrcLoc $ modSrcLoc modName) hsModule
          , hpm_src_files = []
          , hpm_annotations = (M.empty, M.empty)
          }
        , ms_hspp_file = temp_nonexistent
        , ms_hspp_opts = dflags
        , ms_hspp_buf = Nothing
        }
  pure modSummary

copyHostDyn :: DynFlags -> DynFlags
copyHostDyn = if dynamicGhc then updateWays . addWay' WayDyn else id

loadHomeModule :: GhcMonad m => HomeModInfo -> m ()
loadHomeModule hmi = do
  env <- getSession
  let mod = mi_module $ hm_iface hmi
  let env' = env { hsc_HPT = addToHpt (hsc_HPT env) (moduleName mod) hmi }
  setSession env'
  liftIO $ linkModule env' mod

loadScope :: HscEnv -> HomeModInfo -> IO Scope
loadScope env hmi = do
  let dflags = hsc_dflags env
  forM (filter isValName $ concatMap availNames $ md_exports $ hm_details hmi) $ \name -> do

    let closureSym = unpackFS $ nameToCLabel name "closure"
    res <- lookupClosure env closureSym
    HValue any <- case res of
      Nothing -> throwGhcExceptionIO $ ProgramError $ "Could not lookup " <> closureSym
      Just hvRef -> finally (wormholeRef dflags hvRef) (freeHValueRefs env [hvRef])

    info <- case lookupTypeEnv (md_types $ hm_details hmi) name of
      Nothing -> throwGhcExceptionIO $ ProgramError $ "Could not find type for " <> getOccString name
      Just tyThing -> case reifyTyThing tyThing of
        Left errs -> throwGhcExceptionIO $ PprProgramError ("Failed type conversion for " <> getOccString name) errs
        Right info -> pure info

    pure (info, any)
  where
    availNames (Avail name) = [name]
    availNames (AvailTC name pieces labels) = name : pieces ++ map flSelector labels

runQuasiGhc :: GhcMonad m => Module -> TH.Q a -> m a
runQuasiGhc mod act = withSession $ \env -> do
  result <- liftIO $ initTc env HsSrcFile True mod (realSrcLocSpan $ modSrcLoc $ moduleName mod) $ TH.runQ act
  case result of
    ((_, errs), Nothing) -> liftIO $ throwGhcExceptionIO $ PprProgramError "Executing splice failed"
      $ hcat $ map pprLocErrMsg $ bagToList errs
    (_, Just res) -> pure res

{-# NOINLINE tempSuffixVar #-}
tempSuffixVar :: IORef Int
tempSuffixVar = unsafePerformIO $ newIORef 0

runGHC :: Ghc a -> IO a
runGHC act = runGhc (Just GHC.Paths.libdir) $ do
  orig_dflags <- getSessionDynFlags
  _ <- setSessionDynFlags $ updOptLevel 2 $ copyHostDyn orig_dflags { nextTempSuffix = tempSuffixVar }
  act

spliceModGHC :: GhcMonad m => TH.Q Mod -> m Scope
spliceModGHC qmod = do
  dflags <- getSessionDynFlags

  modName <- liftIO newModuleName

  mod <- runQuasiGhc (mkModule mainUnitId modName) qmod

  ms <- liftIO $ mkFakeModSummary dflags modName mod

  hmi <- withSession $ \env -> do
    liftIO $ compileOne env ms 1 1 Nothing Nothing SourceModified

  loadHomeModule hmi
  withSession $ \env -> do
    liftIO $ loadScope env hmi
