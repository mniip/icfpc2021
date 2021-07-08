module Splice
  ( splice
  , spliceIO
  , copyHostDyn
  , Mod(..)
  ) where

import Control.Exception
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock
import GHC.Exts
import qualified GHC.Paths
import System.IO.Unsafe

import qualified Language.Haskell.TH as TH

import GHC
import DynFlags
import GhcMonad
import DriverPhases
import DriverPipeline
import FieldLabel
import HscTypes
import ByteCodeLink
import FastString
import BasicTypes
import Avail
import Name
import Module
import Panic
import GHC.ThToHs
import GHCi
import GHCi.RemoteTypes
import RdrName
import FileCleanup

import Splice.Linker

data Mod = Mod (Maybe [TH.Name]) [TH.Dec]

thNameToRdr :: TH.Name -> Located RdrName
thNameToRdr = noLoc . mkRdrUnqual . mkVarOcc . TH.nameBase

{-# NOINLINE moduleSeqVar #-}
moduleSeqVar :: IORef Int
moduleSeqVar = unsafePerformIO $ newIORef 0

{-# NOINLINE tempSuffixVar #-}
tempSuffixVar :: IORef Int
tempSuffixVar = unsafePerformIO $ newIORef 0

mkFakeModSummary :: DynFlags -> Mod -> IO ModSummary
mkFakeModSummary dflags (Mod exports decs) = do
  hsDecls <- case convertToHsDecls Generated noSrcSpan decs of
    Left errs -> throwGhcExceptionIO $ PprProgramError "TH conversion failed" errs
    Right hsDecls -> pure hsDecls
  now <- liftIO getCurrentTime
  modSeq <- atomicModifyIORef' moduleSeqVar (\x -> (x + 1, x))
  temp_nonexistent <- newTempName dflags TFL_GhcSession mempty
  temp_hi <- newTempName dflags TFL_GhcSession $ show modSeq <> "." <> hiSuf dflags
  temp_o <- newTempName dflags TFL_GhcSession $ show modSeq <> "." <> objectSuf dflags
  let modName = mkModuleName $ "Splice.#" <> show modSeq
  let hsModule = HsModule
        { hsmodName = Just $ noLoc modName
        , hsmodExports = noLoc . map (noLoc . IEVar noExtField . noLoc . IEName . thNameToRdr) <$> exports
        , hsmodImports = []
        , hsmodDecls = hsDecls
        , hsmodDeprecMessage = Nothing
        , hsmodHaddockModHeader = Nothing
        }
  let modSpan = srcLocSpan $ mkSrcLoc (moduleNameFS modName) 0 0
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
          { hpm_module = cL modSpan hsModule
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

loadHomeModule :: HscEnv -> HomeModInfo -> IO (M.Map String (TyThing, Any))
loadHomeModule env hmi = do
  linkHomeModule env hmi

  let dflags = hsc_dflags env

  scope <- forM (filter isValName $ concatMap availNames $ md_exports $ hm_details hmi) $ \name -> do

    ty <- case lookupTypeEnv (md_types $ hm_details hmi) name of
      Nothing -> throwGhcExceptionIO $ ProgramError $ "Could not find type for " <> getOccString name
      Just ty -> pure ty

    let closureSym = unpackFS $ nameToCLabel name "closure"
    res <- lookupClosure env closureSym
    HValue any <- case res of
      Nothing -> throwGhcExceptionIO $ ProgramError $ "Could not lookup " <> closureSym
      Just hvRef -> finally (wormholeRef dflags hvRef) (freeHValueRefs env [hvRef])

    pure (getOccString name, (ty, any))

  pure $ M.fromList scope
  where
    availNames (Avail name) = [name]
    availNames (AvailTC name pieces labels) = name : pieces ++ map flSelector labels

splice :: GhcMonad m => Mod -> m (M.Map String (TyThing, Any))
splice mod = do
  dflags <- getSessionDynFlags
  ms <- liftIO $ mkFakeModSummary dflags mod

  withSession $ \env -> do
    hmi <- liftIO $ compileOne env ms 1 1 Nothing Nothing SourceModified
    liftIO $ loadHomeModule env hmi

spliceIO :: Mod -> IO (M.Map String (TyThing, Any))
spliceIO mod = runGhc (Just GHC.Paths.libdir) $ do
  orig_dflags <- getSessionDynFlags
  _ <- setSessionDynFlags $ updOptLevel 2 $ copyHostDyn orig_dflags { nextTempSuffix = tempSuffixVar }
  splice mod
