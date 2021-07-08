{-# LANGUAGE BangPatterns, RecordWildCards, TupleSections #-}
module Splice.Linker
  ( linkHomeModule
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.List (nub, partition)
import Data.Maybe

import GHC
import DynFlags
import HscTypes
import ByteCodeLink
import Module
import GHCi
import GHCi.RemoteTypes
import ByteCodeTypes
import NameEnv
import LinkerTypes (DynLinker(..), PersistentLinkerState(..))
import Outputable
import Panic
import SysTools
import FileCleanup

import Linker (initDynLinker, linkPackages)

linkHomeModule :: HscEnv -> HomeModInfo -> IO ()
linkHomeModule env hmi = do
  initDynLinker env
  linkPackages env (fst <$> dep_pkgs (mi_deps $ hm_iface hmi))

  modifyPLS_ (hsc_dynLinker env) $ \pls -> do
    (pls', ok_flag) <- linkModules env pls (maybeToList $ hm_linkable hmi)
    when (failed ok_flag) $ throwGhcExceptionIO (ProgramError "could not link module")
    return pls'

-- Paraphrased from ghc's Linker module:

uninitialised :: a
uninitialised = panic "Dynamic linker not initialised"

modifyPLS_ :: DynLinker -> (PersistentLinkerState -> IO PersistentLinkerState) -> IO ()
modifyPLS_ dl f =
  modifyMVar_ (dl_mpls dl) (fmap pure . f . fromMaybe uninitialised)

linkModules :: HscEnv -> PersistentLinkerState -> [Linkable]
            -> IO (PersistentLinkerState, SuccessFlag)
linkModules hsc_env pls linkables
  = mask_ $ do  -- don't want to be interrupted by ^C in here

        let (objs, bcos) = partition isObjectLinkable
                              (concatMap partitionLinkable linkables)

                -- Load objects first; they can't depend on BCOs
        (pls1, ok_flag) <- dynLinkObjs hsc_env pls objs

        if failed ok_flag then
                return (pls1, Failed)
          else do
                pls2 <- dynLinkBCOs hsc_env pls1 bcos
                return (pls2, Succeeded)

-- HACK to support f-x-dynamic in the interpreter; no other purpose
partitionLinkable :: Linkable -> [Linkable]
partitionLinkable li
   = let li_uls = linkableUnlinked li
         li_uls_obj = filter isObject li_uls
         li_uls_bco = filter isInterpretable li_uls
     in
         case (li_uls_obj, li_uls_bco) of
            (_:_, _:_) -> [li {linkableUnlinked=li_uls_obj},
                           li {linkableUnlinked=li_uls_bco}]
            _ -> [li]

findModuleLinkable_maybe :: [Linkable] -> Module -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        _    -> pprPanic "findModuleLinkable" (ppr mod)

linkableInSet :: Linkable -> [Linkable] -> Bool
linkableInSet l objs_loaded =
  case findModuleLinkable_maybe objs_loaded (linkableModule l) of
        Nothing -> False
        Just m  -> linkableTime l == linkableTime m

dynLinkObjs :: HscEnv -> PersistentLinkerState -> [Linkable]
            -> IO (PersistentLinkerState, SuccessFlag)
dynLinkObjs hsc_env pls objs = do
        -- Load the object files and link them
        let (objs_loaded', new_objs) = rmDupLinkables (objs_loaded pls) objs
            pls1                     = pls { objs_loaded = objs_loaded' }
            unlinkeds                = concatMap linkableUnlinked new_objs
            wanted_objs              = map nameOfObject unlinkeds

        if interpreterDynamic (hsc_dflags hsc_env)
            then do pls2 <- dynLoadObjs hsc_env pls1 wanted_objs
                    return (pls2, Succeeded)
            else do mapM_ (loadObj hsc_env) wanted_objs

                    -- Link them all together
                    ok <- resolveObjs hsc_env

                    -- If resolving failed, unload all our
                    -- object modules and carry on
                    if succeeded ok then do
                            return (pls1, Succeeded)
                      else do
                            pls2 <- unload_wkr hsc_env [] pls1
                            return (pls2, Failed)

dynLoadObjs :: HscEnv -> PersistentLinkerState -> [FilePath]
            -> IO PersistentLinkerState
dynLoadObjs _       pls                           []   = return pls
dynLoadObjs hsc_env pls@PersistentLinkerState{..} objs = do
    let dflags = hsc_dflags hsc_env
    let platform = targetPlatform dflags
    let minus_ls = [ lib | Option ('-':'l':lib) <- ldInputs dflags ]
    let minus_big_ls = [ lib | Option ('-':'L':lib) <- ldInputs dflags ]
    (soFile, libPath, libName) <-
      newTempLibName dflags TFL_CurrentModule (soExt platform)
    let
        dflags2 = dflags {
                      -- We don't want the original ldInputs in
                      -- (they're already linked in), but we do want
                      -- to link against previous dynLoadObjs
                      -- libraries if there were any, so that the linker
                      -- can resolve dependencies when it loads this
                      -- library.
                      ldInputs =
                           concatMap (\l -> [ Option ("-l" ++ l) ])
                                     (nub $ snd <$> temp_sos)
                        ++ concatMap (\lp -> [ Option ("-L" ++ lp)
                                                    , Option "-Xlinker"
                                                    , Option "-rpath"
                                                    , Option "-Xlinker"
                                                    , Option lp ])
                                     (nub $ fst <$> temp_sos)
                        ++ concatMap
                             (\lp ->
                                 [ Option ("-L" ++ lp)
                                 , Option "-Xlinker"
                                 , Option "-rpath"
                                 , Option "-Xlinker"
                                 , Option lp
                                 ])
                             minus_big_ls
                        -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                        ++ map (\l -> Option ("-l" ++ l)) minus_ls,
                      -- Add -l options and -L options from dflags.
                      --
                      -- When running TH for a non-dynamic way, we still
                      -- need to make -l flags to link against the dynamic
                      -- libraries, so we need to add WayDyn to ways.
                      --
                      -- Even if we're e.g. profiling, we still want
                      -- the vanilla dynamic libraries, so we set the
                      -- ways / build tag to be just WayDyn.
                      ways = [WayDyn],
                      buildTag = mkBuildTag [WayDyn],
                      outputFile = Just soFile
                  }
    -- link all "loaded packages" so symbols in those can be resolved
    -- Note: We are loading packages with local scope, so to see the
    -- symbols in this link we must link all loaded packages again.
    linkDynLib dflags2 objs pkgs_loaded

    -- if we got this far, extend the lifetime of the library file
    changeTempFilesLifetime dflags TFL_GhcSession [soFile]
    m <- loadDLL hsc_env soFile
    case m of
        Nothing -> return $! pls { temp_sos = (libPath, libName) : temp_sos }
        Just err -> panic ("Loading temp shared object failed: " ++ err)

rmDupLinkables :: [Linkable]    -- Already loaded
               -> [Linkable]    -- New linkables
               -> ([Linkable],  -- New loaded set (including new ones)
                   [Linkable])  -- New linkables (excluding dups)
rmDupLinkables already ls
  = go already [] ls
  where
    go already extras [] = (already, extras)
    go already extras (l:ls)
        | linkableInSet l already = go already     extras     ls
        | otherwise               = go (l:already) (l:extras) ls

dynLinkBCOs :: HscEnv -> PersistentLinkerState -> [Linkable]
            -> IO PersistentLinkerState
dynLinkBCOs hsc_env pls bcos = do

        let (bcos_loaded', new_bcos) = rmDupLinkables (bcos_loaded pls) bcos
            pls1                     = pls { bcos_loaded = bcos_loaded' }
            unlinkeds :: [Unlinked]
            unlinkeds                = concatMap linkableUnlinked new_bcos

            cbcs :: [CompiledByteCode]
            cbcs      = map byteCodeOfObject unlinkeds


            ies        = map bc_itbls cbcs
            gce       = closure_env pls
            final_ie  = foldr plusNameEnv (itbl_env pls) ies

        names_and_refs <- linkSomeBCOs hsc_env final_ie gce cbcs

        -- We only want to add the external ones to the ClosureEnv
        let (to_add, to_drop) = partition (isExternalName.fst) names_and_refs

        -- Immediately release any HValueRefs we're not going to add
        freeHValueRefs hsc_env (map snd to_drop)
        -- Wrap finalizers on the ones we want to keep
        new_binds <- makeForeignNamedHValueRefs hsc_env to_add

        return pls1 { closure_env = extendClosureEnv gce new_binds,
                      itbl_env    = final_ie }

-- Link a bunch of BCOs and return references to their values
linkSomeBCOs :: HscEnv
             -> ItblEnv
             -> ClosureEnv
             -> [CompiledByteCode]
             -> IO [(Name,HValueRef)]
                        -- The returned HValueRefs are associated 1-1 with
                        -- the incoming unlinked BCOs.  Each gives the
                        -- value of the corresponding unlinked BCO

linkSomeBCOs hsc_env ie ce mods = foldr fun do_link mods []
 where
  fun CompiledByteCode{..} inner accum =
    case bc_breaks of
      Nothing -> inner ((panic "linkSomeBCOs: no break array", bc_bcos) : accum)
      Just mb -> withForeignRef (modBreaks_flags mb) $ \breakarray ->
                   inner ((breakarray, bc_bcos) : accum)

  do_link [] = return []
  do_link mods = do
    let flat = [ (breakarray, bco) | (breakarray, bcos) <- mods, bco <- bcos ]
        names = map (unlinkedBCOName . snd) flat
        bco_ix = mkNameEnv (zip names [0..])
    resolved <- sequence [ linkBCO hsc_env ie ce bco_ix breakarray bco
                         | (breakarray, bco) <- flat ]
    hvrefs <- createBCOs hsc_env resolved
    return (zip names hvrefs)

-- | Useful to apply to the result of 'linkSomeBCOs'
makeForeignNamedHValueRefs
  :: HscEnv -> [(Name,HValueRef)] -> IO [(Name,ForeignHValue)]
makeForeignNamedHValueRefs hsc_env bindings =
  mapM (\(n, hvref) -> (n,) <$> mkFinalizedHValue hsc_env hvref) bindings

unload_wkr :: HscEnv
           -> [Linkable]                -- stable linkables
           -> PersistentLinkerState
           -> IO PersistentLinkerState
-- Does the core unload business
-- (the wrapper blocks exceptions and deals with the PLS get and put)

unload_wkr hsc_env keep_linkables pls@PersistentLinkerState{..}  = do
  -- NB. careful strictness here to avoid keeping the old PLS when
  -- we're unloading some code.  -fghci-leak-check with the tests in
  -- testsuite/ghci can detect space leaks here.

  let (objs_to_keep, bcos_to_keep) = partition isObjectLinkable keep_linkables

      discard keep l = not (linkableInSet l keep)

      (objs_to_unload, remaining_objs_loaded) =
         partition (discard objs_to_keep) objs_loaded
      (bcos_to_unload, remaining_bcos_loaded) =
         partition (discard bcos_to_keep) bcos_loaded

  mapM_ unloadObjs objs_to_unload
  mapM_ unloadObjs bcos_to_unload

  -- If we unloaded any object files at all, we need to purge the cache
  -- of lookupSymbol results.
  when (not (null (objs_to_unload ++
                   filter (not . null . linkableObjs) bcos_to_unload))) $
    purgeLookupSymbolCache hsc_env

  let !bcos_retained = mkModuleSet $ map linkableModule remaining_bcos_loaded

      -- Note that we want to remove all *local*
      -- (i.e. non-isExternal) names too (these are the
      -- temporary bindings from the command line).
      keep_name (n,_) = isExternalName n &&
                        nameModule n `elemModuleSet` bcos_retained

      itbl_env'     = filterNameEnv keep_name itbl_env
      closure_env'  = filterNameEnv keep_name closure_env

      !new_pls = pls { itbl_env = itbl_env',
                       closure_env = closure_env',
                       bcos_loaded = remaining_bcos_loaded,
                       objs_loaded = remaining_objs_loaded }

  return new_pls
  where
    unloadObjs :: Linkable -> IO ()
    unloadObjs lnk
      | dynamicGhc = return ()
        -- We don't do any cleanup when linking objects with the
        -- dynamic linker.  Doing so introduces extra complexity for
        -- not much benefit.

      -- Code unloading currently disabled due to instability.
      -- See #16841.
      | False -- otherwise
      = mapM_ (unloadObj hsc_env) [f | DotO f <- linkableUnlinked lnk]
                -- The components of a BCO linkable may contain
                -- dot-o files.  Which is very confusing.
                --
                -- But the BCO parts can be unlinked just by
                -- letting go of them (plus of course depopulating
                -- the symbol table which is done in the main body)
      | otherwise = return () -- see #16841
