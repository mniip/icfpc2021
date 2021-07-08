{-# LANGUAGE TemplateHaskell #-}
module Splice
  ( Mod(..)
  , Scope
  , spliceMod
  , spliceDecs
  , spliceExp
  , spliceExpDyn
  , spliceTExp
  ) where

import Data.Dynamic
import Data.Maybe
import GHC.Exts
import Unsafe.Coerce

import GhcMonad
import Panic

import Language.Haskell.TH

import Splice.Mod
import Splice.GHC

spliceDecsGHC :: Q [Dec] -> Ghc Scope
spliceDecsGHC qdecs = spliceModGHC $ Mod Nothing <$> qdecs

spliceExpGHC :: Q Exp -> Ghc (Type, Any)
spliceExpGHC qexp = do
  scope <- spliceDecsGHC [d| expr = $qexp |]
  case listToMaybe $ mapMaybe exprTy scope of
    Nothing -> liftIO $ throwGhcExceptionIO $ ProgramError $ "No expr binding: " <> show (map fst scope)
    Just tyAny -> pure tyAny
  where
    exprTy (info, any)
      | VarI name ty _ <- info, nameBase name == "expr" = Just (ty, any)
      | otherwise = Nothing

spliceExpDynGHC :: Q Exp -> Ghc Dynamic
spliceExpDynGHC qexp = unsafeCoerce . snd <$> spliceExpGHC [| toDyn $qexp |]

spliceTExpGHC :: Q (TExp a) -> Ghc a
spliceTExpGHC qtexp = unsafeCoerce . snd <$> spliceExpGHC (unType <$> qtexp)

spliceMod :: Q Mod -> IO Scope
spliceMod = runGHC . spliceModGHC

spliceDecs :: Q [Dec] -> IO Scope
spliceDecs = runGHC . spliceDecsGHC

spliceExp :: Q Exp -> IO (Type, Any)
spliceExp = runGHC . spliceExpGHC

spliceExpDyn :: Q Exp -> IO Dynamic
spliceExpDyn = runGHC . spliceExpDynGHC

spliceTExp :: Q (TExp a) -> IO a
spliceTExp = runGHC . spliceTExpGHC
