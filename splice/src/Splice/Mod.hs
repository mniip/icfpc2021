module Splice.Mod
  ( Mod(..)
  , Scope
  ) where

import GHC.Exts

import Language.Haskell.TH

data Mod = Mod (Maybe [Name]) [Dec]

type Scope = [(Info, Any)]
