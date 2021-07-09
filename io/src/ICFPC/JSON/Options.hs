module ICFPC.JSON.Options where

import Data.Aeson.TH
import Data.Char

jsonOptions = defaultOptions
  { fieldLabelModifier = map toLower . dropWhile isLower
  }
