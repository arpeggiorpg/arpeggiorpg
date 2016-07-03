{-# LANGUAGE TemplateHaskell #-}
module PandT.Interpolate (ui) where

import           ClassyPrelude
import qualified Data.String.Interpolate      as Interpolate (i)
import qualified Data.String.Interpolate.Util as Interpolate (unindent)
import           Language.Haskell.TH.Quote    (QuasiQuoter (..))

ui :: QuasiQuoter
ui = QuasiQuoter {
    quoteExp = \s -> [|fromString $ Interpolate.unindent $(quoteExp Interpolate.i s)|]
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("PandT.Prelude.ui: This QuasiQuoter can not be used as a " ++ name ++ "!")
