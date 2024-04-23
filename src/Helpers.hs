module Helpers (roundUp) where

roundUp :: Double -> Int
roundUp x = if x > toEnum (round x) then toEnum $ round x + 1 else toEnum $ round x
