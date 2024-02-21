module Helpers (takeFst, takeSnd, takeThrd, roundUp) where

takeFst :: (a, b, c) -> a
takeFst (a, _, _) = a

takeSnd :: (a, b, c) -> b
takeSnd (_, b, _) = b

takeThrd :: (a, b, c) -> c
takeThrd (_, _, c) = c

roundUp :: Double -> Int
roundUp x = if x > toEnum (round x) then toEnum $ round x + 1 else toEnum $ round x
