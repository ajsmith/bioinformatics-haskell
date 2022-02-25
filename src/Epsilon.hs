-- An overly complicated implementation of Epsilon using functional
-- programming methods.
module Epsilon where

-- The value of epsilon
epsilon = findEpsilon 1
  where
    -- Find the value of epsilon
    findEpsilon x
      | isEpsilon x = x
      | otherwise   = findEpsilon (x / 2)

-- Test if the number is the minimum decimal value allowed by floating
-- point precision.
isEpsilon x = x > 0 && (x / 2) + 1 == 1
