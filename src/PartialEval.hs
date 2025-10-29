-- A Partial Evaluator
-- The compiler eagerly computes the parts of the program that do not
--    depend on any inputs, a process known as partial evaluation
--    (Jones, Gomard, and Sestoft 1993).

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PartialEval where

import Syntax

-- Interpreter: Optimize formula - sum up constant terms
--   This is an interpreter with additional state
instance (Syn1 repr) => Syn1 (repr, Bool, Int) where
    lit n = (lit n, True, n)
    neg (e, b, n) = (neg e, b, negate n)
    add (e1, b1, n1) (e2, b2, n2)
        | bb1 && bb2 = (lit sum12, True, sum12)
        | otherwise = (add e1 e2, False, sum12)
      where
        (_, bb1,nn1) = optAdd (e1, b1, n1)
        (_, bb2,nn2) = optAdd (e2, b2, n2)
        sum12 = nn1 + nn2
    sub (e1, b1, n1) (e2, b2, n2)
        | bb1 && bb2 = (lit sub12, True, sub12)
        | otherwise = (add e1 e2, False, sub12)
      where
        (_, bb1,nn1) = optAdd (e1, b1, n1)
        (_, bb2,nn2) = optAdd (e2, b2, n2)
        sub12 = nn1 - nn2
    qprint e = e

optAdd :: (repr, Bool, Int) -> (repr, Bool, Int)
optAdd = id

fst3 :: Syn1 repr => (repr, Bool, Int) -> repr
fst3 (r, _, _) = r

optimizeAdd :: Syn1 repr => (repr, Bool, Int) -> repr
optimizeAdd e = fst3 $ optAdd e
