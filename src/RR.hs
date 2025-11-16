{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- See: https://okmij.org/ftp/tagless-final/course/optimizations.html
-- See: https://okmij.org/ftp/tagless-final/course/RR.hs

-- Optimization framework: Automate to write the Partial Evaluators

-- * Reflection-Reification pair

module RR where

class RR t repr where                  -- * Reflection-Reification pair
  fwd :: repr a -> t repr a                 -- Reflection:  Map to the annotated functions
  bwd :: t repr a -> repr a                 -- Reification: Remove the annotations

  map1 :: (repr a -> repr b) -> (t repr a -> t repr b)
  map1 f = fwd . f . bwd

  map2 :: (repr a -> repr b -> repr c) ->
          (t repr a -> t repr b -> t repr c)
  map2 f e1 e2 = fwd (f (bwd e1) (bwd e2))
