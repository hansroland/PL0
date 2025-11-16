{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- A Partial Evaluator
-- The compiler eagerly computes the parts of the program that do not
--    depend on any inputs, a process known as partial evaluation
--    (Jones, Gomard, and Sestoft 1993).
--    See: https://hackmd.io/@gridtools/BJ-tiaCSY

-- This interpreter is written in the framework of Oleg Kiselyov.
--    See: https://okmij.org/ftp/tagless-final/course/optimizations.html

-- Operations with multiple constants are evaluated at compile time
module PartialEval (optConstants) where

import Syntax
import RR

-- Data structure to optimize constants
data OptConst repr a where
  Unk :: repr a -> OptConst repr a
  Opt :: Int -> OptConst repr Int

-- Define (OptConst repr) as an instance of RR
instance Expr repr => RR OptConst repr where
  bwd (Unk e) = e
  bwd (Opt n) = int n
  fwd = Unk

-- Define an interpreter to optimize arithmetic operations
instance Expr repr => Expr (OptConst repr) where
  int n = Opt n
  neg (Opt n) = Opt $ negate n
  neg (Unk n) = Unk $ neg n
  add (Opt n1) (Opt n2) = Opt (n1 + n2)
  add e1 e2  = Unk $ add (bwd e1) (bwd e2)
  sub (Opt n1) (Opt n2) = Opt (n1 - n2)
  sub e1 e2  = Unk $ sub (bwd e1) (bwd e2)
  qprint e1  = Unk $ qprint (bwd e1)
  getInt es  = Unk $ getInt es

-- Optimize constants
optConst:: Expr repr => OptConst repr a -> repr a
optConst = bwd
