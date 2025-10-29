-- Evaluator (aka Language Interpreter) for the first languagne define in LangInt.
--
-- Implement an instance of the LangInt.Syntax type class Syn1

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Eval where

import Syntax

-- This is a classic interpreter for a standard ADT Syntax tree.
-- It has nothing to do with tagless final.(*)
-- It runs in the IO Monad and allows us to keep IO out of the
-- type classes used to define our language.
evalExpr :: Expr -> IO Int
evalExpr (Lit n) = pure n
evalExpr (Neg expr) = do
    e <- evalExpr expr
    pure $ negate e
evalExpr (Add exp1 exp2) = do
    e1 <- evalExpr exp1
    e2 <- evalExpr exp2
    pure (e1 + e2)
evalExpr (Sub exp1 exp2) = do
    e1 <- evalExpr exp1
    e2 <- evalExpr exp2
    pure (e1 - e2)
evalExpr (Qprint expr) = do
    e <- evalExpr expr
    putStrLn $ show e
    return 0
