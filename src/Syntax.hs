-- Abstract Syntax Tree for PL0
-- Tagless final implementation

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Syntax where

-- Syntax of the language
class Syn1 r1  where
    lit :: Int -> r1
    neg :: r1 -> r1
    add :: r1 -> r1 -> r1
    sub :: r1 -> r1 -> r1
    qprint :: r1 -> r1

data Expr  =
    Lit Int
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Qprint Expr
  deriving (Eq, Show)

toTagless :: Syn1 r1 => Expr -> r1
toTagless (Lit n) = lit n
toTagless (Neg e) = neg $ toTagless e
toTagless (Add e1 e2) = add (toTagless e1) (toTagless e2)
toTagless (Sub e1 e2) = sub (toTagless e1) (toTagless e2)
toTagless (Qprint e) = qprint $ toTagless e

-- We transform the final tagless representation to an Expression.(*)
--  Expr is always about numbers for the moment.
instance Syn1 Expr where
    lit n = Lit n
    neg n = Neg $ toExpr n
    add e1 e2 = Add (toExpr e1) (toExpr e2)
    sub e1 e2 = Sub (toExpr e1) (toExpr e2)
    qprint e = Qprint e
toExpr :: Expr -> Expr
toExpr = id

stringToInt :: String -> Int
stringToInt s = read s



