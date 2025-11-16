{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- Evaluator (aka Language Interpreter) for the PL0 language

module Eval where

import Control.Monad.IO.Class
import Syntax

{-}
-- General Formal of a standard interpreter (without partial evaluators and IO!)
newtype R a = R a deriving Show
unR :: R a -> a
unR (R x) = x

instance Expr R where
    int x = R x
    -- bool b = R b

    -- lam f = R (unR . f . R)
    -- app e1 e2 = R( (unR e1) (unR e2) )
    -- fix f = R( fx (unR . f . R)) where fx f = f (fx f)
    neg e = R( unR e)
    add e1 e2 = R( (unR e1) + (unR e2) )
    sub e1 e2 = R( (unR e1) - (unR e2) )


    -- mul e1 e2 = R( (unR e1) * (unR e2) )
    -- eq e1 e2 = R( (unR e1) == (unR e2) )

    -- leq e1 e2 = R( (unR e1) <= (unR e2) )
    -- if_ be et ee = R( if (unR be) then unR et else unR ee )

compR :: R a -> a
compR = unR

mktestR :: (() -> R a) -> a
mktestR f = compR (f ())
-}

-- This is our evaluator with IO . We can't newtype IO, so it's unique
instance Expr IO where
    int x = pure x
    neg e = negate <$> e
    add = liftA2 (+)
    sub = liftA2 (-)
    getInt p = do
        liftIO $ putStrLn p
        str <- getLine
        return $ stringToInt str

    qprint eo = do
      e <- eo
      liftIO $ putStrLn $ show e

eval :: Expr IO => IO e -> IO e
eval= id

stringToInt :: String -> Int
stringToInt s = read s

-- Test example here
-- TODO: Make a test in the testdriver, with the 2 inputs in a file
ex01 :: Expr repr => repr Int
ex01 = add (getInt "Enter first integer") (getInt "Enter second integer")
