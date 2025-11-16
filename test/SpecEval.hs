{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module SpecEval where

import Test.Hspec
import Syntax
import Eval

import System.IO.Silently

-- Run the test in the IO Monad
evaltest :: IO a -> IO String
evaltest e = do
    output <- capture_ $ ((eval e) )
    pure $ checkdata $ lines output
  where
    checkdata :: [String] -> String
    checkdata (l : _)  = l
    checkdata []       = "No output on stdout found"

specEval :: Spec
specEval = do
  describe "Tests for module Eval" $ do
    it "eval testLit01" $ do
        eval testLit01  `shouldReturn` 34
    it "eval testNeg01" $ do
        eval testNeg01 `shouldReturn` -42
    it "eval testNeg02" $ do
        eval testNeg02 `shouldReturn` 5
    it "eval testAdd01" $ do
        eval testAdd01 `shouldReturn` 42
    it "eval testAdd02" $ do
        eval testAdd02 `shouldReturn` 10

    it "evaltest testQprint01" $ do
        ((evaltest testQprint01):: IO String) `shouldReturn` "42"

testLit01 :: Expr repr => repr Int
testLit01 = int 34

testNeg01 :: Expr repr => repr Int
testNeg01 = neg (int 42)

testNeg02 :: Expr repr => repr Int
testNeg02 = add (int 8) (neg (add (int 1) (int 2)))

testAdd01 :: Expr repr => repr Int
testAdd01 = add (int 8) (int 34)

testAdd02 :: Expr repr => repr Int
testAdd02 = add (add (int 1) (int 2)) (add (int 3) (int 4))

testQprint01 :: Expr repr => repr ()
testQprint01 = qprint (int 42)
