module SpecEval where

import Test.Hspec
import Syntax
import Eval

import System.IO.Silently

-- This is very hacky! But we get always 2 lines!!!
-- I don't know yet why !!!
evaltest :: Expr -> IO String
evaltest e = do
    (output, _) <- capture $ evalExpr $ toExpr e
    pure $ checkdata $ lines output
  where
    checkdata :: [String] -> String
    checkdata (l : _)  = l
    checkdata []       = "No output on stdout found"

specEval :: Spec
specEval = do
  describe "Tests for module Eval" $ do
    it "evaltest testLit01" $ do
        evaltest testLit01 `shouldReturn` "34"
    it "evaltest testNeg01" $ do
        evaltest testNeg01 `shouldReturn` "-42"
    it "evaltest testNeg02" $ do
        evaltest testNeg02 `shouldReturn` "5"
    it "evaltest testAdd01" $ do
        evaltest testAdd01 `shouldReturn` "42"
    it "evaltest testAdd02" $ do
        evaltest testAdd02 `shouldReturn` "10"

testLit01 :: Syn1 r1 => r1
testLit01 = qprint (lit 34)

testNeg01 :: Syn1 r1 => r1
testNeg01 = qprint (neg (lit 42))

testNeg02 :: Syn1 r1 => r1
testNeg02 = qprint (add (lit 8) (neg (add (lit 1) (lit 2))))

testAdd01 :: Syn1 r1  => r1
testAdd01 = qprint (add (lit 8) (lit 34))

testAdd02 :: Syn1 r1  => r1
testAdd02 = qprint (add (add (lit 1) (lit 2)) (add (lit 3) (lit 4)))



