{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module PL0Spec (spec) where

import Test.Hspec
import Syntax
import Eval
import Print
import PartialEval
import Lexer
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

spec :: Spec
spec = do
  describe "Test Lexer" $ do
    it "show $ lexScan testLex01" $ do
        (lexScan testLex01) `shouldBe`
            Right [((1,1),LexLParen),((1,2),LexInt 111),((1,7),LexSumOp Plus),
              ((1,11),LexInt 2222), ((1,17),LexSumOp Plus),((1,20),LexLParen),
              ((1,22),LexInt 3333), ((1,28),LexSumOp Plus), ((1,31),LexInt 4444),
              ((1,35),LexRParen),((1,36),LexRParen)]
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

  describe "Tests for module Print - quickprint" $ do
    it "quickprint evalLit01" $ do
        quickprint testLit01 `shouldBe` "qprint 34"
    it "quickprint testNeg01" $ do
        quickprint testNeg01 `shouldBe` "qprint -42"
    it "quickprint testNeg02" $ do
        quickprint testNeg02 `shouldBe` "qprint (8 + -(1 + 2))"
    it "quickprint testAdd01" $ do
        quickprint testAdd01 `shouldBe` "qprint (8 + 34)"
    it "quickprint testAdd02" $ do
        quickprint testAdd02 `shouldBe` "qprint ((1 + 2) + (3 + 4))"
    it "quickprint testSub01" $ do
        quickprint testSub01 `shouldBe` "qprint ((-8) - (-50))"
    it "quickprint testSub02" $ do
        quickprint testSub02 `shouldBe` "qprint ((1 - 2) - (3 - 4))"

  describe "Tests for module Print - StrDebug" $ do
    it "undebug $ strDebug testLit01" $ do
        (undebug $ strDebug testLit01) `shouldBe` "qprint (lit 34)"
    it "undebug $ strDebug testNeg02" $ do
        (undebug $ strDebug testNeg02) `shouldBe` "qprint (add (lit 8) neg ((add (lit 1) (lit 2))))"

  describe "Tests for module PartialEval" $ do
    it "quickprint $ fst3 $ optAdd testPart01" $ do
        (quickprint $ fst3 $ optAdd testPart01) `shouldBe` "10"

-- Lexer Tests
testLex01 :: String
testLex01 = "(111  +   2222  +  ( 3333  +  4444))"
--           1234567890123456789012345678901234567
--                    1         2         3

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

testSub01 :: Syn1 r1  => r1
testSub01 = qprint (sub (lit (-8)) (lit (-50)))

testSub02 :: Syn1 r1  => r1
testSub02 = qprint (sub (sub (lit 1) (lit 2)) (sub (lit 3) (lit 4)))

-- Parts of the syntax

testPart01 :: Syn1 r1  => r1
testPart01 = add (add (lit 1) (lit 2)) (add (lit 3) (lit 4))
