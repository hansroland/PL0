module SpecPrint where

import Test.Hspec
import Syntax
import Print

-- Tests for the Print interpreters
specPrint :: Spec
specPrint = do
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
