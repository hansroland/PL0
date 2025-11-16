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
        quickprint testAdd01 `shouldBe` "(8 + 34)"
    it "quickprint testAdd02" $ do
        quickprint testAdd02 `shouldBe` "qprint ((1 + 2) + (3 + 4))"
    it "quickprint testSub01" $ do
        quickprint testSub01 `shouldBe` "((-8) - (-50))"
    it "quickprint testSub02" $ do
        quickprint testSub02 `shouldBe` "qprint ((1 - 2) - (3 - 4))"
    it "quickprint testGet01" $ do
        quickprint testGet01 `shouldBe` "((getInt \"Enter first number\") + (getInt \"Enter second number\"))"
  describe "Tests for module Print - StrDebug" $ do
    it "debugprint testLit01" $ do
        (debugprint testLit01) `shouldBe` "qprint (int 34)"
    it "undebug $ strDebug testNeg02" $ do
        (debugprint testNeg02) `shouldBe` "qprint (add (int 8) neg ((add (int 1) (int 2))))"
    it "undebug $ strDebug testGet01" $ do
        (debugprint testGet01) `shouldBe` "(add (getInt \"Enter first number\") (getInt \"Enter second number\"))"

testLit01 :: Expr repr =>  repr ()
testLit01 = qprint (int 34)

testNeg01 :: Expr repr => repr ()
testNeg01 = qprint (neg (int 42))

testNeg02 :: Expr repr => repr ()
testNeg02 = qprint (add (int 8) (neg (add (int 1) (int 2))))

-- testAdd01 ::
testAdd01 :: Expr repr => repr (Int)
testAdd01 = add (int 8) (int 34)

testAdd02 :: Expr repr => repr ()
testAdd02 = qprint (add (add (int 1) (int 2)) (add (int 3) (int 4)))

testSub01 :: Expr repr => repr (Int)
testSub01 = sub (int (-8)) (int (-50))

testSub02 :: Expr repr => repr ()
testSub02 = qprint (sub (sub (int 1) (int 2)) (sub (int 3) (int 4)))

testGet01 :: Expr repr => repr Int
testGet01 = add (getInt "Enter first number") (getInt "Enter second number")
