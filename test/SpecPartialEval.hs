module SpecPartialEval where

import Test.Hspec
import Syntax
import Print
import PartialEval

specPartialEval :: Spec
specPartialEval = do
  describe "Tests for module PartialEval" $ do
    it "quickprint $ optConst testPart01" $ do
        (quickprint $ optConst testPart01) `shouldBe` "((-3) + -(3 + (getInt \"user input\")))"

-- Optimizing Arithmetic operations
testPart01 :: Expr repr => repr Int
testPart01 = add (neg (add (int 1) (int 2))) (neg (add (int 3) (getInt "user input")))


