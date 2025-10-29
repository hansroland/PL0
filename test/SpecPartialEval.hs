module SpecPartialEval where

import Test.Hspec
import Syntax
import Print
import PartialEval

specPartialEval :: Spec
specPartialEval = do
  describe "Tests for module PartialEval" $ do
    it "quickprint $ fst3 $ optAdd testPart01" $ do
        (quickprint $ fst3 $ optAdd testPart01) `shouldBe` "10"

-- Parts of the syntax
testPart01 :: Syn1 r1  => r1
testPart01 = add (add (lit 1) (lit 2)) (add (lit 3) (lit 4))


