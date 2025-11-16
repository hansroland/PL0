module PL0Spec (spec) where
import Test.Hspec
import SpecPrint (specPrint)
import SpecEval  (specEval)
{-}
-- import SpecLexer (specLexer)
-- import SpecPartialEval (specPartialEval)
-}
-- Main module for test driver
spec :: Spec
spec = do
  -- specLexer
  specPrint
  specEval
  -- specPartialEval

