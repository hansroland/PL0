module PL0Spec (spec) where
import Test.Hspec
import SpecPrint (specPrint)
import SpecEval  (specEval)
import SpecPartialEval (specPartialEval)

-- import SpecLexer (specLexer)

-- Main module for test driver
spec :: Spec
spec = do
  -- specLexer
  specPrint
  specEval
  specPartialEval

