module SpecLexer where
{-}
import Test.Hspec
import Lexer(Token(..), Operator(..), lexScan)

-- Lexer Tests
specLexer :: Spec
specLexer = do
  describe "Test Lexer" $ do
    it "show $ lexScan testLex01" $ do
        (lexScan testLex01) `shouldBe`
            Right [((1,1),LexLParen),((1,2),LexInt 111),((1,7),LexSumOp Plus),
              ((1,11),LexInt 2222), ((1,17),LexSumOp Plus),((1,20),LexLParen),
              ((1,22),LexInt 3333), ((1,28),LexSumOp Plus), ((1,31),LexInt 4444),
              ((1,35),LexRParen),((1,36),LexRParen)]

testLex01 :: String
testLex01 = "(111  +   2222  +  ( 3333  +  4444))"
--           1234567890123456789012345678901234567
--                    1         2         3
-}