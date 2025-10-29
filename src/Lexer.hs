module Lexer
     ( Token(..)
     , PosToken
     , Operator(..)
     , lexScan
     , token
     , pos
     ) where

import qualified Hlex as Hlex
import Text.Printf (FormatSign(SignPlus))

-- Specialize the generic data types from the Utils.Hlex to the PL0 language
type Grammar  = Hlex.Grammar Token
type PosToken = Hlex.PosToken Token
type Lexer = Hlex.PosLexer Token       -- our lexers are always with positions

data Operator = Plus
              | Minus
     deriving (Read, Show, Eq)

-- Token: Tokens of the PL0 language
data Token = LexIdent String
           | LexKeyword String
           | LexString String
--           | Number Float
           | LexInt Int
--           | Assign
           | LexLParen
           | LexRParen
           | LexSumOp Operator
           deriving(Read, Show, Eq)

--  Extract the effective Token from a PosToken
token :: PosToken -> Token
token = snd

-- Extract the Position information from a PosToken
pos :: PosToken -> (Int,Int)
pos = fst

-- Grammar for our Lexer
grammar :: Grammar
grammar = [ Hlex.Error "\"[^\"]*\n" "Can't have a new line in a string"
          , Hlex.Tokenize "\"[^\"]*\"" $ LexString . init . tail
--          , Hlex.JustToken "=" Assign
          , Hlex.JustToken "\\+" $ LexSumOp Plus
          , Hlex.JustToken "\\-" $ LexSumOp Minus
          , Hlex.JustToken "\\(" LexLParen
          , Hlex.JustToken "\\)" LexRParen
          , Hlex.Tokenize "[a-zA-Z]+" $
               \str -> if str `elem` keywords then LexKeyword str else LexIdent str
          , Hlex.Tokenize "[0-9]+" $ LexInt . read
--          , Tokenize "[0-9]+(\\.[0-9]+)?" $ Number . read
          , Hlex.Skip "[ \n\r\t]+"
          ]

lexScan :: Lexer
lexScan = Hlex.hlex grammar

keywords :: [String]
keywords = ["getint"]
