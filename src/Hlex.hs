{-|
Module      : Utils.Hlex
Description : Lexer creation tools
Copyright   : (c) Sebastian Tee, 2023
License     : MIT

Tools needed to create a 'Lexer' from a lexical 'Grammar'.
-}
{-
Modifications by Roland Senn:
 - Add row and column information to every token.
-}
module Hlex
     ( -- * Example
       -- $example

       -- * Types
       Grammar
     , GrammarRule(..)
     , PosLexer
     , PosToken
       -- ** Exceptions
     , LexException(..)
       -- * Functions
     , hlex
     ) where

import Text.Regex.TDFA ((=~))
import Data.Maybe (maybeToList)

-- | Exception thrown when a 'Lexer' encounters an error when lexxing a string.
data LexException
  = UnmatchedException -- ^ Exception thrown when a substring cannot be matched.
    Int -- ^ The line number where the substring that couldn't be lexed is located.
    Int -- ^ The column where the substring that couldn't be lexed is located.
    String -- ^ The subtring that couldn't be lexed.
  | MatchedException -- ^ Exception thrown when a macth is found on the 'Error' 'GrammarRule'.
    Int -- ^ The line number where the matched string is located.
    Int -- ^ The column where the matched string is located.
    String -- ^ The matched string.
    String -- ^ Error message.
  deriving(Read, Show, Eq)

-- | These are the individual rules that make up a 'Grammar'.
--
-- Takes a __POSIX regular expression__ then converts it to a token or skips it.
data GrammarRule token
  = Skip -- ^ Skips over any matches.
    String -- ^ Regular expression.
  | Tokenize -- ^ Takes a function that converts the matched string to a token.
    String -- ^ Regular expression.
    (String -> token) -- ^ Function that converts the matched string into a token.
  | JustToken -- ^ Converts any regular expression matches to a given token.
    String -- ^ Regular expression.
    token -- ^ Given token.
  | Error -- ^ Returns an error with a message when a match occurs.
    String -- ^ Regular expression.
    String -- ^ Error message.

-- | Lexical grammar made up of 'GrammarRule's.
--
-- The __order is important__. The 'Lexer' will apply each 'GrammarRule' rule in the order listed.
type Grammar token = [GrammarRule token]

-- | A token with a position
type PosToken token = ((Int,Int), token)

-- | Converts a string into a list of tokens with positions.
-- If the string does not follow the Lexer's 'Grammar' a 'LexException' will be returned.
-- type Lexer token = String -> Either LexException [token]

type PosLexer token = String -> Either LexException [PosToken token]

-- | Takes a given 'Grammar' and turns it into a 'Lexer'.
hlex :: Grammar token -> PosLexer token
hlex = hlex' 1 1

hlex' :: Int -> Int -> Grammar token -> PosLexer token
hlex' _ _ _ [] = Right []
hlex' row col tzss@(tz:tzs) program =
  if null matchedText
  then hlex' row col tzs program
  else case tz of
    Error _ errMessage -> Left $ uncurry MatchedException (getLastCharPos row col beforeProgram) matchedText errMessage
    Skip _ -> lexCont Nothing
    Tokenize _ f -> lexCont $ Just $ ((row,col'), f matchedText)
    JustToken _ token -> lexCont $ Just ((row,col'), token)
  where
    (beforeProgram, matchedText, afterProgram) = program =~ getRegex tz :: (String, String, String)
    (_,c) = getLastCharPos row col (beforeProgram ++ matchedText)
    col' = c - length matchedText
    lexCont t = do
      before <- hlex' row col tzs beforeProgram
      after <- uncurry hlex' (getLastCharPos row col (beforeProgram ++ matchedText)) tzss afterProgram
      Right $ before ++ maybeToList t ++ after
hlex' row col _ invalidString = Left $ UnmatchedException row col invalidString

getLastCharPos :: Int -> Int -> String -> (Int, Int)
getLastCharPos startRow startCol x = (startRow + addRow, addCol + if addRow == 0 then startCol else 1)
  where
    ls = lines x
    addRow = length ls - 1
    addCol = length $ last ls

getRegex :: GrammarRule token -> String
getRegex (Skip regex) = regex
getRegex (Tokenize regex _) = regex
getRegex (JustToken regex _) = regex
getRegex (Error regex _) = regex

{-
Copyright (c) Sebastian Tee, 2023

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the “Software”), to deal in the Software without
restriction, including without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom
the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}