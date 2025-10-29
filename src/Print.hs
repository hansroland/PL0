-- Print - Module with Print interpreters
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Print where

import Syntax

-- The quickprint function should print the expression in the concrete syntax
-- The input is supposed to be processed by the Language Parser

-- Define a quick printer. It produces an easy and quick string of a sentence.
instance Syn1 String where
    lit n
          | n >= 0    = show n
          | otherwise = concat ["(", show n, ")"]
    neg e = concat ["-", e]
    add e1 e2 = concat [ "(", e1, " + ", e2, ")" ]
    sub e1 e2 = concat [ "(", e1, " - ", e2,")" ]
    qprint e = concat [ "qprint ", e ]

-- quickprint: Run the interpreter to create a quick print
-- Note: This is NOT a function of PL0
quickprint :: String -> String
quickprint = id

-- Define a debug printer. It shwos functions and values
newtype StrDebug = StrDebug {undebug :: String}
    deriving (Show, Semigroup, Monoid)

instance Syn1 StrDebug where
    lit n = StrDebug $ concat [ "(lit ", show n, ")"]
    neg e = StrDebug $ concat ["neg (", undebug e , ")"]
    add e1 e2 = StrDebug $ concat ["(add ", undebug e1, " ", undebug e2, ")"]
    sub e1 e2 = StrDebug $ concat ["(sub ", undebug e1, " ", undebug e2, ")"]
    qprint e = StrDebug $ concat [ "qprint ", undebug e ]
strDebug :: StrDebug -> StrDebug
strDebug = id


