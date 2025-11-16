{-# OPTIONS_GHC -Wno-orphans #-}

-- Print - Module with Print interpreters

module Print (quickprint, debugprint)  where

import Syntax ( Expr(..) )

-- See also: https://stackoverflow.com/questions/25733889/how-to-reinterpret-a-dsl-term-in-final-tagless-approach

-- The quickprint function should print the expression in the concrete syntax
-- The input is supposed to be processed by the Language Parser
newtype QP a = QP String deriving Show
unQP :: QP a -> String
unQP (QP x) = x

instance Expr QP where
    int n
        | n >= 0    = QP (show n)
        | otherwise = QP (concat ["(", show n, ")"])
    -- bool b = R b
    -- lam f = R (unR . f . R)
    -- app e1 e2 = R( (unR e1) (unR e2) )
    -- fix f = R( fx (unR . f . R)) where fx f = f (fx f)
    neg e = QP $ concat ["-", unQP e]
    add e1 e2 = QP ( concat["(", unQP e1, " + ", unQP e2, ")" ] )
    sub e1 e2 = QP ( concat["(", unQP e1, " - ", unQP e2, ")" ] )
    -- mul e1 e2 = R( (unR e1) * (unR e2) )
    -- eq e1 e2 = R( (unR e1) == (unR e2) )
    -- leq e1 e2 = R( (unR e1) <= (unR e2) )
    -- if_ be et ee = R( if (unR be) then unR et else unR ee )
    getInt p1 = QP (concat ["(getInt ",  "\"", p1, "\")"])
    qprint e = QP $ concat ["qprint ", unQP e]

quickprint :: QP a -> String
quickprint = unQP


-- Define a debug printer. It produces a output similar to the tagless final input syntax
newtype DP a = DP String deriving Show
unDP :: DP a -> String
unDP (DP x) = x

instance Expr DP where
    int n = DP $ concat ["(int ", show n, ")"]
    neg e = DP $ concat ["neg (", unDP e, ")"]
    add e1 e2 = DP $ concat [ "(add ", unDP e1, " ", unDP e2, ")" ]
    sub e1 e2 = DP $ concat [ "(sub ", unDP e1, " ", unDP e2,")" ]
    getInt p = DP $ concat [ "(getInt \"", p,   "\")"]
    qprint e = DP $ concat [ "qprint ", unDP e ]

debugprint :: DP a -> String
debugprint = unDP
