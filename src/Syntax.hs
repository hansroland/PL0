-- Abstract Syntax Tree for PL0
-- Tagless final implementation

module Syntax where

class Expr repr where
    int :: Int -> repr Int                      -- int literal
    neg :: repr Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    sub :: repr Int -> repr Int -> repr Int

    getInt :: String -> repr Int                -- read from user
    qprint :: repr Int -> repr ()               -- write to user

