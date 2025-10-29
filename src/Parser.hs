{-# Language NamedFieldPuns #-}

module Parser where

import Lexer
import Syntax
-- import Print
-- import qualified Hlex as Hlex (LexException)

import Control.Monad.State.Strict
import Control.Monad.Except

data ParserState = ParserState {
    psToks :: [PosToken]
    }

-- type Parser a = ExceptT String (State ParserState) a
-- type Parser a = StateT ParserState (Either String) a

type Parser = ExceptT String (State ParserState)

-- Peek at the next token
peekPosTok :: Parser PosToken
peekPosTok = do
    ps <- get
    case psToks ps of
        t:_ -> pure $ t
        []  -> throwError "peekPosTok: End of Tokens"

-- Read, remove and return the next PosToken
getPosToken :: Parser PosToken
getPosToken = do
    pstate <- get
    case psToks pstate of
        []  -> throwError "Error in getPosToken: End of Tokens"
        (p : pss) -> do
            put $ pstate {psToks = pss}
            pure p

skipToken :: Parser ()
skipToken = do
    _ <- getPosToken
    pure ()

parserError :: PosToken -> String -> Parser Expr
parserError pt str = do
    throwError ("ERROR: " ++ show (pos pt) ++ str )

-- First we do a regular AST
parse :: [PosToken] -> Either String Expr
parse toks = evalState (runExceptT (parseExpr)) initialState
      where initialState = ParserState {psToks = toks}

-- parseExpr :: Parser Expr
parseExpr :: Parser Expr
parseExpr = do
    f1 <- parseFactor
    p <- peekPosTok
    case (token p) of
       (LexSumOp op) -> do
          -- Accept the peeked PosToken
          () <- skipToken
          f2 <- parseTerm
          let node = (if op == Plus then Add else Sub)
          pure $ node f1 f2
       _ -> pure f1

parseTerm :: Parser Expr
parseTerm = parseFactor

-- parse a Factor
parseFactor :: Parser Expr
parseFactor = do
    posTok <- getPosToken
    case token posTok of
        (LexInt n) -> pure $ Lit n
        LexLParen -> do
            expr <- parseExpr
            rp <- getPosToken
            case (token rp) of
                LexRParen -> pure expr
                _ -> parserError posTok "Missing right parenthesis"
        _ -> parserError posTok "Error in parseFactor"

{-}
main :: IO ()
main = do
    let toks = checkTokens $ lexScan testStr
    putStrLn ("Output of the lexer: " ++ show (length toks))
    putStrLn $ show toks
    let expr = parse toks
    putStrLn $ show expr

-- Test
testStr :: String
testStr = "10 + 32"

checkTokens :: Either Hlex.LexException [PosToken] -> [PosToken]
checkTokens (Left ex) = error $ show ex
checkTokens (Right toks) = toks
-}

{-}
checkLit :: PosToken -> Expr
checkLit pt = case getToken  pt of
    (LexInt n) ->  Lit n
    _          -> error ("Token not a Literal: " ++ show pt)
-}