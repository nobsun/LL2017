{- |
算術式の構文解析器

@
expression ::= additive
additive   ::= multive ('+' multive | '-' multitive)*
multitive  ::= primary ('*' primary | '/' primary)*
primary    ::= '(' expression ')' | number
number     ::= [1-9][0-9]*
@

-}

module Expr where

import Data.Char
import Parser

data Expr = ENum Int
          | EBinOp Expr Op Expr

data Op = Plus  { evalop :: Int -> Int -> Int }
        | Minus { evalop :: Int -> Int -> Int }
        | Times { evalop :: Int -> Int -> Int }
        | Divid { evalop :: Int -> Int -> Int }

pExpression :: Parser Expr
pExpression = pAdditive

pAdditive :: Parser Expr
pAdditive = mkBinOp
    `pFmap` pMultive
    `pApp`  pZeroOrMore (pPair pPlus pMultive `pAlt` pPair pMinus pMultive)

mkBinOp :: Expr -> [(Op, Expr)] -> Expr
mkBinOp = foldl (uncurry . EBinOp)

pPlus, pMinus, pTimes, pDivid :: Parser Op
pPlus  = const (Plus  (+)) `pFmap` pChar '+'
pMinus = const (Minus (-)) `pFmap` pChar '-'
pTimes = const (Times (*)) `pFmap` pChar '*'
pDivid = const (Divid div) `pFmap` pChar '/'

pMultive :: Parser Expr
pMultive = mkBinOp
   `pFmap` pPrimary
   `pApp`  pZeroOrMore (pPair pTimes pPrimary `pAlt` pPair pDivid pPrimary)

pPrimary :: Parser Expr
pPrimary = pBetween (pChar '(') (pChar ')') pExpression `pAlt` pNumber

pNumber :: Parser Expr
pNumber = (ENum . read) `pFmap` pNumString

pNumString :: Parser String
pNumString = (:)
     `pFmap` pSat (`elem` "123456789")
     `pApp`  pZeroOrMore (pSat (`elem` "0123456789"))

pChar :: Char -> Parser Char
pChar c = pSat (c ==)

--

eval :: Expr -> Int
eval (ENum n) = n
eval (EBinOp e1 op e2) = evalop op (eval e1) (eval e2)

--

instance Show Expr where
  show (ENum i)          = show i
  show (EBinOp e1 op e2) = show' e1 ++ show op ++ show' e2
    where
      show' e@(EBinOp _ _ _) = "("++show e++")"
      show' e                = show e

instance Read Expr where
  readsPrec _ = pExpression

instance Show Op where
  show (Plus _)  = "+"
  show (Minus _) = "-"
  show (Times _) = "*"
  show (Divid _) = "/"
