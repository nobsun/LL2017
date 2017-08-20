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

import Prelude hiding ((<$>),(<*>))
import Data.Char (isDigit)
import Parser

data Expr = ENum Int
          | EBinOp Expr Op Expr

data Op = Plus | Minus | Times | Divid

pExpression :: Parser Expr
pExpression = pAdditive

pAdditive :: Parser Expr
pAdditive = mkBinOp
        <$> pMultitive
        <*> many (pair (pPlus +++ pMinus) pMultitive)

pMultitive :: Parser Expr
pMultitive = mkBinOp
         <$> pPrimary
         <*> many (pair (pTimes +++ pDivid) pPrimary)

pPrimary :: Parser Expr
pPrimary = between (char '(') (char ')') pExpression +++ pNumber

pNumber :: Parser Expr
pNumber = (ENum . read) <$> many1 (satisfy isDigit)

pPlus, pMinus, pTimes, pDivid :: Parser Op
pPlus  = const Plus  <$> char '+'
pMinus = const Minus <$> char '-'
pTimes = const Times <$> char '*'
pDivid = const Divid <$> char '/'

mkBinOp :: Expr -> [(Op, Expr)] -> Expr
mkBinOp = foldl (uncurry . EBinOp)

--

eval :: Expr -> Int
eval (ENum n) = n
eval (EBinOp e1 op e2) = evalop op (eval e1) (eval e2)

evalop :: Op -> (Int -> Int -> Int)
evalop Plus  = (+)
evalop Minus = (-)
evalop Times = (*)
evalop Divid = div

--

instance Show Expr where
  show (ENum i)          = show i
  show (EBinOp e1 op e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

instance Read Expr where
  readsPrec _ = pExpression

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Times = "*"
  show Divid = "/"

eof :: Parser ()
eof "" = [((),"")]
eof _  = []

--
{- |
>>> test ("1+2","(1+2)")
True
-}
test :: Test -> Bool
test (p, a) = show (read p :: Expr) == a

type Test    = (Problem, Answer)
type Problem = String
type Answer  = String
