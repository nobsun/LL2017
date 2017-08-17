module Parser where

type Parser a = String -> [(a, String)]

pUnit :: a -> Parser a
pUnit a s = [(a, s)]

pFail :: Parser a
pFail s = []

pFmap :: (a -> b) -> (Parser a -> Parser b)
pFmap f p s = [ (f a, t) | (a, t) <- p s ]

pApp :: Parser (a -> b) -> (Parser a -> Parser b)
pApp pf pa s = [ (f a, u) | (f, t) <- pf s, (a, u) <- pa t ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pUnit [] `pAlt` pOneOrMore p

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = (:) `pFmap` p `pApp` pZeroOrMore p

pReplicate :: Int -> Parser a -> Parser [a]
pReplicate 0 _ = pUnit []
pReplicate n p = (:) `pFmap` p `pApp` pReplicate (n-1) p

pAlt :: Parser a -> Parser a -> Parser a
pAlt p q s = p s ++ q s

pSat :: (Char -> Bool) -> Parser Char
pSat p (c:cs) | p c = [(c, cs)]
pSat p _            = []

pBetween :: Parser open -> Parser close -> Parser a -> Parser a
pBetween open close p s = [ (a, v) | (_, t) <- open s
                                   , (a, u) <- p t
                                   , (_, v) <- close u ]

pPair :: Parser a -> Parser b -> Parser (a, b)
pPair p q s = [ ((a, b), u) | (a, t) <- p s, (b, u) <- q t ]

--

newtype Parser' a = Parser' { parse :: String -> [(a, String)] }

instance Functor Parser' where
  fmap f p = Parser' (pFmap f (parse p))

instance Applicative Parser' where
  pure a  = Parser' (pUnit a)
  p <*> q = Parser' (pApp (parse p) (parse q))
