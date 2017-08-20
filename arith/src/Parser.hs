module Parser where

import Prelude hiding ((<$>), (<*>))

type Parser a = String -> [(a, String)]

(+++) :: Parser a -> Parser a -> Parser a
(p +++ q) s = p s ++ q s

punit :: a -> Parser a
punit a s = [(a, s)]

pfail :: Parser a
pfail = const []

eof :: Parser ()
eof s = if null s then [((), s)] else []

(<$>) :: (a -> b) -> (Parser a -> Parser b)
(f <$> p) s = [ (f a, t) | (a, t) <- p s ]

(<*>) :: Parser (a -> b) -> (Parser a -> Parser b)
(p <*> q) s = [ (f a, u) | (f, t) <- p s, (a, u) <- q t ]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p (c:cs) | p c = [(c, cs)]
satisfy p _            = []

char :: Char -> Parser Char
char c = satisfy (c ==)

many :: Parser a -> Parser [a]
many p = punit [] +++ many1 p

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

pair :: Parser a -> Parser b -> Parser (a, b)
pair p q = (,) <$> p <*> q

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = const const <$> open <*> p <*> close

