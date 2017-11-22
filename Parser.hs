module Parser where

import Control.Monad
import GHC.Base hiding ((<|>))

-- | Parser type 
newtype Parser a = P {runP :: String -> [(a,String)]}

instance Functor Parser where
  fmap f p = P $ \cs -> [(f a,cs') | (a,cs') <- runP p cs]

instance Applicative Parser where
  pure a =  P (\cs -> [(a,cs)])
  -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
  (P p) <*> (P q) = P $ \cs -> [ (f a, cs'')  |  (f , cs')   <- p cs
                                              ,  (a , cs'')  <- q cs']

instance Monad Parser where
  return a    = P $ \cs -> [(a,cs)]
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a,cs') <- p cs]

-- | Parsers primitivos

pFail :: Parser a
pFail = P $ \cs -> []

-- no determinista
(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> p cs ++ q cs

-- determinista
(<||>) :: Parser a -> Parser a -> Parser a
(P p) <||> (P q) = P $ \cs -> case p cs ++ q cs of
                              []     -> []
                              (x:xs) -> [x]

item :: Parser Char
item = P $ \cs -> case cs of
                    ""     -> []
                    (c:cs) -> [(c,cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do c <- item
            if p c then return c
                   else pFail

pSym :: Char -> Parser Char
pSym c = pSat (== c)

-- | recursión
-- | cero o más veces p

pList :: Parser a -> Parser [a]
pList p = do a <- p
             as <- pList p
             return (a:as)
          <|>
          return [] 
          
-- | una o más veces p

pList1 :: Parser a -> Parser [a]
pList1 p = do a <- p
              as <- pList p
              return (a:as)
