module Interpreter where

import Control.Monad.Reader
import Data.Char (isAsciiLower, isDigit, isSpace,
                  digitToInt)
import Data.Maybe (fromMaybe)

-- | Código mon-parsing.hs del curso
import Parser

-- | Tipos
data Expr = Let String Expr Expr
          | Add Expr Expr
          | Num Int
          | Var String
    deriving (Show, Eq)

type Env = [(String, Int)]
type InterM = Reader Env Int

-- | Función `interp'
interp :: Expr -> InterM
interp (Let x e1 e2) = do
                       v <- interp e1
                       local ((x,v):) (interp e2)
interp (Num n) = return n
interp (Add e1 e2) = liftM2 (+) (interp e1) (interp e2)
interp (Var x) = do
                 env <- ask
                 return $ fromMaybe (error ("Error: Unbound Identifier " ++ x)) (lookup x env)

-- | Función `eval'
eval :: Expr -> Int
eval e = runReader (interp e) []

-- | Helper parsers
digit :: Parser Int
digit = do c <- pSat isDigit
           return (digitToInt c)

nat :: Parser Int
nat = fmap (foldl f 0) (pList1 digit)
          where
            f acc d = acc * 10 + d

lower :: Parser Char
lower = pSat isAsciiLower

varname :: Parser String
varname = pList1 lower

space :: Parser Char
space = pSat isSpace

spaces, spaces1 :: Parser String
spaces = pList space
spaces1 = pList1 space

-- | Definición de `parser'
parser :: Parser Expr
parser = do spaces
            pSym 'l'
            pSym 'e'
            pSym 't'
            spaces1
            x <- varname
            spaces1
            e1 <- parser
            spaces1
            e2 <- parser
            spaces
            return (Let x e1 e2)
         <|>
         do spaces
            pSym 'a'
            pSym 'd'
            pSym 'd'
            spaces1
            e1 <- parser
            spaces1
            e2 <- parser
            spaces
            return (Add e1 e2)
         <|>
         do spaces
            n <- nat
            spaces
            return (Num n)
         <|>
         do spaces
            var <- varname
            spaces
            return (Var var)
         <|>
         do spaces
            pSym '('
            e <- parser
            pSym ')'
            spaces
            return e
