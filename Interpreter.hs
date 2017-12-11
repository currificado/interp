{-# LANGUAGE ApplicativeDo, FlexibleInstances, UndecidableInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Interpreter where

import Control.Monad.Reader
import Control.Monad.Trans.Class
import Data.Char (isAsciiLower, isDigit, isSpace,
                  digitToInt)

-- | Código mon-parsing.hs del curso
import Parser


-- | Tipos
data Expr = Let String Expr Expr
          | Add Expr Expr
          | Div Expr Expr -- se agrega la operación de división
          | Num Int
          | Var String
    deriving Show

data Res a = DivZero
           | Unbound String
           | Res a

newtype ResT m a = ResT { runResT :: m (Res a) }


-- | `Show` instance for `Res`

instance Show a => Show (Res a) where
    show DivZero       = "Error: Division by Zero"
    show (Unbound var) = "Error: Unbound identifier " ++ var
    show (Res a)       = show a

    
    
-- | `Functor`, `Applicative` and `Monad` instances for `Res`

instance Functor Res where
    fmap f DivZero = DivZero
    fmap f (Unbound var) = Unbound var
    fmap f (Res x) = Res (f x)

instance Applicative Res where
    pure x = Res x
    DivZero <*> _ = DivZero
    (Unbound var) <*> _ = Unbound var
    (Res f) <*> something = fmap f something
    
instance Monad Res where
    return x = Res x
    DivZero >>= _ = DivZero
    Unbound var >>= _ = Unbound var
    (Res x) >>= f = f x



-- | `Functor`, `Applicative` and `Monad` instances for `(Res m)`

instance Functor m => Functor (ResT m) where
    -- fmap :: (a -> b) -> ResT m a -> ResT m b
    fmap f = ResT . fmap (fmap f) . runResT

instance Applicative m => Applicative (ResT m) where
    -- pure :: a -> ResT m a
    pure = ResT . pure . pure -- el primer `pure` es el de `Res` mientras que el segundo es el de `m`
    
    -- <*> :: ResT m (a -> b) -> ResT m a -> ResT m b
    (ResT mf) <*> (ResT mx) = ResT $ do rf <- mf -- this version isn't short-circuiting (both `mf` and `mx` will always run)
                                        rx <- mx -- to implement short-circuiting `Monad m` must be required instead of `Applicative m`and `mx` will run or not depending on the value `rf`
                                        return (rf <*> rx)  

instance Monad m => Monad (ResT m) where
    -- return :: a -> ResT m a
    return = ResT . return . return

    -- (>>=) :: ResT m a -> (a -> ResT m b) -> ResT m b
    (ResT mx) >>= f = ResT $ do rx <- mx
                                case rx of DivZero -> return DivZero
                                           (Unbound var) -> return (Unbound var)
                                           (Res x) -> runResT (f x)



-- | `MonadTrans` instance for `ResT`
instance MonadTrans ResT where
    -- lift :: (Monad m) => m a -> ResT m a
    lift = ResT . fmap return
    -- lift mx = ResT $ do x <- mx; return (return x)


mapResT :: (m (Res a) -> n (Res b)) -> ResT m a -> ResT n b
mapResT f = ResT . f . runResT


-- | `MonadReader` instance for `ResT`

instance MonadReader r m => MonadReader r (ResT m) where
    -- ask :: ResT m r
    ask = lift ask
    -- local :: (r -> r) -> ResT m a -> ResT m a
    local = mapResT . local
    -- reader :: (r -> a) -> RestT m a
    reader = lift . reader
    


type Env = [(String, Int)]
type InterM = ResT (Reader Env) Int

-- | Función `interp'

interp :: Expr -> InterM
interp (Let x e1 e2) = do v <- interp e1
                          local ((x,v):) (interp e2)
interp (Num n) = return n
interp (Add e1 e2) = liftM2 (+) (interp e1) (interp e2)
interp (Div e1 e2) = do m <- interp e2
                        if m == 0 then
                            ResT $ return DivZero
                        else
                            do n <- interp e1
                               return (n `div` m)
interp (Var x) = do env <- ask
                    case (lookup x env) of
                        Nothing -> ResT $ return (Unbound x)
                        Just v  -> return v

-- | Función `eval'

eval :: Expr -> Res Int
eval e = runReader (runResT (interp e)) []



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
            pSym 'd'
            pSym 'i'
            pSym 'v'
            spaces1
            e1 <- parser
            spaces1
            e2 <- parser
            spaces
            return (Div e1 e2)
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
