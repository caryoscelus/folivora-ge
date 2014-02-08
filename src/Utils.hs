module Utils where

import Control.Applicative
import Data.Foldable

import Linear.V2

instance (Enum a) => Enum (V2 a) where
    succ = fmap succ

instance (Real a) => Real (V2 a) where
    toRational _ = error "why would you convert vector to rational?"

instance (Integral a) => Integral (V2 a) where
    quotRem a b = (liftA2 quot a b, liftA2 rem a b)
    divMod  a b = (liftA2 div  a b, liftA2 mod a b)
    toInteger _ = error "why would you convert vector to integer?"


data Table a = Table { getTC :: [[a]]
                     , getXs :: Int
                     , getYs :: Int
                     } deriving (Show)

setTC :: [[a]] -> Table b -> Table a
setTC c t = t { getTC = c }

getCell :: V2 Int -> Table a -> a
getCell (V2 x y) t = getTC t !! y !! x

setCell :: V2 Int -> a -> Table a -> Table a
setCell (V2 x y) cell t = setTC (take y tt ++ [line] ++ drop (y+1) tt) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = tt !! y
          tt = getTC t

instance Functor Table where
    fmap f t = setTC (fmap f' $ getTC t) t
        where
            f' = fmap f

instance Foldable Table where
    foldMap f t = foldMap f' $ getTC t
        where
            f' = foldMap f

table :: a -> Int -> Int -> Table a
table empty x y | x > 0 || y > 0 = let line = take x (repeat empty)
                                   in  Table (take y (repeat line)) x y
                | otherwise      = error "non-positive table size"

