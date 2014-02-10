module Utils where

import Control.Applicative
import Control.Arrow

import Data.Monoid

import Debug.Trace

import Prelude hiding (any)
import Data.Foldable

import Linear.V2

import System.Random (StdGen, randomR)

instance (Enum a) => Enum (V2 a) where
    succ = fmap succ

instance (Real a) => Real (V2 a) where
    toRational _ = error "why would you convert vector to rational?"

instance (Integral a) => Integral (V2 a) where
    quotRem a b = (liftA2 quot a b, liftA2 rem a b)
    divMod  a b = (liftA2 div  a b, liftA2 mod a b)
    toInteger _ = error "why would you convert vector to integer?"

v2Cast :: (Real a, Fractional b) => V2 a -> V2 b
v2Cast (V2 x y) = V2 (realToFrac x) (realToFrac y)

v2CastI :: (Integral a, Num b) => V2 a -> V2 b
v2CastI (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

v2ToPair :: V2 a -> (a, a)
v2ToPair (V2 x y) = (x, y)

enumerateV2 :: (Integral i) => (V2 i -> a) -> V2 i -> [[a]]
enumerateV2 f (V2 mx my) = fmap (\y -> fmap (\x -> f $ V2 x y) [0..mx]) [0..my]


type DefaultTileGrid = Table

class (Foldable t, Functor t) => TileGrid t where
    getGridSize :: (Integral i) => t a -> V2 i
    getTile :: (Integral i) => V2 i -> t a -> a
    setTile :: (Integral i) => V2 i -> a -> t a -> t a
    modifyTile :: (Integral i) => (a -> a) -> V2 i -> t a -> t a
    modifyTile f xy grid = setTile xy (f (getTile xy grid)) grid
    gridMap :: (Integral i) => (V2 i -> a -> b) -> t a -> t b
    gridFoldMap :: (Integral i, Monoid b) => (V2 i -> a -> b) -> t a -> b
    tileGrid :: (Integral i) => (V2 i -> a) -> V2 i -> t a
    emptyGrid :: (Integral i, Monoid a) => V2 i -> t a
    emptyGrid = tileGrid mempty


data Table a = Table { getTC :: [[a]]
                     , getTSize :: V2 Int
                     } deriving (Show)

setTC :: [[a]] -> Table b -> Table a
setTC c t = t { getTC = c }

getCell :: (Integral i) => V2 i -> Table a -> a
getCell xy t = getTC t !! y !! x
    where
        (V2 x y) = v2CastI xy

setCell :: (Integral i) => V2 i -> a -> Table a -> Table a
setCell xy cell t = setTC (take y tt ++ [line] ++ drop (y+1) tt) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = tt !! y
          tt = getTC t
          (V2 x y) = v2CastI xy

instance Functor Table where
    fmap f t = setTC (fmap f' $ getTC t) t
        where
            f' = fmap f

instance Foldable Table where
    foldMap f t = foldMap f' $ getTC t
        where
            f' = foldMap f

instance TileGrid Table where
    getGridSize = getTSize >>> v2CastI
    getTile = getCell
    setTile = setCell
    
    gridMap f table =
        setTC
            (enumerateV2 f' (getGridSize table))
            table
        where
            t = getTC table
            f' xy = f xy (getCell xy table)
    
    gridFoldMap f = gridMap f >>> foldMap id
    
    tileGrid f size = Table (enumerateV2 f size) (v2CastI size)

findRandomCell
    :: (TileGrid t)
    => (a -> Bool)
    -> t a
    -> StdGen
    -> (StdGen, Maybe (V2 Int))
findRandomCell check t gen
    | not (any check t) = (gen, Nothing)
    | check cell        = (gen', Just (V2 x y))
    | otherwise         = findRandomCell check t gen'
    where
        cell = getTile (V2 x y) t
        (x, gen') = randomR (0, mx-1) gen
        (y, gen'') = randomR (0, my-1) gen'
        (V2 mx my) = getGridSize t

traceMe :: (Show a) => a -> a
traceMe x = trace (show x) x

dropSecond :: (Arrow a) => a b c -> a (b, b') c
dropSecond a = (uncurry . flip . const $ id) ^>> a
