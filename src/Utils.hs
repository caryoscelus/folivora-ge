module Utils where

import Control.Applicative
import Control.Arrow

import Data.Monoid

import Debug.Trace

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

traceMe :: (Show a) => a -> a
traceMe x = trace (show x) x

dropSecond :: (Arrow a) => a b c -> a (b, b') c
dropSecond a = (uncurry . flip . const $ id) ^>> a
