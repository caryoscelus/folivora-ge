{-# LANGUAGE Arrows #-}

module World where

-- import Debug.Trace (trace)

import Control.Monad.Fix

import Data.Maybe
import Data.Foldable (any)
import Data.List hiding (any)

import Linear.V2

import Prelude hiding ((.), id, filter, null, any)
import qualified Prelude as Prelude

import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)

import System.Random (StdGen, randomR)

import Utils
import Direction

data SnakeCell = CellEmpty | CellFood | CellSnake Int deriving (Show, Eq)

isSnake :: SnakeCell -> Bool
isSnake (CellSnake _) = True
isSnake _ = False

mapSnake :: (Int -> Int) -> SnakeCell -> SnakeCell
mapSnake f (CellSnake x) = CellSnake (f x)
mapSnake _ cell = cell

type SnakeTable = Table SnakeCell

-- FIXME
putSnake :: SnakeTable -> SnakeTable
putSnake table = setCell (V2 0 0) (CellSnake 0)
               . setCell (V2 1 0) (CellSnake 1)
               $ table

increaseSnakeCells :: SnakeTable -> SnakeTable
increaseSnakeCells = fmap (mapSnake (+1))

removeTail :: Int -> SnakeTable -> SnakeTable
removeTail m = fmap (remC m)
    where
        remC :: Int -> SnakeCell -> SnakeCell
        remC m (CellSnake n) | n >= m = CellEmpty
        remC _ x = x

applyDirection :: Direction -> V2 Int -> V2 Int -> V2 Int
applyDirection dir mxy xy = xy' `mod` mxy
    where
        xy' = xy + dirToPair dir

addHead :: V2 Int
        -> Direction
        -> SnakeTable
        -> (SnakeTable, V2 Int, SnakeCell)
addHead (V2 x y) dir t = (setCell (V2 x' y') (CellSnake 0) t, (V2 x' y'), cell)
    where
        cell = getTC t !! y' !! x'
        (V2 x' y') = applyDirection dir (V2 mx my) (V2 x y)
        mx = getXs t
        my = getYs t

moveSnake :: V2 Int
          -> Int
          -> Direction
          -> SnakeTable
          -> (SnakeTable, V2 Int, SnakeCell)
moveSnake oldHead len dir table = increaseSnakeCells
                              >>> removeTail len
                              >>> addHead oldHead dir
                                $ table


data SnakeWorld = SnakeWorld
        { getTable :: SnakeTable
        , getLength :: Int
        , getHead :: V2 Int
        , getDirection :: Direction
        , getRandom :: StdGen
        , getFailed :: Bool
        } deriving (Show)

snakeWorld :: Int -> Int -> StdGen -> SnakeWorld
snakeWorld x y gen = SnakeWorld (putSnake (table CellEmpty x y)) 2 (V2 0 0) DDown gen False

-- FIXME
setTable :: SnakeTable -> SnakeWorld -> SnakeWorld
setTable t w = w { getTable = t }

setDir :: Direction -> SnakeWorld -> SnakeWorld
setDir d w = w { getDirection = d }

setRandom :: StdGen -> SnakeWorld -> SnakeWorld
setRandom g w = w { getRandom = g }

setLength :: Int -> SnakeWorld -> SnakeWorld
setLength l w = w { getLength = l }

setFailed :: Bool -> SnakeWorld -> SnakeWorld
setFailed f w = w { getFailed = f }

setHead :: V2 Int -> SnakeWorld -> SnakeWorld
setHead h w = w { getHead = h }

findRandomCell :: (SnakeCell -> Bool) -> SnakeWorld -> (SnakeWorld, Maybe (V2 Int))
findRandomCell check w
    | not (any check t) = (w', Nothing)
    | check cell        = (w', Just (V2 x y))
    | otherwise         = findRandomCell check w'
    where
        t = (getTable w)
        cell = getCell (V2 x y) t
        gen = getRandom w
        (x, gen') = randomR (0, mx) gen
        (y, gen'') = randomR (0, my) gen'
        mx = getXs t-1
        my = getYs t-1
        w' = setRandom gen'' w

addRandomFood :: SnakeWorld -> SnakeWorld
addRandomFood w = setTable t' w'
    where
        t' = maybe t (\xy -> setCell xy CellFood t) mxy
        (w', mxy) = findRandomCell (==CellEmpty) w
        t = getTable w

stepSnake :: SnakeWorld -> SnakeWorld
stepSnake w = setTable t
          >>> setLength len'
          >>> setFailed failed
          >>> setHead snakeHead'
            $ w
    where
        failed = isSnake cell
        len' = len + if cell == CellFood then 1 else 0
        (t, snakeHead', cell) = moveSnake snakeHead len dir table
        dir = getDirection w
        len = getLength w
        table = getTable w
        snakeHead = getHead w

stepWorld :: SnakeWorld -> DirectionChange -> SnakeWorld
stepWorld w input = setDir dir''
                >>> stepSnake
                >>> addRandomFood
                  $ w
    where
        dir = getDirection w
        dir' = fromMaybe dir input
        dir'' = if opposite dir dir' then dir else dir'

snakeNew :: SnakeWorld -> Wire s e m (Event DirectionChange) (Event SnakeWorld)
snakeNew = accumE stepWorld

stopOnFail :: Wire s e m (Event SnakeWorld) (Event SnakeWorld)
stopOnFail = takeWhileE (not . getFailed)

snake :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => SnakeWorld -> Wire s e m DirectionChange SnakeWorld
snake start = periodic 0.2 >>> (snakeNew start) >>> stopOnFail >>> asSoonAs
