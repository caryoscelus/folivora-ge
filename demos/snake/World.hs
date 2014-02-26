{-# LANGUAGE Arrows #-}

module World where

import Control.Monad.Fix

import Data.Maybe
import Data.Foldable (any)

import Linear.V2

import Prelude hiding ((.), id, filter, null, any, until)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty)

import System.Random (StdGen, randomR)

import Game.Folivora.Utils
import Game.Folivora.TileGrid
import Game.Folivora.Direction

data SnakeTile = TileEmpty | TileFood | TileSnake Int deriving (Show, Eq)

instance Monoid SnakeTile where
    mempty = TileEmpty
    mappend = error "mappend is not implemented for SnakeTile"

isSnake :: SnakeTile -> Bool
isSnake (TileSnake _) = True
isSnake _ = False

mapSnake :: (Int -> Int) -> SnakeTile -> SnakeTile
mapSnake f (TileSnake x) = TileSnake (f x)
mapSnake _ cell = cell

type SnakeTable = DefaultTileGrid SnakeTile

-- FIXME
putSnake :: SnakeTable -> SnakeTable
putSnake table = setTile (V2 0 0) (TileSnake 0)
             >>> setTile (V2 1 0) (TileSnake 1)
               $ table

increaseSnakeTiles :: SnakeTable -> SnakeTable
increaseSnakeTiles = fmap (mapSnake (+1))

removeTail :: Int -> SnakeTable -> SnakeTable
removeTail m = fmap (remC m)
    where
        remC :: Int -> SnakeTile -> SnakeTile
        remC m (TileSnake n) | n >= m = TileEmpty
        remC _ x = x

applyDirection :: Direction -> V2 Int -> V2 Int -> V2 Int
applyDirection dir mxy xy = xy' `mod` mxy
    where
        xy' = xy + dirToPair dir

addHead :: V2 Int
        -> Direction
        -> SnakeTable
        -> (SnakeTable, V2 Int, SnakeTile)
addHead xy dir t = (setTile xy' (TileSnake 0) t, xy', cell)
    where
        cell = getTile xy' t
        xy' = applyDirection dir mxy xy
        mxy = getGridSize t

moveSnake :: V2 Int
          -> Int
          -> Direction
          -> SnakeTable
          -> (SnakeTable, V2 Int, SnakeTile)
moveSnake oldHead len dir table = increaseSnakeTiles
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
snakeWorld x y gen = SnakeWorld (putSnake (emptyGrid $ V2 x y)) 2 (V2 0 0) DDown gen False

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

findRandomTileSnake :: (SnakeTile -> Bool) -> SnakeWorld -> (SnakeWorld, Maybe (V2 Int))
findRandomTileSnake check w = (w', result)
    where
        w' = setRandom gen' $ w
        (gen', result) = findRandomTile check (getTable w) (getRandom w)

addRandomFood :: SnakeWorld -> SnakeWorld
addRandomFood w = setTable t' w'
    where
        t' = maybe t (\xy -> setTile xy TileFood t) mxy
        (w', mxy) = findRandomTileSnake (==TileEmpty) w
        t = getTable w

haveFood :: SnakeWorld -> Bool
haveFood w = any (==TileFood) (getTable w)

stepSnake :: SnakeWorld -> SnakeWorld
stepSnake w = setTable t
          >>> setLength len'
          >>> setFailed failed
          >>> setHead snakeHead'
            $ w
    where
        failed = isSnake cell
        len' = len + if cell == TileFood then 1 else 0
        (t, snakeHead', cell) = moveSnake snakeHead len dir table
        dir = getDirection w
        len = getLength w
        table = getTable w
        snakeHead = getHead w

stepWorld :: SnakeWorld -> DirectionChange -> SnakeWorld
stepWorld w input = setDir dir''
                >>> stepSnake
                >>> (if not (haveFood w) then addRandomFood else id)
                  $ w
    where
        dir = getDirection w
        dir' = fromMaybe dir input
        dir'' = if opposite dir dir' then dir else dir'

snakeNew :: SnakeWorld -> Wire s e m (Event DirectionChange) (Event SnakeWorld)
snakeNew = accumE stepWorld

stopOnFail :: Wire s e m SnakeWorld (Event SnakeWorld)
stopOnFail = became getFailed

snake
    :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
    => t
    -> SnakeWorld
    -> Wire s e m DirectionChange SnakeWorld
snake speed start =
        periodic speed
    >>> (snakeNew start)
    >>> hold
    >>> (id &&& stopOnFail)
    >>> (until --> dropSecond (now >>> hold))
