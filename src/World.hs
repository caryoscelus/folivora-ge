{-# LANGUAGE Arrows #-}

module World where

-- import Debug.Trace (trace)

import Control.Monad.Fix

import Data.Maybe
import Data.List

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)

import System.Random (StdGen, randomR)

data SnakeCell = CellEmpty | CellFood | CellSnake Int deriving (Show, Eq)

data Table a = Table { getTC :: [[a]]
                     , getXs :: Int
                     , getYs :: Int
                     } deriving (Show)

setTC :: [[a]] -> Table b -> Table a
setTC c t = t { getTC = c }

instance Functor Table where
    fmap f t = setTC (fmap f' $ getTC t) t
        where
            f' = fmap f

type SnakeTable = Table SnakeCell

snakeTable :: Int -> Int -> SnakeTable
snakeTable x y | x > 0 || y > 0 = let line = take x (repeat CellEmpty)
                                  in  Table (take y (repeat line)) x y
               | otherwise      = error "non-positive table size"

getCell :: (Int, Int) -> SnakeTable -> SnakeCell
getCell (x, y) t = getTC t !! y !! x

changeCell :: (Int, Int) -> SnakeCell -> SnakeTable -> SnakeTable
changeCell (x, y) cell t = setTC (take y tt ++ [line] ++ drop (y+1) tt) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = tt !! y
          tt = getTC t

putSnake :: SnakeTable -> SnakeTable
putSnake table = changeCell (0, 0) (CellSnake 0)
               . changeCell (1, 0) (CellSnake 1)
               $ table

data Direction = DDown | DRight | DUp | DLeft deriving (Show, Eq)

tupleOp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tupleOp f = (f *** f) >>> uncurry (***)

opposite :: Direction -> Direction -> Bool
opposite a b = tupleOp (+) (dirToPair a) (dirToPair b) == (0, 0)

dirToPair :: Direction -> (Int, Int)
dirToPair DDown  = ( 0,  1)
dirToPair DRight = ( 1,  0)
dirToPair DUp    = ( 0, -1)
dirToPair DLeft  = (-1,  0)

increaseSnakeCells :: SnakeTable -> SnakeTable
increaseSnakeCells = fmap inc
    where
        inc :: SnakeCell -> SnakeCell
        inc (CellSnake n) = CellSnake (n+1)
        inc x = x

removeTail :: Int -> SnakeTable -> SnakeTable
removeTail m = fmap (remC m)
    where
        remC :: Int -> SnakeCell -> SnakeCell
        remC m (CellSnake n) | n >= m = CellEmpty
        remC _ x = x

applyDirection :: Direction -> (Int, Int) -> (Int, Int) -> (Int, Int)
applyDirection dir (mx, my) (x, y) = (x1 `mod` mx, y1 `mod` my)
    where
        (x1, y1) = (x+xd, y+yd)
        (xd, yd) = dirToPair dir

addHead :: Direction -> SnakeTable -> (SnakeTable, SnakeCell)
addHead dir t = (setTC (take y' table ++ [line'] ++ drop (y'+1) table) t, cell)
    where
        table = getTC t
        line' = take x' line ++ [CellSnake 0] ++ drop (x'+1) line
        
        line = table !! y'
        cell = line !! x'
        (x', y') = applyDirection dir (mx, my) (x, y)
        my = length table
        mx = length . head $ table
        y = fromMaybe (error "can't find head") $ findIndex lineHasHead table
        x = fromMaybe (error "can't find head") $ findIndex cellIsHead (table !! y)
        lineHasHead ln = isJust $ findIndex cellIsHead ln
        cellIsHead (CellSnake 1) = True
        cellIsHead _ = False

-- for y in range():
--     for x in range():
--         if t[y][x] == 0:
--             r = (x, y)
-- x', y' = directionApply x, y
-- t[y'][x'] = None

moveSnake :: Int -> Direction -> SnakeTable -> (SnakeTable, SnakeCell)
moveSnake len dir table = increaseSnakeCells
                      >>> removeTail len
                      >>> addHead dir
                        $ table


data SnakeWorld = SnakeWorld
        { getTable :: SnakeTable
        , getLength :: Int
        , getDirection :: Direction
        , getRandom :: StdGen
        } deriving (Show)

snakeWorld :: Int -> Int -> StdGen -> SnakeWorld
snakeWorld x y gen = SnakeWorld (putSnake (snakeTable x y)) 2 DDown gen

-- FIXME
setTable :: SnakeTable -> SnakeWorld -> SnakeWorld
setTable t w = w { getTable = t }

setDir :: Direction -> SnakeWorld -> SnakeWorld
setDir d w = w { getDirection = d }

setRandom :: StdGen -> SnakeWorld -> SnakeWorld
setRandom g w = w { getRandom = g }

setLength :: Int -> SnakeWorld -> SnakeWorld
setLength l w = w { getLength = l }

data InputState = InputState
        { getUp :: Bool
        , getDown :: Bool
        , getRight :: Bool
        , getLeft :: Bool
        , getEsc :: Bool
        } deriving (Show)

dirFromInput :: InputState -> Maybe Direction
dirFromInput inp = horiz <|> vert
    where
        horiz = if right /= left
                    then Just (if right then DRight else DLeft)
                    else Nothing
        vert = if up /= down
                    then Just (if up then DUp else DDown)
                    else Nothing
        up    = getUp inp
        down  = getDown inp
        right = getRight inp
        left  = getLeft inp

findRandomCell :: (SnakeCell -> Bool) -> SnakeWorld -> (SnakeWorld, (Int, Int))
findRandomCell check w = if check cell
                            then (w', (x, y))
                            else findRandomCell check w'
    where
        t = (getTable w)
        cell = getCell (x, y) t
        gen = getRandom w
        (x, gen') = randomR (0, getXs t-1) gen
        (y, gen'') = randomR (0, getYs t-1) gen'
        w' = setRandom gen'' w

addRandomFood :: SnakeWorld -> SnakeWorld
addRandomFood w = setTable (changeCell xy CellFood t)
              >>> setRandom gen''
                $ w
    where
        xy = (x, y)
        gen = getRandom w
        (x, gen') = randomR (0, getXs t-1) gen
        (y, gen'') = randomR (0, getYs t-1) gen'
        t = getTable w

stepSnake :: SnakeWorld -> SnakeWorld
stepSnake w = setTable t
          >>> setLength len'
            $ w
    where
        len' = len + if cell == CellFood then 1 else 0
        (t, cell) = moveSnake len dir table
        dir = getDirection w
        len = getLength w
        table = getTable w

stepWorld :: SnakeWorld -> InputState -> SnakeWorld
stepWorld w input = setDir dir''
                >>> stepSnake
                >>> addRandomFood
                  $ w
    where
        dir = getDirection w
        dir' = fromMaybe dir (dirFromInput input)
        dir'' = if opposite dir dir' then dir else dir'

snakeNew :: SnakeWorld -> Wire s e m (Event InputState) (Event SnakeWorld)
snakeNew = accumE stepWorld

snake :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => SnakeWorld -> Wire s e m InputState SnakeWorld
snake start = rtLoop >>> (snakeNew start) >>> asSoonAs

rtLoop :: (Monad m, Monoid e, HasTime t s, Fractional t) => Wire s e m a (Event a)
rtLoop = for 0.2 . now --> rtLoop
