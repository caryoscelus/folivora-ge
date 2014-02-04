{-# LANGUAGE Arrows #-}

module World where

import Control.Monad.Fix

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)


data SnakeCell = CellEmpty | CellFood | CellSnake Int

newtype SnakeTable = SnakeTable { getST :: [[SnakeCell]] }

snakeTable :: Int -> Int -> SnakeTable
snakeTable x y | x > 0 || y > 0 = let line = take x (repeat CellEmpty)
                                  in  SnakeTable $ take y (repeat line)
               | otherwise      = error "non-positive table size"

changeCell :: (Int, Int) -> SnakeCell -> SnakeTable -> SnakeTable
changeCell (x, y) cell (SnakeTable t) = SnakeTable $ take y t ++ [line] ++ drop (y+1) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = t !! y

putSnake :: SnakeTable -> SnakeTable
putSnake table = changeCell (0, 0) (CellSnake 0)
               . changeCell (1, 0) (CellSnake 1)
               $ table

data Direction = DDown | DRight | DUp | DLeft

increaseSnakeCells :: SnakeTable -> SnakeTable
increaseSnakeCells (SnakeTable table) = SnakeTable $ map incLine table
    where
        incLine :: [SnakeCell] -> [SnakeCell]
        incLine = map inc
        
        inc :: SnakeCell -> SnakeCell
        inc (CellSnake n) = CellSnake (n+1)
        inc x = x

-- TODO: make fmap over SnakeTable

removeTail :: Int -> SnakeTable -> SnakeTable
removeTail m (SnakeTable table) = SnakeTable $ map (remTL m) table
    where
        remTL :: Int -> [SnakeCell] -> [SnakeCell]
        remTL m = map (remC m)
        
        remC :: Int -> SnakeCell -> SnakeCell
        remC m (CellSnake n) | n >= m = CellEmpty
        remC _ x = x

addHead :: Direction -> SnakeTable -> SnakeTable
addHead dir = id

moveSnake :: Int -> Direction -> SnakeTable -> SnakeTable
moveSnake len dir table = addHead dir
                        . removeTail len
                        . increaseSnakeCells
                        $ table


data SnakeWorld = SnakeWorld
        { getTable :: SnakeTable
        , getLength :: Int
        , getDirection :: Direction
        }

snakeWorld :: Int -> Int -> SnakeWorld
snakeWorld x y = SnakeWorld (putSnake (snakeTable x y)) 2 DDown

-- FIXME
changeTable :: SnakeTable -> SnakeWorld -> SnakeWorld
changeTable t w = w { getTable = t }

data InputState = InputState
        { getUp :: Bool
        , getDown :: Bool
        , getRight :: Bool
        , getLeft :: Bool
        , getEsc :: Bool
        }

stepWorld :: (InputState, SnakeWorld) -> SnakeWorld
stepWorld (input, w) = changeTable (moveSnake len dir table) w
    where
        table = getTable w
        dir = getDirection w
        len = getLength w

realSnake :: (Monad m) => SnakeWorld -> Wire s e m (InputState, SnakeWorld) (SnakeWorld, SnakeWorld)
realSnake w0 = proc (input, w) -> do
    w' <- arr stepWorld . (second $ delay w0) -< (input, w)
    returnA -< (w', w')

snake :: (MonadFix m) => SnakeWorld -> Wire s e m InputState SnakeWorld
snake start = loop (realSnake start)
