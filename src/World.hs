module World where

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

changeCell :: SnakeTable -> (Int, Int) -> SnakeCell -> SnakeTable
changeCell (SnakeTable t) (x, y) cell = SnakeTable $ take y t ++ [line] ++ drop (y+1) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = t !! y

putSnake :: SnakeTable -> SnakeTable
putSnake table = changeCell table (0, 0) (CellSnake 0)

data SnakeWorld = SnakeWorld
        { getTable :: SnakeTable
        , getLength :: Int
        }

snakeWorld :: Int -> Int -> SnakeWorld
snakeWorld x y = SnakeWorld (putSnake (snakeTable x y)) 1

snake :: SnakeWorld -> Wire s e m () SnakeWorld
snake start = mkConst (Right start)
