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

data SnakeCell = CellEmpty | CellFood | CellSnake Int deriving (Show)

newtype Table a = Table { getTC :: [[a]] } deriving (Show)

instance Functor Table where
    fmap f (Table t) = Table $ fmap f' t
        where
            f' = fmap f

type SnakeTable = Table SnakeCell

snakeTable :: Int -> Int -> SnakeTable
snakeTable x y | x > 0 || y > 0 = let line = take x (repeat CellEmpty)
                                  in  Table $ take y (repeat line)
               | otherwise      = error "non-positive table size"

changeCell :: (Int, Int) -> SnakeCell -> SnakeTable -> SnakeTable
changeCell (x, y) cell (Table t) = Table $
    take y t ++ [line] ++ drop (y+1) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = t !! y

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

addHead :: Direction -> SnakeTable -> SnakeTable
addHead dir (Table table) =
    Table (take y' table ++ [line'] ++ drop (y'+1) table)
    
    where
        line' = take x' line ++ [CellSnake 0] ++ drop (x'+1) line
        
        line = table !! y'
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

moveSnake :: Int -> Direction -> SnakeTable -> SnakeTable
moveSnake len dir table = addHead dir
                        . removeTail len
                        . increaseSnakeCells
                        $ table


data SnakeWorld = SnakeWorld
        { getTable :: SnakeTable
        , getLength :: Int
        , getDirection :: Direction
        } deriving (Show)

snakeWorld :: Int -> Int -> SnakeWorld
snakeWorld x y = SnakeWorld (putSnake (snakeTable x y)) 2 DDown

-- FIXME
changeTable :: SnakeTable -> SnakeWorld -> SnakeWorld
changeTable t w = w { getTable = t }

setDir :: Direction -> SnakeWorld -> SnakeWorld
setDir d w = w { getDirection = d }

data InputState = InputState
        { getUp :: Bool
        , getDown :: Bool
        , getRight :: Bool
        , getLeft :: Bool
        , getEsc :: Bool
        , getRandom :: StdGen
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

stepWorld :: SnakeWorld -> InputState -> SnakeWorld
stepWorld w input = changeTable (moveSnake len dir'' table)
                  . setDir dir''
                  $ w
    where
        table = getTable w
        dir = getDirection w
        dir' = fromMaybe dir (dirFromInput input)
        dir'' = if opposite dir dir' then dir else dir'
        len = getLength w

snakeNew :: SnakeWorld -> Wire s e m (Event InputState) (Event SnakeWorld)
snakeNew = accumE stepWorld

snake :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => SnakeWorld -> Wire s e m InputState SnakeWorld
snake start = rtLoop >>> (snakeNew start) >>> asSoonAs

rtLoop :: (Monad m, Monoid e, HasTime t s, Fractional t) => Wire s e m a (Event a)
rtLoop = for 0.2 . now --> rtLoop
