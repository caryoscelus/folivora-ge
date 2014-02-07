{-# LANGUAGE Arrows #-}

module World where

-- import Debug.Trace (trace)

import Control.Monad.Fix

import Data.Maybe
import Data.Foldable
import Data.List hiding (any)

import Linear.V2

import Prelude hiding ((.), id, filter, null, any)
import qualified Prelude as Prelude

import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)

import System.Random (StdGen, randomR)

data SnakeCell = CellEmpty | CellFood | CellSnake Int deriving (Show, Eq)

isSnake :: SnakeCell -> Bool
isSnake (CellSnake _) = True
isSnake _ = False

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

instance Foldable Table where
    foldMap f t = foldMap f' $ getTC t
        where
            f' = foldMap f

type SnakeTable = Table SnakeCell

snakeTable :: Int -> Int -> SnakeTable
snakeTable x y | x > 0 || y > 0 = let line = take x (repeat CellEmpty)
                                  in  Table (take y (repeat line)) x y
               | otherwise      = error "non-positive table size"

getCell :: V2 Int -> SnakeTable -> SnakeCell
getCell (V2 x y) t = getTC t !! y !! x

changeCell :: V2 Int -> SnakeCell -> SnakeTable -> SnakeTable
changeCell (V2 x y) cell t = setTC (take y tt ++ [line] ++ drop (y+1) tt) t
    where line = take x ln ++ [cell] ++ drop (x+1) ln
          ln = tt !! y
          tt = getTC t

-- FIXME
putSnake :: SnakeTable -> SnakeTable
putSnake table = changeCell (V2 0 0) (CellSnake 0)
               . changeCell (V2 1 0) (CellSnake 1)
               $ table

data Direction = DDown | DRight | DUp | DLeft deriving (Show, Eq)

opposite :: Direction -> Direction -> Bool
opposite a b = dirToPair a + dirToPair b == V2 0 0

dirToPair :: Direction -> V2 Int
dirToPair DDown  = (V2   0    1)
dirToPair DRight = (V2   1    0)
dirToPair DUp    = (V2   0  (-1))
dirToPair DLeft  = (V2 (-1)   0)

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

instance (Enum a) => Enum (V2 a) where
    succ = fmap succ

instance (Real a) => Real (V2 a) where
    toRational _ = error "why would you convert vector to rational?"

instance (Integral a) => Integral (V2 a) where
    quotRem a b = (liftA2 quot a b, liftA2 rem a b)
    divMod  a b = (liftA2 div  a b, liftA2 mod a b)
    toInteger _ = error "why would you convert vector to integer?"

applyDirection :: Direction -> V2 Int -> V2 Int -> V2 Int
applyDirection dir mxy xy = xy' `mod` mxy
    where
        xy' = xy + dirToPair dir

addHead :: V2 Int
        -> Direction
        -> SnakeTable
        -> (SnakeTable, V2 Int, SnakeCell)
addHead (V2 x y) dir t = (changeCell (V2 x' y') (CellSnake 0) t, (V2 x' y'), cell)
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
snakeWorld x y gen = SnakeWorld (putSnake (snakeTable x y)) 2 (V2 0 0) DDown gen False

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

data InputState = InputState
        { getUp :: Bool
        , getDown :: Bool
        , getRight :: Bool
        , getLeft :: Bool
        , getEsc :: Bool
        } deriving (Show, Eq)

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
        t' = maybe t (\xy -> changeCell xy CellFood t) mxy
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

stopOnFail :: Wire s e m (Event SnakeWorld) (Event SnakeWorld)
stopOnFail = takeWhileE (not . getFailed)

snake :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => SnakeWorld -> Wire s e m InputState SnakeWorld
snake start = periodic 0.2 >>> (snakeNew start) >>> stopOnFail >>> asSoonAs
