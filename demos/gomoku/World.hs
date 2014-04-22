{-# LANGUAGE RecordWildCards #-}
module World where

-- import Debug.Trace

import Data.Monoid
import Data.Maybe
import Control.Arrow
import Control.Applicative

import Linear.V2

import Game.Folivora.TileGrid

data GomokuUser = Player1 | Player2 deriving (Show, Eq)
instance Monoid GomokuUser where
    mempty = Player1
    mappend a _ = a

nextPlayer :: GomokuUser -> GomokuUser
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

type GomokuTile = Maybe GomokuUser

type GomokuTable = DefaultTileGrid GomokuTile

data GomokuStatus = Playing | Win | Full deriving (Show, Eq)

data GomokuWorld = GomokuWorld
        { getTable :: GomokuTable
        , getTurn :: GomokuUser
        , getPosition :: V2 Int
        , getStatus :: GomokuStatus
        } deriving (Show)

newWorld :: GomokuWorld
newWorld = GomokuWorld (emptyGrid (V2 17 17)) Player1 (V2 5 5) Playing

modifyPosition :: V2 Int -> GomokuWorld -> GomokuWorld
modifyPosition diff w = w { getPosition = pos' }
    where
        pos' = normalize (getPosition w + diff)
        normalize (V2 x y) = V2 (norm x) (norm y)
        norm x = x `mod` 17

modifyTable :: (GomokuTable -> GomokuTable) -> GomokuWorld -> GomokuWorld
modifyTable f w = w { getTable = f (getTable w) }

advanceTurn :: GomokuWorld -> GomokuWorld
advanceTurn w@(GomokuWorld {..}) =
    if getStatus == Playing then
        w { getTurn = nextPlayer getTurn }
    else
        w

checkWin :: GomokuWorld -> GomokuWorld
checkWin w =
    if win then
        w { getStatus = Win }
    else
        w
    where
        win = or $ (>=5) <$> ($w) <$> calcLen <$> [V2 0 1, V2 1 0, V2 1 1, V2 1 (-1)]
        calcLen dir w = walkUntil (countId tile) dir w 0 + walkUntil (countId tile) (-dir) w 0 - 1
        tile = getCell (getPosition w) (getTable w)
        countId :: GomokuTile -> (GomokuWorld, Int) -> (Bool, Int)
        countId tile0 (w, count) =
            if getCell (getPosition w) (getTable w) == tile0 then
                (True, count + 1)
            else
                (False, count)
        walkUntil :: ((GomokuWorld, a) -> (Bool, a)) -> V2 Int -> GomokuWorld -> a -> a
        walkUntil f dir w val =
            if (fst $ f (w, val)) then
                walkUntil f dir (modifyPosition dir w) (snd $ f (w, val))
            else
                val

-- try to occupy current tile and change active user if success
occupyTile :: GomokuWorld -> GomokuWorld
occupyTile w@(GomokuWorld {..}) =
    if tileEmpty && getStatus == Playing then
        modifyTable (setCell getPosition (Just getTurn))
    >>> checkWin
    >>> advanceTurn
      $ w
    else
        w
    where
        tileEmpty = isNothing $ getCell getPosition getTable
