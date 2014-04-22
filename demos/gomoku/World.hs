{-# LANGUAGE RecordWildCards #-}
module World where

-- import Debug.Trace

import Data.Monoid
import Data.Maybe

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

data GomokuWorld = GomokuWorld
        { getTable :: GomokuTable
        , getTurn :: GomokuUser
        , getPosition :: V2 Int
        } deriving (Show)

newWorld :: GomokuWorld
newWorld = GomokuWorld (emptyGrid (V2 17 17)) Player1 (V2 5 5)

modifyPosition :: V2 Int -> GomokuWorld -> GomokuWorld
modifyPosition diff w = w { getPosition = pos' }
    where
        pos' = normalize (getPosition w + diff)
        normalize (V2 x y) = V2 (norm x) (norm y)
        norm x = x `mod` 17

modifyTable :: (GomokuTable -> GomokuTable) -> GomokuWorld -> GomokuWorld
modifyTable f w = w { getTable = f (getTable w) }

advanceTurn :: GomokuWorld -> GomokuWorld
advanceTurn w@(GomokuWorld {..}) = w { getTurn = nextPlayer getTurn }

-- try to occupy current tile and change active user if success
occupyTile :: GomokuWorld -> GomokuWorld
occupyTile w@(GomokuWorld {..}) =
    if tileEmpty then
        advanceTurn
      . modifyTable (setCell getPosition (Just getTurn))
      $ w
    else
        w
    where
        tileEmpty = isNothing $ getCell getPosition getTable
