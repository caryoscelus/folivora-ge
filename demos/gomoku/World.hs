module World where

import Data.Monoid

import Linear.V2

import Game.Folivora.TileGrid

data GomokuUser = Player | Computer deriving (Show, Eq)
instance Monoid GomokuUser where
    mempty = Player
    mappend a _ = a

type GomokuTile = Maybe GomokuUser

type GomokuTable = DefaultTileGrid GomokuTile

data GomokuWorld = GomokuWorld
        { getTable :: GomokuTable
        , getTurn :: GomokuUser
        } deriving (Show)

newWorld :: GomokuWorld
newWorld = GomokuWorld (emptyGrid (V2 17 17)) Player

computerMakeMove :: GomokuWorld -> GomokuWorld
computerMakeMove world = GomokuWorld (setTile (V2 5 5) (Just Computer) (getTable world)) Player
