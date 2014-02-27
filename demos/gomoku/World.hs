module World where

import Game.Folivora.TileGrid

data GomokuUser = Player | Computer deriving (Show, Eq)
type GomokuTile = Maybe GomokuUser

type GomokuTable = DefaultTileGrid GomokuTile

data GomokuWorld = GomokuWorld
        { getTable :: GomokuTable
        , getTurn :: GomokuUser
        } deriving (Show)
