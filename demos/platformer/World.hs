{-# LANGUAGE RecordWildCards #-}
module World where

import Debug.Trace

import Data.Monoid
import Data.Maybe
import Control.Arrow
import Control.Applicative

import Linear.V2

import Game.Folivora.TileGrid

-- enemy: delay to fire next bullet
data PlatformerTile = Empty | Floor | Player | Enemy Int | Bullet

instance Monoid PlatformerTile where
    mempty = Empty
    mappend = error "do you really want to use monoid here?.."
--     mappend a _ = a

type PlatformerMap = DefaultTileGrid PlatformerTile

data PlatformerWorld = PlatformerWorld
        { tileMap :: PlatformerMap
        , player :: V2 Int
        , enemy :: V2 Int
        , bullet :: V2 Int
        }

data UserControl = UserControl
        { walkTo :: V2 Int
        }

worldStep :: PlatformerWorld -> UserControl -> PlatformerWorld
worldStep world (UserControl {..}) =
    fixMap world $ world
        { player  = player world + walkTo
        , enemy   = enemy world
        , bullet  = bullet world + V2 (-1) 0
        }
    
    where
        fixMap w w' = w' { tileMap =
                eraseTile (player w)
            >>> eraseTile (enemy w)
            >>> eraseTile (bullet w)
            >>> setCell (player w') Player
            >>> setCell (enemy w') (Enemy 3)
            >>> setCell (bullet w') Bullet
              $ (tileMap w)
            }
        eraseTile x = setCell x Empty

newWorld :: PlatformerWorld
newWorld =
    PlatformerWorld
        { tileMap = tileGrid fill (V2 40 10)
        , player = V2 3 7
        , enemy = V2 20 7
        , bullet = V2 19 7
        }
    where
        fill (V2 x y) | y > 7                   = Floor
                      | y == 7 && x == 3        = Player
                      | y == 7 && x == 20       = Enemy 3
                      | y == 7 && x == 19       = Bullet
                      | otherwise               = Empty
