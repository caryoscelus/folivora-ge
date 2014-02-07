{-# LANGUAGE MultiParamTypeClasses #-}

module SnakeRender where

import Control.Applicative

import Linear.V2
import Game.Graphics

import Render
import World
import Game

data SnakeTextures = SnakeTextures
        { getRedSquare :: Image
        , getGreenSquare :: Image
        }

instance Loadable SnakeTextures where
    load = do
        etex <- loadTexture Standard Linear "tiles-16x16.png"
        let tex = either error id etex
        return SnakeTextures
                { getRedSquare   = sprite (V2 0 0) (V2 16 16) tex
                , getGreenSquare = sprite (V2 16 0) (V2 16 16) tex
                }


instance Renderable SnakeWorld SnakeTextures where
    render texs world = fixCoords
                     *> translate (V2 16 16)
                     *> renderedTable
        where
            renderedTable = renderTable (\c -> case c of
                                    CellSnake _ -> redSquare
                                    CellFood    -> greenSquare
                                    _           -> empty)
                                (getTC . getTable $ world)
            redSquare = translate (V2 8 8) *> getRedSquare texs
            greenSquare = translate (V2 8 8) *> getGreenSquare texs

instance Renderable GameState SnakeTextures where
    render texs state = case getWorld state of
                            Just w  -> render texs w
                            Nothing -> empty