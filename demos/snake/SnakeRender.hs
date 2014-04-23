{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SnakeRender where

import Control.Applicative

import Linear.V2
import Game.Graphics hiding (loadFont)

import Game.Folivora.Utils
import Game.Folivora.TileGrid
import Game.Folivora.Render
import Game.Folivora.Wires

import World
import Game

data SnakeTextures = SnakeTextures
        { getRedSquare :: Image
        , getGreenSquare :: Image
        , getNormalFont :: Font
        }

instance Loadable SnakeTextures where
    load = do
        etex <- loadTexture Standard Linear "tiles-16x16.png"
        let tex = either error id etex
        font <- loadFont "LiberationMono-Regular.ttf"
        return SnakeTextures
                { getRedSquare   = sprite (V2 0 0) (V2 16 16) tex
                , getGreenSquare = sprite (V2 16 0) (V2 16 16) tex
                , getNormalFont  = font
                }


instance Renderable SnakeWorld SnakeTextures where
    render texs world = translate (V2 16 16)
                     *> renderedTable
        where
            renderedTable = renderTable (V2 16 16) (\c -> case c of
                                    TileSnake _ -> redSquare
                                    TileFood    -> greenSquare
                                    _           -> empty)
                                (getTC . getTable $ world)
            redSquare = translate (V2 8 8) *> getRedSquare texs
            greenSquare = translate (V2 8 8) *> getGreenSquare texs

instance Renderable Game SnakeTextures where
    render texs state = renderGui <|> renderWorld
        where
            renderGui =
                case mode state of
                    NotStarted -> translate (V2 300 290) *>
                                  drawText font "Press [space] to (re)start"
                    Playing    -> empty
                    Paused     -> translate (V2 300 290) *>
                                  drawText font "Press [space] to continue"
            renderWorld =
                case getData state of
                    Right w -> render texs w
                    Left _  -> empty
            font = getNormalFont texs
