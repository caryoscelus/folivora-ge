{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module GomokuRender where

import Control.Applicative

import Linear.V2
import Game.Graphics hiding (loadFont, sprite)

import Game.Folivora.Utils
import Game.Folivora.TileGrid
import Game.Folivora.Render
import Game.Folivora.Wires

import World
import Game

data GomokuTextures = GomokuTextures
        { getRedSquare :: Image
        , getGreenSquare :: Image
        , getEmptySquare :: Image
        , getHighlightSquare :: Image
        , getNormalFont :: Font
        }

instance Loadable GomokuTextures where
    load = do
        etex <- loadTexture Standard Linear "tiles-16x16.png"
        let tex = either error id etex
        font <- loadFont "LiberationMono-Regular.ttf"
        return GomokuTextures
                { getRedSquare       = sprite (V2 0 0) (V2 16 16) tex
                , getGreenSquare     = sprite (V2 16 0) (V2 16 16) tex
                , getEmptySquare     = sprite (V2 32 0) (V2 16 16) tex
                , getHighlightSquare = sprite (V2 48 0) (V2 16 16) tex
                , getNormalFont      = font
                }

instance Renderable GomokuWorld GomokuTextures where
    renderGfx texs world =
            translate (V2 16 16) *> renderTable (V2 16 16) tr (getTC . getTable $ world)
        <|> translate ((v2Cast (getPosition world) + V2 1 1) * 16) *> getHighlightSquare texs
    
        where
            tr (Just Player1)  = getGreenSquare texs
            tr (Just Player2)  = getRedSquare texs
            tr Nothing         = getEmptySquare texs

instance Renderable Game GomokuTextures where
    renderGfx texs game =
        case getData game of
            (Just world) -> translate (V2 20 500) *> drawText font (worldMessage world)
                        <|> renderGfx texs world
            Nothing      -> translate (V2 20 500) *> drawText font "No world, duh.."
        where
            font = getNormalFont texs
            worldMessage (GomokuWorld {..})
                | getStatus == Playing  = "It's " ++ show getTurn ++ "'s move.."
                | getStatus == Win      = show getTurn ++ " HAVE WON!"
                | getStatus == Full     = "Woah, board is full!"
                | otherwise             = "Duh, i'm not aware of such status: " ++ show getStatus
