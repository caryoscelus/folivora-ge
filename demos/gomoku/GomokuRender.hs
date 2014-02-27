{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GomokuRender where

import Control.Applicative

import Linear.V2
import Game.Graphics hiding (loadFont)

import Game.Folivora.Utils
import Game.Folivora.TileGrid
import Game.Folivora.Render
import Game.Folivora.Wires

import World
import Game

data GomokuTextures = GomokuTextures
        { getRedSquare :: Image
        , getGreenSquare :: Image
        , getNormalFont :: Font
        }

instance Loadable GomokuTextures where
    load = do
        etex <- loadTexture Standard Linear "tiles-16x16.png"
        let tex = either error id etex
        font <- loadFont "LiberationMono-Regular.ttf"
        return GomokuTextures
                { getRedSquare   = sprite (V2 0 0) (V2 16 16) tex
                , getGreenSquare = sprite (V2 16 0) (V2 16 16) tex
                , getNormalFont  = font
                }

instance Renderable GomokuWorld GomokuTextures where
    render texs world = translate (V2 16 16)
                     *> renderTable tr (getTC . getTable $ world)
        where
            tr (Just Player)   = getGreenSquare texs
            tr (Just Computer) = getRedSquare texs
            tr Nothing         = empty

instance Renderable Game GomokuTextures where
    render texs game = case getData game of
        (Just world) -> render texs world
        Nothing      -> empty
