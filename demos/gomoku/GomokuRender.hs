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

instance Loadable GomokuTextures where
    load = do
        return GomokuTextures

instance Renderable GomokuWorld GomokuTextures where
    render texs world = empty

instance Renderable Game GomokuTextures where
    render texs game = empty
