{-# LANGUAGE MultiParamTypeClasses #-}

module Render where

import Linear.V2

import Game.Graphics

import World

type Image = Space Sprite
type Rendered = Image

class Loadable t where
    load :: IO t

class (Loadable t) => Renderable r t where
    render :: t -> r -> Rendered

-- snake
data SnakeTextures = SnakeTextures
        { getRedSquare :: Space Sprite
        }

instance Loadable SnakeTextures where
    load = do
        etex <- loadTexture Standard Linear "16x16-red.png"
        let tex = either error id etex
        return . SnakeTextures $ sprite (V2 0 0) (V2 8 8) tex

instance Renderable SnakeWorld SnakeTextures where
    render texs world = getRedSquare texs
