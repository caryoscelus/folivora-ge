{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module PlatformerRender where

import Debug.Trace

import Control.Applicative

import Linear.V2
import Game.Graphics hiding (loadFont, sprite)

import Game.Folivora.Render
import Game.Folivora.Wires
import Game.Folivora.TileGrid

import World
import Game

data PlatformerTextures = PlatformerTextures
        { tileEmpty :: Image
        , tileFloor :: Image
        , tilePlayer :: Image
        , tileEnemy :: Image
        , tileBullet :: Image
        , fontDefault :: Font
        }

instance Loadable PlatformerTextures where
    load = do
        etex <- loadTexture Standard Linear "platformer-tileset.png"
        let tex = either error id etex
        font <- loadFont "LiberationMono-Regular.ttf"
        let mkSprite x y = sprite (V2 (x*32) (y*32)) (V2 32 32) tex
        return PlatformerTextures
                { tileEmpty     = mkSprite 0 0
                , tileFloor     = mkSprite 0 1
                , tilePlayer    = mkSprite 0 2
                , tileEnemy     = mkSprite 0 3
                , tileBullet    = mkSprite 0 4
                , fontDefault   = font
                }

instance Renderable PlatformerWorld PlatformerTextures where
    renderGfx (PlatformerTextures {..}) (PlatformerWorld {..}) =
        translate (V2 32 32) *> renderTable (V2 32 32) tr (getTC tileMap)
        
        where
            tr Empty            = tileEmpty
            tr Floor            = tileFloor
            tr Player           = tileEmpty <|> tilePlayer
            tr (Enemy _)        = tileEmpty <|> tileEnemy
            tr Bullet           = tileEmpty <|> tileBullet

instance Renderable GameData PlatformerTextures where
    render texs gdata = Rendered (renderGfx texs gdata) (soundCommand gdata)
    renderGfx texs (GameData world _) = renderGfx texs world

instance Renderable Game PlatformerTextures where
    render texs (NModeState {..}) = render texs getData
