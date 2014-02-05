{-# LANGUAGE MultiParamTypeClasses #-}

module Render where

import Control.Applicative

import Linear.V2

import Game.Graphics

import World

windowWidth, windowHeight :: Num a => a
windowWidth = 800
windowHeight = 600

type Image = Space Sprite
type Rendered = Image

class Loadable t where
    load :: IO t

class (Loadable t) => Renderable r t where
    render :: t -> r -> Rendered

fixCoords :: Space ()
fixCoords = do
    -- scale coords from (-1, 1) to (-size/2, size/2)
    scale $ V2 (recip windowWidth * 2) (0 - recip windowHeight * 2)
    -- translate coords to (0, size)
    translate $ V2 (-windowWidth / 2) (-windowHeight / 2)

-- snake
data SnakeTextures = SnakeTextures
        { getRedSquare :: Space Sprite
        }

instance Loadable SnakeTextures where
    load = do
        etex <- loadTexture Standard Linear "16x16-red.png"
        let tex = either error id etex
        return . SnakeTextures $ sprite (V2 0 0) (V2 16 16) tex

renderLine :: (a -> Image) -> [a] -> Image
renderLine f t = foldr folding empty t
    where
        folding x r = f x <|> (translate (V2 16 0) *> r)

renderTable :: (a -> Image) -> [[a]] -> Image
renderTable f t = foldr folding empty t
    where
        folding line r = renderLine f line <|> (translate (V2 0 16) *> r)

instance Renderable SnakeWorld SnakeTextures where
    render texs world = fixCoords
                     *> translate (V2 16 16)
                     *> renderedTable
        where
            renderedTable = renderTable (\c -> case c of
                                    CellSnake _ -> redSquare
                                    _           -> empty)
                                (getTC . getTable $ world)
            redSquare = translate (V2 8 8) *> getRedSquare texs
