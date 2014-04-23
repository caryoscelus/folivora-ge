{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Folivora.Render where

import Control.Applicative
import Control.Arrow

import Data.Foldable
import Prelude hiding (foldr)

import Linear.V2

import Game.Graphics hiding (loadFont)
import qualified Game.Graphics as Graphics

import Game.Folivora.Utils

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


renderLine :: (Foldable f) => Int -> (a -> Image) -> f a -> Image
renderLine step f t = foldr folding empty t
    where
        folding x r = f x <|> (translate (v2C step 0) *> r)

renderTable
    :: (Foldable f, Foldable g)
    => V2 Int
    -> (a -> Image)
    -> f (g a)
    -> Image
renderTable (V2 stepX stepY) f t = foldr folding empty t
    where
        folding line r = renderLine stepX f line <|> (translate (v2C 0 stepY) *> r)

loadFont :: String -> IO Font
loadFont = Graphics.loadFont

drawText :: Font -> String -> Image
drawText = fontMessage
