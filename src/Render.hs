{-# LANGUAGE MultiParamTypeClasses #-}

module Render where

import Control.Applicative
import Control.Arrow

import Data.Foldable
import Prelude hiding (foldr)

import Linear.V2

import Game.Graphics

import Utils

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


renderLine :: (Foldable f) => (a -> Image) -> f a -> Image
renderLine f t = foldr folding empty t
    where
        folding x r = f x <|> (translate (V2 16 0) *> r)

renderTable :: (Foldable f, Foldable g) => (a -> Image) -> f (g a) -> Image
renderTable f t = foldr folding empty t
    where
        folding line r = renderLine f line <|> (translate (V2 0 16) *> r)
