-- {-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Applicative
import Data.Maybe
import Control.Monad.Writer

import Linear.V2

import Game.Graphics

-- import Codec.Picture

import qualified Graphics.UI.GLFW as GLFW

import Render

-- import Game.Graphics.Utils
-- import Graphics.Rendering.OpenGL.Raw.Core32

windowWidth, windowHeight :: Num a => a
windowWidth = 800
windowHeight = 600

initGL :: IO (GLFW.Window, GraphicsState)
initGL = do
    isInited <- GLFW.init
    unless isInited $ error "Cannot init GLFW"
    
    maybeW <- GLFW.createWindow windowWidth windowHeight "snake" Nothing Nothing
    let window = fromMaybe (error "No window was created") maybeW
    GLFW.makeContextCurrent $ Just window
    
    glViewport 0 0 windowWidth windowHeight
    
    graphicsState <- initializeGraphics
    return (window, graphicsState)

renderFrame :: GLFW.Window -> GraphicsState -> Space Sprite -> IO ()
renderFrame window graphics frame = do
    clear
    _ <- draw graphics frame
    GLFW.swapBuffers window

getSprite :: IO (Space Sprite)
getSprite = do
    etex <- loadTexture Standard Linear "16x16-red.png"
    let tex = either error id etex
    return $ sprite (V2 0 0) (V2 8 8) tex

instance Monoid (Space a) where
    mempty = empty
    mappend = (<|>)

-- type RenderQueue = Writer [Space Sprite] ()
type RenderQueue = Writer (Space Sprite) ()

doDraw :: Space Sprite -> RenderQueue
doDraw = tell -- . (:[])

v2Cast :: (Real a, Fractional b) => V2 a -> V2 b
v2Cast (V2 x y) = V2 (realToFrac x) (realToFrac y)

drawAt :: (Real a) => Space Sprite -> V2 a -> RenderQueue
drawAt spr xy = doDraw ((translate (v2Cast xy)) *> spr)

frame2_render :: Space Sprite -> RenderQueue
frame2_render spr = do
    drawAt spr (V2 4 4)
    drawAt spr (V2 44 44)

fixCoords :: Space ()
fixCoords = do
    -- scale coords from (-1, 1) to (-size/2, size/2)
    scale $ V2 (recip windowWidth * 2) (0 - recip windowHeight * 2)
    -- translate coords to (0, size)
    translate $ V2 (-windowWidth / 2) (-windowHeight / 2)

runRender :: RenderQueue -> Space Sprite
runRender wr = do
    fixCoords
    let ((), sprs) = runWriter wr
--     msum sprs
    sprs

frame2 :: Space Sprite -> Space Sprite
frame2 spr = runRender $ frame2_render spr

frame :: Space Sprite -> Space Sprite
frame = frame2

main :: IO ()
main = do
    (window, graphics) <- initGL
    sprite0 <- getSprite
    let f = frame sprite0
    renderFrame window graphics f
    loopForever
    where loopForever = putStr "" >> loopForever
