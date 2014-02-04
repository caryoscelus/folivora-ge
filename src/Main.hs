-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE FlexibleContexts #-}

import Control.Monad
-- import Control.Applicative
import Data.Maybe
-- import Control.Monad.Writer
import Data.Dynamic

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty, unless)
import Control.Wire hiding (empty, unless)

-- import Linear.V2

import Game.Graphics

-- import Codec.Picture

import qualified Graphics.UI.GLFW as GLFW

import Render
import World

-- import Game.Graphics.Utils
-- import Graphics.Rendering.OpenGL.Raw.Core32

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

renderFrame :: (GLFW.Window, GraphicsState) -> Space Sprite -> IO ()
renderFrame (window, graphics) frame = do
    clear
    _ <- draw graphics frame
    GLFW.swapBuffers window

-- instance Monoid (Space a) where
--     mempty = empty
--     mappend = (<|>)

-- type RenderQueue = Writer [Space Sprite] ()
-- type RenderQueue = Writer (Space Sprite) ()

-- doDraw :: Space Sprite -> RenderQueue
-- doDraw = tell -- . (:[])

-- v2Cast :: (Real a, Fractional b) => V2 a -> V2 b
-- v2Cast (V2 x y) = V2 (realToFrac x) (realToFrac y)

-- drawAt :: (Real a) => Space Sprite -> V2 a -> RenderQueue
-- drawAt spr xy = doDraw ((translate (v2Cast xy)) *> spr)

-- fixCoords :: Space ()
-- fixCoords = do
--     -- scale coords from (-1, 1) to (-size/2, size/2)
--     scale $ V2 (recip windowWidth * 2) (0 - recip windowHeight * 2)
--     -- translate coords to (0, size)
--     translate $ V2 (-windowWidth / 2) (-windowHeight / 2)

-- runRender :: RenderQueue -> Space Sprite
-- runRender wr = do
--     fixCoords
--     let ((), sprs) = runWriter wr
-- --     msum sprs
--     sprs

gFail :: (Typeable e) => e -> a
gFail x = error $ fromDyn (toDyn x) ("Unknown error produced by " ++ show (typeOf x))

glGo :: (Renderable r t) => () -> t -> (GLFW.Window, GraphicsState) -> Session IO s -> Wire s () Identity () r -> IO ()
glGo inputState textures screen s w = do
    (ds, s') <- stepSession s
    let Identity (mx, w') = stepWire w ds (Right inputState)
    let x = either gFail id mx
    renderFrame screen $ render textures x
    glGo inputState textures screen s' w'

main :: IO ()
main = do
    (window, graphics) <- initGL
    
    texs <- (load :: IO SnakeTextures)
    glGo () texs (window, graphics) clockSession_ $ snake (snakeWorld 10 10)
