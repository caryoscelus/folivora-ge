-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE FlexibleContexts #-}

-- import Debug.Trace (trace)

import Control.Monad
import Data.Maybe
import Data.Dynamic

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty, unless)
import Control.Wire hiding (empty, unless)

import Game.Graphics

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..), KeyState(..), getKey)

import Render
import World

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

-- v2Cast :: (Real a, Fractional b) => V2 a -> V2 b
-- v2Cast (V2 x y) = V2 (realToFrac x) (realToFrac y)

gFail :: (Typeable e) => e -> a
gFail x = error $ fromDyn (toDyn x) ("Unknown error produced by " ++ show (typeOf x))

glGo :: (Renderable r t) => () -> t -> (GLFW.Window, GraphicsState) -> Session IO s -> Wire s () Identity InputState r -> IO ()
glGo inputState textures screen s w = do
    (ds, s') <- stepSession s
    
    GLFW.pollEvents
    
    let window = fst screen
    
    let gk :: Key -> IO Bool
        gk k = liftM (/=KeyState'Released) (getKey window k)
    
    up    <- gk Key'Up
    down  <- gk Key'Down
    right <- gk Key'Right
    left  <- gk Key'Left
    esc   <- gk Key'Escape
    
    let inputState' = InputState up down right left esc
    
    let Identity (mx, w') = stepWire w ds (Right inputState')
    let x = either gFail id mx
    
    renderFrame screen $ render textures x
    
    glGo inputState textures screen s' w'

main :: IO ()
main = do
    (window, graphics) <- initGL
    
    texs <- (load :: IO SnakeTextures)
    glGo () texs (window, graphics) clockSession_ $ snake (snakeWorld 40 30)
