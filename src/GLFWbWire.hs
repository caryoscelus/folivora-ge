module GLFWbWire where

import Control.Monad
import Data.Maybe
import Data.Dynamic
import Data.IORef

import Prelude hiding ((.), id, filter)
import qualified Prelude as Prelude

import Control.Wire hiding (empty, unless)
import Control.Wire.Unsafe.Event (Event(..))

import Game.Graphics

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..), KeyState(..), ModifierKeys(..), getKey)

import Render

gFail :: (Typeable e) => e -> a
gFail x = error $ fromDyn (toDyn x) ("Unknown error produced by " ++ show (typeOf x))

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

type KeyEvent = Event (Key, KeyState, ModifierKeys)

keyEvent :: Key -> KeyState -> ModifierKeys -> KeyEvent
keyEvent key state mods = Event (key, state, mods)

keyCallback :: IORef [KeyEvent] -> GLFW.Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback queue = \win key n state mods ->
    modifyIORef queue (keyEvent key state mods:)

glGo :: (Renderable r t)
     => t -> (GLFW.Window, GraphicsState) -> Session IO s -> Wire s () Identity KeyEvent r -> IO ()
glGo texs (window, state) s w = do
    queue <- newIORef []
    GLFW.setKeyCallback window (Just $ keyCallback queue)
    go queue texs (window, state) s w
    
    where
        go queue textures screen s w = do
            (ds, s') <- stepSession s
            
            GLFW.pollEvents
            
            let window = fst screen
            
            events <- readIORef queue
            let events' = if null events then [NoEvent] else events
            writeIORef queue (tail events')
            let event = head events'
            
            let Identity (mx, w') = stepWire w ds (Right event)
            let x = either gFail id mx
            
            renderFrame screen $ render textures x
            
            go queue textures screen s' w'
