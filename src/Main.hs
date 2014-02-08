import Control.Monad

import Control.Wire (clockSession_)

import System.Random

import GLFWbWire
import Game
import Render
import SnakeRender

main :: IO ()
main = do
    (window, graphics) <- initGL
    
    texs <- (load :: IO SnakeTextures)
    gen <- getStdGen
    
    glGo texs (window, graphics) clockSession_ $ game gen
