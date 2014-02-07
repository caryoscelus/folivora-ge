import Control.Monad

import Control.Wire (clockSession_)

import System.Random

import GLFWbWire
import Render
import Game

main :: IO ()
main = do
    (window, graphics) <- initGL
    
    texs <- (load :: IO SnakeTextures)
    gen <- getStdGen
    
    glGo texs (window, graphics) clockSession_ $ game 40 30 gen
