import Control.Wire (clockSession_)

import Game.Folivora.GLFWbWire
import Game.Folivora.Render

import Game
import PlatformerRender

main :: IO ()
main = do
    (window, graphics) <- initGL
    
    texs <- (load :: IO PlatformerTextures)
    
    glGo texs (window, graphics) clockSession_ game
