module Game where

-- import Debug.Trace

import Control.Arrow
import Control.Monad.Fix

import Data.Maybe

import Prelude hiding ((.), id, filter, null, until)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty)
import Control.Wire hiding (empty)

import Linear.V2

import Graphics.UI.GLFW (Key(..), KeyState(..))

import Game.Folivora.Input
import Game.Folivora.Wires

import World

data GameModes = NotStarted | PlayerMove deriving (Ord, Show, Eq)

type Game = NModeState (Maybe GomokuWorld) GameModes

emptyGame :: Game
emptyGame = NModeState NotStarted Nothing False

newGame :: Game
newGame = NModeState NotStarted (Just newWorld) False

modifyWorld :: (GomokuWorld -> GomokuWorld) -> Game -> Game
modifyWorld f game = game { getData = Just w' }
    where
        w' = f w
        w = fromJust $ getData game

stopGame
    :: (Monad m, Monoid e)
    => Game
    -> Wire s e m (Event Input) Game
stopGame g0 = ((id &&& filterE (keyPressed Key'Space)) >>> until >>> constArr g0)
          --> constArr (switchTo PlayerMove newGame)

playerMove
    :: (Monad m, Monoid e)
    => Game
    -> Wire s e m (Event Input) Game
playerMove g0 = (dropE 1 >>> filterE (keyPressedF $ const True) >>> hold >>^ handleInput)
            <|> constArr g0
    where
        handleInput inp = switchTo PlayerMove $ modifyWorld (chooseMod inp) g0
        chooseMod inp
            | keyPressed Key'Space inp = occupyTile
            | keyPressed Key'Right inp = modifyPosition (V2   1   0)
            | keyPressed Key'Left  inp = modifyPosition (V2 (-1)  0)
            | keyPressed Key'Down  inp = modifyPosition (V2   0   1)
            | keyPressed Key'Up    inp = modifyPosition (V2   0 (-1))
            | otherwise                = id

modeSwitcher
    :: (Monad m, Monoid e)
    => GameModes
    -> Game
    -> Wire s e m (Event Input) Game
modeSwitcher k game = case k of
    NotStarted   -> stopGame game'
    PlayerMove   -> playerMove game'
    
    where
        game' = setNeedSwitch False game

game
    :: (Monad m, Monoid e, Monoid s)
    => Wire s e m (Event Input) Game
game = trueModes modeSwitcher emptyGame
