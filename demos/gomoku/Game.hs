module Game where

import Control.Arrow
import Control.Monad.Fix

import Data.Maybe

import Prelude hiding ((.), id, filter, null, until)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty)
import Control.Wire hiding (empty)

import Graphics.UI.GLFW (Key(..), KeyState(..))

import Game.Folivora.Input
import Game.Folivora.Wires

import World

data GameModes = NotStarted | PlayerMove | ComputerMove deriving (Ord, Show, Eq)

type Game = NModeState (Maybe GomokuWorld) GameModes

emptyGame :: Game
emptyGame = NModeState NotStarted Nothing False

newGame :: Game
newGame = NModeState NotStarted (Just newWorld) False

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
playerMove g0 = constArr g0

computerMove
    :: (Monad m, Monoid e)
    => Game
    -> Wire s e m (Event Input) Game
computerMove g0 = constArr g0

modeSwitcher
    :: (Monad m, Monoid e)
    => GameModes
    -> Game
    -> Wire s e m (Event Input) Game
modeSwitcher k game = case k of
    NotStarted   -> stopGame game'
    PlayerMove   -> playerMove game'
    ComputerMove -> computerMove game'
    
    where
        game' = setNeedSwitch False game

game
    :: (Monad m, Monoid e, Monoid s)
    => Wire s e m (Event Input) Game
game = trueModes modeSwitcher emptyGame
