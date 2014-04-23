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

data GameData = GameData PlatformerWorld
data GameModes = Playing deriving (Ord, Show, Eq)
type Game = NModeState GameData GameModes

emptyGame :: Game
emptyGame = NModeState Playing (GameData newWorld) False

gameToWorld :: Game -> PlatformerWorld
gameToWorld game = data2W $ getData game
    where
        data2W (GameData w) = w

worldToGame :: PlatformerWorld -> Game
worldToGame world = NModeState Playing (GameData world) False

worldUpdater :: PlatformerWorld -> Wire s e m (Event UserControl) (Event PlatformerWorld)
worldUpdater = accumE worldStep

readInput
    :: (Monad m)
    => Wire s e m (Event Input) (Event UserControl)
readInput = mapEvent (ri >>> UserControl)
    where
        ri input | keyPressed Key'Left input    = V2 (-1) 0
                 | keyPressed Key'Right input   = V2   1  0
                 | otherwise                    = V2   0  0

continueGame
    :: (Monad m, Monoid e, HasTime t s)
    => Game
    -> Wire s e m (Event Input) Game
continueGame g0 =
        readInput <& (constArr (UserControl $ V2 0 0) >>> now)
    >>> hold
    >>> periodic 1
    >>> worldUpdater (gameToWorld g0)
    >>> hold
    >>^ worldToGame

modeSwitcher
    :: (Monad m, Monoid e, HasTime t s)
    => GameModes
    -> Game
    -> Wire s e m (Event Input) Game
modeSwitcher k game = case k of
    Playing -> continueGame game'
    
    where
        game' = setNeedSwitch False game

game
    :: (Monad m, Monoid e, Monoid s, HasTime t s)
    => Wire s e m (Event Input) Game
game = trueModes modeSwitcher emptyGame
