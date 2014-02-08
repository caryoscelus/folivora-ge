module Game where

import Control.Arrow
import Control.Monad.Fix

import Prelude hiding ((.), id, filter, null, until)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty)
import Control.Wire hiding (empty)

import System.Random (StdGen)

import Graphics.UI.GLFW (Key(..), KeyState(..))

import Input
import Wires
import World

noInput :: InputState
noInput = InputState False False False False

makeInput :: Input -> InputState
makeInput (k, s, m) =
    if s /= KeyState'Released then
        case k of
            Key'Up     -> noInput { getUp    = True }
            Key'Down   -> noInput { getDown  = True }
            Key'Right  -> noInput { getRight = True }
            Key'Left   -> noInput { getLeft  = True }
            _          -> noInput
    else
        noInput

inputHandler :: (Monad m, Monoid e) => Wire s e m (Event Input) InputState
inputHandler = accumE (flip (const . makeInput)) noInput >>> filterE (/=noInput) >>> hold
           <|> arr (const noInput)

data GameModes = NotStarted | Paused | Playing deriving (Ord, Show, Eq)

type Game = NModeState (Either StdGen SnakeWorld) GameModes

getGen :: Game -> StdGen
getGen ms = case getData ms of
    Left gen    -> gen
    Right world -> getRandom world

newGame :: StdGen -> Game
newGame gen = NModeState NotStarted (Left gen) False

newWorld :: StdGen -> SnakeWorld
newWorld gen = snakeWorld 40 30 gen

stopGame :: (Monad m, Monoid e) => Game -> Wire s e m (Event Input) Game
stopGame g0 = ((id &&& filterE (keyPressed Key'Space)) >>> until >>> constArr g0)
          --> constArr (switchTo Playing (newGame $ getGen g0))

resumeGame :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => Game -> Wire s e m (Event Input) Game
resumeGame g0 = inputHandler >>> snake world' >>^ (\w -> NModeState Playing (Right w) False)
    where
        gData = getData g0
        world' = case gData of
            Left gen     -> newWorld gen
            Right world  -> world

pauseGame :: (Monad m, Monoid e) => Game -> Wire s e m (Event Input) Game
pauseGame g0 = ((id &&& filterE (keyPressed Key'Space)) >>> until >>> constArr g0)
           --> constArr (switchTo Playing g0)

modeSwitcher :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => GameModes -> Game -> Wire s e m (Event Input) Game
modeSwitcher k = case k of
    NotStarted -> stopGame
    Paused     -> pauseGame
    Playing    -> resumeGame

game :: (Monoid s, MonadFix m, Monoid e, HasTime t s, Fractional t) => StdGen -> Wire s e m (Event Input) Game
game gen = trueModes modeSwitcher (newGame gen)
