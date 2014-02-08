module Game where

import Control.Arrow
import Control.Monad.Fix

import Data.Maybe

import Prelude hiding ((.), id, filter, null, until)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty)
import Control.Wire hiding (empty)

import System.Random (StdGen)

import Graphics.UI.GLFW (Key(..), KeyState(..))

import Input
import Wires
import World

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

readDirectionChange :: (Monad m) => Wire s e m (Event Input) (Event DirectionChange)
readDirectionChange = mapEvent $ \input ->
    foldl (\b (k, r) -> b <|> (if keyPressed k input then Just r else Nothing)) Nothing
        [ (Key'Down,  DDown)
        , (Key'Up,    DUp)
        , (Key'Right, DRight)
        , (Key'Left,  DLeft)
        ]

resumeGame :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => Game -> Wire s e m (Event Input) Game
resumeGame g0 = readDirectionChange >>> (filterE isJust <& once) >>> hold >>> snake world' >>^ (\w -> NModeState Playing (Right w) False)
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
