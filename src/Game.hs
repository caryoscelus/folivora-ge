module Game where

-- import Debug.Trace

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
import Direction
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

worldToGame :: SnakeWorld -> Game
worldToGame w =
    if not $ getFailed w then
        NModeState Playing (Right w) False
    else
        NModeState NotStarted (Right w) True

resumeGame :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => Game -> Wire s e m (Event Input) Game
resumeGame g0 = readDirectionChange >>> (filterE isJust <& once) >>> hold >>> snake world' >>^ worldToGame
    where
        gData = getData g0
        world' = case gData of
            Left gen     -> newWorld gen
            Right world  -> world

pauseGame :: (Monad m, Monoid e) => Game -> Wire s e m (Event Input) Game
pauseGame g0 = ((id &&& filterE (keyPressed Key'Space)) >>> until >>> constArr g0)
           --> constArr (switchTo Playing g0)

modeSwitcher :: (MonadFix m, Monoid e, HasTime t s, Fractional t) => GameModes -> Game -> Wire s e m (Event Input) Game
modeSwitcher k game = case k of
    NotStarted -> stopGame game'
    Paused     -> pauseGame game'
    Playing    -> resumeGame game'
    
    where
        game' = setNeedSwitch False game

game :: (Monoid s, MonadFix m, Monoid e, HasTime t s, Fractional t) => StdGen -> Wire s e m (Event Input) Game
game gen = trueModes modeSwitcher (newGame gen)
