module Game where

import Control.Arrow
import Control.Monad.Fix

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty, unless)
-- import Control.Wire hiding (empty, unless)

import System.Random (StdGen)

import World

data GameState = Playing SnakeWorld
               | Paused SnakeWorld
               | Fail SnakeWorld
               | Win SnakeWorld
               | NotStarted

game :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
     => Int -> Int -> StdGen -> Wire s e m InputState GameState
game xs ys gen = snake (snakeWorld xs ys gen) >>^ Playing
