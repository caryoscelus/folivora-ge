module Game where

import Control.Arrow
import Control.Monad.Fix

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty, unless)
-- import Control.Wire hiding (empty, unless)

import World

data GameState = Playing SnakeWorld
               | Paused SnakeWorld
               | Fail SnakeWorld
               | Win SnakeWorld
               | NotStarted

game :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
     => SnakeWorld -> Wire s e m InputState GameState
game st = snake st >>^ Playing
