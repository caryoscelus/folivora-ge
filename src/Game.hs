module Game where

import Control.Arrow
import Control.Monad.Fix

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

import FRP.Netwire hiding (empty)
import Control.Wire hiding (empty)

import System.Random (StdGen)

import Graphics.UI.GLFW (Key(..), KeyState(..), ModifierKeys(..))

import World

data GameStatus = Playing | Paused | Fail | Win | NotStarted deriving (Show)
data GameState = GameState
        { getStatus :: GameStatus
        , getWorld :: Maybe SnakeWorld
        } deriving (Show)

stateNotStarted :: GameState
stateNotStarted = GameState NotStarted Nothing

statePlaying :: SnakeWorld -> GameState
statePlaying = GameState Playing . Just

game0 :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
      => Int -> Int -> StdGen -> Wire s e m InputState GameState
game0 xs ys gen = (unless getEsc >>> mkConst (Right stateNotStarted))
             --> (snake (snakeWorld xs ys gen) >>^ statePlaying)

game :: (MonadFix m, Monoid e, HasTime t s, Fractional t)
      => Int -> Int -> StdGen -> Wire s e m (Event (Key, KeyState, ModifierKeys)) GameState
game xs ys gen = inputHandler >>> (game0 xs ys gen)

noInput :: InputState
noInput = InputState False False False False False

makeInput :: (Key, KeyState, ModifierKeys) -> InputState
makeInput (k, s, m) =
    if s /= KeyState'Released then
        case k of
            Key'Up     -> noInput { getUp    = True }
            Key'Down   -> noInput { getDown  = True }
            Key'Right  -> noInput { getRight = True }
            Key'Left   -> noInput { getLeft  = True }
            Key'Escape -> noInput { getEsc   = True }
            _          -> noInput
    else
        noInput

inputHandler :: (Monad m, Monoid e) => Wire s e m (Event (Key, KeyState, ModifierKeys)) InputState
inputHandler = (hold >>^ makeInput)
           <|> arr (const noInput)
