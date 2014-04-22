module Game.Folivora.Input where

import Prelude hiding ((.), id, filter)
import qualified Prelude as Prelude

import Control.Wire hiding (empty, unless)
import Control.Wire.Unsafe.Event (Event(..))

import Graphics.UI.GLFW (Key(..), KeyState(..), ModifierKeys(..))

type Input = (Key, KeyState, ModifierKeys)

keyEvent :: Key -> KeyState -> ModifierKeys -> Event Input
keyEvent key state mods = Event (key, state, mods)

keyPressed :: Key -> Input -> Bool
keyPressed key0 (key, state, mods) = key == key0 && state /= KeyState'Released

keyPressedF :: (Key -> Bool) -> Input -> Bool
keyPressedF kf (key, state, mods) = kf key && state /= KeyState'Released
