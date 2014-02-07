module Input where

import Prelude hiding ((.), id, filter)
import qualified Prelude as Prelude

import Control.Wire hiding (empty, unless)
import Control.Wire.Unsafe.Event (Event(..))

import Graphics.UI.GLFW (Key(..), KeyState(..), ModifierKeys(..))

type KeyMsg = (Key, KeyState, ModifierKeys)
type KeyEvent = Event KeyMsg

keyEvent :: Key -> KeyState -> ModifierKeys -> KeyEvent
keyEvent key state mods = Event (key, state, mods)
