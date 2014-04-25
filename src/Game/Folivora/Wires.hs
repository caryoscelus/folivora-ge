{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Game.Folivora.Wires where

import Prelude hiding ((.), id, filter, null)
import qualified Prelude as Prelude

-- !
import Control.Wire.Unsafe.Event (Event(..))
import FRP.Netwire hiding (empty)

mapEvent
    :: (Monad m)
    => (a -> b)
    -> Wire s e m (Event a) (Event b)
mapEvent = arr . fmap

properModes
    :: (Monad m, Monoid s)
    => k
    -> (k -> Wire s e m a (b, Event k))
    -> Wire s e m a b
properModes mode0 choose = switch (choose mode0 >>> (id *** toWire))
    where
--         toWire :: Wire s e m (Event k) (Event (Wire s e m a b))
        toWire = mapEvent (\k -> properModes k choose)


class (Ord k) => ModeState ms k where
    mode :: ms k -> k
    setMode :: k -> ms k -> ms k
    needSwitch :: ms k -> Bool
    setNeedSwitch :: Bool -> ms k -> ms k
    nextMode :: ms k -> Maybe k
    nextMode s = if needSwitch s
        then Just (mode s)
        else Nothing
    switchTo :: k -> ms k -> ms k
    switchTo k = setMode k . setNeedSwitch True

data NModeState b k = NModeState
        { getMode :: k
        , getData :: b
        , getSwitch :: Bool
        }

setData :: b -> NModeState b k -> NModeState b k
setData d s = s { getData = d }

instance (Ord k) => ModeState (NModeState b) k where
    mode = getMode
    setMode k s = s { getMode = k }
    needSwitch = getSwitch
    setNeedSwitch b s = s { getSwitch = b }

trueModes
    :: (Monoid s, Monad m, ModeState ms k, Ord k)
    => (k -> ms k -> Wire s e m a (ms k))
    -> ms k
    -> Wire s e m a (ms k)
trueModes choose state0 = switch (choose (mode state0) state0 >>> id &&& checkWire)
    where
--         checkWire :: Wire s e m ms (Event (Wire s e m a ms))
        checkWire = became needSwitch >>> mapEvent (trueModes choose)

constArr :: (Arrow a) => c -> a b c
constArr = arr . const

changed :: (Monad m, Eq a) => Wire s e m a (Event a)
changed = arr Just >>> (id &&& delay Nothing) >>> arr test >>> maybeToEvent
    where
        test (x1, x0) | x1 /= x0        = x1
                      | otherwise       = Nothing


maybeToEvent :: (Monad m) => Wire s e m (Maybe a) (Event a)
maybeToEvent = arr m2e
    where
        m2e Nothing = NoEvent
        m2e (Just x) = Event x

eventToMaybe :: (Monad m) => Wire s e m (Event a) (Maybe a)
eventToMaybe = arr e2m
    where
        e2m NoEvent = Nothing
        e2m (Event x) = Just x
