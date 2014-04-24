module Game.Folivora.Sound where

import Control.Concurrent

import Data.IORef

import Control.Monad ( when, unless )
import Sound.ALUT

type Sound = Maybe String

type NowPlaying = [String]

newPlayerState :: IO (IORef NowPlaying)
newPlayerState = newIORef []

renderSound :: IORef NowPlaying -> Sound -> IO ()
renderSound _ Nothing = return ()
renderSound ref (Just sound) = atomicModifyIORef' ref $ \lst -> (sound:lst, ())

playerThread :: IORef NowPlaying -> IO ThreadId
playerThread nowPlayingRef = forkIO $ withProgNameAndArgs runALUT $ \_ _ -> thread nowPlayingRef []
    where
        thread toPlayRef nowPlaying = do
            toPlay <- atomicModifyIORef' toPlayRef (\x -> ([], x))
            mapM_ (\sound -> forkIO $ playFile sound) toPlay
            -- TODO: fix garbaging..
            thread toPlayRef (nowPlaying ++ toPlay)

-- taken from ALUT examples..
playFile :: FilePath -> IO ()
playFile fileName = do
    -- Create an AL buffer from the given sound file.
    buf <- createBuffer (File fileName)

    -- Generate a single source, attach the buffer to it and start playing.
    source <- genObjectName
    buffer source $= Just buf
    play [source]
    
    -- Check every 0.1 seconds if the sound is still playing.
    let waitWhilePlaying = do
            sleep 0.1
            state <- get (sourceState source)
            when (state == Playing) $
                waitWhilePlaying
    waitWhilePlaying
