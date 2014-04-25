module Game.Folivora.Sound where

import Control.Concurrent

import Data.IORef
import Data.List

import Control.Monad ( when, unless )
import Sound.ALUT

type SoundCommand = Maybe String
type Sound = SoundCommand

data Channel = ChannelPlaying ThreadId | ChannelToPlay SoundCommand | ChannelGarbage deriving (Eq, Show)
type NowPlaying = [Channel]

newPlayerState :: IO (IORef NowPlaying)
newPlayerState = newIORef []

renderSound :: IORef NowPlaying -> SoundCommand -> IO ()
renderSound _ Nothing = return ()
renderSound ref sound = lst <- atomicModifyIORef' ref $ \lst -> (ChannelToPlay sound : lst, lst)

playerThread :: IORef NowPlaying -> IO ThreadId
playerThread nowPlayingRef = forkIO $ withProgNameAndArgs runALUT $ \_ _ -> thread nowPlayingRef
    where
        thread nowPlayingRef = do
            channels <- atomicModifyIORef nowPlayingRef $
                        \chans ->
                            let chans' = filter (/= ChannelGarbage) chans
                                (toPlay, playing) = partition isToPlay chans'
                                isToPlay (ChannelToPlay _) = True
                                isToPlay _ = False
                            in
                                (playing, toPlay)
            
            channels' <- mapM processChannel channels
            
            atomicModifyIORef nowPlayingRef $ \chans -> (chans ++ channels', ())
            
            thread nowPlayingRef
        
        processChannel :: Channel -> IO Channel
        processChannel (ChannelToPlay (Just sound)) = do
            tid <- forkIO $ playFile sound
            return $ ChannelPlaying tid
        processChannel (ChannelToPlay Nothing) = return ChannelGarbage
        processChannel x = return x


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
