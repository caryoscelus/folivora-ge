module Game.Folivora.Sound where

import Control.Concurrent

import Control.Monad ( when, unless )
import Sound.ALUT


type Sound = Maybe String

renderSound :: Sound -> IO ()
renderSound Nothing = return ()
renderSound (Just fname) = do
    forkIO $ playFile fname
    return ()

-- taken from ALUT examples..
playFile :: FilePath -> IO ()
playFile fileName = withProgNameAndArgs runALUT $ \_ _ -> do
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
