{-# OPTIONS_HADDOCK show-extensions  #-}
{-# OPTIONS_GHC -Wno-dodgy-exports   #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import Control.Concurrent
    ( newChan, readChan, writeChan, threadDelay, forkIO, Chan )
import Control.Monad ( when, replicateM_ )

main :: IO ()
main = do
    -- Create a new channel
    chan <- newChan
    
    -- Create threads for ping and pong agents
    forkIO $ pingAgent chan
    forkIO $ pongAgent chan
    
    -- Wait for a while to allow agents to run
    threadDelay 1000000
    
    -- Close the channel
    writeChan chan "terminate"
    writeChan chan "terminate"
    
    -- Wait for agents to terminate
    threadDelay 1000000

-- Ping agent function
pingAgent :: Chan String -> IO ()
pingAgent chan = replicateM_ 6 $ do
    writeChan chan "ping"
    putStrLn "ping sent"
    threadDelay 100000 -- Small delay for demonstration

-- Pong agent function
pongAgent :: Chan String -> IO ()
pongAgent chan = replicateM_ 6 $ do
    msg <- readChan chan
    when (msg == "ping") $ do
        writeChan chan "pong"
        putStrLn (msg ++ "Received")
        putStrLn "pong sent"
        threadDelay 100000 -- Small delay for demonstration
