{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

import Control.Concurrent (newChan, readChan, writeChan, forkIO, Chan)
import Control.Monad (forever)
import Numeric.Natural (Natural)
import Text.Printf (printf)

data Agent msg a where
    Delay :: Agent msg ()  -- Change the return type to ()
    Broadcast :: msg -> Agent msg ()
    Receive :: Agent msg msg

instance Functor (Agent msg) where
    fmap :: (a -> b) -> Agent msg a -> Agent msg b
    fmap f action = do
        f <$> action

instance Applicative (Agent msg) where
    pure :: a -> Agent msg a

    (<*>) :: Agent msg (a -> b) -> Agent msg a -> Agent msg b
    f <*> x = do
        func <- f
        func <$> x
    pure = return

instance Monad (Agent msg) where
    return :: a -> Agent msg a

    (>>=) :: Agent msg a -> (a -> Agent msg b) -> Agent msg b
    action >>= f = case action of
        Delay -> delay >>= f
        Broadcast msg -> broadcast msg >>= f
        Receive -> receive >>= f
    return = pure

delay :: Agent msg ()
delay = Delay  -- Assuming a delay of 1 time step

broadcast :: msg -> Agent msg ()
broadcast = Broadcast

receive :: Agent msg msg
receive = Receive

data PingPongMessage =
      Ping
    | Pong
    deriving (Show)

ping :: Agent PingPongMessage ()
ping = delay >> broadcast Ping >> go
  where
    go = do
        msg <- receive
        case msg of
            Ping -> go
            Pong -> broadcast Ping >> ping

pong :: Agent PingPongMessage ()
pong = do
    msg <- receive
    case msg of
        Ping -> delay >> broadcast Pong >> pong
        Pong -> broadcast Ping >> pong


runAgent :: Agent msg a -> IO a
runAgent action = case action of
    Delay -> do
        return ()  -- Returning () since the Delay action has type Agent msg ()
    Broadcast msg -> do
        return ()
    Receive -> undefined  -- You need to define the logic for receiving here

main :: IO ()
main = do
    chan <- newChan
    _ <- forkIO $ runAgent ping
    _ <- forkIO $ runAgent pong
    _ <- runAgent $ broadcast Ping

    
    -- forever $ do
    printf "Received message: %s\n"  
        -- msg <- readChan chan