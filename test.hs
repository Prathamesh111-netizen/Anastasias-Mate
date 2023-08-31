{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main
    ( Agent (..)
    , delay
    , broadcast
    , receive
    , PingPongMessage (..)
    , ping
    , pong
    , runIO
    , Slot
    , NodeId
    , Block (..)
    , Chain (..)
    , chainLength
    , slotLeader
    , chainValid
    , clock
    , node
    , runPure
    ) where

import Control.Concurrent.STM
import Control.Monad.Free
import Numeric.Natural (Natural)
import Text.Printf (printf)

data AgentF msg a =
    Delay a
  | Broadcast msg a
  | Receive (msg -> a)

instance Functor (AgentF msg) where
    fmap f (Delay a) = Delay (f a)
    fmap f (Broadcast msg a) = Broadcast msg (f a)
    fmap f (Receive g) = Receive (f . g)

instance Applicative (AgentF msg) where
    pure = Delay
    Delay f <*> a = fmap f a
    Broadcast msg f <*> a = Broadcast msg (f <*> a)
    Receive g <*> a = Receive (\msg -> g msg (run a))

instance Monad (AgentF msg) where
    return = pure
    Delay a >>= f = f a
    Broadcast msg a >>= f = Broadcast msg (a >>= f)
    Receive g >>= f = Receive (\msg -> g msg >>= f)

type Agent msg = Free (AgentF msg)

delay :: Agent msg ()
delay = liftF (Delay ())

broadcast :: msg -> Agent msg ()
broadcast msg = liftF (Broadcast msg ())

receive :: Agent msg msg
receive = liftF (Receive id)

data PingPongMessage =
    Ping
  | Pong
  deriving (Show)

ping :: Agent PingPongMessage ()
ping = do
    delay
    broadcast Ping
    msg <- receive
    case msg of
        Pong -> ping
        _ -> pure ()

pong :: Agent PingPongMessage ()
pong = do
    msg <- receive
    case msg of
        Ping -> do
            delay
            broadcast Pong
            pong
        _ -> pure ()

runIO :: Show msg => [Agent msg ()] -> IO ()
runIO agents = do
    chan <- newTChanIO
    mapM_ (runAgent chan) agents
  where
    runAgent chan agent = void $ forkIO $ run agent chan

    run :: Show msg => Agent msg a -> TChan msg -> IO a
    run (Pure a) _ = pure a
    run (Free (Delay next)) chan = do
        threadDelay 1000000
        run next chan
    run (Free (Broadcast msg next)) chan = do
        atomically $ writeTChan chan msg
        putStrLn $ "Broadcast: " ++ show msg
        run next chan
    run (Free (Receive f)) chan = do
        msg <- atomically $ readTChan chan
        run (f msg) chan

type Slot = Natural
type NodeId = Natural

data Block = Block
    { slot :: Slot
    , creator :: NodeId
    }

instance Show Block where
    show b = printf "{%d %d}" (slot b) (creator b)

infixl 5 :>

data Chain =
    Genesis
  | Chain :> Block

instance Show Chain where
    showsPrec _ Genesis = showString "Genesis"
    showsPrec d (c :> b) = showsPrec 0 c . showString " :> " . showsPrec 0 b

chainLength :: Chain -> Int
chainLength Genesis = 0
chainLength (c :> _) = 1 + chainLength c

slotLeader :: Int -> Slot -> NodeId
slotLeader totalSlots currentSlot = currentSlot `mod` fromIntegral totalSlots

chainValid :: Int -> Slot -> Chain -> Bool
chainValid totalNodes _ Genesis = True
chainValid totalNodes currentSlot (chain :> newBlock) =
    slot newBlock == currentSlot && creator newBlock == slotLeader totalNodes currentSlot && chainValid totalNodes (currentSlot - 1) chain

data BftMessage =
    Time Slot
  | NewChain Chain
  deriving Show

clock :: Agent BftMessage a
clock = do
    broadcast (Time 0)
    loop 1
  where
    loop n = do
        delay
        broadcast (Time n)
        loop (n + 1)

node :: Int -> NodeId -> Agent BftMessage a
node totalNodes nodeId = loop Genesis
  where
    loop currentChain = do
        Time currentSlot <- receive
        let newBlock = Block currentSlot nodeId
        let newChain = currentChain :> newBlock
        if chainValid totalNodes currentSlot newChain
            then do
                broadcast (NewChain newChain)
                loop newChain
            else loop currentChain

runPure :: [Agent msg ()] -> [(Natural, msg)]
runPure agents = loop 0 agents []
  where
    loop _ [] broadcasts = broadcasts
    loop currentTime (a:as) broadcasts =
        case run a currentTime of
            (currentTime', Broadcast msg next) -> loop currentTime' as ((currentTime', msg) : broadcasts)
            (currentTime', _) -> loop currentTime' as broadcasts

    run :: Agent msg a -> Natural -> (Natural, a)
    run (Pure x) currentTime = (currentTime, x)
    run (Free (Delay next)) currentTime = run next (currentTime + 1)
    run (Free (Broadcast _ next)) currentTime = run next (currentTime + 1)
    run (Free (Receive f)) currentTime = run (f undefined) (currentTime + 1)
