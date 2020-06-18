module STL.Runtime.Remote.InMemory
  ( newChannelPair
  , ChanTransport
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)

import STL.Runtime.Remote (MessageTransport(..))
import STL.Runtime.Remote.Logger

data ChanTransport = ChanTransport
  { chanIn   :: TQueue ByteString
  , chanOut  :: TQueue ByteString
  , chanOpen :: TVar Bool
  }

newChannelPair :: IO (ChanTransport, ChanTransport)
newChannelPair = do
  chan1 <- newTQueueIO
  chan2 <- newTQueueIO
  isOpen <- newTVarIO True
  return
    ( ChanTransport chan1 chan2 isOpen
    , ChanTransport chan2 chan1 isOpen
    )

instance MessageTransport ChanTransport where
  send ct msg = do
    tid <- myThreadId
    logStrLn DebugLevel $ show tid ++ ": Sending..."
    atomically $ do
      isOpen <- readTVar (chanOpen ct)
      if isOpen
        then writeTQueue (chanOut ct) msg
        else error "Sending to a closed channel"

  recv ct = do
    tid <- myThreadId
    res <- atomically $ do
      isOpen <- readTVar (chanOpen ct)
      if isOpen
        then Just <$> readTQueue (chanIn ct)
        else pure Nothing
    logStrLn DebugLevel $ show tid ++ ": Received..."
    case res of
      Just msg -> pure msg
      Nothing -> error "Receiving from a closed channel"

  close ct = atomically $ do
    modifyTVar (chanOpen ct) (const False)
