{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module STL.Runtime.Remote.Sockets
  ( SocketTransport
  , connectTo
  , listenOnPort
  ) where

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams

import Control.Monad
import Control.Applicative ((<|>))
import Control.Exception (bracket, throwIO, Exception(..), fromException)
import Control.Concurrent

import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.Socket (Socket)
import qualified Network.Socket as N

import STL.Runtime.Remote (MessageTransport(..))
import STL.Runtime.Remote.Logger

parsePacket :: Atto.Parser BS.ByteString
parsePacket = do
  len <- Atto.skipSpace
    *> (Atto.string "Content-Length:" <?> "Content-Length keyword")
    *> Atto.skipSpace
    *> (Atto.decimal <?> "Content-Length argument")
    <* crlf
    <* crlf
  Atto.take len <?> "payload"
  where
    crlf = (Atto.char '\r' <* Atto.char '\n') <?> "CRLF"

parsePacket' :: Atto.Parser (Maybe BS.ByteString)
parsePacket' =
  (Nothing <$ Atto.skipSpace <* Atto.endOfInput)
  <|> (Just <$> parsePacket)

writePacket :: BL.ByteString -> BL.ByteString
writePacket json = BL.toLazyByteString $
  BL.byteString "Content-Length: " <> BL.int64Dec (BL.length json) <>
  BL.byteString "\r\n\r\n" <>
  BL.lazyByteString json <>
  BL.byteString "\r\n"

----------------------------------------------------------------------

data SocketTransport = SocketTransport
  (InputStream BS.ByteString)
  (OutputStream BS.ByteString)

data SocketTransportException = SocketTransportClosedException
  deriving (Eq, Show)

instance Exception SocketTransportException

instance MessageTransport SocketTransport where
  send (SocketTransport _ out) bs = do
    forM_ (BL.toChunks (writePacket bs)) $ \chunk ->
      Streams.write (Just chunk) out

  recv (SocketTransport inp _) = do
    msg <- Streams.read inp
    case msg of
      Nothing -> throwIO SocketTransportClosedException
      Just a -> pure (BL.fromStrict a)

  close (SocketTransport _ _) = pure ()

----------------------------------------------------------------------

createSocketTransport :: Socket -> IO SocketTransport
createSocketTransport sock = do
  (inp, out) <- Streams.socketToStreams sock
  inp' <- Streams.parserToInputStream parsePacket' inp
  pure $ SocketTransport inp' out

connectTo :: String -> Int -> (SocketTransport -> IO a) -> IO a
connectTo hostname port act = N.withSocketsDo $ do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  addr : _ <- N.getAddrInfo (Just hints) (Just hostname) (Just (show port))
  bracket
    (do sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
        N.connect sock $ N.addrAddress addr
        return sock)
    N.close
    (\sock -> createSocketTransport sock >>= act)

listenOnPort :: Int -> (SocketTransport -> IO ()) -> IO ()
listenOnPort port act = N.withSocketsDo $ do
  let hints = N.defaultHints
        { N.addrFlags = [N.AI_PASSIVE]
        , N.addrSocketType = N.Stream
        }
  addr : _ <- N.getAddrInfo (Just hints) Nothing (Just (show port))
  bracket (open addr) N.close loop
  where
    open addr = do
      sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
      N.setSocketOption sock N.ReuseAddr 1
      N.setCloseOnExecIfNeeded (N.fdSocket sock)
      N.bind sock (N.addrAddress addr)
      N.listen sock 100
      return sock
    loop sock = forever $ do
      (conn, peer) <- N.accept sock
      logStrLn InfoLevel ("Connected " ++ show peer)
      transport <- createSocketTransport conn
      void $ forkFinally (act transport) $ \case
        Right _ -> N.close conn
        Left err -> do
          case fromException err of
            Just SocketTransportClosedException ->
              pure ()
            Nothing -> do
              logStrLn ErrorLevel $ show peer ++ " : " ++ show err
              N.close conn
          logStrLn InfoLevel ("Disconnected " ++ show peer)
