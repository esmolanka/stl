{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module STL.Runtime.Remote
  ( Serialisable
  , MessageTransport (..)
  , RemoteCaller
  , Registrar
  , withRemote
  , serve
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception (bracket, catches, Handler(..), throwIO, AsyncException(..), SomeException(..))
import Control.Monad.Reader

import Data.Aeson (FromJSON, ToJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Proxy

import STL.Runtime.Builtins
import STL.Runtime.Remote.Logger
import STL.Runtime.Serialisation
import STL.Runtime.TypeRep

type RemoteCaller = forall a b. (Serialisable a, Serialisable b) => (a :~> b) -> a -> IO b
type Registrar    = forall a b. (Serialisable a, Serialisable b) => ApiFunction a b -> IO (a :~> b)

----------------------------------------------------------------------

type ApiFunction a b = RemoteCaller -> a -> IO b

data SomeApiFunction where
  SomeApiFunction :: (Serialisable a, Serialisable b) => ApiFunction a b -> SomeApiFunction

newtype Registry = Registry { getFnMap :: IntMap SomeApiFunction }

emptyRegistry :: Registry
emptyRegistry = Registry mempty

lookupRegistry :: Int -> Registry -> Maybe SomeApiFunction
lookupRegistry n =
  IM.lookup n . getFnMap

insertRegistry :: (Serialisable a, Serialisable b) => Int -> ApiFunction a b -> Registry -> Registry
insertRegistry n f =
  Registry . IM.insert n (SomeApiFunction f) . getFnMap

data RegState = RegState
  { nextId :: !Int
  , fnMap  :: Registry
  }

emptyRegState :: RegState
emptyRegState = RegState 0 emptyRegistry

----------------------------------------------------------------------
-- Low level transport API

class MessageTransport conn where
  send :: conn -> ByteString -> IO ()
  recv :: conn -> IO ByteString
  close :: conn -> IO ()

----------------------------------------------------------------------
-- JSON level transport

sendValue :: (MessageTransport conn, ToJSON a) => conn -> a -> IO ()
sendValue conn = send conn . Aeson.encode

recvValue :: (MessageTransport conn, FromJSON a) => conn -> IO (Either String a)
recvValue conn = Aeson.eitherDecode <$> recv conn

----------------------------------------------------------------------
-- Request/response messaging layer

data RequestResponseState = RequestResponseState
  { nextMessageId :: !Int
  , responseSlots :: !(IntMap (MVar (Either Aeson.Value Aeson.Value)))
  }

data Message
  = Request      { _msgId :: Int, _body :: Aeson.Value }
  | Notification {                _body :: Aeson.Value }
  | Response     { _msgId :: Int, _body :: Aeson.Value }
  | Failure      { _msgId :: Int, _body :: Aeson.Value }

instance ToJSON Message where
  toJSON = \case
    Request msgId  body -> Aeson.object [ "id" .= msgId, "body"   .= body ]
    Notification   body -> Aeson.object [                "body"   .= body ]
    Response msgId body -> Aeson.object [ "id" .= msgId, "result" .= body ]
    Failure msgId  body -> Aeson.object [ "id" .= msgId, "error"  .= body ]

instance FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \obj -> do
    msgid <- obj .:? "id"
    case msgid of
      Nothing -> Notification <$> obj .: "body"
      Just msgid' ->
        (Request  msgid' <$> obj .: "body") <|>
        (Response msgid' <$> obj .: "result") <|>
        (Failure  msgid' <$> obj .: "error")

data RpcTransport conn = RpcTransport
  { _rpcConnection :: conn
  , _rpcState      :: MVar RequestResponseState
  , _rpcActorName  :: String
  }

withRpcTransport :: MessageTransport conn => conn -> String -> (RpcTransport conn -> IO a) -> IO a
withRpcTransport conn name f = do
  bracket
    (newMVar (RequestResponseState 0 mempty))
    (const (pure ()))
    (\st -> f (RpcTransport conn st name))

request :: (MessageTransport conn) => RpcTransport conn -> Aeson.Value -> IO Aeson.Value
request (RpcTransport conn st name) msg = do
  slot <- newEmptyMVar
  n <- modifyMVar st $ \s -> pure
    ( s { nextMessageId = nextMessageId s + 1
        , responseSlots = IM.insert (nextMessageId s) slot (responseSlots s)
        }
    , nextMessageId s
    )
  logStrLn DebugLevel $ name ++ " REQ: Requesting " ++ show n
  sendValue conn (Request n msg)
  resp <- takeMVar slot
  case resp of
    Left err -> error ("Remote error: " ++ show err)
    Right body -> do
      logStrLn DebugLevel $ name ++ " REQ: Got the response " ++ show n
      pure body

respond :: MessageTransport conn => RpcTransport conn -> (Aeson.Value -> IO Aeson.Value) -> IO ()
respond (RpcTransport conn st name) handleRequest = forever $ do
  logStrLn DebugLevel $ name ++ " RSP: Waiting for message..."
  msg <- recvValue conn
  case msg of
    Left _ ->
      logStrLn DebugLevel $ name ++ " RSP: Got an invalid message..."

    Right (Request n body) -> do
      logStrLn DebugLevel $ name ++ " RSP: Got request " ++ show n
      void $ forkIO $ do
        response <- fmap (Response n) (handleRequest body) `catches`
          [ Handler $ \case
              ThreadKilled -> throwIO ThreadKilled
              UserInterrupt -> throwIO UserInterrupt
              e -> return $ Failure n (Aeson.toJSON (show e))
          , Handler $ \e@SomeException{} -> return $ Failure n (Aeson.toJSON (show e))
          ]
        sendValue conn response
        logStrLn DebugLevel $ name ++ " RSP: Sending response " ++ show n

    Right (Notification body) -> do
      logStrLn DebugLevel $ name ++ " RSP: Got notification"
      void $ forkIO $ void (handleRequest body) `catches`
        [ Handler $ \case
            ThreadKilled -> throwIO ThreadKilled
            UserInterrupt -> throwIO UserInterrupt
            _ -> return ()
        , Handler $ \(SomeException _) -> return ()
        ]

    Right (Response n body) -> do
      logStrLn DebugLevel $ name ++ " RSP: Got reponse " ++ show n
      responseSlot <- modifyMVar st $ \s ->
        return
          ( s { responseSlots = IM.delete n (responseSlots s) }
          , IM.lookup n (responseSlots s)
          )
      case responseSlot of
        Nothing ->
          logStrLn DebugLevel $ name ++ " RSP: Ignoring response " ++ show n
        Just responseSlot' -> do
          logStrLn DebugLevel $ name ++ " RSP: Using response " ++ show n
          putMVar responseSlot' (Right body)

    Right (Failure n exc) -> do
      logStrLn DebugLevel $ name ++ " RSP: Got failure " ++ show n
      responseSlot <- modifyMVar st $ \s ->
        return
          ( s { responseSlots = IM.delete n (responseSlots s) }
          , IM.lookup n (responseSlots s)
          )
      case responseSlot of
        Nothing ->
          logStrLn DebugLevel $ name ++ " RSP: Ignoring error message " ++ show n
        Just responseSlot' -> do
          logStrLn DebugLevel $ name ++ " RSP: Using error message " ++ show n
          putMVar responseSlot' (Left exc)

----------------------------------------------------------------------
-- Remote procedure call layer

data Call = Call { _callFunctionId :: Int, _callArg :: Aeson.Value }

instance ToJSON Call where
  toJSON (Call funid arg) =
    Aeson.object
      [ "fun" .= funid
      , "arg" .= arg
      ]

instance FromJSON Call where
  parseJSON = Aeson.withObject "function call" $ \obj ->
    Call <$> obj .: "fun" <*> obj .: "arg"

mkRemoteCaller :: (MessageTransport conn) => RpcTransport conn -> RemoteCaller
mkRemoteCaller transport = \(ApiFunctionId f) a -> do
  resp <- request transport (Aeson.toJSON (Call f (Aeson.toJSON a)))
  case Aeson.parseMaybe Aeson.parseJSON resp of
    Nothing -> error "malformed response"
    Just r -> pure r

dispatchRemoteCalls :: (MessageTransport conn) => RpcTransport conn -> MVar RegState -> Aeson.Value -> IO Aeson.Value
dispatchRemoteCalls transport registryVar msg =
  case Aeson.parseMaybe Aeson.parseJSON msg of
    Nothing -> error "malformed function call"
    Just (Call n arg) -> do
      registry <- fnMap <$> readMVar registryVar
      case lookupRegistry n registry of
        Nothing -> error $ "invalid function handle: @" ++ show n
        Just (SomeApiFunction f) ->
          case Aeson.parseMaybe Aeson.parseJSON arg of
            Nothing -> error "malformed function argument"
            Just arg' -> Aeson.toJSON <$> f (mkRemoteCaller transport) arg'

----------------------------------------------------------------------
-- Service API layer

newtype RootObj a = RootObj a

instance ToJSON a => ToJSON (RootObj a) where
  toJSON (RootObj a) = Aeson.object [ "root" .= a ]
instance FromJSON a => FromJSON (RootObj a) where
  parseJSON = Aeson.withObject "root object" $ \obj ->
    RootObj <$> obj .: "root"

newtype ClientType = ClientType SType

instance ToJSON ClientType where
  toJSON (ClientType a) = Aeson.object [ "client_type" .= a ]
instance FromJSON ClientType where
  parseJSON = Aeson.withObject "ClientType" $ \obj -> ClientType <$> obj .: "client_type"

newtype ServiceType = ServiceType SType

instance ToJSON ServiceType where
  toJSON (ServiceType a) = Aeson.object [ "service_type" .= a ]
instance FromJSON ServiceType where
  parseJSON = Aeson.withObject "ServiceType" $ \obj -> ServiceType <$> obj .: "service_type"

mkRegistrar :: MVar RegState -> Registrar
mkRegistrar var f = do
  n <- modifyMVar var $ \s -> pure
    ( s { nextId = succ (nextId s)
        , fnMap = insertRegistry (nextId s) f (fnMap s)
        }
    , nextId s
    )
  return (ApiFunctionId n)

serve
  :: forall api conn. (Serialisable api, TypeOf api, MessageTransport conn) =>
     conn -> (Registrar -> IO api) -> IO ()
serve conn mkapi = do
  tid <- myThreadId

  -- 1. Exchange types
  logStrLn DebugLevel $ show tid ++ " Service: Exchange types"
  let serviceRep = typeOf (Proxy @api)
  sendValue conn (ServiceType serviceRep)

  logStrLn DebugLevel $ show tid ++ " Service: Receiving the type"
  ClientType clientRep <- either error pure =<< recvValue conn

  -- 2. Run-time subsumption checking
  logStrLn DebugLevel $ show tid ++ " Service: Subsumption checking"
  unless (serviceRep `isSubsumedBy` clientRep) $
    error "Incompatible APIs"

  registryVar <- newMVar emptyRegState
  api <- mkapi (mkRegistrar registryVar)

  -- 3. Publish service's API
  logStrLn DebugLevel $ show tid ++ " Service: Send a root object"
  sendValue conn (RootObj api)

  -- 4. Handle requests:
  logStrLn DebugLevel $ show tid ++ " Service: Handle requests"
  withRpcTransport conn "S" $ \transport ->
    respond transport (dispatchRemoteCalls transport registryVar)

withRemote
  :: forall api conn r. (Serialisable api, TypeOf api, MessageTransport conn) =>
     conn -> (Registrar -> RemoteCaller -> api -> IO r) -> IO r
withRemote conn f = do
  tid <- myThreadId

  -- 1. Exchange types
  logStrLn DebugLevel $ show tid ++ " Client: Exchange types"
  let clientRep = typeOf (Proxy @api)
  sendValue conn (ClientType clientRep)

  logStrLn DebugLevel $ show tid ++ " Client: Receiving the type"
  ServiceType serviceRep <- either error pure =<< recvValue conn

  -- 2. Run-time subsumption checking
  logStrLn DebugLevel $ show tid ++ " Client: Subsumption checking"
  unless (serviceRep `isSubsumedBy` clientRep) $
    error "Incompatible APIs"

  -- 3. Receive service's API
  logStrLn DebugLevel $ show tid ++ " Client: Get a root object"
  RootObj api <- either error pure =<< recvValue conn
  registryVar <- newMVar emptyRegState

  -- 4. Handle callbacks and run client function
  logStrLn DebugLevel $ show tid ++ " Client: Run client handler"
  withRpcTransport conn "                                     C" $ \transport ->
    bracket
      (forkIO (respond transport (dispatchRemoteCalls transport registryVar)))
      killThread
      (\_ -> f (mkRegistrar registryVar) (mkRemoteCaller transport) api)
