{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Server where

import qualified System.Log.Logger
import System.FilePath (takeFileName)

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Strict

import Control.Lens

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import Data.Foldable (toList)
import qualified Data.Rope.UTF16 as Rope
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Language.Haskell.LSP.Control as LSP
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Language.Haskell.LSP.VFS as LSP

import STL
import STL.Core.Check
import STL.Core.Subsumption
import STL.Elab (dsModule, Handlers(..))
import STL.Pretty hiding (list)
import STL.Syntax (parseModule')

main :: IO ()
main = run Nothing

data ServerState = ServerState
  { lspFuncs :: LSP.LspFuncs ()
  }

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run mlog = do
  setupLogger mlog
  st <- newEmptyMVar
  let callbacks = LSP.InitializeCallbacks
        { LSP.onInitialConfiguration = const (pure ())
        , LSP.onConfigurationChange = const (pure ())
        , LSP.onStartup = \funcs -> putMVar st (ServerState funcs) >> return Nothing
        }
  void $ LSP.run callbacks (lspHandlers st) lspOptions Nothing

setupLogger :: Maybe FilePath -> IO ()
setupLogger Nothing           = pure ()
setupLogger (Just "[OUTPUT]") = LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLogger file              = LSP.setupLogger file [] System.Log.Logger.DEBUG

lspOptions :: LSP.Options
lspOptions = def
  { LSP.textDocumentSync = Just syncOptions
  , LSP.executeCommandProvider = Nothing
  , LSP.documentLinkProvider = Nothing
  }

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

lspHandlers :: MVar ServerState -> LSP.Handlers
lspHandlers s = def
  { LSP.initializedHandler                       = Just $ wrapHandler s nullHandler
  , LSP.didOpenTextDocumentNotificationHandler   = Just $ wrapHandler s didOpenTextDocumentNotificationHandler
  , LSP.didChangeTextDocumentNotificationHandler = Just $ wrapHandler s nullHandler
  , LSP.didSaveTextDocumentNotificationHandler   = Just $ wrapHandler s didSaveTextDocumentNotificationHandler
  , LSP.didCloseTextDocumentNotificationHandler  = Just $ wrapHandler s nullHandler
  , LSP.cancelNotificationHandler                = Just $ wrapHandler s nullHandler
  , LSP.responseHandler                          = Just $ wrapHandler s nullHandler
  }

----------------------------------------------------------------------

data Severity
  = Error
  | Warning
  | Info
  | Log

type HandlerM = ExceptT (Severity, Text) (StateT ServerState IO)

wrapHandler
  :: MVar ServerState
  -> (a -> HandlerM ())
  -> a
  -> IO ()
wrapHandler vstate handle message =
  modifyMVar_ vstate $
    execStateT . runExceptT $
      catchError (handle message) lspUserMessage

nullHandler :: a -> HandlerM ()
nullHandler _ = return ()

lspUserMessage :: (Severity, Text) -> HandlerM ()
lspUserMessage (Log, text) =
  lspSendNotification LSP.NotLogMessage J.WindowLogMessage $
    J.LogMessageParams J.MtLog text
lspUserMessage (severity, text) =
  lspSendNotification LSP.NotShowMessage J.WindowShowMessage $
    J.ShowMessageParams severity' text
  where
    severity' =
      case severity of
        Error -> J.MtError
        Warning -> J.MtWarning
        Info -> J.MtInfo
        Log -> J.MtLog

lspSend :: LSP.FromServerMessage -> HandlerM ()
lspSend msg = do
  send <- gets (LSP.sendFunc . lspFuncs)
  liftIO $ send msg

lspRespond :: (J.ResponseMessage response -> LSP.FromServerMessage)
  -> J.RequestMessage J.ClientMethod request response -> response -> HandlerM ()
lspRespond constructor request response =
  lspSend . constructor $ LSP.makeResponseMessage request response

lspSendNotification
  :: (J.NotificationMessage J.ServerMethod params -> LSP.FromServerMessage)
  -> J.ServerMethod -> params -> HandlerM ()
lspSendNotification constructor method params =
  lspSend . constructor $ J.NotificationMessage "2.0" method params

lspRequest
  :: (J.RequestMessage J.ServerMethod params response -> LSP.FromServerMessage)
  -> J.ServerMethod -> params -> HandlerM ()
lspRequest constructor method params = do
  getNextReqId <- gets (LSP.getNextReqId . lspFuncs)
  reqId <- liftIO getNextReqId
  lspSend . constructor $ J.RequestMessage "2.0" reqId method params

----------------------------------------------------------------------

didOpenTextDocumentNotificationHandler
  :: J.DidOpenTextDocumentNotification -> HandlerM ()
didOpenTextDocumentNotificationHandler notification = do
  let uri = notification ^. J.params . J.textDocument . J.uri
  diagnosticsHandler uri

didSaveTextDocumentNotificationHandler
  :: J.DidSaveTextDocumentNotification -> HandlerM ()
didSaveTextDocumentNotificationHandler notification = do
  let uri = notification ^. J.params . J.textDocument . J.uri
  diagnosticsHandler uri

----------------------------------------------------------------------

diagnosticsHandler :: J.Uri -> HandlerM ()
diagnosticsHandler uri = do
  diagnostics <-
    runModule (maybe (T.unpack (J.getUri uri)) takeFileName (J.uriToFilePath uri)) .
      BL.fromStrict . TE.encodeUtf8 <$> readUri uri
  lspSendNotification
    LSP.NotPublishDiagnostics
    J.TextDocumentPublishDiagnostics
    (J.PublishDiagnosticsParams uri (J.List (toList diagnostics)))

readUri :: J.Uri -> HandlerM Text
readUri uri = do
  getVirtualFileFunc <- gets (LSP.getVirtualFileFunc . lspFuncs)
  mVirtualFile <- liftIO $ getVirtualFileFunc (J.toNormalizedUri uri)
  case mVirtualFile of
    Just (LSP.VirtualFile _ rope _) -> return (Rope.toText rope)
    Nothing -> fail $ "Could not find " <> show uri <> " in VFS."


----------------------------------------------------------------------

output :: (MonadWriter (Seq J.Diagnostic) m) => Position -> J.DiagnosticSeverity -> Doc ann -> m ()
output pos@(Position _ l c _ _) severity msg = tell $ Seq.singleton $ J.Diagnostic
  { J._range = J.Range (J.Position (pred l) (pred c)) (J.Position (pred l) c)
  , J._severity = Just severity
  , J._source = Just "evaluation"
  , J._code = Nothing
  , J._message = renderDoc (nest 2 $ vsep [pretty pos <> colon, msg])
  , J._relatedInformation = Nothing
  }

check :: (MonadTC m, MonadWriter (Seq J.Diagnostic) m) => Position -> Type -> Type -> m ()
check pos sub sup = do
  k <- inferKind sub
  void $ expectExactly k $ inferKind sup
  sub' <- normalise lookupGlobal sub
  sup' <- normalise lookupGlobal sup
  let (res, _) = runSubsumption mempty (sub' `subsumedBy` sup')
  output pos (either (const J.DsWarning) (const J.DsInfo) res) $ vsep
    [ either cpretty (\_ -> "OK") $ res
    , mempty
    , "While checking:"
    , cpretty sub'
    , indent 2 "<:"
    , cpretty sup'
    ]

eval :: (MonadTC m, MonadWriter (Seq J.Diagnostic) m) => Position -> Type -> m ()
eval pos ty = do
  k   <- inferKind ty
  ty' <- normalise lookupGlobal ty
  void $ expectExactly k $ inferKind ty'
  output pos J.DsInfo $ vsep
    [ cpretty ty'
    , ":" <+> cpretty k
    , mempty
    ]

mkParserDiagnostic :: (Position, Doc ann) -> Seq J.Diagnostic
mkParserDiagnostic (pos@(Position _ l c l' c'), msg) = Seq.singleton $ J.Diagnostic
  { J._range = J.Range (J.Position (pred l) (pred c)) (J.Position (pred l') (pred c'))
  , J._severity = Just J.DsError
  , J._source = Just "parser"
  , J._code = Nothing
  , J._message = renderDoc (nest 2 $ vsep [pretty pos <> colon <+> "error:", msg])
  , J._relatedInformation = Nothing
  }

mkTypecheckerDiagnostic :: Err -> Seq J.Diagnostic
mkTypecheckerDiagnostic err = Seq.singleton $ J.Diagnostic
  { J._range = J.Range (J.Position (pred l) (pred c)) (J.Position (pred l') (pred c'))
  , J._severity = Just J.DsError
  , J._source = Just "typechecker"
  , J._code = Nothing
  , J._message = renderDoc (cpretty err)
  , J._relatedInformation = Nothing
  }
  where
    (Position _ l c l' c') = errPosition err

runModule :: FilePath -> BL.ByteString -> Seq J.Diagnostic
runModule fn str = do
  let hnd :: (MonadTC m, MonadWriter (Seq J.Diagnostic) m) => Handlers (m ())
      hnd = Handlers eval check (\_ k -> purifyProgram k)
  case parseModule' fn str of
    Left err -> mkParserDiagnostic err
    Right modul ->
      let res = runIdentity $ runRawTCT $ runWriterT $ checkProgram (dsModule hnd modul) $ \case
            Just ty -> hNormalise hnd (getPosition ty) ty
            Nothing -> pure ()
      in case res of
           Left err -> mkTypecheckerDiagnostic err
           Right ((), diagnostics) -> diagnostics
