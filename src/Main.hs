{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (lookupEnv)
import Control.Lens hiding ((.=))
import Control.Monad (void, forever, guard)
import Control.Monad.Except (runExceptT)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Network.Linklater (startRTM)
import Network.Linklater.Types (APIToken(..))
import Network.WebSockets as WS
import Wuss (runSecureClient)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text.Strict.Lens
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import URI.ByteString
import qualified Data.Text as T

type Bytes = ByteString

data Speech = Speech { _replyTo :: !Line, _t :: !T.Text } deriving (Show)
data Speech' = Speech' { _speech :: !Speech, _id :: !Int } deriving (Show)
data Line = Line { _channel :: !T.Text, _user :: !T.Text, _truth :: !T.Text } deriving (Eq, Ord, Show)

makeLenses ''Line

instance ToJSON Speech' where
  toJSON (Speech' (Speech line t) id_) =
    object [ "id" .= id_
           , "channel" .= (line ^. channel)
           , "text" .= t
           , "type" .= ("message" :: String)
           ]

instance FromJSON Line where
  parseJSON (Object o) = do
    reply_to <- o .:? "reply_to"
    guard (not (isReply reply_to))
    Line <$> o .: "channel" <*> o .: "user" <*> o .: "text"
    where
      isReply :: Maybe Int -> Bool
      isReply = isJust

-- |Connects the websocket and delivers messages
messenger :: URI -> Chan Speech -> Chan Bytes -> IO ()
messenger uri outbox inbox =
  case (uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked,
        uri ^? pathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      runSecureClient host 443 path (consumer inbox)
      return ()
    _ ->
      error ("invalid url: " <> show uri)
  where
    consumer :: Chan Bytes -> WS.ClientApp ()
    consumer chan conn = do
      -- Reading the socket is done in a forked thread while writing
      -- is done in this thread.
      -- Perhaps it should be the other way around?
      void $ forkIO (forever worker)
      forever listener
      where
        worker = do
          msg <- WS.receiveData conn
          writeChan chan msg
        listener = do
          speech <- readChan outbox
          WS.sendTextData conn (encode (Speech' speech 1))

-- |Attempts to decode inbox :: Chan Bytes
-- to whatever FromJSON object you expect from Slack
withInbox :: FromJSON a => Chan Bytes -> (a -> IO b) -> IO ()
withInbox inbox cont = do
  chan <- dupChan inbox
  (void . forkIO . forever) $ do
    bytes <- readChan chan
    case eitherDecode (bytes ^. lazy) of
      Left _ ->
        return ()
      Right o ->
        void (cont o)

-- |This "bot" just prints out every received message
showStdOut :: Chan Bytes -> IO ()
showStdOut inbox = do
  withInbox inbox $ \(line :: Line) -> print line

-- |This bot always replies "Hello World!"
helloWorldBot :: Chan Bytes -> Chan Speech -> IO ()
helloWorldBot inbox outbox = do
  withInbox inbox $ \line ->
    writeChan outbox (Speech line "Hello World!")

setup :: URI -> IO ()
setup uri = do
  -- Create two mailboxes:
  -- Incoming messages will be placed in inbox
  -- Outgoing messages can be placed in outbox
  (outbox :: Chan Speech)  <- newChan
  (inbox :: Chan Bytes) <- newChan
  -- Slack communcations will be set up and run in a separate thread
  forkIO $ messenger uri outbox inbox
  -- Bots operate by listening to inbox and responding to outbox
  showStdOut inbox
  helloWorldBot inbox outbox
  -- Our Work is Never Over
  void . forever $ readChan inbox

main :: IO ()
main = do
  -- Given API token startRTM will authenticate with slack and give
  -- webscoket communication endpoint
  Just token <- fmap (APIToken . T.pack) <$> lookupEnv "API_TOKEN"
  rtm <- runExceptT (startRTM token)
  case rtm of
    Left e -> error (show e)
    Right uri -> setup uri
