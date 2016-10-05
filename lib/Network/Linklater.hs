{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Network.Linklater
-- Copyright: (c) The Linklaterteers
-- License: BSD-style
-- Maintainer: hi@haolian.org
-- Stability: experimental
-- Portability: GHC
--
-- Here's a @/jpgto@ bot! If you run this program and then tell Slack
-- about your server (incoming hook and custom slash command) and then
-- type @/jpgto baby corgi@ in one of your channels, you'll get the
-- image from @http://baby.corgi.jpg.to@.
--
-- <https://github.com/hlian/linklater/blob/master/examples/JointPhotographicExpertsGroupTonga.hs>
--
-- One @/jpgto baby corgi@, et voila.
--
-- <<https://raw.githubusercontent.com/hlian/linklater/6232b950a333cfa6d5fffea997ec9ab8c2ce31ba/corgi.jpg>>

module Network.Linklater
       (
         say,
         slash,
         slashSimple,
         Channel(..),
         User(..),
         Message(..),
         Config(..),
         Command(..),
         Icon(..),
         Format(..)
       ) where

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Text.Strict.Lens
import           Network.Linklater.Batteries
import           Network.Linklater.Exceptions
import           Network.Linklater.Types

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Network.Wai as Wai

import           Data.Map (Map, fromList)
import           Data.Text (Text)
import           Network.HTTP.Types (status200, status400, parseSimpleQuery, ResponseHeaders)
import           Network.Wai (responseLBS, strictRequestBody, Application, Request)
import           Network.Wreq hiding (params, headers)
import           URI.ByteString (URI)

headers :: ResponseHeaders
headers =
  [("Content-type", "text/plain")]

responseOf :: Status -> Text -> Wai.Response
responseOf status message =
  responseLBS status headers (message ^. (re utf8 . lazy))

-- | I use the wreq package to post a 'Message', with a capital M, to
-- Slack and return the HTTP response. However, I need a 'Config' (an
-- incoming hook configured through Slack administration) first.
--
-- I throw a 'UnlikelyUTF8Problem' if the Slack message cannot be
-- decoded.
say :: (MonadThrow m, MonadIO m) => Message -> Config -> m (Response Text)
say message Config{..} =
  postWith _reasonableOptions (_configHookURL ^. unpacked) (Aeson.encode message) >>= _decode & liftIO

-- | A bot server for people who are in a hurry. Make a function that
-- takes a 'Command' and returns some 'Text' in 'IO' world, and we'll
-- convert it into a 'Network.WAI' application. If you want more
-- control over the request and respond, see 'slash'.
slashSimple :: (Command -> IO Text) -> Application
slashSimple f =
  slash (\command _ respond -> f command >>= (respond . responseOf status200))

-- | A bot server! As if by magic. This acts like a 'Network.WAI'
-- middleware: Linklater wraps around your application. (Really, it
-- just gives you a 'Command' to work with instead of a raw HTTP
-- request.)
slash :: (Command -> Application) -> Application
slash inner req respond = do
  params <- _paramsIO req
  case commandOfParams params of
    Right command ->
      inner command req respond
    Left msg ->
      respond (responseOf status400 ("linklater: unable to parse request: " <> msg ^. packed))

startRTM :: MonadIO m => APIToken -> m (Response URI)
startRTM token =
  startRTMWithOptions token (_reasonableOptions & authenticate)
  where
    authenticate =
       (param "token" .~ [view coerced token])
       . (param "simple_latest" .~ ["1"])
       . (param "no_unreads" .~ ["1"])

startRTMWithOptions :: MonadIO m => APIToken -> Options -> m (Response URI)
startRTMWithOptions token options =
  getWith options (_u "/api/rtm.start") >>= decode & liftIO
  where
    decode response =
      case Aeson.decode (response ^. responseBody) of
        Nothing -> throwM (UnlikelyURIProblem (show response))
        Just uri -> pure (uri <$ response)

----------------------------------------
-- ~ Helpers ~

-- | Disables Wreq's default behavior of throwing exceptions, which
-- seems reckless
_reasonableOptions :: Options
_reasonableOptions =
  defaults & checkStatus ?~ (\_ _ _ -> Nothing)

-- | Decoding helper, throws 'UnlikelyUTF8Problem', you get it.
_decode :: MonadThrow m => Response LazyBytes.ByteString -> m (Response Text)
_decode response =
  case response ^? responseBody . strict . utf8 of
    Nothing -> throwM (UnlikelyUTF8Problem (response ^. responseBody . strict))
    Just unicode -> pure (unicode <$ response)

_paramsIO :: Request -> IO (Map Text Text)
_paramsIO req = do
  lazyBytes <- strictRequestBody req
  let query = lazyBytes ^.. (strict . to parseSimpleQuery . traverse . to (both %~ view utf8))
  return (fromList query)

_u :: String -> String
_u = ("https://slack.com" ++)
