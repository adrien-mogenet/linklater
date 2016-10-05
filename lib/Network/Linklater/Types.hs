{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Linklater.Types where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

import BasePrelude
import Control.Lens hiding ((.=))
import Data.Aeson

-- | The unique 'C<number>' Slack assigns to each channel. Used to
-- 'say' things.
type ChannelID = Text

-- | Where 'slash' commands come from and where 'Message's go.
data Channel = Channel ChannelID Text deriving (Eq, Ord, Show)

-- | A username: no at-signs, just text!
newtype User = User Text deriving (Eq, Ord, Show)

-- | Incoming HTTP requests to the slash function get parsed into one
-- of these babies.
data Command = Command {
  -- | The command name.
  _commandName :: Text,
  -- | Who ran your slash command.
  _commandUser :: User,
  -- | Where the person ran your slash command.
  _commandChannel :: Channel,
  -- | Text for the slash command, if any.
  _commandText :: Maybe Text
  } deriving (Eq, Ord, Show)

-- | The icon next to the messages you `say`. (Images unsupported
-- right now, sorry.)
newtype Icon =
  -- | For example, ":stars2:".
  EmojiIcon Text deriving (Eq, Ord, Show)

-- | A little DSL for <https://api.slack.com/docs/formatting Slack formatting>.
data Format =
  -- | @"\<\@user|user>"@
    FormatAt User
  -- | @"\<\@user|user did this and that>"@
  | FormatUser User Text
  -- | @"\<http://example.com|user did this and that>"@
  | FormatLink Text Text
  -- | @"user did this &amp; that"@
  | FormatString Text

-- | Here's how you talk: you make one of these and pass it to 'say'.
-- Before the day is done, Linklater will convert this to a JSON blob
-- using 'Data.Aeson'.
--
--   * Simple messages are parsed by Slack with parse=full (i.e. as if you had typed it into the input box).
--
--   * Complex messages are parsed according to Slack formatting. See 'Format'.
--
data Message =
    SimpleMessage Icon Text Channel Text
  | FormattedMessage Icon Text Channel [Format]

-- | Like a curiosity about the world, you'll need one of these to
-- 'say' something.
data Config = Config {
  -- | This is the incoming web hook URL that Slack gave you. It's
  -- usually @https://hooks.slack.com/services/...@.
  _configHookURL :: Text
  }

-- | An API token, either a tester token or one obtained through
-- OAuth.
--
-- See:
--
--  * @https://api.slack.com/docs/oauth-test-tokens@
--
--  * @https://api.slack.com/docs/oauth@
newtype APIToken =
  APIToken Text
  deriving (Show, Eq, Ord)

unformat :: Format -> Text
unformat (FormatAt user@(User u)) = unformat (FormatUser user u)
unformat (FormatUser (User u) t) = "<@" <> u <> "|" <> t <> ">"
unformat (FormatLink url t) = "<" <> url <> "|" <> t <> ">"
unformat (FormatString t) = foldr (uncurry Text.replace) t [("<", "&lt;"), (">", "&gt;"), ("&", "&amp;")]

commandOfParams :: Map.Map Text Text -> Either String Command
commandOfParams params = do
  user <- userOf <$> paramOf "user_name"
  channel <- Channel <$> paramOf "channel_id" <*> paramOf "channel_name"
  Command <$> (nameOf <$> paramOf "command")
          <*> pure user
          <*> pure channel
          <*> pure (either (const Nothing) Just (paramOf "text"))
  where
    userOf = User . Text.filter (/= '@')
    nameOf = Text.filter (/= '/')
    paramOf key = case params ^. at key of
      Just value -> Right value
      Nothing -> Left ("paramOf: no key: " <> show key)

instance ToJSON Channel where
  toJSON (Channel cid _) = toJSON cid

instance ToJSON Message where
  toJSON m = case m of
    (FormattedMessage emoji username channel formats) ->
      toJSON_ emoji username channel (Text.unwords (unformat <$> formats)) False
    (SimpleMessage emoji username channel text) ->
      toJSON_ emoji username channel text True
    where
      toJSON_ (EmojiIcon emoji) username channel raw toParse =
        object [ "channel" .= channel
               , "icon_emoji" .= (":" <> emoji <> ":")
               , "parse" .= String (if toParse then "full" else "poop")
               , "username" .= username
               , "text" .= raw
               , "unfurl_links" .= True
               ]
