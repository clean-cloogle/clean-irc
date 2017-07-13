module test

import Gast
import IRC
import GenBimap
import Data.Func
import Data.Either

import Text

derive ggen IRCMessage, Either, IRCUser, IRCCommand, Maybe, CSepList, IRCNumReply, IRCReplies
derive genShow IRCMessage, Either, IRCUser, IRCCommand, Maybe, CSepList, IRCNumReply, IRCReplies

//Doesn't work, generates illegal irc commands with spaces in recipients
Start = concat $ Test [] pParsePrint

pParsePrint :: IRCMessage -> Bool
pParsePrint a
# str = toString a
= either (const False) ((==)str o toString) $ parseIRCMessage str
