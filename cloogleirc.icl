module cloogleirc

import Cloogle
import GenPrint
import StdEnv

import Data.Functor
import Data.Maybe
import Data.Either
from Data.Func import $, mapSt
from Text import class Text(..), instance Text String, instance + String

import Internet.HTTP

import Text.JSON

import Text.URI
import System.Time

import Control.Applicative
import qualified Control.Monad as CM
import qualified Data.Map as DM
from Control.Monad import class Monad, instance Monad Maybe, >>=
from Text.Encodings.UrlEncoding import urlEncode
import System.CommandLine
import Internet.HTTP
import Data.Error
import Data.List
import Data.Functor
import Data.Tuple

import TCPIP
import IRC
import IRCBot

TIMEOUT :== Just 10000
SERVER :== "irc.freenode.net"

shorten :: String *World -> (String, *World)
shorten s w 
# s = if (startsWith "http://" s) s (if (startsWith "https://" s) s ("http://" + s))
# data = "type=regular&url="+urlEncode s+"&token=a"
# (mer, w) = doHTTPRequest 
		{ newHTTPRequest
		& req_method = HTTP_POST
		, req_path = "/"
		, server_name = "cloo.gl"
		, server_port = 80
		, req_headers = 'DM'.fromList
			[("Content-Type", "application/x-www-form-urlencoded")
			,("Content-Length", toString $ size data)
			,("Accept", "*/*")]
		, req_data = data} 10000 w
| isError mer = ("request failed: " + fromError mer, w)
# resp = fromOk mer
= (resp.rsp_data, w)

cloogle :: String *World -> (String, *World)
cloogle data w
# (mer, w) = doHTTPRequestL
		{ newHTTPRequest
		& req_path = "/api.php"
		, req_query = "?str=" + urlEncode data
		, req_headers = 'DM'.fromList [("User-Agent", "cloogle-irc")]
		, server_name = "cloogle.org"
		, server_port = 80} 10000 10 w
| isError mer = ("request failed: " + fromError mer, w)
# resp = fromOk mer
= case fromJSON $ fromString resp.HTTPResponse.rsp_data of
	Nothing = ("couldn't parse json", w)
	Just clr = ("Results for " + data + " -- https://cloogle.org/#" +
		replaceSubString "+" "%20" (urlEncode data) + "\n" +
		processResults clr, w)
	where
		processResults :: Response -> String
		processResults resp
		| resp.return > 127 = "Failed: return code: " + toString resp.return + ", " + resp.msg
		= join "\n" $ map processResult $ take 3 resp.data
		
		processResult :: Result -> String
		processResult (FunctionResult (br, {func}))
			= "Function in " +++ br.library +++ ": " +++ br.modul +++ "\n" +++ func
		processResult (TypeResult (br, {type}))
			= "Type in " +++ br.library +++ ": " +++ br.modul +++ "\n" +++ limitResults type
		processResult (ClassResult (br, {class_name,class_funs}))
			= "Class in " +++ br.library +++ ": " +++ br.modul +++ "\n" +++ class_name +++ " with "
				+++ toString (length class_funs) +++ " class functions"
		processResult (ModuleResult (br, _))
			= "Module in " +++ br.library +++ ": " +++ br.modul

		limitResults :: String -> String
		limitResults s
		# lines = split "\n" s
		| length lines > 4 = limitResults (join "\n" (take 3 lines) + "\n...")
		= join "\n" (map maxWidth lines)
		
		maxWidth :: String -> String
		maxWidth s
		| size s > 80 = subString 0 77 s + "..."
		= s

:: BotSettings =
		{ bs_nick     :: String
		, bs_nickserv :: Maybe String
		, bs_autojoin :: [String]
		, bs_port     :: Int
		, bs_server   :: String
		, bs_strftime :: String
		}

Start :: *World -> (Maybe String, *World)
Start w
# ([arg0:args], w) = getCommandLine w
# (io, w) = stdio w
# bs = parseCLI args 
| isError bs = (Just $ "\n" +++ fromError bs +++ "\n", snd $ fclose io w)
# (Ok bs) = bs
# (merr, io, w) = bot (bs.bs_server, bs.bs_port) (startup bs) shutdown io (process bs.bs_strftime) w
= (merr, snd $ fclose io w)
	where
		parseCLI :: [String] -> MaybeErrorString BotSettings
		parseCLI [] = Ok
			{ bs_nick     = "clooglebot"
			, bs_nickserv = Nothing
			, bs_autojoin = []
			, bs_port     = 6667
			, bs_server   = "irc.freenode.net"
			, bs_strftime = "%s"
			}
		parseCLI [a:as]
		| a == "-f" || a == "--strftime"
			= arg1 "--strftime" as \a c->{c & bs_strftime=a}
		| a == "-n" || a == "--nick"
			= arg1 "--nick" as \a c->{c & bs_nick=a}
		| a == "-ns" || a == "--nickserv"
			= arg1 "--nickserv" as \a c->{c & bs_nickserv=Just a}
		| a == "-a" || a == "--autojoin"
			= arg1 "--autojoin" as \a c->{c & bs_autojoin=c.bs_autojoin ++ [a]}
		| a == "-p" || a == "--port"
			= arg1 "--port" as \a c->{c & bs_port=toInt a}
		| a == "-s" || a == "--server"
			= arg1 "--server" as \a c->{c & bs_server=a}
		| a == "-h" || a == "--help" = Error $ join "\n" $
			[ "Usage: cloogle [OPTS]"
			, "Options:"
			, "\t--strftime/-f FORMAT   strftime format used in the output. default: %s\n"
			, "\t--nick/-n NICKNAME     Use the given nickname instead of clooglebot"
			, "\t--nickserv/-ns PW      Identify via the given password with NickServ"
			, "\t--port/-p PORT         Use the given port instead of port 6667"
			, "\t--server/-s SERVER     Use the given server instead of irc.freenode.net"
			, "\t--autojoin/-a CHANNEL  Add CHANNEL to the autojoin list. This command "
			, "\t                       can be called multiple times. Beware that #"
			, "\t                       has to be escaped in most shells"
			]
		= Error $ "Unknown option: " +++ a

		arg1 name [] _ = Error $ name +++ " requires an argument"
		arg1 name [a:as] f = parseCLI as >>= Ok o f a

		nickserv pw = PRIVMSG (CSepList ["NickServ"]) $ "IDENTIFY " +++ pw

		toPrefix c = {irc_prefix=Nothing,irc_command=Right c}

		startup bs = map toPrefix $
			[   NICK bs.bs_nick Nothing
			,   USER "cloogle" "cloogle" "cloogle" "Cloogle bot"
			]++ maybe [] (pure o nickserv) bs.bs_nickserv
			 ++ if (isEmpty bs.bs_autojoin) []
				[JOIN (CSepList bs.bs_autojoin) Nothing]
		shutdown = map toPrefix [QUIT $ Just "Bye"]

		process :: String IRCMessage *File *World -> *(Maybe [IRCMessage], *File, *World)
		process strf im io w
		# (io, w) = log strf " (r): " im (io, w)
		= case im.irc_command of
			Left numr = (Just [], io, w)
			Right cmd = case process` im.irc_prefix cmd w of
				(Nothing, w) = (Nothing, io, w)
				(Just cs, w)
				# msgs = map toPrefix cs
				# (io, w) = foldr (log strf " (s): ") (io, w) msgs
				= (Just msgs, io, w)

		log :: String String IRCMessage (*File, *World) -> (*File, *World)
		log strf pref m (io, w)
		# (t, w) = localTime w
		= (io <<< strfTime strf t <<< pref <<< toString m, w)

		process` :: (Maybe (Either IRCUser String)) IRCCommand *World -> (Maybe [IRCCommand], *World)
		process` (Just (Left user)) (PRIVMSG t m) w
			| m == "!restart" = (Nothing, w)
			| m.[0] == '!'
				# (msgs, w) = realProcess (split " " $ m % (1, size m)) w
				= (Just $ map reply msgs, w)
			= (Just [], w)
		where
			reply = case (\(CSepList [t:_]) -> t.[0]) t of
				'#' -> PRIVMSG t
				_   -> NOTICE user.irc_nick
		process` _ (PING t mt) w = (Just [PONG t mt], w)
		process` _ _ w = (Just [], w)

		realProcess :: [String] *World -> ([String], *World)
		realProcess ["help",x:xs] w = ((case x of
			"help" =
				[ "Usage: !help [ARG]"
				, "Show this help, or the specific help of the argument"]
			"ping" =
				[ "Usage: !ping [ARG [ARG ...]]"
				, "Ping the bot, it will pong the arguments back"]
			"shorten" =
				[ "Usage: !shorten URL [URL [URL ...]]"
				, "Shorten the given urls with the cloo.gl url shortener"]
			"query" =
				[ "Usage: !query QUERY"
				, "Query QUERY in cloogle and return the results"]
			"restart" =
				[ "Usage: !restart"
				, "Restart the bot"]
			x = ["Unknown command: " +++ x]
			), w)
		realProcess ["help"] w = (
			["Type !help cmd for command specific help"
			,"available commands: help, ping, shorten, query, restart"], w)
		realProcess ["ping":xs] w = (["pong " +++ join " " xs], w)
		realProcess ["shorten":xs] w = case xs of
			[] = (["shorten requires at least one argument"], w)
			xs = mapSt shorten xs w
		realProcess ["query":xs] w = case xs of
			[] = (["query requires one or more arguments"], w)
			xs = appFst (split "\n") $ cloogle (join " " xs) w
		realProcess ["restart":_] w = (["restart takes no arguments"], w)
		realProcess [c:_] w = ([join " " [
			"Unknown cmd: ", c, ",  type !help to get help"]], w)
