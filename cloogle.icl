module cloogle

import Cloogle
import GenPrint
import StdEnv

import Data.Functor
import Data.Maybe
import Data.Either
from Data.Func import $, mapSt
from Text import class Text(..), instance Text String, instance + String

import Text.JSON

import Text.URI

import Control.Applicative
import qualified Control.Monad as CM
import qualified Data.Map as DM
from Control.Monad import class Monad, instance Monad Maybe
from Text.Encodings.UrlEncoding import urlEncode
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

doRequest :: HTTPRequest *World -> *(MaybeErrorString HTTPResponse, *World)
doRequest req w
# (ip,w) = lookupIPAddress server_name w
| isNothing ip
	= (Error $ "DNS lookup for " + server_name + " failed.", w)
# (Just ip) = ip
# (rpt,chan,w) = connectTCP_MT TIMEOUT (ip, req.server_port) w
| rpt == TR_Expired
	= (Error $ "Connection to " + toString ip + " timed out.", w)
| rpt == TR_NoSuccess
	= (Error $ "Could not connect to " + server_name + ".", w)
# (Just {sChannel,rChannel}) = chan
# (rpt,i,sChannel,w) = send_MT TIMEOUT (toByteSeq req) sChannel w
| rpt <> TR_Success
	= (Error $ "Could not send request to " + server_name + ".", w)
# (rpt,resp,rChannel,w) = receive_MT TIMEOUT rChannel w
| rpt <> TR_Success
	= (Error $ "Did not receive a reply from " + server_name + ".", w)
# resp = 'CM'.join $ parseResponse <$> toString <$> resp
| isNothing resp
	# w = closeChannel sChannel (closeRChannel rChannel w)
	= (Error $ "Server did not respond with HTTP.", w)
# (resp,rChannel,w) = receiveRest (fromJust resp) rChannel w
# w = closeChannel sChannel (closeRChannel rChannel w)
= (resp,w)
where
	server_name = req.server_name
	receiveRest resp chan w
	# cl = lookup "Content-Length" resp.HTTPResponse.rsp_headers
	| isNothing cl
		= (Ok resp, chan, w)
	| size resp.rsp_data >= toInt (fromJust cl)
		= (Ok resp, chan, w)
	# (rpt,newresp,chan,w) = receive_MT TIMEOUT chan w
	| rpt <> TR_Success
		= (Error $ server_name + " hung up during transmission.", chan, w)
	= receiveRest {resp & rsp_data=resp.rsp_data + toString (fromJust newresp)} chan w

import StdMisc
import StdDebug

doRequestL :: HTTPRequest Int *World -> *(MaybeErrorString HTTPResponse, *World)
doRequestL req 0 w = (Error "Maximal redirect number exceeded", w)
doRequestL req maxRedirects w
| not (trace_tn $ toString req) = undef
# (er, w) = doRequest req w
| isError er = (er, w)
# resp = fromOk er
| isMember resp.HTTPResponse.rsp_code [301, 302, 303, 307, 308]
	= case lookup "Location" resp.HTTPResponse.rsp_headers of
		Nothing = (Error $ "Redirect given but no Location header", w)
		Just loc = case parseURI loc of
			Nothing = (Error $ "Redirect URI couldn't be parsed", w)
			Just uri = doRequestL {req 
				& server_name = maybe loc id uri.uriRegName
				, server_port = maybe 80 id uri.uriPort
				, req_path = uri.uriPath
				, req_query = maybe "" ((+++) "?") uri.uriQuery
				} (maxRedirects-1) w
= (er, w)

shorten :: String *World -> (String, *World)
shorten s w 
# s = if (startsWith "http://" s) s (if (startsWith "https://" s) s ("http://" + s))
# data = "type=regular&url="+urlEncode s+"&token=a"
# (mer, w) = doRequest 
		{ newHTTPRequest
		& req_method = HTTP_POST
		, req_path = "/"
		, server_name = "cloo.gl"
		, server_port = 80
		, req_headers = 'DM'.fromList
			[("Content-Type", "application/x-www-form-urlencoded")
			,("Content-Length", toString $ size data)
			,("Accept", "*/*")]
		, req_data = data} w
| isError mer = ("request failed: " + fromError mer, w)
# resp = fromOk mer
= (resp.rsp_data, w)

cloogle :: String *World -> (String, *World)
cloogle data w
# (mer, w) = doRequestL
		{ newHTTPRequest
		& req_path = "/api.php"
		, req_query = "?str=" + urlEncode data
		, req_headers = 'DM'.fromList [("User-Agent", "cloogle-irc")]
		, server_name = "cloogle.org"
		, server_port = 80} 10 w
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


Start :: *World -> (MaybeErrorString (), *World)
Start w = bot ("irc.freenode.net", 6667) startup shutdown () process w
	where
		toPrefix c = {irc_prefix=Nothing,irc_command=Right c}
		startup = map toPrefix
			[NICK "clooglebot" Nothing
			,USER "cloogle" "cloogle" "cloogle" "Cloogle bot"
			,JOIN (CSepList ["#cloogle", "#cleanlang"]) Nothing]
		shutdown = map toPrefix [QUIT $ Just "Bye"]

		process :: IRCMessage () *World -> (Maybe [IRCMessage], (), *World)
		process im s w = case im.irc_command of
			Left numr = (Just [], (), w)
			Right cmd = case process` cmd w of
				(Nothing, w) = (Nothing, (), w)
				(Just cs, w) = (Just $ map toPrefix cs, (), w)

		process` :: IRCCommand *World -> (Maybe [IRCCommand], *World)
		process` (PRIVMSG t m) w
			| m.[0] == '!'
				# (msgs, w) = realProcess (split " " $ m % (1, size m)) w
				= (Just $ map (PRIVMSG t) msgs, w)
			= (Just [], w)
		process` (PING t mt) w = (Just [PONG t mt], w)
		process` _ w = (Just [], w)

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
		realProcess ["restart"] w = abort "Restarted"
		realProcess ["restart":_] w = (["restart takes no arguments"], w)
		realProcess [c:_] w = ([join " " [
			"Unknown cmd: ", c, ",  type !help to get help"]], w)
