module cloogle

import GenPrint
import IRC
import StdEnv

import Data.Functor
import Data.Maybe
from Data.Func import $
from Text import class Text(..), instance Text String, instance + String


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

commands :: [String]
commands = map toString
	[NICK "clooglebot"
	,USER "cloogle" 0 "Cloogle bot"
	,JOIN [("#cloogle", Nothing)]
	]

TIMEOUT :== Just 10000
SERVER :== "irc.freenode.net"

KEY :== "PRIVMSG #cloogle :!"

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

shorten :: String *World -> (String, *World)
shorten s w 
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

send :: [String] TCP_DuplexChannel *World -> (TCP_DuplexChannel, *World)
send [] chan w = (chan, w)
send [msg:msgs] {sChannel,rChannel} w
# (rpt,i,sChannel,w) = send_MT TIMEOUT (toByteSeq msg) sChannel w
| rpt <> TR_Success = abort "Could not send request\n"
= send msgs {sChannel=sChannel,rChannel=rChannel} w

recv :: TCP_DuplexChannel *World -> (Maybe String, TCP_DuplexChannel, *World)
recv {sChannel,rChannel} w
# (rpt, resp, rChannel, w) = receive_MT TIMEOUT rChannel w
| rpt == TR_Expired = (Nothing, {sChannel=sChannel,rChannel=rChannel}, w)
| rpt == TR_NoSuccess || isNothing resp = abort "Halp?\n"
= (toString <$> resp, {sChannel=sChannel,rChannel=rChannel}, w)

msg :: (String -> IRCCommands)
msg = PRIVMSG "#cloogle"

process :: *File TCP_DuplexChannel *World -> (*File, TCP_DuplexChannel, *World)
process io chan w 
# (mr, chan, w) = recv chan w
| isNothing mr = process io chan w
# resp = fromJust mr
#! io = io <<< ("Received: " +++ resp +++ "\n")
# ind = indexOf KEY resp
| ind >= 0
	# cmd = split " " $ rtrim $ subString (ind + size KEY) (size resp) resp
	#! io =  io <<< ("Received command: " +++ printToString cmd +++ "\n")
	# (w, toSend) = case cmd of
		["stop":_] = (w, Nothing)
		["ping":xs] = (w, Just [msg $ "pong " +++ join " " xs])
		["short"] = (w, Just [msg $ "short requires an url argument"])
		["short":xs]
			# (s, w) = shorten (join " " xs) w
			= (w, Just [msg s])
		["help"] = (w, Just 
			[msg "type !help cmd for command specific help"
			,msg "available commands: help, short, ping"])
		["help":c:_] = (w, case c of
			"help"  = Just [msg "help  [CMD] - I will print general help or the help of CMD"] 
			"short" = Just [msg "short  URL  - I will give the url to https://cloo.gl shortening service and post back the result"]
			"ping"  = Just [msg "ping  [TXT] - I will reply with pong and the optionar TXT"] 
			_ = Just [msg "Unknown command"])
		[c:_] = (w, Just [msg $ join " " ["unknown command: " , c, ",  type !help to get help"]])
	| isNothing toSend = (io, chan, w)
	# (chan, w) = send (map toString $ fromJust toSend) chan w
	= process io chan w
| indexOf "PING :" resp >= 0
	# cmd = rtrim $ subString (indexOf "PING :" resp + size "PING :") (size resp) resp
	#! io = io <<< (toString $ PONG cmd Nothing) <<< "\n"
	# (chan, w) = send [toString $ PONG cmd Nothing] chan w
	= process io chan w
= process io chan w

Start :: *World -> *World
Start w
# (io, w) = stdio w
# (ip, w) = lookupIPAddress SERVER w
| isNothing ip = abort $ "DNS lookup for " +++ SERVER +++ " failed\n"
# (Just ip) = ip
# (rpt,chan,w) = connectTCP_MT TIMEOUT (ip, 6667) w
| rpt == TR_Expired = abort $ "Connection to " +++ SERVER +++ " timed out\n"
| rpt == TR_NoSuccess = abort $ "Could not connect to " +++ SERVER +++ "\n"
# chan = fromJust chan
# (chan, w) = send commands chan w
# (io, chan, w) = process io chan w
# ({sChannel,rChannel}, w) = send [toString $ QUIT Nothing] chan w
# (_, w) = fclose io w
= closeChannel sChannel (closeRChannel rChannel w)
