implementation module IRCBot

from Data.Func import $
import Data.Either
import Data.Error
import Data.Maybe
import IRC
import TCPIP

from Text import class Text(split,join), instance Text String

import StdList
import StdBool

TIMEOUT :== Just 1000

bot :: (String, Int) [IRCMessage] [IRCMessage] a (IRCMessage a *World -> (Maybe [IRCMessage], a, *World)) *World -> (MaybeErrorString a, *World)
bot (host, port) start end state bot w
//Lookup hostname
# (ip, w) = lookupIPAddress host w
| isNothing ip = (Error $ "DNS lookup for " +++ host +++ " failed", w)
//Connect
# (rpt,chan,w) = connectTCP_MT TIMEOUT (fromJust ip, port) w
| rpt == TR_Expired = (Error $ "Connection to " +++ host +++ " timed out", w)
| rpt == TR_NoSuccess = (Error $ "Could not connect to " +++ host, w)
// Send startup commands
# (merr, chan, w) = send (map toString start) (fromJust chan) w
| isError merr = (Error $ fromError merr, w)
//Start processing function
# (mer, chan, state, w) = process chan "" state bot w
| isError mer = (Error $ fromError mer, w)
// Send shutdown commands
# (merr, {rChannel,sChannel}, w) = send (map toString end) chan w
| isError merr = (Error $ fromError merr, w)
//Close channels
= (Ok state, closeChannel sChannel (closeRChannel rChannel w))

import StdDebug,StdMisc
process :: TCP_DuplexChannel String a (IRCMessage a *World -> (Maybe [IRCMessage], a, *World)) *World -> (MaybeErrorString (), TCP_DuplexChannel, a, *World)
process chan acc state bot w
//See if we have a message
= case split "\r\n" acc of
	//We only have one message that is not complete
	[m] 
		//Receive
		# (merr_resp, chan, w) = recv chan w
		| isError merr_resp = (Error (fromError merr_resp), chan, state, w)
		# (Ok mresp) = merr_resp
		| isNothing mresp = process chan acc state bot w
		= process chan (m +++ fromJust mresp) state bot w
	//We have a successfull split and therefore we process at least one message
	[m:xs]
		# acc = join "\r\n" xs
		| not (trace_tn $ "Full message: '" +++ m +++ "'") = undef
		= case parseIRCMessage $ m +++ "\r\n" of
			(Left err) = (Error $ "IRC Parsing error: " +++ join "\n" err, chan, state, w)
			(Right msg)
			# (mircc, state, w) = bot msg state w
			| isNothing mircc = (Ok (), chan, state, w) // Bot asks to quit
			//Possible send the commands
			# (merr, chan, w) = send (map toString $ fromJust mircc) chan w
			| isError merr = (Error $ fromError merr, chan, state, w)
			//Recurse
			= process chan acc state bot w

send :: [String] TCP_DuplexChannel *World -> (MaybeErrorString (), TCP_DuplexChannel, *World)
send [] chan w = (Ok (), chan, w)
send [msg:msgs] {sChannel,rChannel} w
# (_, w) = sleep 250000 w
# (rpt,i,sChannel,w) = send_MT TIMEOUT (toByteSeq msg) sChannel w
| rpt <> TR_Success = (Error "Could not send message", {sChannel=sChannel,rChannel=rChannel}, w)
= send msgs {sChannel=sChannel,rChannel=rChannel} w
	where
		sleep :: !Int !*World -> (!Int, *World)
		sleep i w = code {
				ccall usleep "I:I:A"
			}

recv :: TCP_DuplexChannel *World -> (MaybeErrorString (Maybe String), TCP_DuplexChannel, *World)
recv {sChannel,rChannel} w
# (rpt, resp, rChannel, w) = receive_MT TIMEOUT rChannel w
| rpt == TR_Expired = (Ok Nothing, {sChannel=sChannel,rChannel=rChannel}, w)
| rpt == TR_NoSuccess || isNothing resp = (Error "Timeout recv fail", {sChannel=sChannel,rChannel=rChannel}, w)
= (Ok $ Just $ toString $ fromJust resp, {sChannel=sChannel,rChannel=rChannel}, w)