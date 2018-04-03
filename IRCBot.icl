implementation module IRCBot

from Data.Func import $
import Data.Either
import Data.Error
import Data.Maybe
import IRC
from Text import class Text(concat,split,join), instance Text String
import StdList, StdString
import TCPServer.Connection

bot ::  (String,Int) [IRCMessage] [IRCMessage] a (IRCMessage -> a -> .(*World -> *(Maybe [IRCMessage],a,*World))) *World -> *(Maybe String,a,*World)
//bot :: (String, Int) [IRCMessage] [IRCMessage] .a (IRCMessage -> (.a -> .(*World -> *(Maybe [IRCMessage], .a, *World)))) *World -> *(Maybe String, .a, *World)
bot (host, port) start end state bot w = case connect host port
		{ emptyConnection
		& onConnect = onConnect
		, onData    = onData
		} ("", state) w of
	(Error e, w) = (Just e, state, w)
	(Ok (acc, state), w) = (Nothing, state, w)
where
	onConnect s w = (Just (concat (map toString start)), connectionResponse s, w)
	onData d (acc, s) w = case split "\r\n" (acc +++ d) of
		[m,rest:xs]
			= case parseIRCMessage $ m +++ "\r\n" of
				// Do something with the error
				(Left err) = (Nothing, {connectionResponse ("", s) & stop=True}, w)// (Error $ "IRC Parsing error: " +++ join "\n" err, chan, state, w)
				(Right msg)
				# acc = join "\r\n" [rest:xs]
				# (mircc, state, w) = bot msg state w
				| isNothing mircc = (Just (concat (map toString end)), {connectionResponse (acc, s) & stop=True}, w)
				# tosendthis = concat (map toString (fromJust mircc))
				# (tosend, cr, w) = onData "" (acc, s) w
				= (Just (maybe tosendthis ((+++) tosendthis) tosend), cr, w)	
		[m] = (Nothing, connectionResponse (m, s), w)
