module test

from Data.Func import $
import Data.Either
import Data.Maybe
import StdEnv
import IRC

Start :: [String]
Start = map toString
	[NICK "clooglebot"
	,USER "cloogle" 0 "Cloogle bot"
	,JOIN $ Right [("#cloogle", Nothing)]
	,PRIVMSG "#cloogle" "Hello world"
	,QUIT Nothing
	]
