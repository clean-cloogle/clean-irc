module test

from Data.Func import $
import Data.Either
import Data.Maybe
import StdEnv
import IRC

Start :: [String]
Start = map toString
	[NICK "clooglebot" Nothing
	,USER "cloogle" "0" "Cloogle bot"
	,JOIN (CSepList ["#cloogle"]) Nothing
	,PRIVMSG (CSepList ["#cloogle"]) "Hello world"
	,QUIT Nothing
	]
