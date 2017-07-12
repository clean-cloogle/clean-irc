module test

from Data.Func import $
import Data.Either
import Data.Maybe
import StdEnv
import IRC

from Text import class Text(concat), instance Text String

Start = concat $ map toString
	[NICK "clooglebot" Nothing
	,USER "cloogle" "0" "cloogle" "Cloogle bot"
	,JOIN (CSepList ["#cloogle"]) Nothing
	,PRIVMSG (CSepList ["#cloogle"]) "Hello world"
	,QUIT Nothing
	]
