definition module GenIRC

from IRC import :: IRCCommand, :: CSepList
from Data.Either import :: Either
from Data.Maybe import :: Maybe
from Text.Parsers.Simple.Core import :: Error

generic gIRCParse a :: [String] -> (Either Error a, [String])
generic gIRCPrint a :: a -> [String]

derive gIRCParse IRCCommand, String, Int, Maybe, (,), [], CSepList
derive gIRCPrint IRCCommand, String, Int, Maybe, (,), [], CSepList
