module test


import gast
import IRC

derive ggen IRCMessage

Start = Test [] pParsePrint

pParsePrint :: IRCMessage -> Bool
pParsePrint a = toString (parseIRCMessage (toString a)) == toString a
