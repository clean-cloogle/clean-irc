definition module IRCBot

from IRC import :: IRCMessage
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeErrorString, :: MaybeError

/*
 * Spawn an IRC Bot
 *
 * param: Hostname and port
 * param: Startup commands that are sent initially. For example:
 *        [NICK "clooglebot" Nothing
 *        ,USER "cloogle" "0" "Cloogle bot"
 *        ,JOIN [("#cloogle",Nothing)]]
 * param: Shutdown commands. For example
 *        [QUIT (Just "Bye")]
 * param: Processing function
 *        param: command received by the server
 *        param: State
 *        param: World
 *        return: Maybe a response, the updated state and the updated world
 *                If the response is nothing the connection is closed
 *                All items in the list are sent back
 * param: World
 * return: Maybe an error, the state and the new world
*/
bot ::  (String,Int) [IRCMessage] [IRCMessage] a (IRCMessage -> a -> .(*World -> *(Maybe [IRCMessage],a,*World))) *World -> *(Maybe String,a,*World)
//bot :: (String, Int) [IRCMessage] [IRCMessage] .a (IRCMessage -> (.a -> .(*World -> *(Maybe [IRCMessage], .a, *World)))) *World -> *(Maybe String, .a, *World)
