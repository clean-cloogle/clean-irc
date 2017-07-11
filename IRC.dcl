definition module IRC

import IRCBot
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from StdOverloaded import class fromInt, class toInt, class toString, class fromString
from Text.Parsers.Simple.Core import :: Error

:: IRCMessage =
	{ irc_prefix :: Maybe (Either IRCUser String)
	, irc_command :: Either IRCNumReply IRCCommand}

:: IRCNumReply =
	{ irc_reply :: IRCReplies
	, irc_recipient :: String
	, irc_message :: String
	}

:: IRCUser = 
	{ irc_nick :: String
	, irc_user :: Maybe String
	, irc_host :: Maybe String
	}

parseIRCMessage :: String -> Either [Error] IRCMessage

instance toString IRCCommand, IRCReplies, IRCErrors, IRCMessage, IRCUser, IRCNumReply
instance fromInt IRCReplies, IRCErrors
instance toInt IRCReplies, IRCErrors

:: IRCCommand
	= ADMIN (Maybe String)
	| AWAY String
	| CONNECT String (Maybe (Int, Maybe String))
	| DIE 
	| ERROR String
	| INFO (Maybe String)
	| INVITE String String
	| ISON [String]
	| JOIN [(String, Maybe String)]
	| KICK String String (Maybe String)
	| KILL String String
	| LINKS (Maybe (Maybe String, String))
	| LIST (Maybe ([String], Maybe String))
	| LUSERS (Maybe (String, Maybe String))
	| MODE String String (Maybe String) (Maybe String) (Maybe String)
	| MOTD (Maybe String)
	| NAMES [String]
	| NICK String (Maybe String)
	| NJOIN 
	| NOTICE String String
	| OPER String String 
	| PART [String]
	| PASS String
	| PING String (Maybe String)
	| PONG String (Maybe String)
	| PRIVMSG [String] String
	| QUIT (Maybe String)
	| REHASH 
	| RESTART 
	| SERVER 
	| SERVICE String String String String
	| SERVLIST (Maybe (String, Maybe String))
	| SQUERY String String
	| SQUIRT 
	| SQUIT String String
	| STATS (Maybe (String, Maybe String))
	| SUMMON String (Maybe (String, Maybe String))
	| TIME (Maybe String)
	| TOPIC String (Maybe String)
	| TRACE (Maybe String)
	| USER String String String
	| USERHOST [String]
	| USERS (Maybe String)
	| VERSION (Maybe String)
	| WALLOPS String
	| WHO (Maybe String)
	| WHOIS (Maybe String) [String]
	| WHOWAS (Maybe String) [String]

:: IRCReplies = RPL_WELCOME | RPL_YOURHOST | RPL_CREATED | RPL_MYINFO |
	RPL_BOUNCE | RPL_TRACELINK | RPL_TRACECONNECTING | RPL_TRACEHANDSHAKE |
	RPL_TRACEUNKNOWN | RPL_TRACEOPERATOR | RPL_TRACEUSER | RPL_TRACESERVER |
	RPL_TRACESERVICE | RPL_TRACENEWTYPE | RPL_TRACECLASS | RPL_TRACERECONNECT |
	RPL_STATSLINKINFO | RPL_STATSCOMMANDS | RPL_ENDOFSTATS | RPL_UMODEIS |
	RPL_SERVLIST | RPL_SERVLISTEND | RPL_STATSUPTIME | RPL_STATSOLINE |
	RPL_LUSERCLIENT | RPL_LUSEROP | RPL_LUSERUNKNOWN | RPL_LUSERCHANNELS |
	RPL_LUSERME | RPL_ADMINME | RPL_ADMINLOC1 | RPL_ADMINLOC2 | RPL_ADMINEMAIL |
	RPL_TRACELOG | RPL_TRACEEND | RPL_TRYAGAIN | RPL_AWAY | RPL_USERHOST |
	RPL_ISON | RPL_UNAWAY | RPL_NOWAWAY | RPL_WHOISUSER | RPL_WHOISSERVER |
	RPL_WHOISOPERATOR | RPL_WHOWASUSER | RPL_ENDOFWHO | RPL_WHOISIDLE |
	RPL_ENDOFWHOIS | RPL_WHOISCHANNELS | RPL_LISTSTART | RPL_LIST |
	RPL_LISTEND | RPL_CHANNELMODEIS | RPL_UNIQOPIS | RPL_NOTOPIC | RPL_TOPIC |
	RPL_INVITING | RPL_SUMMONING | RPL_INVITELIST | RPL_ENDOFINVITELIST |
	RPL_EXCEPTLIST | RPL_ENDOFEXCEPTLIST | RPL_VERSION | RPL_WHOREPLY |
	RPL_NAMREPLY | RPL_LINKS | RPL_ENDOFLINKS | RPL_ENDOFNAMES | RPL_BANLIST |
	RPL_ENDOFBANLIST | RPL_ENDOFWHOWAS | RPL_INFO | RPL_MOTD | RPL_ENDOFINFO |
	RPL_MOTDSTART | RPL_ENDOFMOTD | RPL_YOUREOPER | RPL_REHASHING |
	RPL_YOURESERVICE | RPL_TIME | RPL_USERSSTART | RPL_USERS | RPL_ENDOFUSERS |
	RPL_NOUSERS

:: IRCErrors = ERR_NOSUCHNICK | ERR_NOSUCHSERVER | ERR_NOSUCHCHANNEL |
	ERR_CANNOTSENDTOCHAN | ERR_TOOMANYCHANNELS | ERR_WASNOSUCHNICK |
	ERR_TOOMANYTARGETS | ERR_NOSUCHSERVICE | ERR_NOORIGIN | ERR_NORECIPIENT |
	ERR_NOTEXTTOSEND | ERR_NOTOPLEVEL | ERR_WILDTOPLEVEL | ERR_BADMASK |
	ERR_UNKNOWNCOMMAND | ERR_NOMOTD | ERR_NOADMININFO | ERR_FILEERROR |
	ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE |
	ERR_NICKCOLLISION | ERR_UNAVAILRESOURCE | ERR_USERNOTINCHANNEL |
	ERR_NOTONCHANNEL | ERR_USERONCHANNEL | ERR_NOLOGIN | ERR_SUMMONDISABLED |
	ERR_USERSDISABLED | ERR_NOTREGISTERED | ERR_NEEDMOREPARAMS |
	ERR_ALREADYREGISTRED | ERR_NOPERMFORHOST | ERR_PASSWDMISMATCH |
	ERR_YOUREBANNEDCREEP | ERR_YOUWILLBEBANNED | ERR_KEYSET | ERR_CHANNELISFULL |
	ERR_UNKNOWNMODE | ERR_INVITEONLYCHAN | ERR_BANNEDFROMCHAN |
	ERR_BADCHANNELKEY | ERR_BADCHANMASK | ERR_NOCHANMODES | ERR_BANLISTFULL |
	ERR_NOPRIVILEGES | ERR_CHANOPRIVSNEEDED | ERR_CANTKILLSERVER |
	ERR_RESTRICTED | ERR_UNIQOPPRIVSNEEDED | ERR_NOOPERHOST |
	ERR_UMODEUNKNOWNFLAG | ERR_USERSDONTMATCH
