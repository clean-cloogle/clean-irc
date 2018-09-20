implementation module IRC

import StdList, StdTuple, StdOverloaded, StdFunc, StdString, StdChar, StdBool
import _SystemArray

import Text.GenPrint
import GenIRC

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Tuple
import Text.Parsers.Simple.Chars
import Text.Parsers.Simple.Core

import StdDebug

from Data.Functor import <$>
from Data.Func import $
from StdMisc import undef, abort
from Text import class Text(lpad,trim,rtrim,split,indexOf,concat),
	instance Text String
import qualified Text

jon :== 'Text'.join

derive gPrint IRCErrors, IRCReplies, Maybe, Either, IRCUser, IRCNumReply

Start = (map (fmap toString) msgs, msgs)
where
	msgs =
		[ parseIRCMessage ":clooglebot!~cloogle@dhcp-077-249-221-037.chello.nl QUIT\r\n"
		, parseIRCMessage ":clooglebot!~cloogle QUIT\r\n"
		, parseIRCMessage ":frobnicator!~frobnicat@92.110.128.124 QUIT\r\n"
		, parseIRCMessage ":frobnicator!~frobnicat@92.110.128.124 AWAY test\r\n"
		, parseIRCMessage ":frobnicator!~frobnicat@92.110.128.124 AWAY :test with spaces\r\n"
		, parseIRCMessage ":cherryh.freenode.net NOTICE * :*** Found your hostname\r\n"
		, parseIRCMessage ":cherryh.freenode.net QUIT :hoi hoi\r\n"
		, parseIRCMessage ":cherryh.freenode.net JOIN #cha,#ch-b #twilight\r\n"
		, parseIRCMessage ":cherryh.freenode.net ISON a b c d e f :g h\r\n"
		, parseIRCMessage ":wilhelm.freenode.net 001 clooglebot :Welcome to the freenode Internet Relay Chat Network clooglebot\r\n"
		, parseIRCMessage "PING :orwell.freenode.net\r\n"
		, parseIRCMessage ":ChanServ!ChanServ@services. MODE #cloogle +o frobnicator\r\n"
		, parseIRCMessage ":qbot_v01!~qbot@ip-213-124-170-20.ip.prioritytelecom.net PRIVMSG ##chinees :[link] Cloogle - https://cloogle.org"
		]

parseIRCMessage :: String -> Either [Error] IRCMessage
parseIRCMessage s = case runParser parsePrefix (fromString s) of
	// Prefix is parsed
	([(prefix, rest):_], _)
		//Try parsing a numeric reply
		= case parse parseReply rest of
			//Try a normal command
			Left e = case parseCmd rest of
				Left e2 = Left [e2:e]
				Right cmd = Right {IRCMessage | irc_prefix=prefix, irc_command=Right cmd}
			Right repl = Right {IRCMessage | irc_prefix=prefix, irc_command=Left repl}
	// Error parsing prefix
	(_, es) = Left ["Error parsing prefix"]

//Prefix
parsePrefix :: Parser Char (Maybe (Either IRCUser String))
parsePrefix
	= optional (pToken ':' >>| parseEither parseUser parseHost <* pToken ' ')
where
	parseEither :: (Parser a b) (Parser a c) -> Parser a (Either b c)
	parseEither p q = Left <$> p <|> Right <$> q

	parseUser :: Parser Char IRCUser
	parseUser = parseNick
			>>= \nick->optional (pToken '!' >>| parseUsr)
			>>= \muser->optional (pToken '@' >>| parseHost)
			>>= \mhost->pure {IRCUser
				| irc_nick=nick, irc_user=muser, irc_host=mhost}
	
	parseUsr :: Parser Char String
	parseUsr = toString <$> pSome (pNoneOf [' ', '@':illegal])
	
	parseNick :: Parser Char String
	parseNick = pAlpha 
		>>= \c ->pMany (pAlpha <|> pDigit <|> pOneOf (fromString "_-[]\\`^{}"))
		>>= \cs->pure (toString [c:cs])

	parseHost :: Parser Char String
	parseHost = jon "." <$> (pSepBy parseName (pToken '.'))
		>>= \s->optional (pToken '.') >>= pure o maybe s (\p->s+++toString s)
		where
			parseName :: Parser Char String
			parseName = toString <$> pSome (pAlpha <|> pDigit <|> pOneOf ['-', '/'])

//Parse Cmd
parseCmd :: [Char] -> Either Error IRCCommand
parseCmd cs = fst $ gIRCParse{|*|} $ argfun $ split " " $ toString cs
	where
		argfun :: [String] -> [String]
		argfun [] = []
		argfun [x:xs]
		# x = trim x
		| x.[0] == ':' = [jon " " $ [x % (1, size x):map rtrim xs]]
		| otherwise = [x:argfun xs]

//Reply
parseReply :: Parser Char IRCNumReply
parseReply = spaceParser
	>>|       (pMany (pToken '0') >>| pSome pDigit <* spaceParser)
	>>= \rep->(toString <$> pSome (pNoneOf [' ':illegal]) <* spaceParser)
	>>= \rec->(toString <$> pSome (pNoneOf illegal))
	>>= \msg->pure {IRCNumReply
		| irc_reply     = fromInt $ toInt $ toString rep
		, irc_recipient = rec
		, irc_message   = msg % (if (msg.[0] == ':') 1 0, size msg)
		}
	<* pToken '\r' <* pToken '\n'
	where
		spaceParser :: Parser Char [Char]
		spaceParser = pMany $ pToken ' '

//Common parsers
pNoneOf :: [a] -> Parser a a | Eq a
pNoneOf l = pSatisfy (not o flip isMember l)

illegal :: [Char]
illegal = ['\x00','\r','\n']

instance toString IRCNumReply where
	toString m = lpad (toString $ toInt m.irc_reply) 3 '0' <+ " " <+
		m.irc_recipient <+ " " <+ concat (gIRCPrint{|*|} m.irc_message)
instance toString IRCMessage where
	toString m = maybe "" (\s->either ((<+) ":") id s <+ " ") m.irc_prefix
		<+ either toString toString m.irc_command
instance toString IRCUser where
	toString m = m.irc_nick <+ maybe "" ((<+) "!") m.irc_user
		<+ maybe "" ((<+) "@") m.irc_host
instance toString IRCCommand where
	toString m = jon " " (gIRCPrint{|*|} m) +++ "\r\n"
instance toString IRCReplies where toString r = printToString r
instance toString IRCErrors where toString r = printToString r

(<+) infixr 5 :: a b -> String | toString a & toString b
(<+) a b = toString a +++ toString b

instance fromInt IRCReplies where
	fromInt r = case r of 
		1 = RPL_WELCOME
		2 = RPL_YOURHOST
		3 = RPL_CREATED
		4 = RPL_MYINFO
		5 = RPL_BOUNCE
		200 = RPL_TRACELINK
		201 = RPL_TRACECONNECTING
		202 = RPL_TRACEHANDSHAKE
		203 = RPL_TRACEUNKNOWN
		204 = RPL_TRACEOPERATOR
		205 = RPL_TRACEUSER
		206 = RPL_TRACESERVER
		207 = RPL_TRACESERVICE
		208 = RPL_TRACENEWTYPE
		209 = RPL_TRACECLASS
		210 = RPL_TRACERECONNECT
		211 = RPL_STATSLINKINFO
		212 = RPL_STATSCOMMANDS
		219 = RPL_ENDOFSTATS
		221 = RPL_UMODEIS
		234 = RPL_SERVLIST
		235 = RPL_SERVLISTEND
		242 = RPL_STATSUPTIME
		243 = RPL_STATSOLINE
		251 = RPL_LUSERCLIENT
		252 = RPL_LUSEROP
		253 = RPL_LUSERUNKNOWN
		254 = RPL_LUSERCHANNELS
		255 = RPL_LUSERME
		256 = RPL_ADMINME
		257 = RPL_ADMINLOC1
		258 = RPL_ADMINLOC2
		259 = RPL_ADMINEMAIL
		261 = RPL_TRACELOG
		262 = RPL_TRACEEND
		263 = RPL_TRYAGAIN
		301 = RPL_AWAY
		302 = RPL_USERHOST
		303 = RPL_ISON
		304 = RPL_UNAWAY
		305 = RPL_NOWAWAY
		311 = RPL_WHOISUSER
		312 = RPL_WHOISSERVER
		313 = RPL_WHOISOPERATOR
		314 = RPL_WHOWASUSER
		315 = RPL_ENDOFWHO
		317 = RPL_WHOISIDLE
		318 = RPL_ENDOFWHOIS
		319 = RPL_WHOISCHANNELS
		321 = RPL_LISTSTART
		322 = RPL_LIST
		323 = RPL_LISTEND
		324 = RPL_CHANNELMODEIS
		325 = RPL_UNIQOPIS
		331 = RPL_NOTOPIC
		332 = RPL_TOPIC
		341 = RPL_INVITING
		342 = RPL_SUMMONING
		346 = RPL_INVITELIST
		347 = RPL_ENDOFINVITELIST
		348 = RPL_EXCEPTLIST
		349 = RPL_ENDOFEXCEPTLIST
		351 = RPL_VERSION
		352 = RPL_WHOREPLY
		353 = RPL_NAMREPLY
		364 = RPL_LINKS
		365 = RPL_ENDOFLINKS
		366 = RPL_ENDOFNAMES
		367 = RPL_BANLIST
		368 = RPL_ENDOFBANLIST
		369 = RPL_ENDOFWHOWAS
		371 = RPL_INFO
		372 = RPL_MOTD
		374 = RPL_ENDOFINFO
		375 = RPL_MOTDSTART
		376 = RPL_ENDOFMOTD
		381 = RPL_YOUREOPER
		382 = RPL_REHASHING
		383 = RPL_YOURESERVICE
		391 = RPL_TIME
		392 = RPL_USERSSTART
		393 = RPL_USERS
		394 = RPL_ENDOFUSERS
		395 = RPL_NOUSERS
		_ = RPL_UNKNOWN

instance toInt IRCReplies where
	toInt r = case r of 
		RPL_WELCOME = 1
		RPL_YOURHOST = 2
		RPL_CREATED = 3
		RPL_MYINFO = 4
		RPL_BOUNCE = 5
		RPL_TRACELINK = 200
		RPL_TRACECONNECTING = 201
		RPL_TRACEHANDSHAKE = 202
		RPL_TRACEUNKNOWN = 203
		RPL_TRACEOPERATOR = 204
		RPL_TRACEUSER = 205
		RPL_TRACESERVER = 206
		RPL_TRACESERVICE = 207
		RPL_TRACENEWTYPE = 208
		RPL_TRACECLASS = 209
		RPL_TRACERECONNECT = 210
		RPL_STATSLINKINFO = 211
		RPL_STATSCOMMANDS = 212
		RPL_ENDOFSTATS = 219
		RPL_UMODEIS = 221
		RPL_SERVLIST = 234
		RPL_SERVLISTEND = 234
		RPL_STATSUPTIME = 242
		RPL_STATSOLINE = 243
		RPL_LUSERCLIENT = 251
		RPL_LUSEROP = 252
		RPL_LUSERUNKNOWN = 253
		RPL_LUSERCHANNELS = 254
		RPL_LUSERME = 255
		RPL_ADMINME = 256
		RPL_ADMINLOC1 = 257
		RPL_ADMINLOC2 = 258
		RPL_ADMINEMAIL = 259
		RPL_TRACELOG = 261
		RPL_TRACEEND = 262
		RPL_TRYAGAIN = 263
		RPL_AWAY = 301
		RPL_USERHOST = 302
		RPL_ISON = 303
		RPL_UNAWAY = 304
		RPL_NOWAWAY = 305
		RPL_WHOISUSER = 311
		RPL_WHOISSERVER = 312
		RPL_WHOISOPERATOR = 313
		RPL_WHOWASUSER = 314
		RPL_ENDOFWHO = 315
		RPL_WHOISIDLE = 317
		RPL_ENDOFWHOIS = 318
		RPL_WHOISCHANNELS = 319
		RPL_LISTSTART = 321
		RPL_LIST = 322
		RPL_LISTEND = 323
		RPL_CHANNELMODEIS = 324
		RPL_UNIQOPIS = 325
		RPL_NOTOPIC = 331
		RPL_TOPIC = 332
		RPL_INVITING = 341
		RPL_SUMMONING = 342
		RPL_INVITELIST = 346
		RPL_ENDOFINVITELIST = 347
		RPL_EXCEPTLIST = 348
		RPL_ENDOFEXCEPTLIST = 349
		RPL_VERSION = 351
		RPL_WHOREPLY = 352
		RPL_NAMREPLY = 353
		RPL_LINKS = 364
		RPL_ENDOFLINKS = 365
		RPL_ENDOFNAMES = 366
		RPL_BANLIST = 367
		RPL_ENDOFBANLIST = 367
		RPL_ENDOFWHOWAS = 369
		RPL_INFO = 371
		RPL_MOTD = 372
		RPL_ENDOFINFO = 374
		RPL_MOTDSTART = 375
		RPL_ENDOFMOTD = 376
		RPL_YOUREOPER = 381
		RPL_REHASHING = 382
		RPL_YOURESERVICE = 383
		RPL_TIME = 391
		RPL_USERSSTART = 392
		RPL_USERS = 393
		RPL_ENDOFUSERS = 394
		RPL_NOUSERS = 395
		RPL_UNKNOWN = 998

instance fromInt IRCErrors where
	fromInt r = case r of
		401 = ERR_NOSUCHNICK
		402 = ERR_NOSUCHSERVER
		403 = ERR_NOSUCHCHANNEL
		404 = ERR_CANNOTSENDTOCHAN
		405 = ERR_TOOMANYCHANNELS
		406 = ERR_WASNOSUCHNICK
		407 = ERR_TOOMANYTARGETS
		408 = ERR_NOSUCHSERVICE
		409 = ERR_NOORIGIN
		411 = ERR_NORECIPIENT
		412 = ERR_NOTEXTTOSEND
		413 = ERR_NOTOPLEVEL
		414 = ERR_WILDTOPLEVEL
		415 = ERR_BADMASK
		421 = ERR_UNKNOWNCOMMAND
		422 = ERR_NOMOTD
		423 = ERR_NOADMININFO
		424 = ERR_FILEERROR
		431 = ERR_NONICKNAMEGIVEN
		432 = ERR_ERRONEUSNICKNAME
		433 = ERR_NICKNAMEINUSE
		436 = ERR_NICKCOLLISION
		437 = ERR_UNAVAILRESOURCE
		441 = ERR_USERNOTINCHANNEL
		442 = ERR_NOTONCHANNEL
		443 = ERR_USERONCHANNEL
		444 = ERR_NOLOGIN
		445 = ERR_SUMMONDISABLED
		446 = ERR_USERSDISABLED
		451 = ERR_NOTREGISTERED
		461 = ERR_NEEDMOREPARAMS
		462 = ERR_ALREADYREGISTRED
		463 = ERR_NOPERMFORHOST
		464 = ERR_PASSWDMISMATCH
		465 = ERR_YOUREBANNEDCREEP
		466 = ERR_YOUWILLBEBANNED
		467 = ERR_KEYSET
		471 = ERR_CHANNELISFULL
		472 = ERR_UNKNOWNMODE
		473 = ERR_INVITEONLYCHAN
		474 = ERR_BANNEDFROMCHAN
		475 = ERR_BADCHANNELKEY
		476 = ERR_BADCHANMASK
		477 = ERR_NOCHANMODES
		478 = ERR_BANLISTFULL
		481 = ERR_NOPRIVILEGES
		482 = ERR_CHANOPRIVSNEEDED
		483 = ERR_CANTKILLSERVER
		484 = ERR_RESTRICTED
		485 = ERR_UNIQOPPRIVSNEEDED
		491 = ERR_NOOPERHOST
		501 = ERR_UMODEUNKNOWNFLAG
		502 = ERR_USERSDONTMATCH
		_ = ERR_UNKNOWN

instance toInt IRCErrors where
	toInt r = case r of
		ERR_NOSUCHNICK = 401
		ERR_NOSUCHSERVER = 402
		ERR_NOSUCHCHANNEL = 403
		ERR_CANNOTSENDTOCHAN = 404
		ERR_TOOMANYCHANNELS = 405
		ERR_WASNOSUCHNICK = 406
		ERR_TOOMANYTARGETS = 407
		ERR_NOSUCHSERVICE = 408
		ERR_NOORIGIN = 409
		ERR_NORECIPIENT = 411
		ERR_NOTEXTTOSEND = 412
		ERR_NOTOPLEVEL = 413
		ERR_WILDTOPLEVEL = 414
		ERR_BADMASK = 415
		ERR_UNKNOWNCOMMAND = 421
		ERR_NOMOTD = 422
		ERR_NOADMININFO = 423
		ERR_FILEERROR = 424
		ERR_NONICKNAMEGIVEN = 431
		ERR_ERRONEUSNICKNAME = 432
		ERR_NICKNAMEINUSE = 433
		ERR_NICKCOLLISION = 436
		ERR_UNAVAILRESOURCE = 437
		ERR_USERNOTINCHANNEL = 441
		ERR_NOTONCHANNEL = 442
		ERR_USERONCHANNEL = 443
		ERR_NOLOGIN = 444
		ERR_SUMMONDISABLED = 445
		ERR_USERSDISABLED = 446
		ERR_NOTREGISTERED = 451
		ERR_NEEDMOREPARAMS = 461
		ERR_ALREADYREGISTRED = 462
		ERR_NOPERMFORHOST = 463
		ERR_PASSWDMISMATCH = 464
		ERR_YOUREBANNEDCREEP = 465
		ERR_YOUWILLBEBANNED = 466
		ERR_KEYSET = 467
		ERR_CHANNELISFULL = 471
		ERR_UNKNOWNMODE = 472
		ERR_INVITEONLYCHAN = 473
		ERR_BANNEDFROMCHAN = 474
		ERR_BADCHANNELKEY = 475
		ERR_BADCHANMASK = 476
		ERR_NOCHANMODES = 477
		ERR_BANLISTFULL = 478
		ERR_NOPRIVILEGES = 481
		ERR_CHANOPRIVSNEEDED = 482
		ERR_CANTKILLSERVER = 483
		ERR_RESTRICTED = 484
		ERR_UNIQOPPRIVSNEEDED = 485
		ERR_NOOPERHOST = 491
		ERR_UMODEUNKNOWNFLAG = 501
		ERR_USERSDONTMATCH = 502
		ERR_UNKNOWN = 999
