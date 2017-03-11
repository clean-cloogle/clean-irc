implementation module IRC

import StdList
import GenPrint
import StdOverloaded
import Data.Maybe
import Data.Either
import StdFunc
import StdString
import StdChar

import Text.Parsers.Simple.Core
import Text.Parsers.Simple.Chars
import Data.Tuple
import Control.Monad
import Control.Applicative
from Data.Functor import <$>

from Data.Func import $
from Text import class Text(concat), instance Text String
import qualified Text
from StdMisc import undef

jon :== 'Text'.join

derive gPrint IRCCommand, IRCReplies, IRCErrors, (,), Maybe, (), Either

//Start = runParser parseMessage $ fromString ":frobnicator!~frobnicat@92.110.128.124 QUIT\r\n"
//Start = runParser parseMessage $ fromString ":frobnicator!~frobnicat@92.110.128.124 AWAY test\r\n"
Start = runParser parseMessage $ fromString ":frobnicator!~frobnicat@92.110.128.124 AWAY test with spaces\r\n"

(<+) infixr 5 :: a b -> String | toString a & toString b
(<+) a b = toString a +++ toString b

parseIRCMessage :: (String -> Either [Error] IRCMessage)
parseIRCMessage = parse parseMessage o fromString

parseMessage :: Parser Char IRCMessage
parseMessage = optional (parseEither parseHost parseUser) <* spaceParser
	>>= \mprefix->parseCommand
	<* pToken '\r' <* pToken '\n' 
	>>= \cmd->pure {IRCMessage | irc_prefix=mprefix, irc_command=cmd}

spaceParser :: Parser Char [Char]
spaceParser = pMany $ pToken ' '

parseServer :: Parser Char String
parseServer = pFail

parseEither :: (Parser a b) (Parser a c) -> Parser a (Either b c)
parseEither p q = Left <$> p <|> Right <$> q

parseUser :: Parser Char IRCUser
parseUser = pToken ':' >>| parseNick
		>>= \nick->optional (pToken '!' >>| parseUsr)
		>>= \muser->optional (pToken '@' >>| parseHost)
		>>= \mhost->pure {IRCUser | irc_nick=nick, irc_user=muser, irc_host=mhost}

parseUsr :: Parser Char String
parseUsr = toString <$> pSome (pNoneOf [' ', '@':illegal])

parseNick :: Parser Char String
parseNick = pAlpha >>= \c->pMany (pAlpha <|> pDigit <|> pSpecial)
	>>= \cs->pure (toString [c:cs])

pSpecial :: Parser Char Char
pSpecial = pOneOf ['-', '[', ']', '\\', '\`', '^', '{', '}']

parseHost :: Parser Char String
parseHost = parseName
	>>= \nm->pMany (pToken '.' >>| parseName)
	>>= \nms->pure (concat [nm:nms])
	where
		parseName :: Parser Char String
		parseName = toString <$> pSome (pAlpha <|> pDigit <|> pToken '.')

instance toString IRCMessage where
	toString m = maybe "" (\s->either id ((<+) ":") s <+ " ") m.irc_prefix <+ m.irc_command

instance toString IRCUser where
	toString m = m.irc_nick <+ maybe "" ((<+) "!") m.irc_user
		<+ maybe "" ((<+) "@") m.irc_host

cons :: a [a] -> [a]
cons a as = [a:as]

pMiddle :: Parser Char String
pMiddle = fmap toString $
	spaceParser >>| pToken ':' >>| pMany (pNoneOf illegal)

pTrailing :: Parser Char String
pTrailing = fmap toString $ 
	spaceParser >>| liftM2 cons (pNotSatisfy ((==)':')) (pMany $ pNoneOf [' ':illegal])

pParam :: Parser Char String
pParam = pMiddle <|> pTrailing

pNoneOf :: [a] -> Parser a a | Eq a
pNoneOf l = pSatisfy (not o flip isMember l)

pNotSatisfy :: (a -> Bool) -> Parser a a | Eq a
pNotSatisfy f = pSatisfy (not o f)

pInt :: Parser Char Int
pInt = toInt o toString <$> (spaceParser >>| pSome pDigit)

illegal :: [Char]
illegal = ['\x00','\r','\n']

pCommand :: String -> Parser Char [Char]
pCommand s = pList (map pToken $ fromString s)

pCommand0 :: String IRCCommand -> Parser Char IRCCommand
pCommand0 s c = pCommand s >>| pure c

pCommand1 :: String (Parser Char a) (a -> IRCCommand) -> Parser Char IRCCommand
pCommand1 s p c = pCommand s >>| liftM c p

pCommand2 :: String (Parser Char a) (Parser Char b) (a b -> IRCCommand) -> Parser Char IRCCommand
pCommand2 s p q c = pCommand s >>| liftM2 c p q

pCommand3 :: String (Parser Char a) (Parser Char b) (Parser Char c) (a b c -> IRCCommand) -> Parser Char IRCCommand
pCommand3 s p q r c = pCommand s >>| liftM3 c p q r

pCommand4 :: String (Parser Char a) (Parser Char b) (Parser Char c) (Parser Char d) (a b c d -> IRCCommand) -> Parser Char IRCCommand
pCommand4 s p q r t c = pCommand s >>| liftM4 c p q r t

pCommand5 :: String (Parser Char a) (Parser Char b) (Parser Char c) (Parser Char d) (Parser Char e) (a b c d e -> IRCCommand) -> Parser Char IRCCommand
pCommand5 s p q r t u c = pCommand s >>| liftM5 c p q r t u

pMode :: Parser Char String
pMode = toString <$> pSome (pOneOf ['+','-','o','p','i','t','n','b','v','w','s'])

parseCommand :: Parser Char IRCCommand
parseCommand = 
	    pCommand1 "ADMIN" (optional pMiddle) ADMIN
	<|> pCommand1 "AWAY" pParam AWAY
	<|> pCommand2 "CONNECT" pParam (optional $ liftM2 tuple pInt (optional pParam)) CONNECT
	<|> pCommand0 "DIE" DIE
	<|> pCommand1 "ERROR" pParam ERROR
	<|> pCommand1 "INFO" (optional pParam) INFO
	<|> pCommand2 "INVITE" pMiddle pMiddle INVITE
	<|> pCommand1 "ISON" (pSome pMiddle) ISON
	<|> pCommand1 "JOIN" (pSepBy (liftM2 tuple pMiddle $ optional pMiddle) pComma) JOIN
	<|> pCommand3 "KICK" pMiddle pMiddle (optional pParam) KICK
	<|> pCommand2 "KILL" pMiddle pParam KILL
	<|> pCommand1 "LINKS" (optional $ liftM2 tuple (optional pMiddle) pMiddle) LINKS
	<|> pCommand1 "LIST" (optional $ liftM2 tuple (pSepBy pMiddle pComma) $ optional pMiddle) LIST
	<|> pCommand1 "LUSERS" (optional $ liftM2 tuple pMiddle $ optional pMiddle) LUSERS
	<|> pCommand5 "MODE" pMiddle pMode (optional pMiddle) (optional pMiddle) (optional pMiddle) MODE
	<|> pCommand1 "MOTD" (optional pMiddle) MOTD
	<|> pCommand1 "NAMES" (pSepBy pMiddle pComma) NAMES
	//NJOIN 
	//NOTICE String String
	//OPER String String 
	//PART [String]
	//PASS String
	<|> pCommand2 "PING" pMiddle (optional pMiddle) PING
	<|> pCommand2 "PONG" pMiddle (optional pMiddle) PONG
	<|> pCommand2 "PRIVMSG" (pSepBy pMiddle pComma) pParam PRIVMSG
	<|> pCommand1 "QUIT" (optional pParam) QUIT
	//REHASH 
	//RESTART 
	//SERVER 
	//SERVICE String String String String
	//SERVLIST (Maybe (String, Maybe String))
	//SQUERY String String
	//SQUIRT 
	//SQUIT String String
	//STATS (Maybe (String, Maybe String))
	//SUMMON String (Maybe (String, Maybe String))
	//TIME (Maybe String)
	//TOPIC String (Maybe String)
	//TRACE (Maybe String)
	<|> pCommand3 "USER" pMiddle pMiddle (pMiddle >>| pParam) USER
	//USERHOST [String]
	//USERS (Maybe String)
	//VERSION (Maybe String)
	//WALLOPS String
	//WHO (Maybe String)
	//WHOIS (Maybe String) [String]
	//WHOWAS (Maybe String) [String]

instance toString IRCCommand where
	toString r = flip (+++) "\r\n" case r of
	//ADMIN (Maybe String)
	//AWAY String
	//CONNECT String (Maybe (Int, Maybe String))
	//DIE 
	//ERROR String
	//INFO (Maybe String)
	//INVITE String String
	//ISON [String]
		JOIN chs = "JOIN " +++ (if (isEmpty chs) "0"
			(jon ", " [jon " " [ch:maybeToList mk]\\(ch, mk)<-chs]))
	//KICK String String (Maybe String)
	//KILL String String
	//LINKS (Maybe (Maybe String, String))
	//LIST (Maybe ([String], Maybe String))
	//LUSERS (Maybe (String, Maybe String))
	//MODE String String (Maybe String) (Maybe String) (Maybe String)
	//MOTD (Maybe String)
	//NAMES [String]
		NICK n ms = jon " " ["NICK", n]
	//NJOIN 
	//NOTICE String String
	//OPER String String 
	//PART [String]
	//PASS String
		PING a mb = jon " " ["PING",a:maybeToList mb]
		PONG a mb = jon " " ["PONG",a:maybeToList mb]
		PRIVMSG dest msg = undef //jon " " ["PRIVMSG", dest, ":"+++msg]
		QUIT msg = jon " " ["QUIT":maybeToList msg]
	//REHASH 
	//RESTART 
	//SERVER 
	//SERVICE String String String String
	//SERVLIST (Maybe (String, Maybe String))
	//SQUERY String String
	//SQUIRT 
	//SQUIT String String
	//STATS (Maybe (String, Maybe String))
	//SUMMON String (Maybe (String, Maybe String))
	//TIME (Maybe String)
	//TOPIC String (Maybe String)
	//TRACE (Maybe String)
		USER login mode rn = jon " " ["USER", login, mode, "*", ":"+++rn]
	//USERHOST [String]
	//USERS (Maybe String)
	//VERSION (Maybe String)
	//WALLOPS String
	//WHO (Maybe String)
	//WHOIS (Maybe String) [String]
	//WHOWAS (Maybe String) [String]
		_ = printToString r


instance toString IRCReplies where toString r = printToString r
instance toString IRCErrors where toString r = printToString r

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
		_ = undef

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
