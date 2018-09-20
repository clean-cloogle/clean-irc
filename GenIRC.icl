implementation module GenIRC

import StdEnv
import StdGeneric

import Data.Either
import Data.Func
import Data.Functor
import Data.Maybe
import Data.Tuple
import Text

import IRC

pOne [] = (Left "Expected an argument", [])
pOne [a:as] = (Right a, as)

generic gIRCParse a :: [String] -> (Either Error a, [String])
gIRCParse{|UNIT|} a = (Right UNIT, a)
gIRCParse{|String|} as = pOne as
gIRCParse{|Int|} as = appFst (fmap toInt) $ pOne as
gIRCParse{|EITHER|} lp rp as = case lp as of
	(Right a, rest) = (Right $ LEFT a, rest)
	(Left e1, _) = case rp as of
		(Right a, rest) = (Right $ RIGHT a, rest)
		(Left e2, _) = (Left $ e2, [])
gIRCParse{|OBJECT|} p as = appFst (fmap OBJECT) $ p as
gIRCParse{|CONS of d|} p []
	= (Left $ concat ["Expected a cmd constructor: ", d.gcd_name], [])
gIRCParse{|CONS of d|} p [a:as]
	| a <> d.gcd_name = (Left $ concat [
		"Wrong constructor. expected: ", d.gcd_name, ", got: ", a], [])
	= case p as of
		(Right a, rest) = (Right $ CONS a, rest)
		(Left e, _) = (Left e, [])
gIRCParse{|PAIR|} pl pr as = case pl as of
	(Right a1, rest) = case pr rest of
		(Right a2, rest) = (Right $ PAIR a1 a2, rest)
		(Left e, _) = (Left e, [])
	(Left e, _) = (Left e, [])
gIRCParse{|[]|} pl as = case pl as of
		(Right e, rest) = case gIRCParse{|*->*|} pl rest of
			(Right es, rest) = (Right [e:es], rest)
			(Left e, _) = (Left e, [])
		(Left e, _) = (Right [], as)
gIRCParse{|Maybe|} pm as
	= appFst (either (const $ Right Nothing) $ Right o Just) $ pm as
gIRCParse{|CSepList|} as = appFst (fmap $ CSepList o split ",") $ pOne as

derive gIRCParse (,), IRCCommand
derive gIRCPrint (,), IRCCommand

generic gIRCPrint a :: a -> [String]
gIRCPrint{|UNIT|} _ = []
gIRCPrint{|String|} s = if (indexOf " " s == -1) [s] [":"+++s]
gIRCPrint{|Int|} i = [toString i]
gIRCPrint{|EITHER|} lp rp (LEFT i) = lp i
gIRCPrint{|EITHER|} lp rp (RIGHT i) = rp i
gIRCPrint{|OBJECT|} lp (OBJECT p) = lp p
gIRCPrint{|PAIR|} lp rp (PAIR l r) = lp l ++ rp r
gIRCPrint{|CONS of d|} pc (CONS c) = [d.gcd_name:pc c]
gIRCPrint{|[]|} pl x = flatten $ map pl x
gIRCPrint{|Maybe|} pl m = gIRCPrint{|*->*|} pl $ maybeToList m
gIRCPrint{|CSepList|} (CSepList as) = [join "," as]
