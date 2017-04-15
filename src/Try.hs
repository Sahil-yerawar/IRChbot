module Try
	(
	 displayHelp
	, echoHelp
	, dec2binHelp
	, bin2decHelp
	, googleHelp
	, gmt_timeHelp
	, local_timeHelp
	, cowsNbullsHelp
	, guesssHelp
	, randomCoinHelp
	, rDieHelp
	, weatherHelp
	) where
import System.Random
import System.IO
import Data.List


displayHelp :: String
displayHelp =

	"Implemented Commands:- !commands, !echo, !dec2bin, !bin2dec, !google, !gmt_time, !local_time, !cowsNbulls, !cowsNbullsInst, !guess, !randomCoin, !rDie, !weather. For More Information about each command, type command@help "



commandsHelp :: String
commandsHelp =
	"!commands: It displays the list of all commands available.Syntax: !commands"

weatherHelp :: String
weatherHelp =	"!weather: It displays the forecast of the place at that particular point of time.Syntax: !weather"

echoHelp :: String
echoHelp =
	"!echo: It repeats the same text again.Syntax: !echo string. Example: !echo Hi! How are you? "


dec2binHelp :: String
dec2binHelp =
	"!dec2bin: It displays the binary form of the given decimal number.Note:Only positive integers are accepted.Syntax: !dec2bin number. Example: !dec2bin 17\n"

bin2decHelp :: String
bin2decHelp =
	"!bin2dec: It displays the decimal form of the given binary number.Syntax: !bin2dec binary. Example: !bin2dec 101110010\n"

googleHelp :: String
googleHelp =
	"!google: It search the given text on google and displays the first link from search result. Syntax: !google text Example: !google Learn Haskell\n"

gmt_timeHelp :: String
gmt_timeHelp =
	"!gmt_time: It displays the GMT Time in UTC Format. Syntax: !gmt_time"

local_timeHelp :: String
local_timeHelp =
	"!local_time: It displays the Indian Standard Time in IST Format. Syntax: !local_time"
cowsNbullsHelp :: String
cowsNbullsHelp =
	"!cowsNbulls: It starts a Cows N Bulls game. Enjoy!! Syntax: !cowsNbulls"

guesssHelp :: String
guesssHelp =
	"!guess: It is used to enter a number as input in Cows N Bulls game.Syntax: !guess number. Example: !guess 4325"

randomCoinHelp :: String
randomCoinHelp =
	"!randomCoin: It flips a coin and show what appears-Head or Tail.Syntax: !randomCoin\n"

rDieHelp :: String
rDieHelp =
	"Command: !rDie.Function: It rolls a dice and show the number that appears.Syntax: !randomCoin\n"

cowsNbullsInstHelp :: String
cowsNbullsInstHelp =
	"Command:     !cowsNbullsInstructions\n"++
	"Function:    It displays instructions for the Cows N Bulls game.\n"++
	"Syntax:      !cowsNbullsInstructions\n"

cowsNbullsInst :: String
cowsNbullsInst =
	""
