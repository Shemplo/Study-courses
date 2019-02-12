{-# LANGUAGE TemplateHaskell   #-}

module RunClient where

import Network.Socket as S
--import FTPCommon

import Data.Text as DT (strip, pack, unpack, uncons)
import Data.Maybe as DM
import Data.Void as DV

import Text.Megaparsec.Char.Lexer as TMCL (space, symbol, lexeme, 
    skipLineComment, skipBlockComment)
import Text.Megaparsec.Char as TMC (space1, string, alphaNumChar, 
    char)
import Text.Megaparsec as TM

--import System.IO

type Parser = TM.Parsec DV.Void String

data ConsoleCommand = Open SockAddr
    deriving (Show)

main :: IO ()
main = interpreter

interpreter :: IO ()
interpreter = getLine >>= interpretLine >> interpreter

interpretLine :: String -> IO ()
interpretLine line = do
    print line

    case ((uncons $ strip $ pack line) >>= \_ -> 
        parseCommand line >>= executeLine) of
        Just v -> v >>= \_ -> print "Success"
        _      -> print "Fail"
    return ()

executeLine :: ConsoleCommand -> Maybe (IO ())
executeLine command = Just $ case command of
    Open _ -> print "OPEN"

parseCommand :: String -> Maybe ConsoleCommand
parseCommand line = parseMaybe __parseLine line
    where __parseLine :: Parser ConsoleCommand
          __parseLine = try __parseOpen <|> __parseOpen

          __spaces :: Parser ()
          __spaces = space space1 (skipLineComment "") (skipBlockComment "" "")

          __lexeme :: Parser a -> Parser a
          __lexeme = lexeme __spaces

          __word :: String -> Parser ()
          __word str = (__lexeme . try) (string str *> notFollowedBy alphaNumChar)

          __parseOpen :: Parser ConsoleCommand
          __parseOpen = do
            _ <- string "open"
            return $ Open $ S.SockAddrInet 21 0