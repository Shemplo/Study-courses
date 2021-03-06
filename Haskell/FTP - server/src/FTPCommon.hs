{-# LANGUAGE TemplateHaskell  #-}

module FTPCommon where

import qualified Data.ByteString.Char8 as B8
import Data.Text as DT (strip, pack, unpack)
import Network.Socket.ByteString as BS
import Control.Lens as CL (makeLenses)
import Data.List.Split as DLS
import Network.Socket as S

import System.IO

data DataTransporter = DataTransporter {
    _socketDescriptorDT :: Socket,
    _task               :: Maybe String
} deriving (Show)

makeLenses ''DataTransporter

data ControlCommand = AUTH | USER | PASS | SYST
                    | PWD | TYPE | PASV | LIST
                    | STOR | RETR | CWD | MKD
                    | RMD | CDUP | DELE
    deriving (Show, Read)

data RepresentType = I | A -- | E | L Int
    deriving (Show, Read)


readAllMessages :: Socket -> IO [String]
readAllMessages sockd = socketToHandle sockd ReadMode >>= \src -> __loop src []
    where __loop :: Handle -> [String] -> IO [String]
          __loop hdl list = hIsEOF hdl >>= \eof -> if not eof 
                then hGetLine hdl >>= \line -> __loop hdl (list ++ [line]) 
                else return list


readMessageWithCode :: Socket -> IO (Int, String)
readMessageWithCode sockd = readMessage sockd >>= \message ->
    let list = DLS.splitOn " " $ unpack $ strip $ pack message in
    return (read $ head list, unwords $ tail list)


readMessage :: Socket -> IO String
readMessage sockd = BS.recv sockd (1024 * 4) >>= \msg -> 
    print msg >> (return $ B8.unpack msg)


writeMessage :: Socket -> Int -> String -> IO ()
writeMessage sockd code comment = 
    return msg >>= writeTransportMessage sockd
    where msg = (show code) ++ " " ++ comment


writeTransportMessage :: Socket -> String -> IO ()
writeTransportMessage sockd message = 
    (BS.send sockd $ B8.pack msg) >> print msg
    where msg = message ++ "\r\n"