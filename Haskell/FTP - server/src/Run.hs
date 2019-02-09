{-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Run where

import Network.Socket as S

import Network.Socket.ByteString as BS
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B8
import Data.List.Split as DLS
import Data.Text as DT

data Connection = Connection {
    socketDescriptor :: Socket,
    login            :: Maybe String,
    password         :: Maybe String,
    connected        :: Bool
} deriving (Show)

main :: IO ()
main = runServer 21

runServer :: PortNumber -> IO ()
runServer port = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.bind sock $ S.SockAddrInet port 0
    S.listen sock S.maxListenQueue

    socketHandler sock
    close sock

socketHandler :: Socket -> IO ()
socketHandler sock = do
    (sockd, _) <- accept sock
    let conn = Connection sockd Nothing Nothing True
    _ <- forkIO $ putStrLn "Client connected!"
      >> makeHandshake    conn
      >> listenConnection conn
    socketHandler sock

makeHandshake :: Connection -> IO ()
makeHandshake Connection {socketDescriptor = sockd} = do
    _ <- BS.send sockd $ B8.pack "220 Welcome to Haskell FTP server\r\n"
    print "Handshake made"

readMessage :: Socket -> IO String
readMessage sockd = do
    msg <- BS.recv sockd 1024
    return $ B8.unpack msg

writeMessage :: Socket -> Int -> String -> IO ()
writeMessage sockd code comment = do
    let msg = (show code) ++ " " ++ comment ++ "\r\n"
    _ <- BS.send sockd $ B8.pack msg
    print msg


listenConnection :: Connection -> IO ()
listenConnection conn@Connection {socketDescriptor = sockd} = do
    msg <- readMessage sockd
    
    connU@Connection {
        connected = isConnConnected, 
        socketDescriptor = sockhu
    } <- handleMessage conn msg

    if not isConnConnected
    then close sockhu 
      >> putStrLn "Client disconnected" 
    else listenConnection connU

handleMessage :: Connection -> String -> IO Connection
handleMessage conn msg = do
    let tokens = DLS.splitOn " " $ unpack $ strip $ pack msg
    print tokens -- for debug only
    processMessage conn tokens
    
processMessage :: Connection -> [String] -> IO Connection
processMessage conn (command:tokens) = case command of
    "AUTH" -> do 
        let Connection {socketDescriptor = sockd} = conn
        _ <- writeMessage sockd 502 "Command not implemented" 
        return conn -- TLS and SSL is not supported
    "USER" -> do
        let loginValue = Prelude.head tokens
        let Connection {socketDescriptor = sockd} = conn
        _ <- writeMessage sockd 331 "User name okay, need password"
        processAuthorization conn {login = Just loginValue}
    "PASS" -> do
        let passwordValue = Prelude.head tokens
        processAuthorization conn {password = Just passwordValue}
    "SYST" -> do
        let Connection {socketDescriptor = sockd} = conn
        _ <- writeMessage sockd 215 "Unix Type: L8"
        return conn
    ""     -> do
        return conn {connected = False}
    _      -> do
        let Connection {socketDescriptor = sockd} = conn
        _ <- writeMessage sockd 500 "Syntax error, command unrecognized" 
        return conn {connected = False}

processMessage conn [] = return conn

processAuthorization :: Connection -> IO Connection
processAuthorization conn@Connection {socketDescriptor = sockd, 
                        login    = Just loginValue, 
                        password = Just passwordValue} = do
    let verdict = (loginValue == "root") && (passwordValue == "123")
    
    if verdict
    then writeMessage sockd 230 "User logged in, proceed"
    else writeMessage sockd 230 "Not logged in"

    return conn {connected = verdict}

processAuthorization conn@Connection {login = _, password = _} = return conn