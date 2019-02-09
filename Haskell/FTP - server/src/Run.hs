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
    socketDescriptorC :: Socket,
    login             :: Maybe String,
    password          :: Maybe String,
    connected         :: Bool,
    directory         :: String,
    representation    :: Maybe String,
    transporter       :: Maybe DataTransporter
} deriving (Show)

data DataTransporter = DataTransporter {
    socketDescriptorDT :: Socket,
    task               :: Maybe String
} deriving (Show)


main :: IO ()
main = do
    _ <- runServer 21 socketHandler
    return ()


runServer :: PortNumber -> (Socket -> IO t) -> IO t
runServer port handler = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.bind sock $ S.SockAddrInet port 0
    S.listen sock S.maxListenQueue

    res <- handler sock
    close sock
    return res


socketHandler :: Socket -> IO ()
socketHandler sock = do
    (sockd, _) <- accept sock
    let conn = Connection sockd Nothing Nothing True "/" Nothing Nothing
    _ <- forkIO $ putStrLn "Client connected!"
      >> makeHandshake    conn
      >> listenConnection conn
    socketHandler sock


makeHandshake :: Connection -> IO ()
makeHandshake Connection {socketDescriptorC = sockd} = do
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
listenConnection conn@Connection {socketDescriptorC = sockd} = do
    msg <- readMessage sockd
    
    connU@Connection {
        connected = isConnConnected, 
        socketDescriptorC = sockhu
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
        let Connection {socketDescriptorC = sockd} = conn
        _ <- writeMessage sockd 502 "Command not implemented" 
        return conn -- TLS and SSL is not supported
    "USER" -> do
        let Connection {socketDescriptorC = sockd} = conn
        let loginValue = Prelude.head tokens
        _ <- writeMessage sockd 331 "User name okay, need password"
        processAuthorization conn {login = Just loginValue}
    "PASS" -> do
        let passwordValue = Prelude.head tokens
        processAuthorization conn {password = Just passwordValue}
    "SYST" -> do
        let Connection {socketDescriptorC = sockd} = conn
        _ <- writeMessage sockd 215 "Unix Type: L8"
        return conn
    "PWD"  -> do
        let Connection {socketDescriptorC = sockd,
                        directory = dir} = conn
        _ <- writeMessage sockd 257 ("\"" ++ dir ++ "\" created")
        return conn
    "TYPE" -> do
        let Connection {socketDescriptorC = sockd} = conn
        let typeValue = Prelude.head tokens
        if (typeValue == "A") || (typeValue == "I")
        then do
            _ <- writeMessage sockd 200 ("Set type to " ++ typeValue)
            return conn {representation = Just typeValue}
        else do
            _ <- writeMessage sockd 504 ("Command not implemented for that parameter (" 
                                         ++ typeValue ++ ")")
            return conn
    "PASV" -> do
        trans <- runServer 0 $ openDataTransportSocket conn
        return conn {transporter = Just trans}
    "LIST" -> do
        let Connection {socketDescriptorC = sockd, directory = dir,
                        transporter = Just DataTransporter {
                            socketDescriptorDT = sockd2
                        }} = conn
        -- Make listing of files
        _ <- writeMessage sockd 150 ("Ready to List Directory of \"" ++ dir ++ "\"")
        _ <- close sockd2
        _ <- writeMessage sockd 226 "Closing data connection"
        return conn 
    "CWD"  -> do
        return conn
    ""     -> do
        return conn {connected = False}
    _      -> do
        let Connection {socketDescriptorC = sockd} = conn
        _ <- writeMessage sockd 500 "Syntax error, command unrecognized" 
        return conn

processMessage conn [] = do
    let Connection {socketDescriptorC = sockd} = conn
    _ <- writeMessage sockd 500 "Syntax error, command unrecognized" 
    return conn


processAuthorization :: Connection -> IO Connection
processAuthorization conn@Connection {socketDescriptorC = sockd, 
                        login    = Just loginValue, 
                        password = Just passwordValue} = do
    let verdict = (loginValue == "root") && (passwordValue == "123")
    
    if verdict
    then writeMessage sockd 230 "User logged in, proceed"
    else writeMessage sockd 230 "Not logged in"

    return conn {connected = verdict}

processAuthorization conn@Connection {login = _, password = _} = return conn


openDataTransportSocket :: Connection -> Socket -> IO DataTransporter
openDataTransportSocket Connection {socketDescriptorC = sockd} sock = do
    port <- socketPort sock
    let intPort = toInteger port

    -- TODO: get host address from socket
    _ <- writeMessage sockd 227 ("Entering Passive Mode (127,0,0,1," 
                                 ++ (show $ intPort `div` 256) ++ "," 
                                 ++ (show $ intPort `mod` 256) ++ ")")
    (sockd2, _) <- accept sock
    return $ DataTransporter sockd2 Nothing