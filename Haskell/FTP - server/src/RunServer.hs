{-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE OverloadedStrings #-}

module RunServer where

import Network.Socket as S

import qualified Data.ByteString.Char8 as B8
import Data.Text as DT (strip, pack, unpack)
import Network.Socket.ByteString as BS
import Control.Concurrent (forkIO)
import Data.List.Split as DLS
-- import Foreign.Ptr as FP
-- import Data.Word as DW
import Text.Read

import System.Directory
import System.FilePath
import System.IO

data ControlCommand = AUTH | USER | PASS | SYST
                    | PWD | TYPE | PASV | LIST
                    | STOR | RETR | CWD | MKD
                    | RMD | CDUP | DELE
    deriving (Show, Read)

data RepresentType = I | A -- | E | L Int
    deriving (Show, Read)

data Connection = Connection {
    socketDescriptorC :: Socket,
    login             :: Maybe String,
    password          :: Maybe String,
    connected         :: Bool,
    directory         :: FilePath,
    representation    :: Maybe RepresentType,
    transporter       :: Maybe DataTransporter
} deriving (Show)

data DataTransporter = DataTransporter {
    socketDescriptorDT :: Socket,
    task               :: Maybe String
} deriving (Show)


main :: IO ()
main = runServer 21 socketHandler


runServer :: PortNumber -> (Socket -> IO t) -> IO t
runServer port handler = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.bind sock $ S.SockAddrInet port 0
    S.listen sock S.maxListenQueue
    (handler sock) <* (close sock)


socketHandler :: Socket -> IO ()
socketHandler sock = do
    (sockd, _) <- accept sock
    let conn = Connection sockd Nothing Nothing True "/" Nothing Nothing
    _ <- forkIO $ print "Client connected!" -- new thread for socket
      >> makeHandshake conn >> listenConnection conn
    socketHandler sock


makeHandshake :: Connection -> IO ()
makeHandshake Connection {socketDescriptorC = sockd} =
    writeMessage sockd 220 "Welcome to Haskell FTP server" >> 
    print "Handshake made"


readMessage :: Socket -> IO String
readMessage sockd = BS.recv sockd (1024 * 4) >>= 
    return . B8.unpack


writeMessage :: Socket -> Int -> String -> IO ()
writeMessage sockd code comment = 
    return msg >>= writeTransportMessage sockd
    where msg = (show code) ++ " " ++ comment


writeTransportMessage :: Socket -> String -> IO ()
writeTransportMessage sockd message = 
    (BS.send sockd $ B8.pack msg) >> print msg
    where msg = message ++ "\r\n"


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
    print tokens

    case readEither $ head tokens of
        Left _ -> do
            let Connection {socketDescriptorC = sockd} = conn
            writeMessage sockd 500 "Syntax error, command unrecognized"
            return conn {connected = False}
        Right command -> do
            processMessage conn (tail tokens) command
    

processMessage :: Connection -> [String] -> ControlCommand -> IO Connection
processMessage conn@Connection {socketDescriptorC = sockd, directory = dir} 
               [] command = case command of
    PWD  -> writeMessage sockd 257 ("\"" ++ dir ++ "\" created") >> return conn
    SYST -> writeMessage sockd 215 "Unix Type: L8" >> return conn
    PASV -> (runServer 0 $ openDataTransportSocket conn) >>= \trans -> 
            return conn {transporter = Just trans}
    LIST -> do
        let Connection {transporter = transporterDesc} = conn
        let result = transporterDesc >>= \desc -> Just $ do -- :: Maybe (IO connection)
                writeMessage sockd 150 $ "Ready to list directory \"" ++ dir ++ "\""
                writeListOfFiles desc dir -- Sending all files and dirs in current
                writeMessage sockd 226 "Closing data connection"
                return conn {transporter = Nothing}

        case result of Nothing -> writeMessage sockd 450 "Requested file action not taken" >> return conn
                       Just io -> io
    CDUP -> writeMessage sockd 502 "Command not implemented" >> return conn
    _    -> undefined -- impossible by RFC 959

processMessage conn@Connection {socketDescriptorC = sockd, directory = dir} 
               (first:_) command = case command of
    AUTH -> writeMessage sockd 502 "Command not implemented" >> return conn -- TLS and SSL is not supported
    USER -> writeMessage sockd 331 "User name okay, need password"
         >> processAuthorization conn {login    = Just first}
    PASS -> processAuthorization conn {password = Just first}
    TYPE -> do
        let value = readMaybe first
        case value of
            j@(Just _) -> writeMessage sockd 200 ("Set type to " ++ (show j))
            Nothing -> writeMessage sockd 504 $ "Unknown type (" ++ (show value) ++ ")"
        return conn {representation = value}
    RETR -> __doFileIO RunServer.writeFile
    STOR -> __doFileIO readAndSaveFile
    CWD  -> do
        result <- case (dir, first) of
            (now, ".")  -> return now
            (now, "..") -> do
                let splitted = splitPath now -- FIXME: need optimization
                let up = take (max (length splitted - 1) 1) splitted
                return $ joinPath up
            (now, path) -> return $ now </> path
        writeMessage sockd 250 ("\"" ++ result ++ "\" is current directory")
        return conn {directory = result}
    MKD -> do
        localPath <- locateFilePath $ dir </> first
        exists <- doesDirectoryExist localPath
        if exists
        then writeMessage sockd 550 $ "\"" ++ localPath ++ "\" already exists"
        else do
            _       <- createDirectory localPath
            exists2 <- doesDirectoryExist localPath

            if exists2 -- Checking that directory created successfully
            then writeMessage sockd 257 $ "\"" ++ localPath ++ "\" created"
            else writeMessage sockd 550 $ "MKD \"" ++ localPath ++ "\" failed"
        return conn
    RMD  -> __doRM doesDirectoryExist removeDirectoryRecursive
    DELE -> __doRM doesFileExist removeFile 
    _ -> undefined -- impossible by RFC 959

    where __doFileIO :: (DataTransporter -> RepresentType -> FilePath -> IO ()) 
                     -> IO Connection
          __doFileIO f = do
            let Connection {transporter = transporterD, representation = represT} = conn
            let result = transporterD >>= \desc -> Just $ do
                    writeMessage sockd 150 $ "Ready to upload \"" ++ first ++ "\""
                    localPath <- locateFilePath $ dir </> first
                    case represT of
                        Just repres -> (f desc repres $ localPath)
                                    >> writeMessage sockd 226 "Closing data connection"
                        Nothing -> writeMessage sockd 450 "Requested file action not taken"
                    return conn {transporter = Nothing}

            case result of Nothing -> writeMessage sockd 450 "Requested file action not taken" >> return conn
                           Just io -> io
          __doRM :: (FilePath -> IO Bool) -> (FilePath -> IO ()) -> IO Connection
          __doRM fT fR = (locateFilePath $ dir </> first) >>= \path ->
                let notExistsMessage = "\"" ++ path ++ "\" doesn't exist" in
                fT path >>= \exists -> (case exists of
                    True  -> fR path >> fT path >>= \exists2 -> case exists2 of
                        True  -> writeMessage sockd 550 $ "DELE command failed"
                        False -> writeMessage sockd 250 $ "\"" ++ path ++ "\" completely removed"
                    False -> writeMessage sockd 500 $ notExistsMessage) >> return conn


processAuthorization :: Connection -> IO Connection
processAuthorization conn@Connection {socketDescriptorC = sockd, 
                                      login    = Just loginValue, 
                                      password = Just passwordValue} = do
    let verdict = (loginValue == "root") && (passwordValue == "123")

    if verdict
    then writeMessage sockd 230 "User logged in, proceed"
    else writeMessage sockd 230 "Not logged in"
    return conn {connected = verdict}

processAuthorization conn = return conn


-- TODO: get host address from socket
openDataTransportSocket :: Connection -> Socket -> IO DataTransporter
openDataTransportSocket Connection {socketDescriptorC = sockd} sock =
    socketPort sock >>= \port -> let intPort = toInteger port in do
        writeMessage sockd 227 ("Entering Passive Mode (127,0,0,1," 
                                ++ (show $ intPort `div` 256) ++ "," 
                                ++ (show $ intPort `mod` 256) ++ ")")
        (sockd2, _) <- accept sock
        return $ DataTransporter sockd2 Nothing


locateFilePath :: FilePath -> IO FilePath
locateFilePath path = getCurrentDirectory >>= \current ->
    return ((current </> "ftp") ++ path)


writeListOfFiles :: DataTransporter -> FilePath -> IO ()
writeListOfFiles DataTransporter {socketDescriptorDT = sockd2} 
                 path = do
    localPath <- locateFilePath path
    listD <- listDirectory localPath

    let list = fmap (localPath </>) listD
    listP <- sequence $ fmap prepareFilePathForTransport list
    _ <- sequence $ fmap (writeTransportMessage sockd2) listP
    _ <- close sockd2
    return ()


prepareFilePathForTransport :: FilePath -> IO String
prepareFilePathForTransport path = do
    isD <- doesDirectoryExist path
    isF <- doesFileExist path

    case (isD, isF) of
        (True, _) -> __compose "drwxr-xr-- 1" $ last $ splitPath path
        (_, True) -> __compose "-rwxr-xr-- 1" $ takeFileName path
        _         -> undefined
    where __compose :: String -> String -> IO String
          __compose prefix name = (getFileSize path) >>= \size -> 
                return $ unwords [prefix, "root root", show size, 
                                  "Feb 18 1997", name]


readAndSaveFile :: DataTransporter -> RepresentType -> FilePath -> IO ()
readAndSaveFile DataTransporter {socketDescriptorDT = sockd2} 
                representType path = do
    openBinaryFile path WriteMode <* print logMessage >>= \file ->
            __copyAllBytes sockd2 file <* hClose file >>= \bytes ->
                print ("Read " ++ (show bytes) ++ " bytes") >> 
                    close sockd2

    where __copyAllBytes :: Socket -> Handle -> IO Int
          __copyAllBytes sockd out = do
                message <- readMessage sockd
                let len = Prelude.length message
                if len == 0 then return 0
                else __getPutFunction out message >>
                    fmap (len +) (__copyAllBytes sockd out)
          __getPutFunction :: (Handle -> String -> IO ())
          __getPutFunction = case representType of
                A -> hPutStrLn
                I -> hPutStr

          logMessage = "File \"" ++ path ++ "\" opened"


writeFile :: DataTransporter -> RepresentType -> FilePath -> IO ()
writeFile DataTransporter {socketDescriptorDT = sockd2} 
          representType path = do
    openBinaryFile path ReadMode <* print logMessage >>= \file ->
        __copyAllBytes file sockd2 <* hClose file >>= \bytes ->
            print ("Read " ++ (show bytes) ++ " bytes") >> 
                close sockd2

    where __copyAllBytes :: Handle -> Socket -> IO Int
          __copyAllBytes src sockd = __readByType src >>= \content -> 
            let len = length content in
            if len > 0 then (BS.send sockd $ B8.pack (content ++ __suffixByType)) >>
                fmap (len +) (__copyAllBytes src sockd)
            else return 0
          __readByType :: Handle -> IO String
          __readByType src = case representType of
            A -> hIsEOF src >>= \eof -> if not eof then hGetLine src else return ""
            I -> hGetContents src
          __suffixByType :: String
          __suffixByType = case representType of
            A -> "\r\n"
            _ -> ""

          logMessage = "File \"" ++ path ++ "\" opened"