{-# LANGUAGE TemplateHaskell   #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Run where

import Network.Socket as S

import qualified Data.ByteString.Char8 as B8
import Network.Socket.ByteString as BS
import Control.Concurrent (forkIO)
import Data.List.Split as DLS
-- import Foreign.Ptr as FP
-- import Data.Word as DW
import Data.Text as DT
import Text.Read

import System.Directory
import System.FilePath
import System.IO

data ControlCommand = AUTH | USER | PASS | SYST
                    | PWD | TYPE | PASV | LIST
                    | STOR | RETR | CWD | MKD
                    | RMD
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
    msg <- BS.recv sockd (1024 * 4)
    return $ B8.unpack msg


writeMessage :: Socket -> Int -> String -> IO ()
writeMessage sockd code comment = do
    let msg = (show code) ++ " " ++ comment
    writeTransportMessage sockd msg


writeTransportMessage :: Socket -> String -> IO ()
writeTransportMessage sockd comment = do
    _ <- BS.send sockd $ B8.pack (comment ++ "\r\n")
    print (comment ++ "\r\n")


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
        let value = readMaybe $ Prelude.head tokens
        case value of
            j@(Just _) -> writeMessage sockd 200 ("Set type to " ++ (show j))
            Nothing -> writeMessage sockd 504 $ "Unknown type (" ++ (show value) ++ ")"

        return conn {representation = value}
    "PASV" -> do
        trans <- runServer 0 $ openDataTransportSocket conn
        return conn {transporter = Just trans}
    "LIST" -> do
        let Connection {transporter = transporterDesc,
                        socketDescriptorC = sockd,
                        directory = dir}  = conn

        result <- case transporterDesc of
                    Just desc -> do -- Make listing of files
                        let message = "Ready to list directory \"" ++ dir ++ "\""
                        writeMessage sockd 150 message

                        writeListOfFiles desc dir

                        writeMessage sockd 226 "Closing data connection"
                        return conn {transporter = Nothing}
                    Nothing   -> do -- DataTransporter is not initialized
                        writeMessage sockd 450 "Requested file action not taken"
                        return conn
        return result
    "STOR" -> do
        let Connection {transporter = transporterDesc,
                        socketDescriptorC = sockd,
                        representation = represT,
                        directory = dir}  = conn
        let fileName = Prelude.head tokens

        result <- case (transporterDesc, represT) of
                    (Just desc, Just repres) -> do -- Make downloading of file
                        let message = "Ready to download \"" ++ fileName ++ "\""
                        writeMessage sockd 150 message

                        localPath <- locateFilePath dir
                        let filePath = localPath </> fileName
                        readAndSaveFile desc repres $ filePath

                        writeMessage sockd 226 "Closing data connection"
                        return conn {transporter = Nothing}
                    (_, _) -> do -- DataTransporter or RepresentType is not initialized
                        writeMessage sockd 450 "Requested file action not taken"
                        return conn
        return result
    "RETR" -> do
        let Connection {transporter = transporterDesc,
                        socketDescriptorC = sockd,
                        representation = represT,
                        directory = dir}  = conn
        let fileName = Prelude.head tokens

        result <- case (transporterDesc, represT) of
                    (Just desc, Just repres) -> do -- Make uploading of file
                        let message = "Ready to upload \"" ++ fileName ++ "\""
                        writeMessage sockd 150 message

                        localPath <- locateFilePath dir
                        let filePath = localPath </> fileName
                        Run.writeFile desc repres $ filePath

                        writeMessage sockd 226 "Closing data connection"
                        return conn {transporter = Nothing}
                    (_, _) -> do -- DataTransporter or RepresentType is not initialized
                        writeMessage sockd 450 "Requested file action not taken"
                        return conn
        return result
    "CWD"  -> do
        let destinationValue = Prelude.head tokens
        let Connection {socketDescriptorC = sockd,
                        directory = dir} = conn
        result <- case (dir, destinationValue) of
            (now, ".")  -> return now
            (now, "..") -> do
                let splitted = splitPath now
                let up = Prelude.take (max ((Prelude.length splitted) - 1) 1) splitted
                return $ joinPath up
            (now, path) -> return $ now </> path
        _ <- writeMessage sockd 250 ("\"" ++ result ++ "\" is current directory")
        return conn {directory = result}
    "MKD"  -> do
        let destinationValue = Prelude.head tokens
        let Connection {socketDescriptorC = sockd,
                        directory = dir} = conn

        localPath <- locateFilePath dir
        let path = localPath </> destinationValue
        exists <- doesDirectoryExist path
        if exists
        then writeMessage sockd 550 $ "\"" ++ path ++ "\" already exists"
        else do
            _       <- createDirectory path
            exists2 <- doesDirectoryExist path

            if exists2 -- Checking that directory created successfully
            then writeMessage sockd 257 $ "\"" ++ path ++ "\" created"
            else writeMessage sockd 550 $ "MKD \"" ++ path ++ "\" failed"
        return conn
    "RMD"  -> do
        let destinationValue = Prelude.head tokens
        let Connection {socketDescriptorC = sockd,
                        directory = dir} = conn

        localPath <- locateFilePath dir
        let path = localPath </> destinationValue
        exists <- doesDirectoryExist path
        if not exists
        then writeMessage sockd 500 $ "\"" ++ path ++ "\" doesn't exist"
        else do
            _       <- removeDirectoryRecursive path
            exists2 <- doesDirectoryExist path

            if not exists2 -- Checking that directory removed successfully
            then writeMessage sockd 250 $ "\"" ++ path ++ "\" completely removed"
            else writeMessage sockd 550 $ "RMD \"" ++ path ++ "\" failed"
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
openDataTransportSocket Connection {socketDescriptorC = sockd} 
                        sock = do
    port <- socketPort sock
    let intPort = toInteger port

    -- TODO: get host address from socket
    _ <- writeMessage sockd 227 ("Entering Passive Mode (127,0,0,1," 
                                 ++ (show $ intPort `div` 256) ++ "," 
                                 ++ (show $ intPort `mod` 256) ++ ")")
    (sockd2, _) <- accept sock
    return $ DataTransporter sockd2 Nothing


locateFilePath :: FilePath -> IO FilePath
locateFilePath path = do
    current <- getCurrentDirectory
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

    value <- case (isD, isF) of
                (True, _) -> do
                    let template = "drwxr-xr-- 1"
                    let owner    = "root root"
                    let name     = Prelude.last $ splitPath path
                    size <- getFileSize path
                    print path
                    print name

                    let result = (template ++ " " ++ owner ++ " " ++ (show size) 
                                  ++ " " ++ "Feb 18 1997" ++ " " ++ name)
                    return result
                (_, True) -> do
                    let template = "-rwxr-xr-- 1"
                    let owner    = "root root"
                    let name     = takeFileName path
                    size <- getFileSize path

                    let result = (template ++ " " ++ owner ++ " " ++ (show size) 
                                  ++ " " ++ "Feb 18 1997" ++ " " ++ name)
                    return result
                _ -> undefined
    return value


getPutFunction :: RepresentType -> (Handle -> String -> IO ())
getPutFunction representType = case representType of
    A -> hPutStrLn
    I -> hPutStr

readAndSaveFile :: DataTransporter -> RepresentType -> FilePath -> IO ()
readAndSaveFile DataTransporter {socketDescriptorDT = sockd2} 
                representType path = do
    fileOut <- openBinaryFile path WriteMode
    print $ "File \"" ++ path ++ "\" opened"

    bytes <- copyAllBytes sockd2 fileOut
    print $ "Read " ++ (show bytes) ++ " bytes"

    _ <- close sockd2
    hClose fileOut

    where copyAllBytes :: Socket -> Handle -> IO Int
          copyAllBytes sockd out = do
                message <- readMessage sockd
                let len = Prelude.length message
                result <- if len == 0 then return 0
                          else do 
                            _    <- (getPutFunction representType) out message
                            next <- copyAllBytes sockd out
                            return $ len + next
                return result


writeFile :: DataTransporter -> RepresentType -> FilePath -> IO ()
writeFile DataTransporter {socketDescriptorDT = sockd2} 
          representType path = do
    fileIn <- openBinaryFile path ReadMode
    print $ "File \"" ++ path ++ "\" opened"
    case representType of
        I -> do
            bytes <- copyAllBytes fileIn sockd2
            _ <- writeTransportMessage sockd2 ""
            print $ "Read " ++ (show bytes) ++ " bytes"
            return ()

            where copyAllBytes :: Handle -> Socket -> IO Int
                  copyAllBytes source sockd = do
                     message <- hGetContents source
                     let len = Prelude.length message
                     result <- if len == 0 then return 0
                               else do 
                                    _    <- BS.send sockd $ B8.pack message
                                    next <- copyAllBytes source sockd
                                    return $ len + next
                     return result
        A -> do
            bytes <- copyAllBytes fileIn sockd2
            _ <- writeTransportMessage sockd2 ""
            print $ "Read " ++ (show bytes) ++ " bytes"
            return ()

            where copyAllBytes :: Handle -> Socket -> IO Int
                  copyAllBytes source sockd = do
                     eof <- hIsEOF source
                     result <- if not eof
                                then do
                                    message <- hGetLine source
                                    let len = Prelude.length message
                                    _    <- writeTransportMessage sockd message
                                    next <- copyAllBytes source sockd
                                    return $ len + next
                                else return 0
                     return result
    _ <- close sockd2
    hClose fileIn