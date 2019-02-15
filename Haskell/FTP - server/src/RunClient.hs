{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE OverloadedStrings #-}

module RunClient where

import Network.Socket as S
--import FTPCommon

import Data.Text as DT (strip, pack, unpack, uncons, replace)
import Control.Lens as CL ((^.), (.~), (&), makeLenses)
import Data.List.Split as DLS

import FTPCommon

data ConsoleCommand = Open AddrInfo | Close | Exit
                    | Login String String | State
                    | List | CurDir FilePath 
                    | MakeDir FilePath | RemoveDir FilePath
    deriving (Show)

data ClientState = ClientState {
    _socketD     :: Maybe Socket,
    _logined     :: Bool,
    _break       :: Bool,
    _transporter :: Maybe DataTransporter
} deriving (Show)

makeLenses ''ClientState

parseCommand :: String -> Maybe (IO ConsoleCommand)
parseCommand line = case DLS.splitOn " " line of
    ["open", address, port] -> Just $ __resolveAddress address port >>= return . Open
    ["open", address]       -> Just $ __resolveAddress address "21" >>= return . Open
    ["open"]                -> Just $ __resolveAddress "127.0.0.1" "21" >>= return . Open
    ["login", login, pass]  -> Just $ return $ Login login pass
    ["removed", dir]        -> Just $ return $ RemoveDir dir
    ["maked", dir]          -> Just $ return $ MakeDir dir
    ["cd", path]            -> Just $ return $ CurDir path
    ["close"]               -> Just $ return Close
    ["state"]               -> Just $ return State
    ["exit"]                -> Just $ return Exit
    ["list"]                -> Just $ return List
    _                       -> Nothing

    where
        __resolveAddress :: String -> String -> IO AddrInfo
        __resolveAddress host port = do
            let hints = defaultHints { addrSocketType = Stream }
            addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
            return addr

main :: IO ()
main = interpreter $ ClientState Nothing False False Nothing

interpreter :: ClientState -> IO ()
interpreter state = do
    getLine >>= interpretLine state >>= \s -> 
        case s ^. RunClient.break of 
            False -> interpreter s
            True  -> return ()

interpretLine :: ClientState -> String -> IO ClientState
interpretLine state line = do
    print $ "DEBUG: " ++ line

    case ((uncons $ strip $ pack line) >>= \_ -> 
        parseCommand line >>= executeLine state) of
        Just v -> v >>= return
        _      -> print "Unknown command, try ? for help"
               >> return state


executeLine :: ClientState -> IO ConsoleCommand -> Maybe (IO ClientState)
executeLine state commandIO = Just $ commandIO >>= 
    \command -> case command of
        Exit      -> __closeConnection state >>= \s -> return s {_break = True}
        Open addr -> socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                  >>= \sock -> (connect sock $ addrAddress addr)
                  >>  readMessageWithCode sock >>= \(code, message) -> case code of
                        220 -> return state {_socketD = Just sock}
                        _   -> print ("Handshake failed: " ++ message) 
                            >> return state
        Close     -> __closeConnection state >>= return
        State     -> print state >> return state

        Login login pass -> __doIfConnected state (\sockd -> 
            writeTransportMessage sockd ("USER " ++ login) >>
            readMessageWithCode sockd >>= \(code, message) -> case code of
                331 -> writeTransportMessage sockd ("PASS " ++ pass)
                    >> readMessageWithCode sockd >>= \(code2, message2) -> case code2 of
                        230 -> writeTransportMessage sockd "PWD"
                            >> readMessage sockd -- read answer for PWD
                            >> return (state & logined .~ True)
                        _   -> print ("Login (PASS) failed: " ++ message2)
                            >> __closeConnection state
                _   -> print ("Login (USER) failed: " ++ message) 
                    >> __closeConnection state)

        List -> __doIfLogined state (\sockd -> (__initTransporter state) >>= \st ->
            case st ^. transporter of
                Just trans -> let sockdt = trans ^. socketDescriptorDT in 
                    writeTransportMessage sockd "LIST" >> 
                    readMessageWithCode sockd >>= \(code, message) -> do 
                        case code of
                            150 -> readMessage sockd >> readAllMessages sockdt >>= \list -> 
                                (sequence $ fmap print list) >> return ()
                            _   -> print ("Transportation (LIST) failed: " ++ message)
                        close sockdt >> return st {_transporter = Nothing}
                Nothing -> print "Transporter is not initialized"
                        >> return st)

        CurDir path -> __doIfLogined state (\sockd ->
            writeTransportMessage sockd ("CWD " ++ path) >> 
            readMessage sockd >>
            return state)

        MakeDir dir -> __doIfLogined state (\sockd ->
            writeTransportMessage sockd ("MKD " ++ dir) >> 
            readMessage sockd >>
            return state)
        RemoveDir dir -> __doIfLogined state (\sockd ->
            writeTransportMessage sockd ("RMD " ++ dir) >> 
            readMessage sockd >>
            return state)

    where
        __closeConnection :: ClientState -> IO ClientState
        __closeConnection st = __doIfConnected st (\sockd -> (writeTransportMessage sockd "") >> 
                close sockd >> return st {_socketD = Nothing, _logined = False})

        __doIfConnected :: ClientState -> (Socket -> IO ClientState) -> IO ClientState
        __doIfConnected st f = case st ^. socketD >>= \sockd -> Just $ f sockd of
                Nothing -> print "Not connected" >> return st
                Just v  -> v

        __doIfLogined :: ClientState -> (Socket -> IO ClientState) -> IO ClientState
        __doIfLogined st f = __doIfConnected st (\sockd -> 
            if st ^. logined then f sockd
            else print "Not authorized" >> return st)

        __initTransporter :: ClientState -> IO ClientState
        __initTransporter st = __doIfLogined st (\sockd ->
            writeTransportMessage sockd "TYPE I" >>
            readMessage sockd >> -- read answer fro TYPE
            writeTransportMessage sockd "PASV" >>
            readMessageWithCode sockd >>= \(code, message) -> case code of
                227 -> let replF = replace (pack ")") (pack "") in
                    let cmessage = unpack $ replF $ strip $ pack message in
                    case DLS.splitOn "," cmessage of
                        [_, _, _, _, hd, tl] -> do
                            sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
                            SockAddrInet _ curAddr <- getPeerName sockd
                            let port = (read hd) * 256 + (read tl)
                            
                            connect sock $ SockAddrInet port curAddr
                            return st {_transporter = Just $ DataTransporter sock Nothing}
                        _ -> print "Failed parse port" >> return st
                _ -> print ("Server doesn't support passive mod, " ++
                    "but PORT in not implemented")
                  >> return st)