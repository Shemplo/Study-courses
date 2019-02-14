{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE OverloadedStrings #-}

module RunClient where

import Network.Socket as S
--import FTPCommon

import Data.Text as DT (strip, pack, unpack, uncons, replace)
import Control.Lens as CL ((^.), (%~), (&), makeLenses)
import Data.List.Split as DLS
import Data.Maybe as DM

import FTPCommon

data ConsoleCommand = Open AddrInfo | Close | Exit
                    | Login String String | State
                    | List
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
                        230 -> writeTransportMessage sockd "PWD /"
                            >> return (state & logined %~ (\_ -> True))
                        _   -> print ("Login (PASS) failed: " ++ message2)
                            >> __closeConnection state
                _   -> print ("Login (USER) failed: " ++ message) 
                    >> __closeConnection state)

        List -> __doIfLogined state (\_ -> (__initTransporter state) >>= \st ->
            case st ^. transporter of
                Just trans -> undefined
                Nothing    -> print "Transporter is not initialized"
                           >> return st)
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
            writeTransportMessage sockd "PASV" >>
            readMessageWithCode sockd >>= \(code, message) -> case code of
                227 -> let replF = replace (pack ")") (pack "") in
                    let cmessage = unpack $ replF $ strip $ pack message in
                    let mtokens = DLS.splitOn "," cmessage in
                    print mtokens >> return st
                _ -> print ("Server doesn't support passive mod, " ++
                    "but PORT in not implemented")
                  >> return st)