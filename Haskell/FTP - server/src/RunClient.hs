{-# LANGUAGE TemplateHaskell   #-}

module RunClient where

import Network.Socket as S
--import FTPCommon

import Data.Text as DT (strip, pack, unpack, uncons)
import Data.List.Split as DLS
import Data.Maybe as DM

import FTPCommon

data ConsoleCommand = Open AddrInfo | Close | Exit
                    | Login String String | State
    deriving (Show)

data ClientState = ClientState {
    socket_  :: Maybe Socket,
    logined_ :: Bool,
    break_   :: Bool
} deriving (Show)

parseCommand :: String -> Maybe (IO ConsoleCommand)
parseCommand line = case DLS.splitOn " " line of
    ["open", address, port] -> Just $ __resolveAddress address port >>= return . Open
    ["open", address]       -> Just $ __resolveAddress address "21" >>= return . Open
    ["open"]                -> Just $ __resolveAddress "127.0.0.1" "21" >>= return . Open
    ["login", login, pass]  -> Just $ return $ Login login pass
    ["close"]               -> Just $ return Close
    ["state"]               -> Just $ return State
    ["exit"]                -> Just $ return Exit
    _                       -> Nothing

    where
        __resolveAddress :: String -> String -> IO AddrInfo
        __resolveAddress host port = do
            let hints = defaultHints { addrSocketType = Stream }
            addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
            return addr

main :: IO ()
main = interpreter $ ClientState Nothing False False

interpreter :: ClientState -> IO ()
interpreter state = do
    getLine >>= interpretLine state >>= \s@ClientState {break_ = stop} -> 
        case stop of False -> interpreter s
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
executeLine state@ClientState {socket_ = msock} commandIO = Just $ commandIO >>= 
    \command -> case command of
        Exit      -> __closeConnection state >>= \s -> return s {break_ = True}
        Open addr -> socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                  >>= \sock -> (connect sock $ addrAddress addr)
                  >>  readMessageWithCode sock >>= \(code, message) -> case code of
                        220 -> return state {socket_ = Just sock}
                        _   -> print ("Handshake failed: " ++ message) 
                            >> return state
        Close     -> __closeConnection state >>= return
        State     -> print state >> return state

        Login login pass -> __doIfConnected (\sockd -> 
            writeTransportMessage sockd ("USER " ++ login) >>
            readMessageWithCode sockd >>= \(code, message) -> case code of
                331 -> writeTransportMessage sockd ("PASS " ++ pass)
                    >> readMessageWithCode sockd >>= \(code2, message2) -> case code2 of
                        230 -> return state {logined_ = True}
                        _   -> print ("Login (PASS) failed: " ++ message2)
                            >> __closeConnection state
                _   -> print ("Login (USER) failed: " ++ message) 
                    >> __closeConnection state)
    where
        __closeConnection :: ClientState -> IO ClientState
        __closeConnection st = __doIfConnected (\sockd -> (writeTransportMessage sockd "") >> 
                close sockd >> return st {socket_ = Nothing, logined_ = False})

        __doIfConnected :: (Socket -> IO ClientState) -> IO ClientState
        __doIfConnected f = case msock >>= \sockd -> Just $ f sockd of
                Nothing -> print "Not connected" >> return state 
                Just v  -> v