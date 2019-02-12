module FTPCommon where

data ControlCommand = AUTH | USER | PASS | SYST
                    | PWD | TYPE | PASV | LIST
                    | STOR | RETR | CWD | MKD
                    | RMD | CDUP | DELE
    deriving (Show, Read)

data RepresentType = I | A -- | E | L Int
    deriving (Show, Read)