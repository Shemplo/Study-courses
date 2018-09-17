{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileSystem where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans ()

import Data.Char
import Data.Text as T
import Data.Text.IO as IOo

import System.Directory
import System.FilePath

data FS = Dir  {_name :: FilePath, _contents :: [FS]}
        | File {_name :: FilePath}

makeLenses ''FS
makePrisms ''FS

instance Show FS where
    show = unpack . printer 0 where
        printer :: Int -> FS -> Text
        printer offset (File _name) = T.replicate offset "  " `append` pack _name `append` "\n"
        printer offset (Dir  _name contents') = 
            let title             = "Directory: " `append` pack _name `append` "\n" in
            let insideFolder fs s = s `append` printer (offset + 1) fs in
            let scope             = Prelude.foldr insideFolder "" contents' in
            T.replicate offset "  " `append` title `append` scope

getDir :: FilePath -> IO FS
getDir path = do
    isD <- doesDirectoryExist path
    isF <- doesFileExist path
    case (isD, isF) of
        (True, _) -> do
            scope <- listDirectory path
            Dir (Prelude.last $ splitPath path) <$> mapM (getDir . (</>) path) scope
        (_, True) -> pure $ File $ takeFileName path
        _ -> undefined {- situation is impossible only for not existing path -}

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir = not . isFile {- in general it isn't truth but here FS can be cnstructed
                        only for Dir(s) and File(s) ... so it's correct -}

-- Type of lens Traversal
-- Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
-- Traversal' s a = Traversal s s a a

_getEntry :: FilePath -> (FS -> Bool) -> Traversal' FS FS
_getEntry nm prd = contents . traversed . filtered criteria where
                   criteria x = prd x && x ^. name == nm

file :: FilePath -> Traversal' FS FS
file fnm = _getEntry fnm isFile

runcCD :: FilePath -> Traversal' FS FS
runcCD dnm = _getEntry dnm isDir

runcLS :: Traversal' FS FS
runcLS = contents.each

runLSR :: FS -> [FilePath]
runLSR fs = let _getF = fs ^.. contents . traversed . filtered isFile . name in
            let _getD = fs ^.. contents . traversed . filtered isDir in
            (_getF ++ (_getD >>= runLSR))

replaceFileExtension :: Text -> FS -> FS
replaceFileExtension ext = _get . name %~ (`replaceExtension` unpack ext) where
                           _get = contents . traversed . filtered isFile

removeDirectory :: FilePath -> FS -> FS
removeDirectory fnm fs = fs & contents .~ (fs ^.. _get) where
                         prd x = not $ x ^. name == fnm && Prelude.null (x ^. contents)
                         _get = contents . traversed . filtered prd

getPath :: [FS] -> FilePath
getPath [] = ""
getPath (Dir dnm _ : xs) = getPath xs ++ "\\" ++  dnm
getPath _ = undefined

data Command = LS      -- Print list of entries
             | UP      -- Go to parent of directory
             | CD Text -- Switch directory
             | Unknown -- Unknown command

parseCommand :: Text -> Command
parseCommand input =
    let command = T.takeWhile (not . isSpace) input
        args = T.dropWhile isSpace $ T.dropWhile (not . isSpace) input in
    if args == "" then
        case command of
            "up" -> UP
            "ls" -> LS
            _    -> Unknown
    else case command of
            "cd" -> CD args
            _    -> Unknown

data CrawlerContext = CrawlerContext {_fs :: [FS], _root :: FilePath, _info :: [(Int, Int)] }
makeLenses ''CrawlerContext

instance Show CrawlerContext where
    show (CrawlerContext ds rp ((a,b):_)) =
        "You in " ++ getPath ds ++ "\n" ++
        "Files from root "  ++ rp ++ " : " ++ show a ++ "\n" ++
        "Directories from " ++ rp ++ " : " ++ show b ++ "\n"
    show _ = undefined

visitor :: (MonadState CrawlerContext m, MonadIO m) => m ()
visitor = do
    context <- get
    liftIO $ print context

    let infos         = _info context
    let (files, dirs) = Prelude.head infos
    let activeFS      = Prelude.head $ _fs context

    input <- liftIO IOo.getLine
    case parseCommand input of
        (CD dnm) ->
            case activeFS ^? runcCD (unpack dnm) of
                Nothing -> liftIO $ IOo.putStrLn "The directory does not exist"
                Just child -> do
                    modify ((%~) fs (child :))
                    let _get prd = child ^.. contents . traversed . filtered prd
                    let files' = files + Prelude.length (_get isFile)
                    let dirs'  = dirs  + Prelude.length (_get isDir)
                    modify ((%~) info ((files', dirs') :))
        UP -> if Prelude.length infos == 1 then
                liftIO $ IOo.putStrLn "The root is reached"
              else do
                modify ((%~) fs   (Prelude.drop 1))
                modify ((%~) info (Prelude.drop 1))
        LS -> do
                let items = activeFS ^.. runcLS . name
                liftIO $ IOo.putStrLn $ intercalate "\n" (Prelude.map pack items)
        _ -> liftIO $ IOo.putStrLn "Unknown command"
    visitor

crawler :: FilePath -> IO ()
crawler path = do
    dir <- getDir path
    let _get prd = dir ^.. contents . traversed . filtered prd
    let files = Prelude.length (_get isFile)
    let dirs  = Prelude.length (_get isDir)
    _ <- runStateT visitor (CrawlerContext [dir] path [(files, dirs)])
    return ()

walker :: FilePath -> IO ()
walker = crawler