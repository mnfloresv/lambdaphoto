module Lambdaphoto where

import System.FilePath
import System.Directory
import Data.List
import Data.Maybe
import Text.Regex
import Control.Monad
import Control.Exception
import Graphics.Exif

sortPhotos :: (FilePath -> FilePath -> IO ()) -> Bool -> FilePath -> FilePath -> IO ()
sortPhotos fileOp recur source target = evalDirectory recur (sortPhoto fileOp dest) source
    where dest = if '%' `elem` target then target else target </> "%date"

evalDirectory :: Bool -> (FilePath -> IO ()) -> FilePath -> IO ()
evalDirectory recur func path = do
    allcontent <- getDirectoryContents path
    let content = map (path </>) $ filter (`notElem` [".", ".."]) allcontent
    files <- filterM doesFileExist content
    mapM_ func files
    when recur $ do
        subdirs <- filterM doesDirectoryExist content
        mapM_ (evalDirectory recur func) subdirs
        unless (null content) $ removeDirectoryIfEmpty path

removeDirectoryIfEmpty :: FilePath -> IO ()
removeDirectoryIfEmpty path = do
    allcontent <- getDirectoryContents path
    let content = filter (`notElem` [".", ".."]) allcontent
    when (null content) $ removeDirectory path

sortPhoto :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
sortPhoto fileOp dest file = do
    exif <- try (fromFile file) :: IO (Either SomeException Exif)
    case exif of
        Left _ -> putStrLn $ "Omitting " ++ file ++ ", no EXIF data :("
        Right exifdata -> do
            target <- mkPath exifdata dest
            case target of
                Nothing -> putStrLn $ "Omitting " ++ file ++ ", no EXIF date :("
                Just target2 -> do
                    createDirectoryIfMissing True target2
                    fileOp file (target2 </> takeFileName file)

mkPath :: Exif -> String -> IO (Maybe String)
mkPath exifdata path = do
    make <- getTag exifdata "Make"
    model <- getTag exifdata "Model"
    datetime <- getTag exifdata "DateTimeOriginal"
    let [year, month, day, _, _, _] = splitRegex (mkRegex "[: ]") $ fromMaybe "" datetime
    let date = intercalate "-" [year, month, day]
    let replaces = [
           ("%make", fromMaybe "" make),
           ("%model", fromMaybe "" model),
           ("%date", date),
           ("%year", year),
           ("%month", month),
           ("%day", day)]
    return $ case datetime of
        Nothing -> Nothing
        _ -> Just $ mkPath2 replaces path

mkPath2 :: [(String, String)] -> String -> String
mkPath2 [] cad = cad
mkPath2 ((pat, val):xs) cad = mkPath2 xs $ subRegex (mkRegex pat) cad val

movePhoto :: FilePath -> FilePath -> IO ()
movePhoto orig dest
    | orig == dest = return ()
    | otherwise = do
        exists <- doesFileExist dest
        case exists of
            True -> putStrLn $ "Skipping " ++ orig ++ ", " ++ dest ++ " already exists!"
            False -> do
                putStrLn $ "Moving " ++ orig ++ " to " ++ dest
                renameFile orig dest

copyPhoto :: FilePath -> FilePath -> IO ()
copyPhoto orig dest = do
    exists <- doesFileExist dest
    unless exists $ do
        putStrLn $ "Copying " ++ orig ++ " to " ++ dest
        copyFile orig dest
