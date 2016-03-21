module Main where

import System.Console.GetOpt
import System.Environment
import Lambdaphoto

data Flag
    = Copy      --copy
    | Move      --move
    | Rec       --rec
    | NoRec     --norec
    | Help      --help
    | Version   --version
    deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [Option ['c'] ["copy"]      (NoArg Copy)    "Copy photos (default)"
    ,Option ['m'] ["move"]      (NoArg Move)    "Move photos"
    ,Option ['r'] ["rec"]       (NoArg Rec)     "Recursive (default)"
    ,Option ['n'] ["norec"]     (NoArg NoRec)   "Non recursive"
    ,Option ['h'] ["help"]      (NoArg Help)    "Show this help"
    ,Option ['v'] ["version"]   (NoArg Version) "Show version information"
    ]

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute options argv of
    (opts,args,[])  -> return (opts, args)
    (_,_,errs)      -> showError (concat errs)

run :: ([Flag], [String]) -> IO ()
run (opts, args)
    | Help `elem` opts      = putStrLn $ help -- $ usageInfo "Usage: lambdaphoto [options] source [target]" options
    | Version `elem` opts   = putStrLn $ "Lambdaphoto " ++ version

    | both Copy Move opts   = showError "Specify only one of --copy or --move"
    | both Rec NoRec opts   = showError "Specify only one of --rec or --norec"

    | otherwise = case args of
        [source]            -> sortPhotos oper rec source source
        [source, target]    -> sortPhotos oper rec source target
        []                  -> showError "Please, specify a path."
        otherwise           -> showError "Too many paths, the maximum is two."
        where
            oper    = if Move `elem` opts then movePhoto else copyPhoto
            rec     = NoRec `notElem` opts

both :: (Eq a) => a -> a -> [a] -> Bool
both e1 e2 l = e1 `elem` l && e2 `elem` l

showError :: String -> IO a
showError str = fail (str ++ "\nTry 'lambdaphoto --help' for more information.")

version :: String
version = "0.3"

main :: IO ()
main = getArgs >>= parse >>= run

help :: String
help = unlines [
    "Usage: lamdaphoto [options] source [target]",
    "Sort photos in the 'source' directory, replacing the patterns in the 'target' path with EXIF metadata.",
    "",
    usageInfo "Options:" options,
    "Patterns:",
    "  %make    Camera manufacturer",
    "  %model   Camera model",
    "  %date    Full date (yyyy-mm-dd)",
    "  %year    Year",
    "  %month   Month",
    "  %day     Day of the month",
    "",
    "Examples:",
    "  lambdaphoto /media/camera myphotos",
    "  lambdaphoto /media/camera myphotos/%make-%model/%year/%date",
    "  lambdaphoto --move myphotos myphotos/%year/%month/%day",
    "",
    "WARNING! This tool is in beta and may mess your photos,",
    "please make sure you have a backup specially in --move mode."
    ]
