module Main (main) where

import Data.Foldable (forM_) -- Added for forM_
import FsFilter (FilterMode (..), filterPath, toFilterMode)
import System.Environment (getArgs)
import System.IO (hPutStrLn, isEOF, stderr)

-- | Displays usage information for the tool.
usage :: String
usage =
    unlines
        [ "Usage: hs-fs-filter [OPTION]"
        , ""
        , "Filters a list of paths from standard input, outputting only files or only directories."
        , ""
        , "Options:"
        , "  --only-files          : Output only paths that refer to existing files."
        , "  --only-dirs           : Output only paths that refer to existing directories."
        , "  --only-executables    : Output only paths that refer to existing executable files."
        , "  --only-symlinks       : Output only paths that refer to existing symbolic links."
        , "  --help                : Show this help message and exit."
        ]

-- | The main processing loop.
loop :: FilterMode -> IO ()
loop mode = do
    eof <- isEOF
    if eof
        then return ()
        else do
            path :: String <- getLine
            maybeFilteredPath <- filterPath mode path
            forM_ maybeFilteredPath putStrLn -- Replaced case with forM_
            loop mode

-- | Kicks off the processing loop or prints an error.
run :: Either String FilterMode -> IO ()
run (Right mode) = do
    loop mode
run (Left err) = do
    hPutStrLn stderr $ "Error: " ++ err
    hPutStrLn stderr usage

-- | Handles the command-line arguments.
handleArgs :: [String] -> IO ()
handleArgs ["--help"] = putStr usage
handleArgs args = run $ toFilterMode args

main :: IO ()
main = do
    args :: [String] <- getArgs
    handleArgs args
