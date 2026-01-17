module FsFilter (
    FilterMode (..),
    toFilterMode,
    filterPath,
) where

import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist, executable, getPermissions, pathIsSymbolicLink)

-- | Defines the filtering mode for paths.
data FilterMode = FilesOnly | DirsOnly | ExecutablesOnly | SymlinksOnly
    deriving (Show, Eq)

-- | Converts a list of arguments into a 'FilterMode'.
toFilterMode :: [String] -> Either String FilterMode
toFilterMode ["--only-files"] = Right FilesOnly
toFilterMode ["--only-dirs"] = Right DirsOnly
toFilterMode ["--only-executables"] = Right ExecutablesOnly
toFilterMode ["--only-symlinks"] = Right SymlinksOnly
toFilterMode [] = Left "No filter mode specified. Use one of the available options."
toFilterMode args = Left $ "Unknown or invalid arguments: " ++ unwords args

{- | Filters a single path based on the given 'FilterMode'.
Returns 'Just path' if the path matches the filter, 'Nothing' otherwise.
-}
filterPath :: FilterMode -> FilePath -> IO (Maybe FilePath)
filterPath FilesOnly path = do
    isFile <- doesFileExist path
    if isFile then return (Just path) else return Nothing
filterPath DirsOnly path = do
    isDir <- doesDirectoryExist path
    if isDir then return (Just path) else return Nothing
filterPath ExecutablesOnly path = do
    isFile <- doesFileExist path
    if isFile
        then do
            perms <- getPermissions path
            if executable perms then return (Just path) else return Nothing
        else return Nothing
filterPath SymlinksOnly path = do
    exists <- doesPathExist path
    if exists
        then do
            isSymlink <- pathIsSymbolicLink path
            if isSymlink then return (Just path) else return Nothing
        else return Nothing
