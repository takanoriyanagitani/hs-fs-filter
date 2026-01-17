module Main (main) where

import Control.Exception (Exception, throwIO)

import Control.Monad (unless)

import Data.Maybe (isNothing) -- Added for isNothing

import FsFilter (FilterMode (..), filterPath, toFilterMode)

import System.Directory (createDirectory, emptyPermissions, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)

import System.FilePath ((</>))

import System.IO.Temp (withSystemTempDirectory)

import System.Posix.Files (createSymbolicLink)

newtype TestException = TestFailure String deriving (Show)

instance Exception TestException

assert :: Bool -> String -> IO ()
assert condition message = unless condition (throwIO $ TestFailure message)

main :: IO ()
main = do
    putStrLn "Running tests..."

    testToFilterMode

    testFilterPath

    putStrLn "All tests passed!"

testToFilterMode :: IO ()
testToFilterMode = do
    putStrLn "Testing toFilterMode..."

    assert (toFilterMode ["--only-files"] == Right FilesOnly) "toFilterMode --only-files"

    assert (toFilterMode ["--only-dirs"] == Right DirsOnly) "toFilterMode --only-dirs"

    assert (toFilterMode ["--only-executables"] == Right ExecutablesOnly) "toFilterMode --only-executables"

    assert (toFilterMode ["--only-symlinks"] == Right SymlinksOnly) "toFilterMode --only-symlinks"

    assert (isLeft $ toFilterMode []) "toFilterMode empty"

    assert (isLeft $ toFilterMode ["--unknown"]) "toFilterMode unknown"

    assert (isLeft $ toFilterMode ["--only-files", "--only-dirs"]) "toFilterMode multiple"

    putStrLn "toFilterMode tests passed."

testFilterPath :: IO ()
testFilterPath = withSystemTempDirectory "hs-fs-filter-test" $ \dir -> do
    putStrLn "Testing filterPath..."

    let file = dir </> "file.txt"

    let subdir = dir </> "subdir"

    let executableFile = dir </> "exec.sh"

    let symlinkToFile = dir </> "link.txt"

    let symlinkToDir = dir </> "link_dir"

    let nonexistentPath = dir </> "nonexistent"

    writeFile file "content"

    createDirectory subdir

    writeFile executableFile "#!/bin/bash\necho hello"

    -- Make executableFile executable

    setPermissions executableFile (setOwnerExecutable True (setOwnerReadable True (setOwnerWritable True emptyPermissions)))

    createSymbolicLink file symlinkToFile

    createSymbolicLink subdir symlinkToDir

    putStrLn "  FilesOnly tests..."

    filterPath FilesOnly file >>= \r -> assert (r == Just file) "FilesOnly, existing file"

    filterPath FilesOnly nonexistentPath >>= \r -> assert (isNothing r) "FilesOnly, nonexistent file"

    filterPath FilesOnly subdir >>= \r -> assert (isNothing r) "FilesOnly, directory"

    filterPath FilesOnly executableFile >>= \r -> assert (r == Just executableFile) "FilesOnly, executable file"

    filterPath FilesOnly symlinkToFile >>= \r -> assert (r == Just symlinkToFile) "FilesOnly, symlink to file (resolves to regular file)"

    putStrLn "  DirsOnly tests..."

    filterPath DirsOnly subdir >>= \r -> assert (r == Just subdir) "DirsOnly, existing directory"

    filterPath DirsOnly nonexistentPath >>= \r -> assert (isNothing r) "DirsOnly, nonexistent directory"

    filterPath DirsOnly file >>= \r -> assert (isNothing r) "DirsOnly, file"

    filterPath DirsOnly symlinkToDir >>= \r -> assert (r == Just symlinkToDir) "DirsOnly, symlink to dir (resolves to directory)"

    putStrLn "  ExecutablesOnly tests..."

    filterPath ExecutablesOnly executableFile >>= \r -> assert (r == Just executableFile) "ExecutablesOnly, executable file"

    filterPath ExecutablesOnly file >>= \r -> assert (isNothing r) "ExecutablesOnly, non-executable file"

    filterPath ExecutablesOnly subdir >>= \r -> assert (isNothing r) "ExecutablesOnly, directory"

    filterPath ExecutablesOnly nonexistentPath >>= \r -> assert (isNothing r) "ExecutablesOnly, nonexistent path"

    filterPath ExecutablesOnly symlinkToFile >>= \r -> assert (isNothing r) "ExecutablesOnly, symlink to executable file"

    putStrLn "  SymlinksOnly tests..."

    filterPath SymlinksOnly symlinkToFile >>= \r -> assert (r == Just symlinkToFile) "SymlinksOnly, symlink to file"

    filterPath SymlinksOnly symlinkToDir >>= \r -> assert (r == Just symlinkToDir) "SymlinksOnly, symlink to directory"

    filterPath SymlinksOnly file >>= \r -> assert (isNothing r) "SymlinksOnly, regular file"

    filterPath SymlinksOnly subdir >>= \r -> assert (isNothing r) "SymlinksOnly, regular directory"

    filterPath SymlinksOnly nonexistentPath >>= \r -> assert (isNothing r) "SymlinksOnly, nonexistent path"

    putStrLn "filterPath tests passed."

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
