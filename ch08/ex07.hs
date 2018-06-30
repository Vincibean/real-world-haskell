-- Modify the type signature of namesMatching so that it encodes the possibility of a bad pattern, 
-- and make it use your rewritten globToRegex function.
-- 
-- You may find the amount of work involved to be surprisingly large. 
-- Don’t worry; we will introduce more concise and sophisticated ways of dealing with errors in later chapters.

import System.Directory (
        doesDirectoryExist,
        doesFileExist,
        getCurrentDirectory,
        getDirectoryContents)
import System.FilePath (
        dropTrailingPathSeparator,
        splitFileName,
        (</>),
        isPathSeparator)
import Control.Exception (handle)
import Control.Monad (forM, filterM)

import Ex06

-- |Finds directories and files that match the given glob.
namesMatching :: String -> IO (Either GlobError [String])
namesMatching pattern
    | not (isPattern pattern) = do exists <- doesNameExist pattern
                                   return  $ if exists then (Right [pattern]) else (Right [])
    | otherwise               = do case splitFileName pattern of
                                    ("",      baseName) -> do curDir <- getCurrentDirectory
                                                              listMatches curDir baseName
                                    (dirName, baseName) -> do dirs <- if isPattern dirName
                                                                      then namesMatching (dropTrailingPathSeparator dirName)
                                                                      else return (Right [dirName])
                                                              let listDir = if isPattern baseName
                                                                            then listMatches
                                                                            else listPlain
                                                              let listDirFromBaseName = \dir -> listDir dir baseName
                                                              let recConcatDir = \dir -> do baseNames <- listDirFromBaseName dir 
                                                                                            let concatDir = \n -> map (dir </>) n
                                                                                            return $ fmap concatDir baseNames 
                                                              pathNames <- recConcatDirsIO dirs recConcatDir  
                                                              return $ fmap concat pathNames


-- |Helper
recConcatDirsIO :: Either GlobError [FilePath] -> (String -> IO (Either GlobError [FilePath])) -> IO (Either GlobError [[FilePath]])
recConcatDirsIO (Left err) _ = return $ Left err
recConcatDirsIO (Right paths) f = do es <- sequence $ map f paths
                                     return $ sequence es

-- |Helper
--   Does it look like a glob?
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

-- |Helper
--  Combines both doesFileExist and doesDirectoryExist.
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do fileExists <- doesFileExist name
                        if fileExists
                        then return True
                        else doesDirectoryExist name

-- |Internal
--  Finds a list of all files matching the given glob pattern in a directory.
listMatches :: FilePath -> String -> IO (Either GlobError [FilePath])
listMatches dirName pattern = do dirName' <- if null dirName
                                             then getCurrentDirectory
                                             else return dirName
                                 names <- getDirectoryContents dirName'
                                 let names' = if isHidden pattern
                                              then filter isHidden names
                                              else filter (not . isHidden) names
                                 return $ filterM (`matchesGlob`pattern) names'

-- |Helper
--  Starts with a dot it must be hidden.
isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

-- |Internal
--  Creates a singleton list or empty list depending on if the name exists.
listPlain :: FilePath -> String -> IO (Either GlobError [FilePath])
listPlain dirName baseName = do exists <- if null baseName
                                          then doesDirectoryExist dirName
                                          else doesNameExist (dirName </> baseName)
                                return $ if exists then Right [baseName] else Right []

