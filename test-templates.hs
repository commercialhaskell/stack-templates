#!/usr/bin/env stack
{-
stack runghc
--resolver lts-5.8 --install-ghc
--no-terminal
--package mockery
--package getopt-generics
--
-Wall -Werror
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad
import           Data.List
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Test.Mockery.Directory
import           WithCli

excluded :: [String]
excluded =
  "ghcjs-old-base" : -- ghcjs takes too long to setup
  "ghcjs" : -- ghcjs takes too long to setup
  "quickcheck-test-framework" : -- test-suite fails (probably intentionally)
  "simple-hpack" : -- stack init fails on missing cabal file (fixed in stack on master)
  "tasty-discover" : -- contains a stack file, makes `stack new` choke
  "yesod-mongo" : -- needs a running db instance
  "yesod-mysql" : -- needs a running db instance
  "yesod-postgres-fay" : -- needs a running db instance
  "yesod-postgres" : -- needs a running db instance
  "yesod-sqlite" : -- missing CSFR cookie?
  []

isExcluded :: FilePath -> Bool
isExcluded file = dropExtension file `elem` excluded

main :: IO ()
main = withHsfiles $ \ hsfiles -> do
  forM_ hsfiles $ \ hsfile -> do
    logImportant ("testing " ++ takeBaseName hsfile)
    inTempDirectory $ do
      callCommand ("stack new test-project " ++ hsfile ++ " --no-terminal")
      setCurrentDirectory "test-project"
      callCommand "stack test --fast --no-terminal"

withHsfiles :: ([FilePath] -> IO ()) -> IO ()
withHsfiles action = withCli $ \ (args :: [FilePath]) -> do
  hsfiles <- case args of
    [] -> getHsfiles
    _ -> do
      mapM_ checkExists args
      return args
  currentDirectory <- canonicalizePath =<< getCurrentDirectory
  action $ map (currentDirectory </>) hsfiles

getHsfiles :: IO [FilePath]
getHsfiles =
  sort <$>
  filter (not . isExcluded) <$>
  filter ((== ".hsfiles") . takeExtension) <$>
  getDirectoryContents "."

checkExists :: FilePath -> IO ()
checkExists file = do
  exists <- doesFileExist file
  when (not exists) $
    die ("file not found: " ++ file)

logImportant :: String -> IO ()
logImportant message =
  hPutStrLn stderr $ unlines [line, message, line]
  where
    line = replicate (length message) '='
