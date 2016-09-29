#!/usr/bin/env stack
{-
stack runghc
--resolver lts-5.11 --install-ghc
--no-terminal
--package mockery
--package getopt-generics
--package text
--package unordered-containers
--package yaml
--
-Wall -Werror
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Arrow       ((***))
import           Control.Monad       (forM_, unless)
import           Data.HashMap.Strict (keys)
import           Data.List
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Yaml           (ParseException, Object)
import qualified Data.Yaml           as Yaml
import           System.Directory
import           System.Exit         (die)
import           System.FilePath     ((</>), dropExtension, takeExtension, takeBaseName)
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
main = do 
  
  logImportant $ "Verifying " <> templateInfoFile
  verified <- verifyInfo
  case verified of
    Left err -> die err
    _ -> return () 

  withHsfiles $ \ hsfiles ->
    forM_ hsfiles $ \ hsfile -> do
      logImportant ("testing " ++ takeBaseName hsfile)
      inTempDirectory $ do
        callCommand ("stack new test-project " ++ hsfile ++ " --no-terminal")
        setCurrentDirectory "test-project"
        callCommand "stack test --fast --no-terminal --install-ghc"

withHsfiles :: ([FilePath] -> IO ()) -> IO ()
withHsfiles action = withCli $ \ (args :: [FilePath]) -> do
  hsfiles <- case args of
    [] -> fmap (filter $ not . isExcluded) getHsfiles
    _ -> do
      mapM_ checkExists args
      return args
  currentDirectory <- canonicalizePath =<< getCurrentDirectory
  action $ map (currentDirectory </>) hsfiles

verifyInfo :: IO (Either String ())
verifyInfo = do
  checkExists templateInfoFile
  decoded <- Yaml.decodeFileEither templateInfoFile :: IO (Either ParseException Object)
  case decoded of
    Left ex -> return . Left $ "Invalid " <> templateInfoFile <> " file. " <> show ex
    Right o -> do
      templates <- getHsfiles
      let info   = map T.unpack (keys o)
          check  = uniqueElems (map takeBaseName templates) info
          output = notEnough *** tooMuch $ check
      case check of
        (Nothing, Nothing) -> return $ Right ()
        _                  -> return $ Left $ uncurry (<>) output 
  where
    formatOutput header items = 
      fromMaybe "" $ unlines . (header :) . map (" - " <>) <$> items    
    notEnough = formatOutput $ "Add the following templates to " <> templateInfoFile <> ":"
    tooMuch   = formatOutput $ "Remove the following templates from " <> templateInfoFile <> ":"

uniqueElems :: Eq a => [a] -> [a] -> (Maybe [a], Maybe [a])
uniqueElems = bothWays unique
  where 
    bothWays f xs ys = (f xs ys, f ys xs)
    unique xs ys = 
      case xs \\ ys of
        []   -> Nothing
        diff -> Just diff

templateInfoFile :: String
templateInfoFile = "template-info.yaml"

getHsfiles :: IO [FilePath]
getHsfiles =
  sort . filter ((== ".hsfiles") . takeExtension) <$>
  getDirectoryContents "."

checkExists :: FilePath -> IO ()
checkExists file = do
  exists <- doesFileExist file
  unless exists $
    die ("file not found: " ++ file)

logImportant :: String -> IO ()
logImportant message =
  hPutStrLn stderr $ unlines [line, message, line]
  where
    line = replicate (length message) '='
