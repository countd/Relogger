module Main where

-- a test module for Lreader
-- it's supposed to copy rplogs

import Preader
import Logger

import System.IO
import System.Directory
import System.Environment
import System.Exit

import Logger
import Lreader
import Message

convertLogs :: FilePath -> String -> IO ()
convertLogs path sid = do
  contents <- readFile path
  let sessions = rplogToSessions contents
  let xml = sessionsToXML sessions
  writeFile (sid ++ ".xml") heading
  appendFile (sid ++ ".xml") xml
  
  
usage :: IO ()
usage = do
  name <- getProgName
  putStr "Usage: "
  putStr name
  putStrLn " rplog.xml new-basename"

main = do
  args <- getArgs
  if length args /= 2 then usage >> exitFailure
  else convertLogs (args !! 0) (args !! 1) >> exitSuccess