module Main where

import System.IO
import System.Directory
import System.Environment
import System.Exit

import Logger
import Treader
import Message

convertLogs :: FilePath -> String -> IO ()
convertLogs path sid = do
  contents <- readFile path
  let messages = trillianToMessages contents
  let xml = sessionToXML (sid,messages)
  writeFile (sid ++ ".xml") heading
  appendFile (sid ++ ".xml") xml
  
  
usage :: IO ()
usage = do
  name <- getProgName
  putStr "Usage: "
  putStr name
  putStrLn " trillian_log.xml session_name"

main = do
  args <- getArgs
  if length args /= 2 then usage >> exitFailure
  else convertLogs (args !! 0) (args !! 1) >> exitSuccess