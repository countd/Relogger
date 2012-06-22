module Main
where

import Preader
import Logger

import System.IO
import System.Directory
import System.Environment
import System.Exit

import Logger
import Preader
import Message

fileName :: FilePath -> String
fileName = reverse . takeWhile (`notElem` ['/', '\\']) . reverse

convertLogs :: FilePath -> String -> IO ()
convertLogs path sid = do
  contents <- readFile path
  let messages = pidginToMessage (fileName path) contents
  let xml = sessionToXML (sid,messages)
  writeFile (sid ++ ".xml") heading
  appendFile (sid ++ ".xml") xml
  
  
usage :: IO ()
usage = do
  name <- getProgName
  putStr "Usage: "
  putStr name
  putStrLn " pidginLog.html session_name"

main = do
  args <- getArgs
  if length args /= 2 then usage >> exitFailure
  else convertLogs (args !! 0) (args !! 1) >> exitSuccess