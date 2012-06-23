module Main where

import Preader
import Logger

import System.IO
import System.Directory
import System.Environment
import System.Exit

import HtmlWriter
import Lreader
import Message

convertLogs :: FilePath -> String -> IO ()
convertLogs path new = do
  contents <- readFile path
  let sessions = rplogToSessions contents
  let html = sessionsToHtml sessions
  writeFile new html
  
  
usage :: IO ()
usage = do
  name <- getProgName
  putStr "Usage: "
  putStr name
  putStrLn " rplog.xml htmlName.html"

main = do
  args <- getArgs
  if length args /= 2 then usage >> exitFailure else convertLogs (args !! 0) (args !! 1) >> exitSuccess