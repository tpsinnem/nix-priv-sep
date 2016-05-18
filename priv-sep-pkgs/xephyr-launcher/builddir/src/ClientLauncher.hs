module Main where

import Data.List
import Data.Maybe

import System.IO
import System.Process
import System.FilePath.Posix
import System.Directory
import System.Environment


-- TODO abstract out all the "runningOn" and "Xauthority" stuff.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (xephDir : program : args)
      | isAbsolute program && isAbsolute xephDir -> do

        xephyrRunning <- doesFileExist $ combine xephDir "runningOn"

        if xephyrRunning 
          then do
            displayNumString <- readFile $ combine xephDir "runningOn"
            oldEnv <- getEnvironment
            let newEnv = (filter 
                           (\(k,v) -> ( (not (k == "DISPLAY"   )) && 
                                        (not (k == "XAUTHORITY"))    ) )
                           oldEnv) ++
                         [("DISPLAY"   , displayNumString),
                          ("XAUTHORITY", combine xephDir "Xauthority")]
            createProcess ((proc program args) { env = Just newEnv })
            return ()
          else
            hPutStrLn stderr ("Xephyr not running. Will not run " ++ program ++ ".")

    _ -> do
      progname <- getProgName
      putStrLn ("Usage: " ++ progname ++ " xeph-status-dir program args")
