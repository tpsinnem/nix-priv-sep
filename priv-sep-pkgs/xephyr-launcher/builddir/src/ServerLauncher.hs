{-# Language DeriveGeneric, PackageImports #-}

module Main where

import GHC.Generics (Generic)

import Control.Monad
import Data.List
import Data.Maybe
import Data.Binary

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import System.IO  as SIO
import System.Process       hiding (createPipe)
import System.Timeout
import GHC.IO.Handle               (hDuplicate, hDuplicateTo)
import System.Posix.Types
import System.Posix.IO
import "unix-bytestring" 
  System.Posix.IO.ByteString.Lazy  (fdWrites)
import qualified
  Data.ByteString.Lazy  as BS      (hGetContents)
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Files
import System.FilePath.Posix
import System.Directory
import System.Environment
import System.Exit


-- TODO abstract out all the "runningOn" and "Xauthority" stuff.
-- TODO usefulize error messages, such as they even are.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [xephDir, xephyr, xauth, mcookie] 
      | isAbsolute xephyr && isAbsolute xauth && 
        isAbsolute mcookie && isAbsolute xephDir -> do

        alreadyRunning <- doesFileExist $ combine xephDir "runningOn"

        unless alreadyRunning $ do
          (readp,writep) <- createPipe

          forkProcess $ manageXephyr xephDir xephyr xauth mcookie writep

          xephmsg <- decode <$> (BS.hGetContents =<< fdToHandle readp) :: IO XephyrMsg

          case xephmsg of
            XephyrOkMsg    -> exitSuccess
            XephyrErrorMsg -> exitFailure

    _ -> do
      progname <- getProgName
      putStrLn ("Usage: " ++ progname ++ " xeph-status-dir xephyr xauth mcookie")
      putStrLn ("First argument is the directory to use for Xephyr status data. " ++
                "Next three arguments are program locations. " ++
                "All arguments must be absolute path names.")
      exitFailure

manageXephyr :: String -> String -> String -> String -> Fd -> IO ()
manageXephyr xephDir xephyr xauth mcookie writep = do
  activeNums <- getActiveDisplayNums
  status <- tryRunXephyr xephyr xephDir ((maximum activeNums) + 1)

  case status of
    XephyrError -> do
      fdWrites writep $ encode XephyrErrorMsg
      closeFd writep
      exitFailure
    XephyrOk dnum phandle -> do

      cookie <- readProcess mcookie [] ""
      callProcess xauth $ xauthArgs xephDir cookie dnum
      setFileMode
        (combine xephDir "Xauthority")
        (ownerReadMode  `unionFileModes`
         ownerWriteMode `unionFileModes`
         groupReadMode)
      writeFile (combine xephDir "runningOn") (":" ++ (show dnum))

      fdWrites writep $ encode XephyrOkMsg
      closeFd writep

      installHandler sigINT (Catch (terminateProcess phandle)) Nothing
      installHandler sigTERM (Catch (terminateProcess phandle)) Nothing

      waitForProcess phandle

      removeFile $ combine xephDir "Xauthority"
      removeFile $ combine xephDir "runningOn"

      exitSuccess -- TODO THINK FIXME? handle errors from xauth etc

(%) :: Functor f => f a -> (a -> b) -> f b
(%) = flip fmap
infixl 4 %

getActiveDisplayNums :: IO [Int]
getActiveDisplayNums = (listDirectory "/tmp") % (\fnames -> mapMaybe parseDisplayNum fnames)

tryRunXephyr :: String -> String -> Int -> IO XephyrStatus
tryRunXephyr xephyr xephDir num =
  if (num > maxDisplayNum)
    then 
      return XephyrError
    else do 
      (inh, outh, errh, phandle) <- runInteractiveProcess xephyr (xephArgs xephDir num) Nothing Nothing

      hDuplicateTo stdout outh
      errd <- hDuplicate errh
      hDuplicateTo stdout errh

      status <- checkTrialStatus errd
      hClose errd

      let handles = [inh, outh, errh]
      case status of
        TrialOk            -> do
          nullify handles
          return $ XephyrOk num phandle
        AlreadyActiveError -> do
          closeHandles handles
          tryRunXephyr xephyr xephDir (num + 1)
        OtherError         -> do
          closeHandles handles
          return XephyrError

checkTrialStatus :: Handle -> IO TrialStatus
checkTrialStatus errHandle = do
  err <- SIO.hGetContents errHandle
  checkActive <- timeout halfSec $ do
    if alreadyActive `isSubsequenceOf` err
      then return True
      else return False -- I presume normally we'll se a timeout instead of this.
  checkOther <- timeout halfSec $ do
    case err of
      [] -> return False
      _  -> return True
  case (checkActive, checkOther) of
    ((Just True), _) -> return AlreadyActiveError
    (_, (Just True)) -> return OtherError
    _                -> return TrialOk

nullify :: [Handle] -> IO ()
nullify handles = do
  null <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  forM_ handles $ handleToFd >=> (\h -> closeFd h >> dupTo null h)
  closeFd null

closeHandles :: [Handle] -> IO ()
closeHandles handles = forM_ handles hClose

xephArgs :: String -> Int -> [String]
xephArgs xephDir num =
  [":"       ++  (show num),
   "-auth",      combine xephDir "Xauthority",
   "-screen",    "1238x966",
   "-resizeable"             ]

xauthArgs :: String -> String -> Int -> [String]
xauthArgs xephDir cookie displayNum = 
  ["-f", combine xephDir "Xauthority",
   "add", (":" ++ (show displayNum)), ".", cookie]

parseDisplayNum :: String -> Maybe Int
parseDisplayNum s = case (parse displayNumParser "" s) of
                      Left  err -> Nothing
                      Right num -> Just num

displayNumParser :: GenParser Char st Int
displayNumParser = do
  string ".X"
  num <- decimal
  string "-lock"
  return num

-- Based on 'well known port numbers' on wikipedia, but this might have to
-- be eventually disregarded in favor of some bigger scheme.
maxDisplayNum :: Int
maxDisplayNum = 63

alreadyActive :: String
alreadyActive = "(EE) Server is already active for display"

halfSec :: Int
halfSec = 500000

data XephyrMsg = XephyrOkMsg | XephyrErrorMsg
  deriving (Generic)
instance Binary XephyrMsg

data TrialStatus  = TrialOk | AlreadyActiveError | OtherError
data XephyrStatus = XephyrOk Int ProcessHandle | XephyrError
