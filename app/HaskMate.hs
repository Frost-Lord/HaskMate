import Control.Concurrent (threadDelay)
import System.Directory (doesFileExist, getModificationTime, getCurrentDirectory)
import System.Process (createProcess, proc, terminateProcess, waitForProcess, callCommand, ProcessHandle)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar, MVar)
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.Environment (getArgs)
import Commands (displayHelpData, displayVersionData)
import Settings (Settings(..), loadSettings)

-- ANSI escape sequences for color
red, white, yellow, green :: String
red = "\ESC[31m"
white = "\ESC[37m"
yellow = "\ESC[33m"
green = "\ESC[32m"

-- Project Name
projectName :: String
projectName = "[HaskMate]"

-- Get the last modification time of a file
getLastModified :: FilePath -> IO UTCTime
getLastModified path = do
  exists <- doesFileExist path
  if exists
    then getModificationTime path
    else getCurrentTime

-- Monitor the script for changes and rerun it
monitorScript :: Int -> FilePath -> UTCTime -> MVar (Maybe ProcessHandle) -> IO ()
monitorScript delayTime path lastModified handleMVar = do
  currentModified <- getLastModified path
  if currentModified > lastModified
    then do
      putStrLn $ yellow ++ projectName ++ white ++ " Detected file modification. Rebuilding and running..."
      let exePath = take (length path - 3) path
      callCommand $ "stack ghc -- " ++ path
      oldHandle <- tryTakeMVar handleMVar
      case oldHandle of
        Just (Just handle) -> do
          terminateProcess handle
          _ <- waitForProcess handle
          return ()
        _ -> return ()
      (_, _, _, newHandle) <- createProcess (proc exePath [])
      putMVar handleMVar (Just newHandle)
      monitorScript delayTime path currentModified handleMVar
    else do
      threadDelay delayTime
      monitorScript delayTime path lastModified handleMVar

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> displayHelpData
    ["--h"] -> displayHelpData
    ["--version"] -> displayVersionData
    ["--v"] -> displayVersionData
    (scriptPath:_) -> do
      currentDir <- getCurrentDirectory
      let jsonPath = currentDir ++ "/haskmate.json"
      jsonExists <- doesFileExist jsonPath
      settings <- if jsonExists
                    then do
                      putStrLn $ green ++ projectName ++ white ++ " Loaded settings from HaskMate.json"
                      loadSettings jsonPath
                    else do 
                    putStrLn (yellow ++ projectName ++ white ++ " No HaskMate.json file found. Using default settings.")
                    return Nothing

      let delayTime = maybe 1000000 id (delay =<< settings) -- use default if not found in settings

      let fullPath = currentDir ++ "/" ++ scriptPath
      putStrLn $ green ++ projectName ++ white ++ " Starting HaskMate v1.0.0..."
      putStrLn $ green ++ projectName ++ white ++ " Running script in directory: " ++ fullPath
      putStrLn $ green ++ projectName ++ white ++ " Watching for file modifications. Press " ++ red ++ "Ctrl+C" ++ white ++ " to exit."
      lastModified <- getLastModified fullPath
      handleMVar <- newEmptyMVar
      monitorScript delayTime fullPath lastModified handleMVar
    [] -> putStrLn "Please provide a file to monitor as an argument." >> putStrLn "Example: HaskMate app/Main.hs"