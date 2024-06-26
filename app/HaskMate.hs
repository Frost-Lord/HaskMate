import Control.Concurrent (threadDelay)
import System.Directory (doesFileExist, getModificationTime, getCurrentDirectory)
import System.Process (terminateProcess, waitForProcess, callCommand, ProcessHandle)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, MVar)
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.FilePath (takeDirectory)
import System.Environment (getArgs)
import Commands (displayHelpData, displayVersionData, displayCommands, displayConfigData, displayLogData, displayClearData, displayCreditsData)
import Settings (Settings(..), loadSettings)
import Generate (generateConfig)

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

-- Decide which command to run based on the settings
runScriptOrCmd :: Maybe Settings -> FilePath -> IO ()
runScriptOrCmd settings path = do
  let rootPath = takeDirectory path
  case settings of
    Just s  -> do
      if not (null (cmd s))
        then do
          callCommand (cmd s)
        else case script s of
                "ghc" -> callCommand $ "stack ghc -- " ++ path
                "stack" -> callCommand $ "stack build && stack run " ++ rootPath
                "cabal" -> callCommand $ "cabal build && cabal run " ++ rootPath
                customScript -> callCommand customScript
    Nothing -> callCommand $ "stack ghc -- " ++ path

-- Monitor file changes
monitorScript :: Int -> FilePath -> UTCTime -> MVar (Maybe ProcessHandle) -> Maybe Settings -> IO ()
monitorScript delayTime path lastModified handleMVar settings = do
  let loop currentLastModified = do
        threadDelay delayTime
        currentModified <- getLastModified path
        if currentModified > currentLastModified
          then do
            putStrLn $ yellow ++ projectName ++ white ++ " Detected file modification. Rebuilding and running..."
            oldHandle <- tryTakeMVar handleMVar
            case oldHandle of
              Just (Just handle) -> do
                terminateProcess handle
                _ <- waitForProcess handle
                return ()
              _ -> return ()
            runScriptOrCmd settings path
            loop currentModified
          else loop currentLastModified
  loop lastModified

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> displayHelpData
    ["--h"] -> displayHelpData
    ["--generate"] -> generateConfig
    ["--gen"] -> generateConfig
    ["--version"] -> displayVersionData
    ["--v"] -> displayVersionData
    ["--commands"] -> displayCommands
    ["--config"] -> displayConfigData
    ["--log"] -> displayLogData
    ["--clear"] -> displayClearData
    ["--credits"] -> displayCreditsData
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
      let delayTime = maybe 1000000 id (delay =<< settings)

      let fullPath = currentDir ++ "/" ++ scriptPath
      putStrLn $ green ++ projectName ++ white ++ " Starting HaskMate v1.3.0..."
      putStrLn $ green ++ projectName ++ white ++ " Running script in directory: " ++ fullPath
      putStrLn $ green ++ projectName ++ white ++ " Watching for file modifications. Press " ++ red ++ "Ctrl+C" ++ white ++ " to exit."
      
      runScriptOrCmd settings fullPath
      lastModified <- getLastModified fullPath
      handleMVar <- newEmptyMVar
      monitorScript delayTime fullPath lastModified handleMVar settings
    [] -> putStrLn "Please provide a file to monitor as an argument." >> putStrLn "Example: HaskMate app/Main.hs"