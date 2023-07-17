module Commands (displayHelpData, displayVersionData) where

-- ANSI escape sequences for color
red, white, yellow, green :: String
red = "\x1b[31m"
white = "\x1b[37m"
yellow = "\x1b[33m"
green = "\x1b[32m"

displayHelpData :: IO ()
displayHelpData = do
  let message = "Welcome to HaskMate!"
      boxWidth = length message + 4
      horizontalLine = red ++ replicate (boxWidth - 2) '\x2500'
      padding = replicate ((boxWidth - length message -1) `div` 2) ' '

  putStrLn $ red ++ "┌" ++ horizontalLine ++ "┐"
  putStrLn $ red ++ "│" ++ padding ++ green ++ message ++ red ++ padding ++ "│"
  putStrLn $ red ++ "└" ++ horizontalLine ++ "┘"
  putStrLn $ yellow ++ "Arguments:"
  putStrLn $ white ++ "  scriptPath  Path to the Haskell script you want to monitor"
  putStrLn $ green ++ "Example:"
  putStrLn $ white ++ "  haskmate app/Main.hs"

-- Function to display version data
displayVersionData :: IO ()
displayVersionData = do
  putStrLn $ green ++ "HaskMate v1.0.0" ++ white
