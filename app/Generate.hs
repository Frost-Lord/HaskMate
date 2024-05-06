module Generate (generateConfig) where 

text :: String
text = "{\n \"delay\": 1000000,\n \"ignore\": [],\n \"script\": \"stack\",\n \"cmd\": \"\"\n}"

generateConfig :: IO ()
generateConfig = do
  writeFile "HaskMate.json" text
  putStrLn "Configuration file generated successfully!"