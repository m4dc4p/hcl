import HCL
import System.Directory

main =
  do
    dir <- getCurrentDirectory
    browseDir dir []
    
browseDir :: FilePath -> [FilePath] -> IO ()
browseDir currDir history =
  do
    putStrLn $ "You are in " ++ currDir
    putStrLn $ "You have visited: " ++ foldr (\acc dir -> acc ++ ", " ++ dir) "" history
    fileChoices <- getDirectoryContents currDir
    let makeDirChoices = map (\d -> (d, putStrLn ("You selected " ++ d) >> return ())) fileChoices
    choice <- runRequest $ promptChoices makeDirChoices reqInt
    maybe (return ()) (\v -> do { v; return () }) choice 
    return ()