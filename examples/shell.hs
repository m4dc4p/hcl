import Data.Map (Map)
import qualified Data.Map as Map
import System.Process (runProcess, waitForProcess)
import System.Environment (getEnvironment)
import Data.Char (toUpper)
import System.Directory (getCurrentDirectory, setCurrentDirectory, canonicalizePath, doesDirectoryExist)
import HCL

type Program = String
type Args = [String]
type Environment = Map.Map String String

-- The commands this shell is capable of.
data ShellCmd = Command Program Args Bool |
        Set |
        SetEnv String String |
        ChangeDir String 

-- Simple data structure holding our environment - current environment
-- variables and current working directory.
data ShellEnv = ShellEnv { currDir :: FilePath, currEnv :: Environment }
      
-- Execute a command in the shell.  
execShell :: ShellEnv -> ShellCmd -> IO ShellEnv
-- Execute a program
execShell env@(ShellEnv dir environment) (Command program args bkgrnd) =
    do
        proc <- runProcess program args (Just $ dir) (Just $ Map.toList environment) Nothing Nothing Nothing
        if bkgrnd
          then return env
          else do { result <- waitForProcess proc;  return env  }
-- Set an environment value
execShell (ShellEnv dir environment) (SetEnv name val) =
  return $ ShellEnv dir (Map.insert (map toUpper name) val environment)
-- Print the current environment
execShell env@(ShellEnv {currEnv = environment}) Set =
  do
    putStrLn $ unlines $ map (\(name,value) -> name ++ " = " ++ value) (Map.toList environment)
    return env
-- Print the current working directory
execShell env@(ShellEnv {currDir = dir}) (ChangeDir []) =
  do
    putStrLn dir
    return env
execShell env@(ShellEnv dir environment) (ChangeDir newDir) =
  do
    workingDir <-
      if head newDir == '\\' -- cheesy check for absolute directory
        then return newDir
        else
          if last dir == '\\'
            then canonicalizePath (dir ++ newDir)
            else canonicalizePath (dir ++ "\\" ++ newDir)
    -- On windows, drive letter is included here. But
    -- doesDirectoryExist doesn't like drive letters (*boggle*)
    valid <- doesDirectoryExist (stripDrive workingDir)
    if valid
      then return (ShellEnv workingDir environment)
      else return env
  where
    -- Cheesy way to pull drive letter off path if it's included
    stripDrive (x:':':rest) = rest
    stripDrive rest = rest
  
-- Implements the shell prompt, which gathers commands and
-- executes them. Takes a current environment and returns a modified environment.
shell :: ShellEnv -> Request ShellEnv
shell env =
  do
    cmd <- (prompt ((currDir env) ++ "> ") reqResp)
    let (command:args) = words cmd
    reqIO $ execShell env $
      case (map toUpper command) of
        "SETENV" -> SetEnv (head args) (unwords (filter (\str -> not ('=' `elem` str)) (tail args)))
        "SET" -> Set 
        "CD" -> ChangeDir (unwords args) 
        otherwise -> Command command (filter (\str -> not ('&' `elem` str)) args) (last cmd == '&')
        
main =
    runRequest $
      do
        env <- reqIO $ getEnvironment
        currDir <- reqIO $ getCurrentDirectory
        let initialEnv = ShellEnv currDir (Map.fromList (map (\(k, v) -> (map toUpper k, v)) env))
        reqIterate shell initialEnv
