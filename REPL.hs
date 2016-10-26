module REPL where

import Parser
import Syntax
import System.Console.Haskeline
import Control.Monad.Trans
import System.IO

processStat :: String -> IO ()
processStat input = do
  case parseTopLevelStat_P input of
    Left err   -> print err
    Right stmt -> mapM_ print (viewStmt stmt)

-- Unfold and view the statment in a human friendly format
viewStmt :: Stat -> [Stat]
viewStmt stmt = deconstr stmt []
  where
    deconstr (StatTopLevel st1 st2) acc = deconstr st2 (st1:acc)
    deconstr st acc                     = st:acc

-- View functions in a human friendly format
viewFunctions :: [Function] -> IO ()
viewFunctions []     = return ()
viewFunctions (f:fs) = (deconstr f) >> (viewFunctions fs)
  where
    deconstr (Function t i ps st) = do
      putStrLn "\n"
      putStrLn ("Function: " ++ (show t) ++ " " ++ (show i) ++ " " ++ (show ps) ++ "\n")
      mapM_ print (viewStmt st)

-- TODO: Process top level program
processProgram :: String -> IO ()
processProgram input = do
  case parseTopLevelProgram_P input of
    Left err              -> print err
    Right (Program fs st) -> viewFunctions fs >> mapM_ print (viewStmt st)

readInFile :: String -> IO ()
readInFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  processProgram contents
  hClose handle

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing    -> liftIO (putStrLn "Exiting interactive session...") >> return ()
        -- We use 'liftIO' to hide underlying code
        Just input -> liftIO (processProgram input) >> loop
