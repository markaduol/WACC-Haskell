module REPL where

import Parser
import Syntax
import System.Console.Haskeline
import Control.Monad.Trans

processStat :: String -> IO ()
processStat input = do
  case parseTopLevelStat_P input of
    Left err   -> print err
    Right stmt -> mapM_ print (viewStmts stmt)
    where
      viewStmts :: Stat -> [Stat]
      viewStmts stmt = deconstr stmt []
      deconstr (StatTopLevel st1 st2) acc = st1:(deconstr st2 acc)
      deconstr st acc = acc ++ [st]

-- TODO: Process top level program
processProgram :: String -> IO ()
processProgram input = do
  case parseTopLevelProgram_P input of
    Left err -> print err
    Right prog -> return ()

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing    -> liftIO (putStrLn "Exiting interactive session...") >> return ()
        -- We use 'liftIO' to hide underlying code
        Just input -> liftIO (processStat input) >> loop
