module REPL where

import Parser
import System.Console.Haskeline
import Control.Monad.Trans

processStat :: String -> IO ()
processStat input = do
  case parseTopLevelStat_P input of
    Left err    -> print err
    Right stmts -> mapM_ print stmts

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
