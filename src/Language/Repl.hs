{-# LANGUAGE LambdaCase #-}

module Language.Repl (
  repl,
) where

import Control.Category
import Control.Monad.IO.Class
import Data.Text
import System.Console.Haskeline
import Language.Parser
import Language.Pretty

process :: Text -> IO ()
process = parses [] >>> \case
  Left e -> print e
  Right x -> do
    pprint x
    print x

loop :: InputT IO ()
loop = getInputLine "> " >>= \case
  Nothing -> return ()
  Just input -> liftIO (process $ pack input) >> loop

repl :: IO ()
repl = runInputT defaultSettings loop
