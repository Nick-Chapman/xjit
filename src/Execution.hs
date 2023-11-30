module Execution
  ( Execution(..)
  , runX
  ) where

import Data.Char (chr)

data Execution where
  Xdebug :: String -> Execution -> Execution
  Xhalt :: Execution
  Xget :: (Char -> Execution) -> Execution
  Xput :: Char -> Execution -> Execution

runX :: Execution -> String -> IO ()
runX x input =
  case x of
    Xdebug s x -> do
      let _ = putStrLn ("debug: " ++ s)
      runX x input
    Xhalt -> pure ()
    Xget f ->
      case input of
        [] -> runX (f (chr 0)) []
        char1:input -> runX (f char1) input
    Xput char x -> do
      print ("XPut",char)
      runX x input
