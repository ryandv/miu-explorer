module Main where

import Happstack.Server (nullConf, ok, port, simpleHTTP)
import System.Environment

main :: IO ()
main = do
  envPort <- getEnv "PORT"
  simpleHTTP (nullConf { port = read envPort }) $ ok "Hello, World!"
