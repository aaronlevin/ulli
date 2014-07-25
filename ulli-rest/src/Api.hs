module Main where

import UlliRest.Types ( delete, get, insert, insertAt, listInterpreter, set )

x = do
  insert 1
  insert 2
  insert 3
  delete 2
  set 100 100


main :: IO ()
main = do
  putStrLn "hellp"
  putStrLn $ show (listInterpreter x)
