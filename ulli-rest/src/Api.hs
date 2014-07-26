module Main where

import Data.Aeson (encode, decode, Result (Error, Success))
import Control.Monad.Free (Free ())
import UlliRest.Types ( delete
                      --, insertAt
                      , jsonToAlg
                      , ListAlg
                      , listInterpreter
                      , push
                      , toJsonInterpreter
                      , stringInterpreter
                      , set )

x :: Free (ListAlg Int) ()
x = do
  delete 4
  push 1
  push 2
  push 4
  push 5
  push 6
  push 7
  push 8
  push 9
  push 10
  set 100 100
  set 1 99999999


main :: IO ()
main = do
  putStrLn $ show (listInterpreter x)
  putStrLn $ stringInterpreter x
  let jString = encode $ toJsonInterpreter x
  putStrLn $ show $ jString
  let jList = decode jString
  case jList of
    Nothing -> putStrLn "oops"
    Just jl -> case (jsonToAlg jl :: Result (Free (ListAlg Int) ())) of
      Error err -> putStrLn err
      Success xxx -> (putStrLn $ show $ listInterpreter xxx) >>
                     (putStrLn $ stringInterpreter xxx)
