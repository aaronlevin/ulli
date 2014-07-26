module Main where

import Data.Aeson (encode, decode, Result (Error, Success), Value)
import Control.Monad.Free (Free (Free))
import Data.Vector (Vector)
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
  push 1
  push 2
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 3
  push 2
  push 3
  set 100 100
  set 1 99999999


main :: IO ()
main = do
  putStrLn "hellp"
  putStrLn $ show (listInterpreter x)
  putStrLn $ stringInterpreter x
  let jString = encode $ toJsonInterpreter x
  putStrLn $ show $ jString
  let jList = decode jString :: Maybe (Vector Value)
  case jList of
    Nothing -> putStrLn "oops"
    Just jl -> case (jsonToAlg jl :: Result (Free (ListAlg Int) ())) of
      Error err -> putStrLn err
      Success xxx -> (putStrLn $ show $ listInterpreter xxx) >>
                     (putStrLn $ stringInterpreter xxx)
