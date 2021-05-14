module Main where
import Lib

main :: IO ()
main = do
  let ps = tax 72500
  print ps
  return ()
