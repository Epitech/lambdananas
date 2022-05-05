module Main where

main :: IO ()
main = print $ fun 1

fun :: Int -> Maybe Bool
fun x
  | x == 0 = Just False
  | x == 1 = Just True
  | otherwise = Nothing
