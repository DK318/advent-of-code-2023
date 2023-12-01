{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Foldable (asum)
import Data.List (isPrefixOf)

numMap :: [(String, Int)]
numMap =
  [ ("zero", 0)
  , ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ] ++ [(show x, x) | x <- [0..9]]

tryGetOnPrefix :: String -> Maybe Int
tryGetOnPrefix line = asum $ map check numMap
  where
    check :: (String, Int) -> Maybe Int
    check (str, n)
      | str `isPrefixOf` line = Just n
      | otherwise = Nothing

parseLine :: String -> [Int]
parseLine = reverse . go []
  where
    go :: [Int] -> String -> [Int]
    go acc [] = acc
    go acc line@(_ : xs) = case tryGetOnPrefix line of
      Nothing -> go acc xs
      Just n -> go (n : acc) xs

getRes :: [Int] -> Int
getRes = \case
  [] -> 0
  xs -> 10 * head xs + last xs

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"

  let res = input
        <&> getRes . parseLine
        & sum

  print res
