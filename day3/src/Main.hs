{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Data.Vector (Vector, (!?))
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (group, find, nub)
import qualified Data.Vector as V
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as M

type Schematic = Vector (Vector Char)

getNumberPositions :: Vector Char -> [(Int, Int)]
getNumberPositions line = go 0 [] groups
  where
    groups :: [[Bool]]
    groups = line
      <&> isDigit
      & V.toList
      & group

    go :: Int -> [(Int, Int)] -> [[Bool]] -> [(Int, Int)]
    go _ acc [] = acc
    go curPos acc (x : xs)
      | and x = go (endPos + 1) ((curPos, endPos) : acc) xs
      | otherwise = go (endPos + 1) acc xs
      where
        endPos = curPos + length x - 1

getAllNumberPositions :: Schematic -> Map Int [(Int, Int)]
getAllNumberPositions sch = numberPositions
  where
    numberPositions :: Map Int [(Int, Int)]
    numberPositions = sch
      & V.toList
      & zip [0..]
      <&> second getNumberPositions
      & M.fromList

extractAllNumbers :: Schematic -> [(Int, Int, Int)] -> [Int]
extractAllNumbers sch numberPositions = flip mapMaybe numberPositions \(line, start, end) -> do
  lineVec <- sch !? line

  let len = end - start + 1
  let numVec = V.slice start len lineVec

  pure $ read $ V.toList numVec

getAdjacentNumbers :: Map Int [(Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
getAdjacentNumbers mp line col = nub $ mapMaybe (uncurry getNumber) adjacentCells
  where
    adjacentCells =
      [ (line, col - 1)
      , (line, col + 1)
      ] ++ [(line - 1, x) | x <- [(col - 1)..(col + 1)]]
        ++ [(line + 1, x) | x <- [(col - 1)..(col + 1)]]

    getNumber :: Int -> Int -> Maybe (Int, Int, Int)
    getNumber n m = do
      lst <- M.lookup n mp
      (start, end) <- find (\(start, end) -> start <= m && m <= end) lst
      pure (n, start, end)

getGearPower :: Schematic -> Map Int [(Int, Int)] -> Int -> Int -> Int
getGearPower sch mp line col
  | length adjacentNumbers == 2 = product $ extractAllNumbers sch adjacentNumbers
  | otherwise = 0
  where
    adjacentNumbers = getAdjacentNumbers mp line col

getPowers :: Schematic -> Map Int [(Int, Int)] -> [Int]
getPowers sch mp = map (uncurry $ getGearPower sch mp) gears
  where
    gears :: [(Int, Int)]
    gears = V.map (V.toList . V.findIndices (== '*')) sch
      & V.toList
      & zipWith (\x xs -> map (x,) xs) [0..]
      & concat

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"

  let sch = V.fromList (V.fromList <$> input)

  let numberPositions = getAllNumberPositions sch
  let gearPowers = getPowers sch numberPositions

  print $ sum gearPowers
