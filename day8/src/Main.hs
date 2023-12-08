{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
module Main (main) where

import Data.Map (Map)
import Control.Lens
import Control.Monad.State (State, evalState)
import Data.Maybe (fromJust)
import qualified Data.Map as M

data TravelState = TravelState
  { tsDesertMap :: Map String (String, String)
  , tsRoute :: String
  , tsTotalSteps :: Int
  , tsCurrentNode :: String
  } deriving stock (Show)

makeLensesWith abbreviatedFields ''TravelState

process :: State TravelState Int
process = do
  curNode <- use currentNode
  if last curNode == 'Z'
  then use totalSteps
  else do
    curRoute <- use route

    let dir : xs = curRoute
    route .= xs

    (onLeft, onRight) <- fromJust <$> use (desertMap . at curNode)
    totalSteps += 1

    case dir of
      'L' -> currentNode .= onLeft
      'R' -> currentNode .= onRight
      _ -> error "Wrong direction"

    process

processNetwork :: [String] -> Map String (String, String)
processNetwork = foldl processNode M.empty
  where
    processNode :: Map String (String, String) -> String -> Map String (String, String)
    processNode acc (words -> [fromNode, _, init . tail -> onLeft, init -> onRight]) =
      M.insert fromNode (onLeft, onRight) acc
    processNode acc _ = acc

main :: IO ()
main = do
  instr : nodes <- filter (/= "") . lines <$> readFile "input.txt"

  let network = processNetwork nodes
  let startNodes = network
        & M.keys
        & filter ((== 'A') . last)

  let initStates = startNodes
        <&> TravelState network (concat $ repeat instr) 0

  let results = evalState process <$> initStates

  print $ foldl1 lcm results
