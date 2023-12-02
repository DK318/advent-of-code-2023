{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.Text.IO as TIO
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Monad.State (State, modify, execState)

data Cube
  = Red
  | Green
  | Blue

type CubeSet = [(Int, Cube)]

data Game = Game
  { gId :: Int
  , gSets :: [CubeSet]
  }

parseCube :: Text -> (Int, Cube)
parseCube (T.split isSpace -> [read @Int . T.unpack -> n, color]) =
  let
    cube =
      case T.unpack color of
        "red" -> Red
        "green" -> Green
        "blue" -> Blue
        _ -> error "Bad color"
  in (n, cube)
parseCube _ = error "Incorrect string"

parseCubeSet :: Text -> CubeSet
parseCubeSet line = parseCube <$> T.splitOn ", " line

parseGame :: Text -> Game
parseGame line = Game{..}
  where
    [game, sets] = T.splitOn ": " line
    [_, read @Int . T.unpack -> gId] = T.split isSpace game
    gSets = parseCubeSet <$> T.splitOn "; " sets

data CubesState = CubesState
  { csRedCnt :: Int
  , csGreenCnt :: Int
  , csBlueCnt :: Int
  }

checkGameS :: Game -> State CubesState ()
checkGameS Game{..} = mapM_ checkCubeSet gSets
  where
    checkCubeSet :: CubeSet -> State CubesState ()
    checkCubeSet = mapM_ checkCube
      where
        checkCube :: (Int, Cube) -> State CubesState ()
        checkCube (cnt, cube) =
          case cube of
            Red -> modify \cs -> cs { csRedCnt = max cnt (csRedCnt cs) }
            Green -> modify \cs -> cs { csGreenCnt = max cnt (csGreenCnt cs) }
            Blue -> modify \cs -> cs { csBlueCnt = max cnt (csBlueCnt cs) }

checkGame :: Game -> CubesState
checkGame game = execState (checkGameS game) (CubesState 0 0 0)

statePower :: CubesState -> Int
statePower CubesState{..} = csRedCnt * csGreenCnt * csBlueCnt

main :: IO ()
main = do
  input <- T.lines <$> TIO.readFile "input.txt"

  let res = input
        <&> statePower . checkGame . parseGame
        & sum

  print res
