{-# LANGUAGE DeriveGeneric #-}

module Loader (loadPuzzle)
where

import Graph
import Node
import Puzzle

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import qualified GHC.Generics as Generic

data RegionRecord = RegionRecord
  { id  :: String
  , color :: String
  , neighbors :: [String]
  } deriving (Generic.Generic, Show)

instance Aeson.FromJSON RegionRecord
instance Aeson.ToJSON RegionRecord

data PuzzleRecord = PuzzleRecord
  { name :: String
  , moves :: Int
  , regions :: [RegionRecord]
  } deriving (Generic.Generic, Show)

instance Aeson.FromJSON PuzzleRecord
instance Aeson.ToJSON PuzzleRecord

parsePuzzleRecord :: PuzzleRecord -> Puzzle
parsePuzzleRecord pr = let
  puzzleName = Loader.name pr
  puzzleMoves = Loader.moves pr
  puzzleGraph = addNodes newGraph (map parseRegionRecord (Loader.regions pr))
  in newPuzzle puzzleName puzzleMoves puzzleGraph

parseRegionRecord :: RegionRecord -> Node
parseRegionRecord rr = let
  regionId = Loader.id rr
  regionColor = Loader.color rr
  regionNeighbors = Loader.neighbors rr
  in updateAdjacentTags (newNode regionId regionColor) regionNeighbors

loadPuzzle :: FilePath -> IO Puzzle
loadPuzzle filepath = do
  fileContents <- B.readFile filepath
  let puzzle = case Aeson.eitherDecode fileContents of
               Left message -> error message
               Right puzzleRecord -> parsePuzzleRecord puzzleRecord
  return puzzle
