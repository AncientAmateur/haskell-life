-- Conway's Gane of Life
-- Rules:
-- 1. Any live cell with fewer than two live neighbours dies (referred to as underpopulation).
-- 2. Any live cell with more than three live neighbours dies (referred to as overpopulation or overcrowding).
-- 3. Any live cell with two or three live neighbours lives, unchanged, to the next generation.
-- 4. Any dead cell with exactly three live neighbours will come to life.

import Data.List.Split
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Control.Concurrent
import Text.Read

type Cell     = (Int, Int)
type CellList = [Cell]
type CellSet  = Set.Set Cell

data Board = Board {width :: Int, height :: Int, population :: CellSet}

cells :: Board -> CellList
cells board = [(x,y) | x <- [0..(height board - 1)], y <- [0..(width board - 1)]]

neighbors :: Cell -> CellSet
neighbors (x,y) = Set.fromList [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

liveCell :: CellSet -> Cell -> Bool
liveCell population cell = Set.member cell population

liveNeighbors :: CellSet -> Cell -> CellSet
liveNeighbors population cell = Set.intersection population $ neighbors cell

willLive :: CellSet -> Cell -> Bool
willLive population cell
    | alive     = Set.size livingNeighbors `elem` [2,3]
    | not alive = Set.size livingNeighbors == 3
    where alive = liveCell population cell
          livingNeighbors = liveNeighbors population cell

nextGeneration :: Board -> Board
nextGeneration board =
  Board {width = width board, height = height board, population = Set.fromList newPopulation}
    where newPopulation = filter (willLive (population board)) $ cells board

generateRandomBoard :: Int -> Int -> Int -> IO Board
generateRandomBoard width height liveCellCount = do
  list1 <- liftM (take liveCellCount . randomRs (0, height - 1)) newStdGen
  list2 <- liftM (take liveCellCount . randomRs (0, width - 1)) newStdGen
  return Board {width = width, height = height, population = Set.fromList $ zip list1 list2 }


instance Show Board where
  show board = unlines $ chunksOf (width board) $ map (printCell $ population board) $ cells board

clearBoard = putStr "\ESC[H\ESC[2J"

printCell :: CellSet -> Cell -> Char
printCell population cell = if liveCell population cell then '*' else '.'

printBoard :: Board -> IO ()
printBoard board = do
  clearBoard
  putStrLn $ show board
  threadDelay 33333 -- gives us roughly 30 frames per second


getLiveCellCount :: IO Int
getLiveCellCount = do
  liveCellInput <- getLine
  let liveCellCount = readMaybe liveCellInput :: Maybe Int

  case liveCellCount of
    Just n  -> return n
    Nothing -> putStrLn "Please enter a number for cell count" >> getLiveCellCount


getGenerationCount :: IO Int
getGenerationCount = do
  generationInput <- getLine
  let generations = readMaybe generationInput :: Maybe Int

  case generations of
    Just n  -> return n
    Nothing -> putStrLn "Please enter a number for generations" >> getGenerationCount


boardWidth  = 80
boardHeight = 20

main = do
  putStrLn "Welcome to 'The Game of Life'"
  putStrLn "Enter the maximum amount of live cells to start with:"
  liveCellCount <- getLiveCellCount

  putStrLn "Enter the number of generations:"
  generations <- getGenerationCount

  board <- generateRandomBoard boardWidth boardHeight liveCellCount

  mapM_ printBoard . take generations $ iterate nextGeneration board
