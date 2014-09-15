module DiceOfDoom
where

import System.Random (newStdGen, randomRs)
import Data.List.Split (chunksOf )
import Data.Char (chr, ord)

data Player = Player Int
              deriving (Eq)

instance Show Player where
    show (Player i) = [chr (i + ord 'a')]

data Cell = Cell {
                player :: Player,
                dice :: Int
            } 

instance Show Cell where
    show c = show (player c) ++ "-" ++ show (dice c)

data Board = Board {
                size :: Int,
                maxDice :: Int,
                numPlayers :: Int,
                cells :: [Cell],
                conqueredDice :: Int
             }
             deriving (Show)

genBoard :: Int -> Int -> Int -> IO Board
genBoard boardSize nplayers ndice = do
    diceGen <- newStdGen
    playerGen <- newStdGen
    let players = map Player $ take numCells $ randomRs (0, nplayers - 1) playerGen
    let dice_values = take numCells $ randomRs (1, ndice) diceGen
    return Board { 
        size = boardSize,
        maxDice = ndice,
        numPlayers = nplayers,
        cells = [Cell {player = p, dice = n} | (p, n) <- zip players dice_values],
        conqueredDice = 0
    }
    where 
        numCells = boardSize * boardSize

drawBoard :: Board -> IO ()    
drawBoard b = putStrLn $ showBoard b

showBoard :: Board -> String
showBoard b = concatMap showRow $ zip rowNumbers rows 
    where 
        showRow (rowNo, rowCells) = indent rowNo ++ showCells rowCells
        indent lineno = concat $ replicate lineno "  "
        showCells cs = unwords (map show cs) ++ "\n"
        boardSize = size b
        rowNumbers = [boardSize, boardSize -1 .. 1]
        rows = chunksOf boardSize (cells b)

test2x2Board :: Board
test2x2Board = Board {
    size = 2,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 1, dice = 2}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 0, dice = 2}
        , Cell {player = Player 1, dice = 1}],
    conqueredDice = 0
}

test3x3Board :: Board
test3x3Board = Board {
    size = 3,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 0, dice = 1}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 1, dice = 1}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 0, dice = 3}],
    conqueredDice = 0
}

-- Find the positions of the neighbours of a player's position
neighbours :: Int -> Int -> [Int]
neighbours board_size pos = 
    [p | p <- possibleNeighbours, p >= 0 && p < (board_size * board_size)]
    where 
        up    = pos - board_size
        down  = pos + board_size
        possibleNeighbours = [up, down] 
            ++ (if pos `mod` board_size == 0
                  then    []
                  else    [up - 1, pos - 1])
            ++ (if (pos + 1) `mod` board_size == 0
                  then    []
                  else    [pos + 1, down + 1])

-- Get a list of board positions and cells
cellPositions :: Board -> [(Int, Cell)]
cellPositions b = zip [0..] (cells b)

-- Find all a player's cells on a board
playerPositions :: Board -> Player -> [(Int, Cell)]
playerPositions b p = filter (\(_, c) -> player c == p) (cellPositions b)

-- Can a cell attack another cell ?
canCellAttack :: Cell -> Cell -> Bool
canCellAttack cell1 cell2 = player cell1 /= player cell2 && dice cell1 > dice cell2

-- Same thing using board positions
canAttack :: Board -> Int -> Int -> Bool
canAttack b pos1 pos2 = canCellAttack cell1 cell2
    where 
        cell1 = cells b !! pos1
        cell2 = cells b !! pos2

-- List all possible attack moves for a given player on a board
attackMoves :: Board -> Player -> [(Int, Int)]
attackMoves b p = [(pos, neigh) | 
                        (pos, _) <- playerPositions b p,
                        neigh <- neighbours (size b) pos, 
                        canAttack b pos neigh]

-- Make the attack and return the new board
attack :: Board -> Player -> Int -> Int -> Board
attack board p src dest
    | canAttack board src dest  = 
        board {
            cells = [afterAttack (pos, c) | (pos, c) <- cellPositions board]
        }
    | otherwise = board
    where
        srcDice = dice (cells board!! src)
        afterAttack (pos, cell)
            -- Attacker cell is just left with 1 die
            | pos == src  = cell { dice = 1}                        
            -- Defender cell switches players and gets remaining dice
            | pos == dest = cell { dice = srcDice - 1, player = p}  
            -- Not involved in the attack
            | otherwise   = cell                                    

-- Reinforce a players cells with the number of dice conquered.
-- Add a die to each cell the player owns, moving left to right, top to bottom,
-- making sure that the maximum no of dice isn't exceeded.
reinforce :: Board -> Player -> Board
reinforce board p = board { 
                        cells = distributeDice (cells board) (conqueredDice board - 1),
                        conqueredDice = 0
                    }
    where
        distributeDice :: [Cell] -> Int -> [Cell]
        distributeDice [] _ = []  -- No cells remaining
        distributeDice cs 0 = cs  -- No dice remaining
        distributeDice (c:cs) remainingDice =
            if player c == p && dice c < maxDice board then
                -- Give this cell an extra dice
                c {dice = dice c + 1} : distributeDice cs (remainingDice  -1)
            else 
                -- Continue on
                c : distributeDice cs remainingDice


