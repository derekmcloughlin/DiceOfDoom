module DiceOfDoom
where

import System.Random (newStdGen, randomRs)
import Data.List.Split (chunksOf )
import Data.Char (chr, ord)

data Player = Player Int

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
          Cell {player = Player 0, dice = 3}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 1, dice = 3}
        , Cell {player = Player 0, dice = 1}],
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
