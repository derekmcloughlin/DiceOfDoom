module DiceOfDoom
where

import System.Random (newStdGen, randomRs)
import Data.List.Split (chunksOf)
import Data.List 
import Data.Char (chr, ord)
import Data.Tree
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad.State
import Text.Printf
import System.Process
import System.IO
import System.Exit

data Player = Player Int
              deriving (Eq, Ord)

instance Show Player where
    show (Player i) = [chr (i + ord 'A')]

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

instance Show Board where
    show b = show (cells b) ++ ", reinforcements : " ++ show (conqueredDice b)

data Move = Attack Int Int |
            Pass
            deriving (Show, Eq)

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

-- Draw a board using a hex-like pattern
drawBoard :: Board -> IO ()    
drawBoard b = putStr $ showBoard b

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

test2x2BoardB :: Board
test2x2BoardB = Board {
    size = 2,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 0, dice = 1}
        , Cell {player = Player 1, dice = 3}
        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 1, dice = 2}],
    conqueredDice = 0
}

test2x2BoardC :: Board
test2x2BoardC = Board {
    size = 2,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 0, dice = 3}
        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 1, dice = 3}],
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
attackMoves :: Board -> Player -> [Move]
attackMoves b p = [Attack pos neigh | 
                        (pos, _) <- playerPositions b p,
                        neigh <- neighbours (size b) pos, 
                        canAttack b pos neigh]

-- Make a move and return the new board
makeAMove :: Board -> Player -> Move -> Board
makeAMove board _ Pass = board
makeAMove board p (Attack src dest)
    | canAttack board src dest = 
        board {
            cells = [afterAttack (pos, c) | (pos, c) <- cellPositions board],
            conqueredDice = conqueredDice board + destDice
        }
    | otherwise = 
        board
    where
        srcDice = dice (cells board!! src)
        destDice = dice (cells board!! dest)
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

nextPlayer :: Board -> Player -> Player
nextPlayer board (Player n) = Player ((n + 1) `mod` numPlayers board)

data GameState = GameState {
                    currentPlayer :: Player,
                    moveMade :: Move,
                    currentBoard :: Board
                 }

instance Show GameState where
    show g = "Player: " ++ show (currentPlayer g) ++
             " Board: " ++ show (cells $ currentBoard g) ++ 
             " Move: " ++ show (moveMade g) ++
             " Conquered: " ++ show (conqueredDice $ currentBoard g) ++
             " Winners: " ++ show (winners $ currentBoard g)


-- The initial tree with the starting position of the game
sampleGameTree :: Tree GameState
sampleGameTree = Node GameState { 
    currentPlayer = Player 0, 
    moveMade = Pass, 
    currentBoard = test2x2Board
} []

sampleGameTreeNext :: Tree GameState
sampleGameTreeNext  = Node GameState { 
    currentPlayer = Player 0, 
    moveMade = Pass, 
    currentBoard = test2x2Board
} [Node GameState {
        currentPlayer = Player 0,
        moveMade = Attack 2 3,
        currentBoard = makeAMove test2x2Board (Player 0) (Attack 2 3)
        } [] ]


gameTree :: Board -> Player -> Move -> Bool -> Tree GameState
gameTree board p fromMove isFirstMove 
    -- Deliberate Pass
    | fromMove == Pass && not isFirstMove = 
         gameTree 
            (reinforce board p)     -- Add reinforcements
            (nextPlayer board p)    -- Switch player
            Pass                    -- Passing move
            True                    -- First move for new player
    -- No further moves possible. Switch players and add the reinforcements
    | null possibleMoves && not isFirstMove = 
        Node GameState {
            currentPlayer = p,
            moveMade = fromMove,
            currentBoard = board
        } [gameTree  
            (reinforce board p)     -- Add reinforcements
            (nextPlayer board p)    -- Switch player
            Pass                    -- Passing move
            True]                   -- First move for new player
   -- Keeping with the same player, recurse through all moves                                              
    | otherwise = 
        Node GameState {
            currentPlayer = p,
            moveMade = fromMove,
            currentBoard = board
        } [gameTree (makeAMove board p m) p m False 
                | m <- addPassingMove ++ possibleMoves] 
     where
        possibleMoves = attackMoves board p
        addPassingMove = if isFirstMove
                             then []
                             else [Pass]

playVsHuman :: Tree GameState -> IO ()
playVsHuman tree@(Node root children) = do
    printGameState tree
    if not (null children)
        then 
            playVsHuman =<< handleHuman tree
        else 
            announceWinner $ currentBoard root 

printGameState :: Tree GameState -> IO ()
printGameState (Node root _) = do
    putStrLn $ "Current player: "  ++ show (currentPlayer root)
    drawBoard $ currentBoard root

announceWinner :: Board -> IO ()
announceWinner board = 
    putStrLn $ 
        if length whoWon > 1 
            then "The result is a tie between " ++ unwords (map show whoWon)
            else "The winner is " ++ show whoWon
    where
        whoWon = winners board

testBoardWithDraw :: Board
testBoardWithDraw  = Board {
    size = 2,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 1, dice = 1}
        , Cell {player = Player 1, dice = 1}
        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 0, dice = 1}],
    conqueredDice = 0
}


winners :: Board -> [Player]
winners board = [head p | p <- players, length p == length (head players)]
    where
        players = sortBy (flip compare `on` length) 
                    $ group 
                    $ sort [player c | c <- cells board]

handleHuman :: Tree GameState -> IO (Tree GameState)
handleHuman tree@(Node _ children) = do
    putStrLn "choose your move:"
    putStr $ showChoice $ allowedMoves tree
    userInput <- getLine 
    let choice = read userInput :: Int
    case Map.lookup choice $ mapOfMoves tree of
        Just _ -> 
            return $ children !! (choice - 1)
        Nothing -> do
            putStrLn "Invalid choice. Choose again."
            handleHuman tree
    where
        showChoice :: [(Int, Move)] -> String
        showChoice choices = unlines [show num ++ ": " ++ show move | (num, move) <- choices]

drawGameTree :: Tree GameState -> IO ()
drawGameTree = putStrLn . drawTree . fmap show

drawGraphvizTree :: Tree GameState -> IO ()
drawGraphvizTree = putStrLn . showGraphvizTree

showGraphvizTree :: Tree GameState -> String
showGraphvizTree tree = "digraph G {\n" ++
    "rankdir=LR;\n" ++
    showGameGraphNodes (numberTree tree) ++
    showGameGraphTree (numberTree tree) ++ 
    "}"

showGameGraphNodes :: Tree (GameState, Int) -> String
showGameGraphNodes (Node (gstate, number) children) = 
    concat $ 
        (nodeStr ++ " " ++ labelStr) :
        [showGameGraphNodes c | c <- children] 
    where
        nodeStr = printf "\"%d\"" number :: String
        board = currentBoard gstate
        boardStr = showBoard board
        playerStr = printf "Player: %s" (show $ currentPlayer gstate) :: String
        diceStr = printf "Conquered: %s" (show $ conqueredDice board) :: String
        winnersColour = if null children
                        then case winners board of 
                                [Player 0] -> "lightpink;style=filled"
                                [Player 1] -> "lightblue;style=filled"
                                _          -> "lightgreen;style=filled"
                        else "black"
        labelStr = printf "[color=%s;label=\"%s\\n%s%s\";]\n" 
                        winnersColour playerStr boardStr diceStr :: String

showGameGraphTree :: Tree (GameState, Int) -> String
showGameGraphTree (Node (_, number) children) = 
    concat $ [
                printf "\"%d\" -> \"%d\" [label=\"%s\";];\n" 
                        number child_number (show $ moveMade child) :: String
                | (Node (child, child_number) _) <- children] ++
             [showGameGraphTree c | c <- children] 

numberTree :: Tree a -> Tree (a, Int)
numberTree t = evalState (numTree t) 0
   where
        numTree :: Tree a -> State Int (Tree (a, Int))
        numTree (Node root children) =  do 
            num <- nextNumber
            newChildren <- mapM numTree children 
            return $ Node (root, num) newChildren
            where 
                nextNumber :: State Int Int
                nextNumber = do 
                    n <- get
                    put (n + 1)
                    return n


makeGraphvizFile :: String -> String -> IO ()    
makeGraphvizFile graphvizData fileName = do
    (Just hIn, _, _, jHandle) <-
        createProcess (proc "dot" ["-Tsvg", "-o", fileName ++ ".svg"])
           { cwd = Just "."
           , std_in = CreatePipe
           }
    hPutStr hIn graphvizData
    hClose hIn
    exitCode <- waitForProcess jHandle
    case exitCode of
        ExitSuccess -> putStr ""
        _           -> putStrLn "DOT command failed"


ratePosition :: Tree GameState -> Player -> Double
ratePosition tree@(Node root children) nodePlayer  
    | null children =
        -- Can't go any further - rate the current board
        if nodePlayer `elem` nodeWinners
            then 1.0 / fromIntegral (length nodeWinners)
            else 0.0
    | otherwise = 
        -- Keep going
        if nodePlayer == currentPlayer root
            then maximum $ childRatings tree nodePlayer
            else minimum $ childRatings tree nodePlayer
    where
        nodeWinners :: [Player]
        nodeWinners = winners $ currentBoard root
        
childRatings :: Tree GameState -> Player -> [Double]
childRatings (Node _ children) nodePlayer = [ratePosition c nodePlayer | c <- children]

playVsComputer :: Player -> Tree GameState -> IO ()
playVsComputer computerPlayer tree@(Node root children) = do
    printGameState tree
    if not (null children)
        then 
            playVsComputer computerPlayer =<< 
                if currentPlayer root == computerPlayer 
                then
                    handleComputer tree
                else
                    handleHuman tree
        else 
            announceWinner $ currentBoard root 

handleComputer :: Tree GameState -> IO (Tree GameState)
handleComputer tree@(Node root children) = do
    let moveChosen = Map.lookup childPosChosen $ mapOfMoves tree
    case moveChosen of
        Just m -> do
            putStrLn $ "  ---> " ++ show m
            return $ children !! (childPosChosen - 1)
        Nothing -> do
            putStrLn $ "Computer: this should never happen: " ++ show childPosChosen
            handleComputer tree
    where 
        ratings :: [Double]
        ratings = childRatings tree (currentPlayer root)
        childPosChosen :: Int
        childPosChosen = maxPos ratings
        maxPos :: Ord a => [a] -> Int
        maxPos xs = case elemIndex (maximum xs) xs of
            Just x -> x + 1     -- "+ 1" because user chocies are 1, 2, 3
                                -- but list indices are 0, 1, 2
            _      -> undefined

allowedMoves :: Tree GameState -> [(Int, Move)]
allowedMoves (Node _ children) = zip [1..] [moveMade c | (Node c _) <- children]

mapOfMoves :: Tree GameState -> Map.Map Int Move
mapOfMoves tree = Map.fromList $ allowedMoves tree


playComputerVsComputer :: Tree GameState -> IO ()
playComputerVsComputer tree@(Node root children) = do
    printGameState tree
    if not (null children)
        then 
            playComputerVsComputer =<< handleComputer tree
        else 
            announceWinner $ currentBoard root 


