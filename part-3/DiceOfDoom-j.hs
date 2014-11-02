module Main
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
import qualified Data.Foldable as F
import Data.Time (getCurrentTime)

data Player = Player Int
              deriving (Eq, Ord)

instance Show Player where
    show (Player i) = [chr (i + ord 'A')]

data Cell = Cell {
                player :: Player,
                dice :: Int
            } 
            deriving(Eq, Ord)

instance Show Cell where
    show c = show (player c) ++ "-" ++ show (dice c)

data Board = Board {
                size :: Int,
                maxDice :: Int,
                numPlayers :: Int,
                cells :: [Cell],
                conqueredDice :: Int
             }
             deriving (Eq, Ord)

instance Show Board where
    show b = show (cells b) ++ ", reinforcements : " ++ show (conqueredDice b) ++ " Total: " ++ show (sum (map dice (cells b)))

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
        distributeDice (c:cs) remainingDice
            | remainingDice > 0 =
                if player c == p && dice c < maxDice board then
                    -- Give this cell an extra dice
                    c {dice = dice c + 1} : distributeDice cs (remainingDice  -1)
                else 
                    -- Continue on
                    c : distributeDice cs remainingDice
            | otherwise = c:cs

nextPlayer :: Board -> Player -> Player
nextPlayer board (Player n) = Player ((n + 1) `mod` numPlayers board)

data GameState = GameState {
                    currentPlayer :: Player,
                    currentMoves :: [Move],
                    currentBoard :: Board
                 }

instance Show GameState where
    show g = "Player: " ++ show (currentPlayer g) ++
             " Board: " ++ show (cells $ currentBoard g) ++ 
             " Conquered: " ++ show (conqueredDice $ currentBoard g) ++
             " Winners: " ++ show (winners $ currentBoard g)


type GameTree = Tree GameState

gameTree :: Board -> Player -> Bool -> GameTree
gameTree board p isFirstMove = memoizeM gameTreeM (board, p, isFirstMove)

gameTreeM :: Monad m => ((Board, Player, Bool) -> m GameTree) -> (Board, Player, Bool) -> m GameTree
gameTreeM f' (board, p, isFirstMove)
    -- No further moves possible. Switch players and add the reinforcements
    | null possibleMoves && not isFirstMove = do
            passTree <- f' (reinforce board p,    -- Add reinforcements
                             nextPlayer board p,  -- Switch player
                             True)                -- First move for new player
            return $ Node GameState {
                    currentPlayer = p,
                    currentMoves = [Pass],
                    currentBoard = board
                 } [passTree]
    --  No moves possible - END OF GAME
    | null possibleMoves && isFirstMove = 
            return $ Node GameState {
                    currentPlayer = p,
                    currentMoves = [Pass],
                    currentBoard = board
                 } []
    -- Keeping with the same player, recurse through all moves                                              
    | otherwise = do
        childTrees <- mapM (\b -> f' (b, p, False)) [makeAMove board p m | m <- possibleMoves] 
        if isFirstMove
            then 
                return $ Node GameState {
                            currentPlayer = p,
                            currentMoves = possibleMoves,
                            currentBoard = board
                         } childTrees
            else do
                passTree <- f' (reinforce board p,  -- Add reinforcements
                                nextPlayer board p, -- Switch player
                                True)               -- First move for new player
                return $ Node GameState {
                            currentPlayer = p,
                            currentMoves = Pass : possibleMoves,
                            currentBoard = board
                         } (passTree : childTrees)
     where
        possibleMoves = attackMoves board p

type StateMap a b = State (Map.Map a b) b
 
memoizeM :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) Map.empty 
    where
        -- Cache miss
        g z = do
            y <- t f z  
            m <- get
            put $ Map.insert z y m
            return y
        -- Cache hit
        f z = get >>= \m -> maybe (g z) return (Map.lookup z m)

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
showGameGraphTree (Node (root, number) children) = 
    concat $ [
                printf "\"%d\" -> \"%d\" [label=\"%s\";];\n" 
                        number child_number (show moveMade) :: String
                | (moveMade, Node (_, child_number) _) <- zip (currentMoves root) children] ++
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
allowedMoves (Node root _) = zip [1..] $ currentMoves root 


mapOfMoves :: Tree GameState -> Map.Map Int Move
mapOfMoves tree = Map.fromList $ allowedMoves tree


treeSizeFlatten :: Tree a -> Int
treeSizeFlatten = length . flatten

treeSizeFold :: Tree a -> Int
treeSizeFold = F.foldl' (\x _ -> x + 1) 0 


treeSizeRecur :: Tree a -> Int
treeSizeRecur (Node _ children) = 1 + sum ( map treeSizeRecur children )

treeDepth :: Tree a -> Integer
treeDepth (Node _ []) = 1
treeDepth (Node _ ts) = 1 + maximum ( map treeDepth ts)

leaves :: Tree a -> [a]
leaves tree = getLeaves tree []
    where
        getLeaves :: Tree a -> [a] -> [a]
        getLeaves (Node root children) total = 
            total ++ (if null children 
                        then [root]
                        else concat [getLeaves c total | c <- children])

allWinners :: Tree GameState -> [([Player], Int)]
allWinners tree = map (\xs@(x:_) -> (x, length xs)) 
                    . group . sort $ map (winners . currentBoard) $ leaves tree 

all2x2Boards :: [Board]
all2x2Boards = [
    Board {
        size = 2,
        maxDice = 3,
        numPlayers = 2,
        cells = [
              Cell {player = cell0Player, dice = cell0Dice}
            , Cell {player = cell1Player, dice = cell1Dice}
            , Cell {player = cell2Player, dice = cell2Dice}
            , Cell {player = cell3Player, dice = cell3Dice}
        ],
        conqueredDice = 0
    } |   cell0Player <- allPlayers
        , cell1Player <- allPlayers 
        , cell2Player <- allPlayers
        , cell3Player <- allPlayers
        , cell0Dice   <- allDice
        , cell1Dice   <- allDice
        , cell2Dice   <- allDice
        , cell3Dice   <- allDice ]
    where
        allPlayers = [Player 0, Player 1]
        allDice = [1, 2, 3]


    -- putStrLn $ printf "A: %d\tB: %d\tA Winners: %s\tB Winners: %s" numPlayerANodes numPlayerBNodes (show winnersA) (show winnersB)
    -- putStrLn $ printf "A: %d\tB: %d" numPlayerANodes numPlayerBNodes 
    -- putStrLn $ printf "A Winners: %s\tB Winners: %s" (show winnersA) (show winnersB)

boardStats :: Board -> IO ()
boardStats board = do
    drawBoard board
    putStrLn $ printf "A: %d\tB: %d\tA Winners: %s\tB Winners: %s" 
        numPlayerANodes numPlayerBNodes (show winnersA) (show winnersB)
    where
        treeA = gameTree board (Player 0) True
        treeB = gameTree board (Player 1) True
        numPlayerANodes = treeSize treeA 
        numPlayerBNodes = treeSize treeB
        winnersA = allWinners treeA
        winnersB = allWinners treeB

playAll2x2Boards :: IO ()
playAll2x2Boards = mapM_ boardStats all2x2Boards

playRandom3x3Board :: IO ()
playRandom3x3Board = genBoard 3 2 3 >>= boardStats

-- A very one-sided board with a small game tree
-- Takes less than a second to generate
test3x3BoardA :: Board
test3x3BoardA = Board {
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

-- A bigger game tree but player A still dominant
-- Takes a few seconds to generate
test3x3BoardB :: Board
test3x3BoardB = Board {
    size = 3,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 0, dice = 3}
        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 1, dice = 2}

        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 0, dice = 2}

        , Cell {player = Player 1, dice = 1}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 1, dice = 1}],
    conqueredDice = 0
}

-- A much bigger tree 
-- Takes a couple of minutes to run
test3x3BoardC :: Board
test3x3BoardC = Board {
    size = 3,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 0, dice = 1}
        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 0, dice = 1}

        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 1, dice = 2}
        , Cell {player = Player 1, dice = 3}

        , Cell {player = Player 0, dice = 2}
        , Cell {player = Player 1, dice = 1}
        , Cell {player = Player 1, dice = 2}],
    conqueredDice = 0
}

-- An even bigger board.
-- Takes a few mins to generate
test3x3BoardD :: Board
test3x3BoardD = Board {
    size = 3,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 1, dice = 2}
        , Cell {player = Player 0, dice = 2}
        , Cell {player = Player 0, dice = 1}

        , Cell {player = Player 0, dice = 1}
        , Cell {player = Player 1, dice = 3}
        , Cell {player = Player 0, dice = 3}

        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 1, dice = 1}
        , Cell {player = Player 0, dice = 2}],
    conqueredDice = 0
}

-- An enormous tree - 1.4 billion nodes
-- Takes a couple of hours to generate
test3x3BoardE :: Board
test3x3BoardE = Board {
    size = 3,
    maxDice = 3,
    numPlayers = 2,
    cells = [
          Cell {player = Player 0, dice = 2}
        , Cell {player = Player 0, dice = 2}
        , Cell {player = Player 1, dice = 2}

        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 1, dice = 1}
        , Cell {player = Player 0, dice = 3}

        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 0, dice = 3}
        , Cell {player = Player 1, dice = 3}],
    conqueredDice = 0
}

main' :: IO ()
main' = do
    t1 <- getCurrentTime
    print t1
    boardStats test3x3BoardC
    t2 <- getCurrentTime
    print t2

treeSize :: Tree a -> Int
treeSize = treeSizeRecur

main :: IO ()
main = do
    t1 <- getCurrentTime
    print t1
    putStrLn $ printf "Size: %d" (treeSize tree) 
    putStrLn $ printf "Depth: %d" (treeDepth tree) 
    t2 <- getCurrentTime
    print t2
    where
        tree = gameTree test3x3BoardE (Player 0) True

