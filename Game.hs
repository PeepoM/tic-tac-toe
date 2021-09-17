module Game where

import Data.Char (isDigit)
import Data.List (intercalate, intersperse, isInfixOf, reverse, transpose)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- As player implements Ord, the relation X < B < O
-- is very important for the correct working of the Minimax algorithm
data Player = X | B | O deriving (Eq, Ord, Show)

data Cell = Filled Player | Empty deriving (Eq)

instance Show Cell where
  show (Filled X) = " X "
  show (Filled O) = " O "
  show Empty = "   "

type Board = [[Cell]]

type Move = Int

-- Constants
width :: Int
width = 3

height :: Int
height = 3

toWin :: Int
toWin = 3

depth :: Int
depth = 9

emptyBoard :: Board
emptyBoard = replicate height . replicate width $ Empty

-------------

-- Rendering functions
showRow :: [Cell] -> String
showRow = intercalate "|" . map show

rowSep :: String
rowSep = "---+---+---"

renderBoard :: Board -> IO ()
renderBoard = putStrLn . unlines . intersperse rowSep . map showRow

welcomeScreen :: IO ()
welcomeScreen = do
  putStrLn "Welcome to Tic Tac Toe :)"
  putStrLn "Use numbers from 1 - 9 to place a symbol!"

-----------------------

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  welcomeScreen
  renderBoard emptyBoard
  gameLoop X emptyBoard

-- The driver code of the whole game
gameLoop :: Player -> Board -> IO ()
gameLoop player board
  | player == X = do
    move <- getMove player

    if isValid move board
      then manageMove player (placeMove move player board)
      else do
        putStrLn "Not a valid move!"
        gameLoop player board
  | player == O = do
    putStrLn "Constructing the game tree..."
    manageMove player (computerMove board)

manageMove :: Player -> Board -> IO ()
manageMove player board = do
  renderBoard board
  checkGameState player board

-- Takes specific actions according to the current state of the game
checkGameState :: Player -> Board -> IO ()
checkGameState player board
  | gameOver player board = putStrLn ("Player " ++ show player ++ " won!") >> restartGame
  | boardFull board = putStrLn "The game is a DRAW!" >> restartGame
  | otherwise = gameLoop (next player) board

-- Prompts the player to play again and validates the input
restartGame :: IO ()
restartGame = do
  putStr "Restart the game? (yes or no) "
  answer <- getLine

  case answer of
    "yes" -> main
    "no" -> putStrLn "Thank you for playing :)"
    _ -> putStrLn "Input either yes or no!" >> restartGame

-- Prompts the current player for a move and validates the input
getMove :: Player -> IO Move
getMove player = do
  putStr $ "Player " ++ show player ++ "'s " ++ "turn: "
  line <- getLine

  if all isDigit line && line /= []
    then return (read line)
    else do
      putStrLn "Not a valid move!"
      getMove player

{- Checks rows, cols and diagonals for a win. The function is designed to cater
for an arbitrary number of rows, cols and pieces, that are needed to win a game. -}
gameOver :: Player -> Board -> Bool
gameOver player board = any (isContainedIn winSeq) [board, transpose board, diags board]
  where
    winSeq = replicate toWin (Filled player)

-- Checks if a lists is an infix of ANY of the subsequent lists inside xss
isContainedIn :: Eq a => [a] -> [[a]] -> Bool
isContainedIn xs = any (isInfixOf xs)

-- Computes the diagonals of a 2D list through shifting of the rows -> https://stackoverflow.com/questions/37511914
diags :: [[a]] -> [[a]]
diags xs = help xs ++ help (reverse xs)
  where
    help = map concat . transpose . padding 0 . listify

    -- Encloses each element in a lists of lists into a separate list
    listify = map (map (: []))

    padding _ [] = []
    padding n (y : ys) = (replicate n [] ++ y) : padding (n + 1) ys

boardFull :: Board -> Bool
boardFull = all (notElem Empty)

next :: Player -> Player
next X = O
next O = X

-- Retrieves the row and col of a given cell
getPosition :: Move -> (Int, Int)
getPosition move = (row, col)
  where
    row = (move - 1) `div` width
    col = (move - 1) `mod` width

-- Checks if a given move is valid => the selected cell is empty and in-bounds
isValid :: Move -> Board -> Bool
isValid move board = inBounds && isEmpty
  where
    (row, col) = getPosition move
    inBounds = move <= width * height && move >= 1
    isEmpty = board !! row !! col == Empty

-- Places the current player's symbol to a given cell on the board
placeMove :: Move -> Player -> Board -> Board
placeMove move player board = insertAt row modifiedRow board
  where
    (row, col) = getPosition move
    rowToModify = board !! row
    modifiedRow = insertAt col (Filled player) rowToModify

-- Inserts an element to the nth position of a given list
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = take n xs ++ [x] ++ drop (n + 1) xs

--------- AI with Minimax algorithm ---------

data Tree a = Node a [Tree a] deriving (Show)

-- Given the current player and board, construct a list of all possible moves
possibleBoards :: Player -> Board -> [Board]
possibleBoards p b
  | gameOver O b = []
  | gameOver X b = []
  | boardFull b = []
  | otherwise = map (\m -> placeMove m p b) moves
  where
    moves = filter (`isValid` b) [1 .. width * height]

-- Given the current player and board state, construct the game tree
gameTree :: Player -> Board -> Tree Board
gameTree p b = Node b [gameTree (next p) b | b <- possibleBoards p b]

-- Limit the depth of a tree to a certain number
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x xs) = Node x [prune (n - 1) e | e <- xs]

minimax :: Player -> Tree Board -> Tree (Board, Player)
minimax player (Node b [])
  | gameOver X b = Node (b, X) []
  | gameOver O b = Node (b, O) []
  | otherwise = Node (b, B) []
minimax player (Node b bs)
  | player == X = Node (b, minimum sublabels) tree
  | player == O = Node (b, maximum sublabels) tree
  where
    -- Recursively label the whole subtree of boards
    tree = [minimax (next player) b | b <- bs]
    -- Get player labels from one level down
    sublabels = [p | Node (_, p) _ <- tree]

computerMove :: Board -> Board
-- Filter out the trees from one level down, that have the same player label as "root"
computerMove board = head [b | Node (b, p) _ <- ts, p == root]
  where
    -- Generate tree of all possible games for a given board
    tree = prune depth $ gameTree O board
    -- Label the whole game tree using minimax
    -- get the root node player label "root" and the labeled subtrees "ts"
    Node (_, root) ts = minimax O tree
