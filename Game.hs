module Game where

import Data.Char (isDigit)
import Data.List (intercalate, intersperse, isInfixOf, reverse, transpose)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

data Player = O | B | X deriving (Eq, Ord, Show)

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
depth = 5

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
gameLoop player board = do
  move <- getMove player

  if isValid move board
    then do
      let newBoard = placeMove move player board
      renderBoard newBoard
      checkGameResult player newBoard
    else do
      putStrLn "Not a valid move!"
      gameLoop player board

-- Takes specific actions according to the current state of the game
checkGameResult :: Player -> Board -> IO ()
checkGameResult player board
  | isGameOver player board = putStrLn ("Player " ++ show player ++ " won!") >> restartGame
  | isBoardFull board = putStrLn "The game is a DRAW!" >> restartGame
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
isGameOver :: Player -> Board -> Bool
isGameOver player board = any (isContainedIn winSeq) [board, transpose board, diags board]
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

isBoardFull :: Board -> Bool
isBoardFull = all (notElem Empty)

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
