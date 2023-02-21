import System.IO

-- Goal: Create Tic Tac Toe in functional programming paradigm in Haskell
-- Author: Kyle Nieva

main :: IO ()
main = do
    putStrLn "Welcome to Tic Tac Toe"
    let gameBoard = "123456789"  -- board represented by a 9-character string representing each tile from left to right, top to bottom
    gameLoop gameBoard 'X' -- start with player X

-- Facilitates input and output from and to the player, alternates between two players
gameLoop :: [Char] -> Char -> IO ()
gameLoop board player = do
    putStrLn $ formatBoard board
    putStrLn $ "Player " ++ [player] ++ " , please input the number of the space you choose to mark:\n"
    input <- getLine
    -- Check if input is integer, reject otherwise
    case readMaybe input :: Maybe Int of
        Just playerMove -> do
            -- Check if integer is valid move, reject otherwise
            if validMove board input
                then do
                    let nextBoard = applyMove board player playerMove -- modify board state with player's move
                        printBoard = formatBoard nextBoard
                        nextPlayer = nextTurn player
                    -- End game on win or tie with appropriate message or continue to next player's turn
                    case winCheck nextBoard player of "win" -> putStr $ printBoard ++ "Player " ++ [player] ++ " wins!\n\n"
                                                      "tie" -> putStr $ printBoard ++ "The game is a tie. Game Over!\n\n"
                                                      "continue" -> gameLoop nextBoard nextPlayer -- continue to next turn w/ new board state
                else do
                    putStrLn "Invalid move, please input number of unmarked space"
                    gameLoop board player
        Nothing -> do
            putStrLn "Invalid input, please input a number from 1-9."
            gameLoop board player

-- Formats board into readable grid
formatBoard :: [Char] -> [Char]
formatBoard board = "\n\n " ++ [board !! 0] ++ " | " ++ [board !! 1] ++ " | " ++ [board !! 2] ++ " \n---|---|---\n "
                        ++ [board !! 3] ++ " | " ++ [board !! 4] ++ " | " ++ [board !! 5] ++ " \n---|---|---\n "
                        ++ [board !! 6] ++ " | " ++ [board !! 7] ++ " | " ++ [board !! 8] ++ "\n\n"

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

-- Checks if move is valid ie. 1-9, unmarked
validMove :: [Char] -> [Char] -> Bool
validMove board move = length move == 1 && ((head move) `elem` board)

-- Reflects new board state after player move
applyMove :: [Char] -> Char -> Int -> [Char]
applyMove board mark move = take (move - 1) board ++ [mark] ++ drop move board

-- Alternates mark between two players
nextTurn :: Char -> Char
nextTurn 'X' = 'O'
nextTurn 'O' = 'X'

-- Checks for tie or win by checking for empty spaces in board string or line of consecutive marks
winCheck :: [Char] -> Char -> [Char]
winCheck board player
    | checkLine board player 0 1 2 = "win" -- top row
    | checkLine board player 3 4 5 = "win" -- middle row
    | checkLine board player 6 7 8 = "win" -- bottom row
    | checkLine board player 0 3 6 = "win" -- left column
    | checkLine board player 1 4 7 = "win" -- middle column
    | checkLine board player 2 5 8 = "win" -- right column
    | checkLine board player 0 4 8 = "win" -- diagonal upper left to lower right
    | checkLine board player 2 4 6 = "win" -- diagonal upper right to lower left
    | null (remainingSpaces board) = "tie"
    | otherwise = "continue"

-- Compare specified row/column/diagonal string to "XXX" or "OOO"
checkLine :: [Char] -> Char -> Int -> Int -> Int -> Bool
checkLine board player x y z = replicate 3 player == extractLine board x y z

-- Filters board string to specified row/column/diagonal indices
extractLine :: [Char] -> Int -> Int -> Int -> [Char]
extractLine board x y z = (board !! x):((board !! y):[board !! z])

-- Filters empty spaces from board string
remainingSpaces :: [Char] -> [Char]
remainingSpaces board = filter (`elem` ['1'..'9']) board
