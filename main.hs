{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}

gameBoard = ['X','A','-','-','X',
            'B','-','-','-','Z',
            'X','C','-','-','X']

main :: IO()
main = do
    putStrLn "Welcome!"
    printBoard gameBoard

    input <- getLine
    let maxMoves = read input :: Int
    let posOfA = 1
        posOfB = 5
        posOfC = 11

    startingSide <- getLine
    if (startingSide /= "firsts" || startingSide /= "last") then do
        putStrLn "Invalid input"
        main
    else do
        let firstTurn = startingSide == "firsts"
            lastTurn = startingSide == "last"

        startGame maxMoves 0 firstTurn lastTurn posOfA posOfB posOfC

startGame :: Int -> Int -> Bool -> Bool -> Int -> Int -> Int -> IO()
startGame maxMoves currentMoves firstTurn lastTurn posOfA posOfB posOfC = do
    if firstTurn then do
        putStrLn "Please select one of first three letters and a cell to move it (e.g., A 6)"
        input <- getLine
        let [letter, pos] = words input
        let cell = read pos :: Int

        let isValidMove =
              if letter == "A" then isValidFirst cell posOfA
              else if letter == "B" then isValidFirst cell posOfB
              else if letter == "C" then isValidFirst cell posOfC
              else False

        if isValidMove then do
            let newBoard =
                  if letter == "A" then updateBoard cell posOfA
                  else if letter == "B" then updateBoard cell posOfB
                  else if letter == "C" then updateBoard cell posOfC
                  else []
                moveCount = currentMoves + 1 
            if letter == "A" then startGame maxMoves moveCount False True cell posOfB posOfC
            else if letter == "B" then startGame maxMoves moveCount False True posOfA cell posOfC
            else if letter == "C" then startGame maxMoves moveCount False True posOfA posOfB cell
            else startGame maxMoves moveCount False True posOfA posOfB posOfC -- never executes

        else do
            putStrLn "Invalid Move"
            startGame maxMoves currentMoves False True posOfA posOfB posOfC



    else putStrLn "merhaba"


updateBoard :: Int -> Int -> [Char]
updateBoard cell pos =
  let letter = gameBoard !! pos
  in map (\(i, c) -> if i == cell then letter else if i == pos then '-' else c) (zip [0..] gameBoard)

printBoard :: [Char] -> IO()
printBoard [] = return ()
printBoard xs = do
    putStrLn (take 5 xs)
    printBoard (drop 5 xs)


isValidFirst :: Int -> Int -> Bool
isValidFirst cell pos
  | (gameBoard !! cell) `elem` ['X', 'A', 'B', 'C'] = False
  | (1 <= pos && pos <= 3 && cell `elem` [pos+1, pos+5, pos+6]) = True
  | (5 <= pos && pos <= 9 && cell `elem` [pos-5, pos-4, pos+1, pos+5, pos+6]) = True
  | (11 <= pos && pos <= 13 && cell `elem` [pos+1, pos-5, pos-4]) = True
  | otherwise = False
