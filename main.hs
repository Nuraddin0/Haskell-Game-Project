gameBoard = ['X','A','-','-','X',
            'B','-','-','-','Z',
            'X','C','-','-','X']

main :: IO()
main = do
    putStrLn "Welcome!"
    printBoard gameBoard

    putStrLn "Enter the maximum number of total moves allowed:"
    input <- getLine
    let maxMoves = read input :: Int
    let posOfA = 1
        posOfB = 5
        posOfC = 11
        posOfZ = 9

    putStrLn "Who starts first? Type 'last' or 'firsts':"
    startingSide <- askWhichToStart
    print startingSide
    let firstTurn = startingSide == "firsts"
        lastTurn = startingSide == "last"

    startGame gameBoard maxMoves 0 firstTurn lastTurn posOfA posOfB posOfC posOfZ

startGame :: [Char] -> Int -> Int -> Bool -> Bool -> Int -> Int -> Int -> Int -> IO()
startGame board maxMoves currentMoves firstTurn lastTurn posOfA posOfB posOfC posOfZ = do
    if firstTurn then do
        putStrLn "Please select one of first three letters and a cell to move it (e.g., A 6)"
        input <- getLine
        let [letter, pos] = words input
            cell = read pos :: Int
            isValidMove =
                if letter == "A" then isValidFirst board cell posOfA
                else if letter == "B" then isValidFirst board cell posOfB
                else if letter == "C" then isValidFirst board cell posOfC
                else False

        if isValidMove then do
            let newBoard =
                    if letter == "A" then updateBoard board cell posOfA
                    else if letter == "B" then updateBoard board cell posOfB
                    else if letter == "C" then updateBoard board cell posOfC
                    else board -- never executes
                moveCount = currentMoves + 1 

            printBoard newBoard
            putStrLn ("Completed moves: " ++ show moveCount)
            

            let isGameEnded = if letter == "A" then checkWinCondition newBoard maxMoves moveCount cell posOfB posOfC posOfZ
                    else if letter == "B" then checkWinCondition newBoard maxMoves moveCount posOfA cell posOfC posOfZ
                    else if letter == "C" then checkWinCondition newBoard maxMoves moveCount posOfA posOfB cell posOfZ
                    else checkWinCondition newBoard maxMoves moveCount posOfA posOfB posOfC posOfZ 

            if isGameEnded == 0 then putStrLn "DRAW!"
            else if isGameEnded == 1 then putStrLn "A&B&C WIN!"
            else if isGameEnded == 2 then putStrLn "Z WIN!"
            else do
                if letter == "A" then startGame newBoard maxMoves moveCount False True cell posOfB posOfC posOfZ
                else if letter == "B" then startGame newBoard maxMoves moveCount False True posOfA cell posOfC posOfZ
                else if letter == "C" then startGame newBoard maxMoves moveCount False True posOfA posOfB cell posOfZ
                else return()-- never executes
        else do
            putStrLn "Invalid move! Turn passes to opponent..."
            startGame board maxMoves currentMoves False True posOfA posOfB posOfC posOfZ

    else do
        putStrLn "Please select a cell for the Z:"
        input <- getLine
        let cell = read input :: Int
            isValidMove = isValidLast board cell posOfZ
        if isValidMove then do
            let newBoard = updateBoard board cell posOfZ
                moveCount = currentMoves + 1

            printBoard newBoard
            putStrLn ("Completed moves: " ++ show moveCount)

            let isGameEnded = checkWinCondition newBoard maxMoves moveCount posOfA posOfB posOfC cell

            if isGameEnded == 0 then putStrLn "DRAW!"
            else if isGameEnded == 1 then putStrLn "A&B&C WIN!"
            else if isGameEnded == 2 then putStrLn "Z WIN!"
            else startGame newBoard maxMoves moveCount True False posOfA posOfB posOfC cell
        else do
            putStrLn "Invalid move! Turn passes to opponent..."
            startGame board maxMoves currentMoves True False posOfA posOfB posOfC posOfZ


checkWinCondition :: [Char] -> Int -> Int -> Int -> Int -> Int -> Int -> Int

checkWinCondition board maxMoves moveCount posOfA posOfB posOfC posOfZ = do
    -- return 0 for DRAW, 1 for firstSideWins, 2 for lastSideWins, 3 for game is not ended
    if (maxMoves == moveCount) then 0 --Draw
    else if ( (posOfZ `mod` 5) < (posOfA `mod` 5) && (posOfZ `mod` 5) < (posOfB `mod` 5) && (posOfZ `mod` 5) < (posOfC `mod` 5)) then 2 --Z wins
    else if (posOfZ == 9 && not (any (\cell -> isValidLast board cell posOfZ) [posOfZ-1, posOfZ+4, posOfZ-6])) || (not (any (\cell -> isValidLast board cell posOfZ) [posOfZ+1, posOfZ-1, posOfZ+5, posOfZ-5, posOfZ+6, posOfZ-6, posOfZ+4, posOfZ-4])) then 1 -- A, B, C win
    else 3 --Game is not ended


updateBoard :: [Char] -> Int -> Int -> [Char]
updateBoard board cell pos =
    let letter = board !! pos
    in map (\(i, c) -> if i == cell then letter else if i == pos then '-' else c) (zip [0..] board)

printBoard :: [Char] -> IO()
printBoard [] = return ()
printBoard row = do
    putStrLn (take 5 row)
    printBoard (drop 5 row)

isValidLast :: [Char] -> Int -> Int -> Bool
isValidLast board cell pos
    | cell<0 || cell>14 = False
    | (board !! cell) `elem` ['X', 'A', 'B', 'C', 'Z'] = False
    | (1 <= pos && pos <= 3 && cell `elem` [pos+1, pos-1, pos+5, pos+6, pos+4]) = True
    | (pos == 5 && cell `elem` [pos+1,pos-4,pos+6]) || (pos == 9 && cell `elem` [pos-6,pos-1,pos+4]) || (5 < pos && pos < 9 && cell `elem` [pos+1, pos-1, pos+5, pos+6, pos+4, pos-5, pos-4, pos-6]) = True
    | (11 <= pos && pos <= 13 && cell `elem` [pos+1, pos-1, pos-5, pos-4, pos-6]) = True
    | otherwise = False

isValidFirst :: [Char] -> Int -> Int -> Bool
isValidFirst board cell pos
    | cell<0 || cell>14 = False
    | (board !! cell) `elem` ['X', 'A', 'B', 'C', 'Z'] = False
    | (1 <= pos && pos <= 3 && cell `elem` [pos+1, pos+5, pos+6]) = True
    | (pos == 5 && cell `elem` [pos+1,pos-4,pos+6]) || (5 < pos && pos < 9 && cell `elem` [pos-5, pos-4, pos+1, pos+5, pos+6]) = True
    | (11 <= pos && pos <= 13 && cell `elem` [pos+1, pos-5, pos-4]) = True
    | otherwise = False

askWhichToStart :: IO String
askWhichToStart = do
    input <- getLine
    if (input /= "firsts" && input /= "last") then do
        putStrLn "Invalid input"
        askWhichToStart
    else return input
