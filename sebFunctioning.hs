-- custom datas and type aliases!
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)
type Move = Int
type Piece = Player
type GameState = ([Column], Player)
type Column = (Int, [Piece])
type Winner = Maybe Player 

--Initial GameState
initialColumns :: [Column]
initialColumns = [(x, []) | x <- [1..7]]
initialGS :: GameState
initialGS = (initialColumns, PlayerOne)

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

--takes a move, checks if it's valid, and returns the new GameState
makeAMove :: Move -> GameState -> GameState
makeAMove mv gs = --undefined
    if elem mv validMoves
        then 
            let turn = opponent (snd gs)
                splitColumns = splitAt (mv - 1) (fst gs)
                beforeCols = fst splitColumns
                (c:cs) = snd splitColumns
                newCol = (mv, snd c ++ [snd gs])
                cols = beforeCols ++ [newCol] ++ cs
            in  (cols, turn)
        else error "Not a valid move!"
    where validMoves = [ fst col | col <- fst gs ]

legalMoves :: GameState -> [Move]
legalMoves gs = [ fst col | col <- fst gs ]

hasWon :: GameState -> Winner
hasWon (columns , _) = let resultColumns = checkColumns (removeIndices columns)
                           resultRows = checkRows (reverseColumns (removeIndices columns))
                           resultDiagonals = checkDiagonals (reverseColumns (removeIndices columns))
                       in if resultColumns /= Nothing then resultColumns else
                          if resultRows /= Nothing then resultRows else
                          if resultDiagonals /= Nothing then resultDiagonals else
                          Nothing where

    removeIndices columns = map snd columns

    reverseColumns columns = map reverse columns


    checkColumns [] = Nothing
    checkColumns (c:cs) = let checkedColumn = checkColumn c
                          in if checkedColumn /= Nothing then checkedColumn else checkColumns cs
    checkColumn (p1:p2:p3:p4:ps) = if p1 == p2 && p2 == p3 && p3 == p4 then Just p1 else checkColumn (p2:p3:p4:ps)
    checkColumn _ = Nothing


    checkRows ((p1:p1s):(p2:p2s):(p3:p3s):(p4:p4s):ps) = let checkedRow = checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s)
                                                         in if checkedRow /= Nothing then checkedRow else checkRows ((p2:p2s):(p3:p3s):(p4:p4s):ps)
    checkRows _ = Nothing


    checkDiagonals columns = let resultLR = checkDiagonalsLR columns
                                 resultRL = checkDiagonalsLR (reverseColumns columns)
                             in if resultLR /= Nothing then resultLR else
                                if resultRL /= Nothing then resultRL else
                                Nothing
    checkDiagonalsLR ((a:b:c:p1:p1s):(d:e:p2:p2s):(f:p3:p3s):(p4:p4s):ps) = let checkedDiagonal = checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s)
                                                                            in if checkedDiagonal /= Nothing then checkedDiagonal else checkDiagonalsLR ((d:e:p2:p2s):(f:p3:p3s):(p4:p4s):ps)
    checkDiagonalsLR _ = Nothing


    checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s) = if p1 == p2 && p2 == p3 && p3 == p4 then Just p1 else checkFour p1s p2s p3s p4s
    checkFour _ _ _ _ = Nothing


prettyPrint :: GameState -> String
prettyPrint (cols, turn) =
    unlines (rows ++ [divider, colNumbers, "", turnText])
    where
        showCell :: Maybe Piece -> String
        showCell (Just PlayerOne) = "X"
        showCell (Just PlayerTwo) = "O"
        showCell Nothing = " "
        
        getPieceAt :: Int -> Column -> Maybe Piece
        getPieceAt rowIdx (_, pieces) =
            if rowIdx < length pieces
                then Just (pieces !! rowIdx) -- yeah yeah whatever i wanna use it so im using it
                else Nothing -- bigger than thing, bad.
        
        renderRow :: Int -> String
        renderRow rowIdx =
            "| " ++ unwords [showCell (getPieceAt rowIdx col) | col <- cols] ++ " |"
        
        rows :: [String]
        rows = [renderRow r | r <- [5, 4..0]]

        divider :: String
        divider = "+---------------+"

        colNumbers :: String
        colNumbers = "  1 2 3 4 5 6 7  "

        turnText :: String
        turnText = "Current Turn: " ++ show turn