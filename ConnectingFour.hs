module ConnectingFour where

-- custom datas and type aliases!
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)
type Move = Int
type Piece = Player
type GameState = ([Column], Player)
type Column = (Int, [Piece])
data Winner = Tie | Won Player deriving (Eq, Show)

--Initial GameState
initialColumns :: [Column]
initialColumns = [(x, []) | x <- [1..7]]
initialGS :: GameState
initialGS = (initialColumns, PlayerOne)

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

--takes a move, checks if it's valid, and returns the new GameState
makeAMove :: Move -> GameState -> Maybe GameState
makeAMove mv (cols, turn) =
        case splitAt (mv-1) cols of
            (beforeCols, (c:cs)) ->
                if mv < 1 || mv > 7 || length (snd c) > 5
                then Nothing
                else let newCol = (mv, snd c ++ [turn])
                         cols = beforeCols ++ [newCol] ++ cs
                     in  Just (cols, opponent turn)
            _ -> Nothing
    --     else error "Not a valid move!"
    -- where thisLegalMoves = legalMoves gs

legalMoves :: GameState -> [Move]
legalMoves gs = [ fst col | col <- fst gs, length (snd col) < 6 ]

combineMaybes :: Maybe a -> Maybe a -> Maybe a
combineMaybes (Just a) _ = Just a
combineMaybes _ (Just a) = Just a
combineMaybes _ _ = Nothing

hasWon :: GameState -> Maybe Winner
hasWon (columns, _) = 
    let resultColumns = checkColumns removeIndices
        resultRows = checkRows removeIndices
        resultDiagonals = checkDiagonals removeIndices
    in case resultColumns `combineMaybes` resultRows `combineMaybes` resultDiagonals of
        Just winner -> Just winner
        Nothing -> if fullBoard (columns, PlayerOne) then Just Tie else Nothing
    where
    removeIndices = map snd columns

    checkColumns [] = Nothing
    checkColumns (c:cs) = checkColumn c `combineMaybes` checkColumns cs
    checkColumn (p1:p2:p3:p4:ps) = if p1 == p2 && p2 == p3 && p3 == p4 then Just (Won p1) else checkColumn (p2:p3:p4:ps)
    checkColumn _ = Nothing


    checkRows ((p1:p1s):(p2:p2s):(p3:p3s):(p4:p4s):ps) = 
        let checkedRow = checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s)
        in if checkedRow /= Nothing then checkedRow else checkRows ((p2:p2s):(p3:p3s):(p4:p4s):ps)
    checkRows _ = Nothing

    checkDiagonals (c1:c2:c3:c4:cs) = (checkFour c1 (drop 1 c2) (drop 2 c3) (drop 3 c4)) `combineMaybes` 
                                      (checkFour (drop 3 c1) (drop 2 c2) (drop 1 c3) c4) `combineMaybes`
                                      checkDiagonals (c2:c3:c4:cs)

    checkDiagonals _ = Nothing
    checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s) = if p1 == p2 && p2 == p3 && p3 == p4 then Just (Won p1) else checkFour p1s p2s p3s p4s
    checkFour _ _ _ _ = Nothing

--fullBoard detects if all the pieces are filled and there is no winner,, meaning it is a tie
fullBoard :: GameState -> Bool
fullBoard gs = 
    let cols = map snd (fst gs)
        totalPieces = sum (map length cols)
    in  totalPieces >= 42

tgs1 :: GameState
tgs1 = ([(1,[]),(2,[]),(3,[PlayerOne, PlayerOne, PlayerOne, PlayerOne]),(4,[]),(5,[]),(6,[]),(7,[])],PlayerOne) --PlayerOne wins verticaly

whoWillWin :: GameState -> Winner
whoWillWin gs@(cols, turn) = 
    if hasWon gs != Nothing
        then hasWon gs
        else
            let allmvs = legalMoves gs
                nextStates = [makeAMove mv gs | mv <- allmvs] -- a list of possible GameStates
                stateScores = [whoWillWin gs | gs <- nextStates]
                bestState = bestFor turn stateScores
            in whoWillWin bestState

bestFor :: Player -> [Winner] -> Winner
bestFor turn wins = 