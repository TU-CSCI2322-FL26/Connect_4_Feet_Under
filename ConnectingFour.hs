module ConnectingFour where

import Text.Read

-- custom datas and type aliases!
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)
type Move = Int
type Piece = Player
type GameState = ([Column], Player)
type Column = (Int, [Piece])
type Winner = Maybe Player --data Winner Tie | Won Player deriving (Eq, Show)

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
makeAMove mv gs =
    if elem mv validMoves
        then 
            let turn = opponent (snd gs)
                (beforeCols, (c:cs)) = splitAt (mv - 1) (fst gs)
                newCol = (mv, snd c ++ [snd gs])
                cols = beforeCols ++ [newCol] ++ cs
            in  (cols, turn)
        else error "Not a valid move!"
    where validMoves = [ fst col | col <- fst gs, length (snd col) < 6 ]

legalMoves :: GameState -> [Move]
legalMoves gs = [ fst col | col <- fst gs ]

combineMaybes :: Maybe a -> Maybe a -> Maybe a
combineMaybes (Just a) _ = Just a
combineMaybes _ (Just a) = Just a
combineMaybes _ _ = Nothing

hasWon :: GameState -> Maybe Player --Winner
hasWon (columns , _) = 
    let resultColumns = checkColumns removeIndices
        resultRows = checkRows removeIndices
        resultDiagonals = checkDiagonals removeIndices
    in resultColumns `combineMaybes` resultRows `combineMaybes` resultDiagonals
    where
    removeIndices = map snd columns

    checkColumns [] = Nothing
    checkColumns (c:cs) = checkColumn c `combineMaybes` checkColumns cs
    checkColumn (p1:p2:p3:p4:ps) = if p1 == p2 && p2 == p3 && p3 == p4 then Just p1 else checkColumn (p2:p3:p4:ps)
    checkColumn _ = Nothing


    checkRows ((p1:p1s):(p2:p2s):(p3:p3s):(p4:p4s):ps) = 
        let checkedRow = checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s)
        in if checkedRow /= Nothing then checkedRow else checkRows ((p2:p2s):(p3:p3s):(p4:p4s):ps)
    checkRows _ = Nothing

    -- checkDiagonals columns = 
    --     let resultLR = checkDiagonalsLR columns
    --         resultRL = checkDiagonalsLR (reverseColumns columns)
    --     in  if resultLR /= Nothing then resultLR else
    --         if resultRL /= Nothing then resultRL else Nothing
    checkDiagonals (c1:c2:c3:c4:cs) =    (checkFour c1 (drop 1 c2) (drop 2 c3) (drop 3 c4)) `combineMaybes` 
                                        (checkFour (drop 3 c1) (drop 2 c2) (drop 1 c3) c4) `combineMaybes`
                                        checkDiagonals (c2:c3:c4:cs)
    -- checkDiagonalsLR ((a:b:c:p1:p1s):(d:e:p2:p2s):(f:p3:p3s):(p4:p4s):ps) = 
    --     let checkedDiagonal = checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s)
    --     in if checkedDiagonal /= Nothing then checkedDiagonal else checkDiagonalsLR ((d:e:p2:p2s):(f:p3:p3s):(p4:p4s):ps)
    checkDiagonals _ = Nothing
    checkFour (p1:p1s) (p2:p2s) (p3:p3s) (p4:p4s) = if p1 == p2 && p2 == p3 && p3 == p4 then Just p1 else checkFour p1s p2s p3s p4s
    checkFour _ _ _ _ = Nothing

tgs1 :: GameState
tgs1 = ([(1,[]),(2,[]),(3,[PlayerOne, PlayerOne, PlayerOne, PlayerOne]),(4,[]),(5,[]),(6,[]),(7,[])],PlayerOne) --PlayerOne wins verticaly
