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