-- custom datas and type aliases!
data Player = PlayerOne | PlayerTwo | NoPlayer deriving (Eq, Show)
type Move = Integer
--type ValidMoves = [Move]
type Piece = Player
type GameState = ([Column], Player)
type Column = (Int, [Piece])
type Winner = Maybe Player 


--Initial GameState
initialColumns = [(x, [(replicate 6 NoPlayer)]) | x <- [1..7]]
initialGS = (initialColumns, PlayerOne)

--takes a move, checks if it's valid, and returns the new GameState
makeAMove :: Move -> GameState -> GameState
makeAMove mv gs = 
    if elem mv validMoves
        then 
            let thisTurn = snd gs
                if thisTurn = PlayerOne then turn = PlayerTwo else turn = PlayerOne
                
            in  (cols, turn)
        else error "Not a valid move!"
    where validMoves = [ fst col | col <- fst gs ]
        
        --    let validMoves = []

-- makeAMove mv mvs gs =
--     case lookup mv x of
--         Just int -> 
--         Nothing -> 


--     if elem mv mvs
--         then 
--             let thisPlayer = snd gs

--         else error "Not a valid move!"

--prints the current GameState to the terminal
showGameState :: GameState -> String
showGameState gs =
    let grid = [ p | (n, [p]) <- (fst gs)]
    in  map showTranslator grid

showTranslator :: Player -> Char
showTranslator PlayerOne = 'Y'
showTranslator PlayerTwo = 'R'
showTranslator _ = '·'
