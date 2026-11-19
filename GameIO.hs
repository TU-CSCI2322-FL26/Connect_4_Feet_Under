module GameIO where
import ConnectingFour

{-

store the gamestate with 1-7 for the column and then 1 or 2 for the pieces
top value is the current player

1
1 1 2 1
2 2 1
3
4 1 1 2 2
5 2
6 1
7

-}

--story 13:
readGame :: String -> GameState
readGame input =
    case lines input of
        (playerLine : colLines) ->
            let currentPlayer = parsePlayer playerLine
                columns = map parseColumn colLines
            in (columns, currentPLayer)
        [] -> error "no input for readGame"

parsePlayer :: String -> Player
parsePlayer "1" = PlayerOne
parsePlayer "2" = PlayerTwo
parsePlayer str = error ("invalid player:" ++ str) -- oo im fancy with it

parseColumn :: String -> Column
parseColumn line =
    case words line of
        (colStr : pieceStrs) ->
            let colNum = read colStr
                pieces = map parsePlayer pieceStrs
            in (colNum, pieces)
        [] -> error "empty column line for readGame"