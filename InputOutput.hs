module InputOutput where

import Text.Read
import ConnectingFour

prettyPrint :: GameState -> IO ()
prettyPrint gs = putStr $ prettyShowGame gs

prettyShowGame :: GameState -> String
prettyShowGame (cols, turn) =
    unlines ([""] ++ rows ++ [divider, colNumbers, "", turnText] ++ [""])
    where
        showCell :: Maybe Piece -> String
        showCell (Just PlayerOne) = "R"
        showCell (Just PlayerTwo) = "Y"
        showCell Nothing = "·"
        
        getPieceAt :: Int -> Column -> Maybe Piece
        getPieceAt rowIdx (_, pieces) =
            if rowIdx < length pieces
                then Just (pieces !! rowIdx) -- yeah yeah whatever i wanna use it so im using it
                else Nothing -- bigger than thing, bad.
        
        renderRow :: Int -> String
        renderRow rowIdx =
            "| " ++ unwords [showCell (getPieceAt rowIdx col) | col <- cols] ++ " |"
        
        rows = [renderRow r | r <- [5, 4, 3, 2, 1, 0]]
        divider = "+---------------+"
        colNumbers = "  1 2 3 4 5 6 7  "
        turnText = "Current Turn: " ++ show turn

playGame :: GameState -> IO ()
playGame gs = do
    prettyPrint gs
    putStrLn "Where would you like to move?"
    mv <- getLine
    case readMaybe mv :: Maybe Int of
        Just n  -> 
            if elem n (legalMoves gs)
                then
                    let newGS = makeAMove n gs
                    in  if hasWon newGS == Just PlayerOne
                            then prettyPrint newGS >> putStrLn "Player One wins :D"
                        else if hasWon newGS == Just PlayerTwo
                            then prettyPrint newGS >> putStrLn "Player Two wins :D"
                        else if fullBoard newGS == True
                            then prettyPrint newGS >> putStrLn "Tie, no player wins :("
                        else playGame newGS
                else putStrLn "That is not a valid move >:(" >> playGame gs
        Nothing -> playGame gs
            -- case readMaybe n :: Maybe Char of
            --     Just 'l' -> show (legalMoves gs)
            --     Just 'q' -> putStrLn "Game quit!"
            --     Nothing -> playGame gs
