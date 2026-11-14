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