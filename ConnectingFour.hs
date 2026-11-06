-- custom datas and type aliases!
data Player = PlayerOne | PlayerTwo deriving (Eq, Show)
type Move = Integer
type Piece = Player
type GameState = ([Column], Player)
type Column = (Int, [Piece])
type Winner = Maybe Player 