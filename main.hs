data Cell = Confirmed Int
          | Perhaps [Int]
          deriving (Show, Eq)

type Board = [Cell]
