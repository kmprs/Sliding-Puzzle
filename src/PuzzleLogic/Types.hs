module PuzzleLogic.Types where


-- sliding puzzle data structures
type Path     = [State]
type State    = [Row]
type Row      = [Int]
type Position = (Int, Int)


directions :: [Position]
directions = [(1, 0), (0, 1), ((-1), 0), (0, (-1))]

