module PuzzleLogic.Types where


-- sliding puzzle data structures
type Path     = [State]
type State    = [Row]
type Row      = [Int]
type Position = (Int, Int)

