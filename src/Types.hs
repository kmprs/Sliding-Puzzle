module Types where

type Path     = [State]
type State    = [Row]
type Row      = [Int]
type Position = (Int, Int)
type ParentChild = (State, State) -- (parent child)  

