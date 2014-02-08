module Direction where

import Linear.V2

data Direction = DDown | DRight | DUp | DLeft deriving (Show, Eq)
type DirectionChange = Maybe Direction

opposite :: Direction -> Direction -> Bool
opposite a b = dirToPair a + dirToPair b == V2 0 0

dirToPair :: Direction -> V2 Int
dirToPair DDown  = (V2   0    1)
dirToPair DRight = (V2   1    0)
dirToPair DUp    = (V2   0  (-1))
dirToPair DLeft  = (V2 (-1)   0)
