module Color (Color(White, Black), getOppositeColor) where

data Color = White | Black deriving (Show, Read, Eq)

getOppositeColor :: Color -> Color
getOppositeColor White = Black
getOppositeColor Black = White



