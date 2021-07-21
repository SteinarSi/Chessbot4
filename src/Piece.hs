module Piece (Piece(..), Color(..), Vector, piece, opposite, color, directions, positionalValue, combinedValue, toFilePath, showP) where

import Data.Array
import qualified Data.Array.IArray as A
import Control.Exception
import Data.Char

data Piece = Pawn Color
           | Rook Color 
           | Knight Color 
           | Bishop Color 
           | Queen Color 
           | King Color
    deriving (Read, Eq, Ord)

type Vector = (Int, Int)

data Color = White | Black deriving (Show, Read, Eq, Ord)

opposite :: Color -> Color
opposite White = Black
opposite Black = White

inherentValue :: Piece -> Int
inherentValue (Pawn c)   = 100
inherentValue (Knight c) = 320
inherentValue (Bishop c) = 325
inherentValue (Rook c)   = 500
inherentValue (Queen c)  = 975
inherentValue (King c)   = 32767

positionalValue :: Piece -> (Char, Int) -> Int
positionalValue (Pawn White)   pos = pawnPosWhite ! pos
positionalValue (Pawn Black)   pos = pawnPosBlack ! pos
positionalValue (Rook White)   pos = rookPosWhite ! pos
positionalValue (Rook Black)   pos = rookPosBlack ! pos
positionalValue (Knight White) pos = knightPosWhite ! pos
positionalValue (Knight Black) pos = knightPosBlack ! pos
positionalValue (Bishop White) pos = bishopPosWhite ! pos
positionalValue (Bishop Black) pos = bishopPosBlack ! pos
positionalValue (Queen White)  pos = queenPosWhite ! pos
positionalValue (Queen Black)  pos = queenPosBlack ! pos
positionalValue (King White)   pos = kingPosWhite ! pos
positionalValue (King Black)   pos = kingPosBlack ! pos

combinedValue :: Piece -> (Char, Int) -> Int
combinedValue p pos = inherentValue p + positionalValue p pos

color :: Piece -> Color
color (Pawn White)   = White
color (Pawn Black)   = Black
color (Rook White)   = White
color (Rook Black)   = Black
color (Knight White) = White
color (Knight Black) = Black
color (Bishop White) = White
color (Bishop Black) = Black
color (Queen White)  = White
color (Queen Black)  = Black
color (King White)   = White
color (King Black)   = Black

directions :: Piece -> ([Vector], Bool)
directions (Pawn White) = ([(-1,  1), (0,  1), (1,  1)], False)
directions (Pawn Black) = ([(-1, -1), (0, -1), (1, -1)], False)
directions (Rook c)     = ([(1, 0), (-1, 0), (0, 1), (0, -1)], True)
directions (Knight c)   = ([(-1, 2), (1, 2), (-1, -2), (1, -2), (2, -1), (2, 1), (-2, -1), (-2, 1)], False)
directions (Bishop c)   = ([(1, 1), (1, -1), (-1, 1), (-1, -1)], True)
directions (Queen c)    = ([(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)], True)
directions (King c)     = ([(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)], False)

instance Show Piece where
  show (Pawn   White) = "P"
  show (Pawn   Black) = "p"
  show (Rook   White) = "R"
  show (Rook   Black) = "r"
  show (Knight White) = "N"
  show (Knight Black) = "n"
  show (Bishop White) = "B"
  show (Bishop Black) = "b"
  show (Queen  White) = "Q"
  show (Queen  Black) = "q"
  show (King   White) = "K"
  show (King   Black) = "k"

showP :: Maybe Piece -> Char
showP Nothing = '-'
showP (Just p) = head (show p)

toFilePath :: Piece -> FilePath
toFilePath p = "../resources/" ++ map toUpper (show p) ++ if color p == White then "w.png" else "b.png"

piece :: Char -> Maybe Piece
piece 'P' = (Just (Pawn White))   
piece 'p' = (Just (Pawn Black))   
piece 'R' = (Just (Rook White))   
piece 'r' = (Just (Rook Black))   
piece 'N' = (Just (Knight White)) 
piece 'n' = (Just (Knight Black)) 
piece 'B' = (Just (Bishop White)) 
piece 'b' = (Just (Bishop Black)) 
piece 'Q' = (Just (Queen White))  
piece 'q' = (Just (Queen Black))  
piece 'K' = (Just (King White))   
piece 'k' = (Just (King Black)) 
piece  c  = Nothing

pawnPosWhite :: Array (Char, Int) Int
pawnPosWhite = arrW pawnPos

pawnPosBlack :: Array (Char, Int) Int
pawnPosBlack = arrB pawnPos

pawnPos :: [Int]
pawnPos = [ 0,   0,   0,   0,   0,   0,   0,   0 ,
            200, 200, 200, 200, 200, 200, 200, 200 ,
            100, 100, 100, 100, 100, 100, 100, 100 ,
            40,  40,  90, 100, 100,  90,  40,  40 ,
            20,  20,  20, 100, 150,  20,  20,  20 ,
            2,   4,   0,  15,   4,   0,   4,   2 ,
            -10, -10, -10, -20, -35, -10, -10, -10 ,
            0,   0,   0,   0,   0,   0,   0,   0 ]

rookPosWhite :: Array (Char, Int) Int
rookPosWhite = arrW rookPos

rookPosBlack :: Array (Char, Int) Int
rookPosBlack = arrB rookPos

rookPos :: [Int]
rookPos = [   0,   0,  0,  0,  0,  0,  0,   0 ,
              10, 20, 20, 20, 20, 20, 20,  10 ,
             -10,  0,  0,  0,  0,  0,  0, -10 ,
             -10,  0,  0,  0,  0,  0,  0, -10 ,
             -10,  0,  0,  0,  0,  0,  0, -10 ,
             -10,  0,  0,  0,  0,  0,  0, -10 , 
             -10,  0,  0,  0,  0,  0,  0, -10 ,
             -30, 30, 40, 10, 10,  0,  0, -30 ]


knightPosWhite :: Array (Char, Int) Int
knightPosWhite = arrW knightPos

knightPosBlack :: Array (Char, Int) Int
knightPosBlack = arrB knightPos

knightPos :: [Int]
knightPos = [-20, -80, -60, -60, -60, -60, -80, -20 ,
             -80, -40,   0,   0,   0,   0, -40, -80 ,
             -60,   0,  20,  30,  30,  20,   0, -60 , 
             -60,  10,  30,  40,  40,  30,  10, -60 ,
             -60,   0,  30,  40,  40,  30,   0, -60 , 
             -60,  10,  20,  30,  30,  30,   1, -60 ,
             -80, -40,   0,  10,  10,   0,  -4, -80 ,
             -20, -80, -60, -60, -60, -60, -80, -20 ]

bishopPosWhite :: Array (Char, Int) Int
bishopPosWhite = arrW bishopPos

bishopPosBlack :: Array (Char, Int) Int
bishopPosBlack = arrB bishopPos

bishopPos :: [Int]
bishopPos = [-40, -20, -20, -20, -20, -20, -20, -40 ,
             -20,   0,   0,   0,   0,   0,   0, -20 ,
             -20,   0,  10,  20,  20,  10,   0, -20 ,
             -20,  10,  10,  20,  20,  10,  10, -20 ,
             -20,   0,  20,  20,  20,  20,   0, -20 ,
             -20,  20,  20,  20,  20,  20,  20, -20 ,
             -20,  10,   0,   0,   0,   0,  10, -20 ,
             -40, -20, -20, -20, -20, -20, -20, -40 ]

queenPosWhite :: Array (Char, Int) Int
queenPosWhite = arrW queenPos

queenPosBlack :: Array (Char, Int) Int
queenPosBlack = arrB queenPos

queenPos :: [Int]
queenPos = [ -40, -20, -20, -10, -10, -20, -20, -40 ,
             -20,   0,   0,   0,   0,   0,   0, -20 ,
             -20,   0,  10,  10,  10,  10,   0, -20 ,
             -10,   0,  10,  10,  10,  10,   0, -10 ,
               0,   0,  10,  10,  10,  10,   0, -10 ,
             -20,  10,  10,  10,  10,  10,   0, -20 ,
             -20,   0,  10,   0,   0,   0,   0, -20 ,
             -40, -20, -20, -10, -10, -20, -20, -40 ]

kingPosWhite :: Array (Char, Int) Int
kingPosWhite = arrW kingPos

kingPosBlack :: Array (Char, Int) Int
kingPosBlack = arrB kingPos

kingPos :: [Int]
kingPos = [  -60, -80, -80, -2, -20, -80, -80, -60 ,
             -60, -80, -80, -2, -20, -80, -80, -60 ,
             -60, -80, -80, -2, -20, -80, -80, -60 ,
             -60, -80, -80, -2, -20, -80, -80, -60 ,
             -40, -60, -60, -8, -80, -60, -60, -40 ,
             -20, -40, -40, -40,-40, -40, -40, -20 ,
              40,  40,   0,   0,  0,   0,  40,  40 ,
              40,  60,  20,   0,  0,  20,  60,  40 ]

arrW :: [Int] -> Array (Char, Int) Int
arrW = array (('A', 1), ('H', 8)) . zip [(c, r) | r<-[8, 7..1], c<-['A'..'H']]

arrB :: [Int] -> Array (Char, Int) Int
arrB = array (('A', 1), ('H', 8)) . zip [(c, r) | r<-[1..8], c<-['A'..'H']]