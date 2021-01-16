module Piece (Piece(..), isOppositeColor, getColor, getDirections, shwP, getPositionalValue, getCombinedValue) where

import Color (Color(..), getOppositeColor)
import Data.Array hiding ((!))
import qualified Data.Array.IArray as A
import Control.Exception

data Piece = Pawn Color
           | Rook Color 
           | Knight Color 
           | Bishop Color 
           | Queen Color 
           | King Color
    deriving (Show, Read, Eq)

arr ! index = mapException (addErrorInfo (" ! "++show index)) $ arr A.! index
addErrorInfo info (ErrorCall str) = ErrorCall (str++":"++info)

getInherentValue :: Piece -> Int
getInherentValue (Pawn c)   = 100
getInherentValue (Knight c) = 320
getInherentValue (Bishop c) = 325
getInherentValue (Rook c)   = 500
getInherentValue (Queen c)  = 975
getInherentValue (King c)   = 32767

getPositionalValue :: Piece -> (Char, Int) -> Int
getPositionalValue (Pawn White)   pos = pawnPosWhite ! pos
getPositionalValue (Pawn Black)   pos = pawnPosBlack ! pos
getPositionalValue (Rook White)   pos = rookPosWhite ! pos
getPositionalValue (Rook Black)   pos = rookPosBlack ! pos
getPositionalValue (Knight White) pos = knightPosWhite ! pos
getPositionalValue (Knight Black) pos = knightPosBlack ! pos
getPositionalValue (Bishop White) pos = bishopPosWhite ! pos
getPositionalValue (Bishop Black) pos = bishopPosBlack ! pos
getPositionalValue (Queen White)  pos = queenPosWhite ! pos
getPositionalValue (Queen Black)  pos = queenPosBlack ! pos
getPositionalValue (King White)   pos = kingPosWhite ! pos
getPositionalValue (King Black)   pos = kingPosBlack ! pos

getCombinedValue :: Piece -> (Char, Int) -> Int
getCombinedValue p pos = getInherentValue p + getPositionalValue p pos

isOppositeColor :: Piece -> Piece -> Bool
isOppositeColor a b = getColor a /= getColor b

getColor :: Piece -> Color
getColor (Pawn White)   = White
getColor (Pawn Black)   = Black
getColor (Rook White)   = White
getColor (Rook Black)   = Black
getColor (Knight White) = White
getColor (Knight Black) = Black
getColor (Bishop White) = White
getColor (Bishop Black) = Black
getColor (Queen White)  = White
getColor (Queen Black)  = Black
getColor (King White)   = White
getColor (King Black)   = Black

getDirections :: Piece -> ([(Int, Int)], Bool)
getDirections (Pawn White) = ([(-1,  1), (0,  1), (1,  1)], False)
getDirections (Pawn Black) = ([(-1, -1), (0, -1), (1, -1)], False)
getDirections (Rook c)     = ([(1, 0), (-1, 0), (0, 1), (0, -1)], True)
getDirections (Knight c)   = ([(-1, 2), (1, 2), (-1, -2), (1, -2), (2, -1), (2, 1), (-2, -1), (-2, 1)], False)
getDirections (Bishop c)   = ([(1, 1), (1, -1), (-1, 1), (-1, -1)], True)
getDirections (Queen c)    = ([(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)], True)
getDirections (King c)     = ([(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)], False)

shwP :: Maybe Piece -> Char
shwP Nothing               = '-'
shwP (Just (Pawn White))   = 'P'
shwP (Just (Pawn Black))   = 'p'
shwP (Just (Rook White))   = 'R'
shwP (Just (Rook Black))   = 'r'
shwP (Just (Knight White)) = 'N'
shwP (Just (Knight Black)) = 'n'
shwP (Just (Bishop White)) = 'B'
shwP (Just (Bishop Black)) = 'b'
shwP (Just (Queen White))  = 'Q'
shwP (Just (Queen Black))  = 'q'
shwP (Just (King White))   = 'K'
shwP (Just (King Black))   = 'k'

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