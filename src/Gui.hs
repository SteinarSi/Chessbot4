module Gui (gui) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Color hiding (Color, White, Black, color)
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(MouseButton), MouseButton(..), KeyState(Down))

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Control.Monad

import Board2
import Toolbox

type GUI = (Board, M.Map Piece Picture, Maybe Position)

gui :: IO ()
gui = piecedict >>= \pd -> playIO FullScreen (greyN 0.60) 20 (newboard, pd, Nothing) draw handleEvent callAI


draw :: GUI -> IO Picture
draw (b, pd, from) = return $ pictures [squares, selected, pieces]
    where 
          squares :: Picture
          squares = pictures $ map square [(c, r) | c<-['A'..'H'], r<-[1..8]]

          square :: Position -> Picture
          square (c, r) = let (x, y) = fromPosition (c, r)
                              wb = if elem c "ACEG" && odd r || elem c "BDFH" && even r then greyN 0.2 else greyN 0.75
                          in  color wb $ polygon [(x, y), (x+stepsize, y), (x+stepsize, y+stepsize), (x, y+stepsize)]
        
          pieces :: Picture
          pieces = pictures $ for (associations b) (\(pos, pie) -> 
              if isNothing pie then blank 
              else let (x, y) = fromPosition pos 
                   in  translate (x+stepsize/2) (y+stepsize/2) (scale 0.85 0.85 (fromJust (M.lookup (fromJust pie) pd))))        

          selected :: Picture
          selected = case from of 
              Nothing -> blank
              Just from -> let (x, y) = fromPosition from
                           in  color (greyN 0.5) $ polygon [(x, y), (x+stepsize, y), (x+stepsize, y+stepsize), (x, y+stepsize)]

piecedict :: IO (M.Map Piece Picture)
piecedict = return . foldr (uncurry M.insert) M.empty =<< mapM (\p -> loadJuicyPNG (toFilePath p) >>= return . (,) p . fromJust) [
    Rook White, Rook Black, Pawn White, Pawn Black, Knight White, Knight Black,
    Bishop White, Bishop Black, King White, King Black, Queen White, Queen Black]


handleEvent :: Event -> GUI -> IO GUI
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (b, pd, Nothing) = return (b, pd, toPosition (x, y))
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (b, pd, Just from) = case toPosition (x, y) of
                                                                                       Nothing -> return (b, pd, Just from)
                                                                                       Just to -> if elem (from, to) (legalmoves b) then return (move b (from, to), pd, Nothing)
                                                                                                  else return (b, pd, Nothing)
handleEvent _ g = return g

callAI :: Float -> GUI -> IO GUI
callAI _ (b, pd, p) | isgameover b = print (winner b) >> return (b, pd, p)
                    | otherwise = return (b, pd, p)


half :: Float
half = 375

stepsize :: Float
stepsize = half / 4

intervals :: [Float]
intervals = linspace 9 (-half, half)

fromPosition :: Position -> (Float, Float)
fromPosition (c, r) = (-half + (fromIntegral (ord c - 65))*stepsize, -half + (fromIntegral r-1)*stepsize)

toPosition :: (Float, Float) -> Maybe Position
toPosition (x, y) = bisect x intervals 'A' >>= \c -> bisect y intervals 1 >>= \r-> Just (c, r)
