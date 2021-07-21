module Board2 (Board, Position, Piece(..), Color(..), Score, Move, newboard, move, legalmoves, moves, associations, winner, score, isgameover, toFilePath, colortomove, pieceat) where

import Piece
import Data.Array (Array, (!), (//), array, assocs)
import Data.Maybe (Maybe(..), maybe, fromJust, isJust, isNothing)
import Data.Char ()

type Position = (Char, Int)
type Move = (Position, Position)

data Board = Board (Array Position (Maybe Piece)) Score GameState Castle Color (Maybe Position)

data GameState = GameOn | GameOver (Maybe Color)
type Castle = (Bool, Bool, Bool, Bool)
type Score  = Int

newboard :: Board
newboard = stringtoboard defaultmap White

instance Show Board where
    --show (Board arr _ _ _ _ _) = concatMap (\r -> concatMap (\c -> let mp = arr ! (c, r) in if isNothing mp then "-" else show (fromJust mp)) ['A'..'H']) [8, 7..1]
    --show (Board arr _ _ _ _ _) = concatMap (\r -> concatMap (\c -> maybe "-" show (arr ! (c, r))) ['A'..'H']) [8, 7..1]
    show (Board arr _ _ _ _ _) = (['A'..'H'] >>=) . ((maybe "-" show . (arr !)) .) . flip (,) =<< [8,7..1]


move :: Board -> Move -> Board
move (Board arr s g (wl, ws, bl, bs) wb ep) (f, t) = Board newarray newscore newgamestate newcastle (opposite wb) enpassant--TODO
    where
        pie = fromJust (arr ! f)
        target = arr ! t

        newscore :: Score
        newscore = let sc = (if target == Nothing then 0 else combinedValue (fromJust target) t)
                                + (if Just t == ep then combinedValue (Pawn (opposite wb)) (fromJust ep) else 0)
                                + (positionalValue pie t) - (positionalValue pie f)
                                + (if pie == Pawn wb && (snd t == 8 || snd t == 1) then combinedValue (Queen wb) t - combinedValue (Pawn White) t else 0)
                   in  if wb == White then s + sc else s - sc
        newarray :: Array Position (Maybe Piece)
        newarray =  arr // ((if pie == Pawn wb && Just t == ep then [((fst t, snd f), Nothing)] else []) ++
                            (if pie == King wb then case (f, t) of 
                                (('E', 1), ('C', 1)) -> [(('A', 1), Nothing), (('D', 1), Just (Rook wb))]
                                (('E', 1), ('G', 1)) -> [(('H', 1), Nothing), (('F', 1), Just (Rook wb))]
                                (('E', 8), ('C', 8)) -> [(('A', 8), Nothing), (('D', 8), Just (Rook wb))]
                                (('E', 8), ('G', 8)) -> [(('H', 8), Nothing), (('F', 8), Just (Rook wb))]
                                _                    -> [] else []) ++
                             [(f, Nothing), (t, arr ! f)] ++
                             (if pie == Pawn wb && (snd t == 8 || snd t == 1) then [(t, Just (Queen wb))] else []))

        enpassant :: Maybe Position
        enpassant | pie == Pawn wb && abs (snd f - snd t) == 2 = Just (fst f, (snd f + snd t) `div` 2)
                  | otherwise = Nothing

        newcastle :: Castle
        newcastle = ( wl && f /= ('A', 1) && t /= ('A', 1) && pie /= King White && target /= Just (King White),
                      ws && f /= ('H', 1) && t /= ('H', 1) && pie /= King White && target /= Just (King White),
                      bl && f /= ('A', 8) && t /= ('A', 8) && pie /= King Black && target /= Just (King Black),
                      bs && f /= ('H', 8) && t /= ('H', 8) && pie /= King Black && target /= Just (King Black) )

        newgamestate :: GameState
        newgamestate = let dummy = Board newarray newscore g newcastle (opposite wb) enpassant
                   in  if null (legalmoves dummy) then 
                            if any (\(_, t) -> newarray ! t == Just (King (opposite wb))) (moves dummy wb)
                                then GameOver (Just wb)
                            else GameOver Nothing
                       else GameOn

moves :: Board -> Color -> [Move]
moves (Board arr _ _ (wl, ws, bl, bs) _ ep) wb = concat [ let (vs, r) = directions p 
                                                               in if p == Pawn wb then pawnmoves i vs 
                                                                  else concatMap (\d -> normalmoves i (i+++d) d r) vs 
                                                               | (i, Just p) <- assocs arr, color p == wb ]
                                                              ++ castlemoves
    where 
        pawnmoves :: Position -> [Vector] -> [Move]
        pawnmoves p [] = []
        pawnmoves p (v@(0, y):xs) | arr ! (p+++v) == Nothing = if (snd p == 2 && wb == White || snd p == 7 && wb == Black) && arr ! (p+++v+++v) == Nothing 
                                                                    then (p, p+++v) : (p, p+++v+++v) : pawnmoves p xs 
                                                               else (p, p+++v) : pawnmoves p xs
                                  | otherwise = pawnmoves p xs
        pawnmoves p (v:xs) | inbounds (p+++v) && isJust (arr ! (p+++v)) && color (fromJust (arr ! (p+++v))) /= wb || Just (p+++v) == ep = (p, p+++v) : pawnmoves p xs
                           | otherwise = pawnmoves p xs

        normalmoves :: Position -> Position -> Vector -> Bool -> [Move]
        normalmoves f p dir r | not (inbounds p) = []
                              | otherwise = case arr ! p of
                                  Nothing -> (f, p) : if r then normalmoves f (p+++dir) dir r else []
                                  Just pie | color pie == wb ->  []
                                           | otherwise -> [(f, p)]
        
        castlemoves :: [Move]
        castlemoves = map snd $ filter (\((b, c, ps), _) -> b && c == wb && all ((==) Nothing .  (!) arr) ps) [
                ((wl, White, [('B', 1), ('C', 1), ('D', 1)]), (('E', 1), ('C', 1))), 
                ((ws, White, [('F', 1), ('G', 1)]), (('E', 1), ('G', 1))),
                ((bl, Black, [('B', 8), ('C', 8), ('D', 8)]), (('E', 8), ('C', 8))),
                ((bs, Black, [('F', 8), ('G', 8)]), (('E', 8), ('G', 8)))
            ]

legalmoves :: Board -> [Move]
legalmoves b@(Board _ _ _ _ wb _) = filter (\m -> let b2@(Board arr _ _ _ bw _) = move b m in all (\c -> arr ! snd c /= Just (King wb)) (moves b2 bw)) (moves b wb)

associations :: Board -> [(Position, Maybe Piece)]
associations (Board arr _ _ _ _ _) = assocs arr

winner :: Board -> Maybe Color
winner (Board _ _ GameOn _ _ _)       = Nothing
winner (Board _ _ (GameOver c) _ _ _) = c

score :: Board -> Score 
score (Board _ s GameOn _ _ _) = s
score (Board _ _ (GameOver Nothing) _ _ _) = 0
score (Board _ _ (GameOver (Just White)) _ _ _) = maxBound
score (Board _ _ (GameOver (Just Black)) _ _ _) = minBound

colortomove :: Board -> Color
colortomove (Board _ _ _ _ wb _) = wb

isgameover :: Board -> Bool
isgameover (Board _ _ GameOn _ _ _) = False
isgameover (Board _ _ (GameOver _) _ _ _) = True

pieceat :: Board -> Position -> Maybe Piece
pieceat (Board arr _ _ _ _ _) p = arr ! p

inbounds :: Position -> Bool
inbounds (c, r) = r >= 1 && r <= 8 && c >= 'A' && c <= 'H'

(+++) :: Position -> Vector -> Position
(+++) (c, r) (0, b)  = (c, r+b)
(+++) (c, r) (1, b)  = (succ c, r+b)
(+++) (c, r) (2, b)  = (succ (succ c), r+b)
(+++) (c, r) (-2, b) = (pred (pred c), r+b)
(+++) (c, r) (-1, b) = (pred c, r+b)
(+++) a b = error (show a ++ ", " ++ show b) --TODO

defaultmap :: String
defaultmap = 
    "rnbqkbnr" ++
    "pppppppp" ++
    "--------" ++
    "--------" ++
    "--------" ++
    "--------" ++
    "PPPPPPPP" ++
    "RNBQKBNR"

castlemap = 
    "r---k--r" ++
    "pppppppp" ++
    "--------" ++
    "--------" ++
    "--------" ++
    "--------" ++
    "PPPPPPPP" ++
    "R---K--R"

evansmap = 
    "r-bq--nr" ++
    "pppp-kpp" ++
    "--n-----" ++
    "--b-----" ++
    "----P---" ++
    "--p--N--" ++
    "P----PPP" ++
    "RNBQ-RK-"

stringtoboard :: String -> Color -> Board
stringtoboard s c = Board arr 0 GameOn (True, True, True, True) c Nothing
    where arr = array (('A', 1), ('H', 8)) $ zip [ (c, r) | r<-[8, 7..1], c<-['A'..'H']] $ map piece s
