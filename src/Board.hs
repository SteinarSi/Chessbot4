module Board (BoardST, BoardPure, Board, Move, Piece, Color, newboard, moveST, movePure, goBackST, 
    goBackPure, getMovesST, getMovesPure, isLegalST, isLegalPure, getScoreST, getScorePure, immedeateValue, 
    orderMoves, boardInfo, toBoardST, toBoardPure, getColorToMoveST, getLegalMovesPure, getLegalMovesST, shwST) where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, runSTArray, writeArray, freeze, thaw, newArray)
import qualified Data.Array.ST as AST
import Control.Exception
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.Array (Array)
import qualified Data.Array as A
import Data.Char (chr, ord)
import Data.List (sort)
import Data.Maybe (fromJust, isNothing, isJust)
import Control.Monad (forM, forM_, when, filterM)
import Control.Monad.Loops (allM, anyM)
import Color (Color(..), getOppositeColor)
import Piece (Piece(..), isOppositeColor, shwP, getColor, getDirections, getPositionalValue, getCombinedValue)
import Debug.Trace




arr ! index = mapException (addErrorInfo (" ! "++show index)) $ arr A.! index
readArray arr i = mapException (addErrorInfo (" ! "++show i)) $ AST.readArray arr i
addErrorInfo info (ErrorCall str) = ErrorCall (str++":"++info)



--Objektorientert. Husker alt som har skjedd.                Moves        Scores           Deaths                          wLeft wRight bLeft bRight     Counter     Hvem sin tur
type BoardST s = (STArray s (Char, Int) (Maybe Piece), STRef s [Move], STRef s [Int], STRef s [(Int, Pos, Piece)], STRef s [(Bool, Bool, Bool, Bool)], STRef s Int, STRef s Color)

--Husker alt som har skjedd, men er også immutable.
type BoardPure = (Board, [Move], [Int], [(Int, Pos, Piece)], [(Bool, Bool, Bool, Bool)], Int, Color)

--Husker ingen informasjon annet enn hvor brikkene står.
type Board = Array (Char, Int) (Maybe Piece)

type Pos = (Char, Int)

type Move = (Pos, Pos)

newboard :: BoardPure
newboard = ( runSTArray $ do
    board <- newArray (('A', 1), ('H', 8)) Nothing
    mapM_ (\(c, p) -> writeArray board (c, 1) (Just p)) $ backrank White
    mapM_ (\c -> writeArray board (c, 2) (Just (Pawn White))) ['A'..'H']
    mapM_ (\(c, p) -> writeArray board (c, 8) (Just p)) $ backrank Black
    mapM_ (\c -> writeArray board (c, 7) (Just (Pawn Black))) ['A'..'H'] 
    return board
    , [(('E', 1), ('E', 2))], [0], [], [(True, True, True, True)], 0, White)
    where
        backrank :: Color -> [(Char, Piece)]
        backrank c = [('A', Rook c), ('B', Knight c), ('C', Bishop c), ('D', Queen c), ('E', King c), ('F', Bishop c), ('G', Knight c), ('H', Rook c)]

toBoardST :: BoardPure -> ST s (BoardST s)
toBoardST (arr, moves, scores, deaths, castles, counter, color) = do
    arrST     <- thaw arr
    movesST   <- newSTRef moves
    scoresST  <- newSTRef scores
    deathsST  <- newSTRef deaths
    castlesST <- newSTRef castles
    counterST <- newSTRef counter
    colorST   <- newSTRef color
    return (arrST, movesST, scoresST, deathsST, castlesST, counterST, colorST)

toBoardPure :: BoardST s -> ST s BoardPure
toBoardPure (arrST, movesST, scoresST, deathsST, castlesST, counterST, colorST) = do
    arr     <- freeze arrST
    moves   <- readSTRef movesST
    scores  <- readSTRef scoresST
    deaths  <- readSTRef deathsST
    castles <- readSTRef castlesST
    counter <- readSTRef counterST
    color   <- readSTRef colorST
    return (arr, moves, scores, deaths, castles, counter, color)

goBackPure :: BoardPure -> BoardPure
goBackPure board = runST $ do
    boardST <- toBoardST board
    goBackST boardST
    toBoardPure boardST >>= return

goBackST :: BoardST s -> ST s ()
goBackST (arrST, movesST, scoresST, deathsST, castlesST, counterST, colorST) = do
    modifySTRef' counterST (+(-1))
    counter <- readSTRef counterST
    when (counter < 0) (error "Can't go further back.")

    (from, to):moves <- readSTRef movesST
    p <- readArray arrST to
    writeArray arrST from p
    writeArray arrST to Nothing

    modifySTRef' movesST tail
    modifySTRef' scoresST tail
    modifySTRef' castlesST tail
    modifySTRef' colorST getOppositeColor

    removeCastle (from, to) arrST

    deaths <- readSTRef deathsST
    respawn deaths counter arrST deathsST

    where
        respawn :: [(Int, Pos, Piece)] -> Int -> STArray s (Char, Int) (Maybe Piece) -> STRef s [(Int, Pos, Piece)] -> ST s ()
        respawn [] _ _ _ = return ()
        respawn ((t, pos, pie):ps) c arrST deathsST | t /= c = return ()           
                                                    | otherwise = do
                                                                writeArray arrST pos (Just pie) 
                                                                modifySTRef' deathsST tail
                                                                respawn ps c arrST deathsST

        removeCastle :: Move -> STArray s (Char, Int) (Maybe Piece) -> ST s ()
        removeCastle move arrST = do
            forM_ [(White, (('E', 1), ('C', 1)), (('A', 1), ('D', 1))),     --Hvit rokerte langt
                   (White, (('E', 1), ('G', 1)), (('H', 1), ('F', 1))),     --Hvit rokerte kort
                   (Black, (('E', 8), ('C', 8)), (('A', 8), ('D', 8))),     --Svart rokerte langt
                   (Black, (('E', 8), ('G', 8)), (('H', 8), ('F', 8)))      --Svart rokerte kort
                ] $ \(c, kmove, (frR, toR)) -> when (move == kmove) (writeArray arrST frR (Just (Rook c)) >> writeArray arrST toR Nothing)


moveST :: BoardST s -> Move -> ST s ()
moveST (arrST, movesST, scoresST, deathsST, castleST, counterST, colorST) (from, to) = do
    
    s:scores <- readSTRef scoresST
    counter <- readSTRef counterST
    c:castles <- readSTRef castleST

    modifySTRef' movesST ((from, to) : )
    updateCastle c castleST
    value <- immedeateValue (arrST, movesST, scoresST, deathsST, castleST, counterST, colorST) (from, to)
    modifySTRef' scoresST ((value + s) :)
    modifySTRef' colorST getOppositeColor

    p <- readArray arrST from
    t <- readArray arrST to
    when (isJust t) (modifySTRef' deathsST ((counter, to, fromJust t) : )) --Dreper fiendtlig brikke
    when (isNothing t && (p == Just (Pawn White) || p == Just (Pawn Black)) && (fst from /= fst to)) (do --Dreper bonden når den blir forbigått
        writeArray arrST (fst to, snd from) Nothing 
        modifySTRef' deathsST ((counter, (fst to, snd from), Pawn Black) : )
        modifySTRef' scoresST (\l -> (head l + 130) : tail l)        --Gir poeng for en passant
        )
    when (p == Just (King White) || p == Just (King Black)) (moveCastle arrST)

    writeArray arrST from Nothing
    writeArray arrST to p

    --Promoterer bønder. Da blir bondene lagt til i listen over døde brikker, så vi kan respawne dem etterpå når vi tar trekket tilbake.
    when (p == Just (Pawn White) && snd to == 8) (writeArray arrST to (Just (Queen White)) >> modifySTRef' deathsST ((counter, from, Pawn White) : ))
    when (p == Just (Pawn Black) && snd to == 1) (writeArray arrST to (Just (Queen Black)) >> modifySTRef' deathsST ((counter, from, Pawn Black) : ))

    modifySTRef' counterST (1+)

    where   
        moveCastle :: STArray s (Char, Int) (Maybe Piece) -> ST s ()
        moveCastle arrST = do
            forM_ [(White, (('E', 1), ('C', 1)), (('A', 1), ('D', 1))),     --Hvit rokerer langt
                   (White, (('E', 1), ('G', 1)), (('H', 1), ('F', 1))),     --Hvit rokerer kort
                   (Black, (('E', 8), ('C', 8)), (('A', 8), ('D', 8))),     --Svart rokerer langt
                   (Black, (('E', 8), ('G', 8)), (('H', 8), ('F', 8)))      --Svart rokerer kort
                ] $ \(c, move, (frR, toR)) -> when ((from, to) == move) 
                    (writeArray arrST frR Nothing >> writeArray arrST toR (Just (Rook c)))

        updateCastle :: (Bool, Bool, Bool, Bool) -> STRef s [(Bool, Bool, Bool, Bool)] -> ST s ()
        updateCastle (wL, wR, bL, bR) castleST = modifySTRef' castleST (
                    (wL && from /= ('A', 1) && to /= ('A', 1) && from /= ('E', 1),
                     wR && from /= ('H', 1) && to /= ('H', 1) && from /= ('E', 1),
                     bL && from /= ('A', 8) && to /= ('A', 8) && from /= ('E', 8),
                     bR && from /= ('H', 8) && to /= ('H', 8) && from /= ('E', 8)
                    ) : )


immedeateValue :: BoardST s -> Move -> ST s Int
immedeateValue (arrST, movesST, scoresST, deathsST, castlesST, counterST, colorST) ((frC, frR), (toC, toR)) = do
    color <- readSTRef colorST
    p <- readArray arrST (frC, frR) >>= return . fromJust
    t <- readArray arrST (toC, toR)
    ret <- newSTRef $ getPositionalValue p (toC, toR) - getPositionalValue p (frC, frR)

    when (isJust t) (modifySTRef' ret (+ (getCombinedValue (fromJust t) (toC, toR))))    --Når en brikke blir tatt
    when ((p == King color) && abs (ord frC - ord toC) == 2) (modifySTRef' ret (+50))    --Når noen rokerer
    when (p == Pawn color && (toR == 8 || toR == 0)) 
        (modifySTRef' ret ((getCombinedValue (Queen color) (toC, toR) - getCombinedValue (Pawn color) (toC, toR)) + ))    --Queening

    readSTRef ret >>= \r -> if color == White then return r
                            else return (-r)

orderMoves :: BoardST s -> [Move] -> ST s [Move]
orderMoves board moves = do
    values <- forM moves $ \move -> do
        score <- immedeateValue board move
        return (score, move)
    return (map snd (sort values))

movePure :: BoardPure -> Move -> BoardPure
movePure board move = runST $ do
    boardST <- toBoardST board
    moveST boardST move
    toBoardPure boardST >>= return

checkCheckMate :: BoardST s -> ST s Bool
checkCheckMate boardST = undefined

isLegalST :: BoardST s -> Move -> ST s Bool
isLegalST boardST move = getLegalMovesST boardST >>= return . elem move

isLegalPure :: BoardPure -> Move -> Bool
isLegalPure board move = elem move $ getLegalMovesPure board
    
getLegalMovesPure :: BoardPure -> [Move]
getLegalMovesPure board = runST $ toBoardST board >>= getLegalMovesST >>= return

getLegalMovesST :: BoardST s -> ST s [Move]
getLegalMovesST boardST = do
    arrST <- justArrST boardST
    moves <- getMovesST boardST
    color <- getColorToMoveST boardST
    legals <- (flip filterM) moves $ \move -> do
        moveST boardST move
        counters <- getMovesST boardST
        hit <- (flip allM) counters $ \counter -> readArray arrST (snd counter) >>= return . (/= (Just (King color)))
        goBackST boardST
        return hit
    return legals

getMovesPure :: BoardPure -> [Move]
getMovesPure board = runST $ toBoardST board >>= getMovesST >>= return

getMovesST :: BoardST s -> ST s [Move]
getMovesST (arrST, movesST, scoresST, deathsST, castlesST, counterST, colorST) = do
    color <- readSTRef colorST
    castle:cs <- readSTRef castlesST
    prev:mvs <- readSTRef movesST

    ret <- newSTRef []
    getCastleMoves color castle ret arrST
    forM_ [(c, r) | r<-[1..8], c<-['A'..'H']] $ \pos -> do
        p <- readArray arrST pos
        if isNothing p || getColor (fromJust p) /= color then return ()
        else let (dirs, run) = getDirections (fromJust p)
             in  forM_ dirs $ \dir -> getMovesST'' (fromJust p) pos pos color prev ret dir run arrST
    
    readSTRef ret >>= return
    where 
        getCastleMoves :: Color -> (Bool, Bool, Bool, Bool) -> STRef s [Move] -> STArray s (Char, Int) (Maybe Piece) -> ST s ()
        getCastleMoves color (wL, wR, bL, bR) ret arr =
            forM_ [(White, wL, [('B', 1), ('C', 1), ('D', 1)], (('E', 1), ('C', 1))),
                   (White, wR, [('F', 1), ('G', 1)],           (('E', 1), ('G', 1))),
                   (Black, bL, [('B', 8), ('C', 8), ('D', 8)], (('E', 8), ('C', 8))),
                   (Black, bR, [('F', 8), ('G', 8)],           (('E', 8), ('G', 8)))
            ] $ \(c, castle, list, move) -> --(Nåverænde farge, om den kan rokere til den siden, liste over ruter som må være tomme, rokadetrekket som skal sjekkes.)
                when (color == c && castle) (mapM (readArray arr) list >>= \ps -> when (all isNothing ps) (modifySTRef' ret (move : )))

        getMovesST'' :: Piece -> Pos -> Pos -> Color -> Move -> STRef s [Move] -> (Int, Int) -> Bool -> STArray s (Char, Int) (Maybe Piece) -> ST s ()
        getMovesST'' (Pawn c) (oc, or) _ color ((fx, fy), (tx, ty)) ret (x, y) run arrST =
            let nextpos = (oc +- x, or + y)
            in  if not $ inBounds nextpos then return ()
                else if x == 0 then do
                    target <- readArray arrST nextpos
                    when (isNothing target) (do
                        modifySTRef' ret (((oc, or), nextpos) : )  --Bonde går ett skritt frem
                        let jumppos = (oc, or + 2*y)
                        target2 <- readArray arrST jumppos
                        when (isNothing target2 && (c == White && or == 2 || c == Black && or == 7)) 
                            (modifySTRef' ret (((oc, or), jumppos) : )) ) --Bonde går to skritt frem
                else do
                    target <- readArray arrST nextpos
                    prevpiece <- readArray arrST (tx, ty)
                    when ((isJust target && getColor (fromJust target) == (getOppositeColor c)) || prevpiece == Just (Pawn (getOppositeColor color)) && abs (fy-ty) == 2 && nextpos == (tx, div (fy+ty) 2)) 
                        (modifySTRef' ret (((oc, or), nextpos) : ))      --Bonde tar skrått, enten om det står en brikke der eller ved en passant.
        getMovesST'' p (oc, or) (cc, cr) color prev ret (x, y) run arrST =
            let nextpos = (cc +- x, cr + y)
            in  when (inBounds nextpos) (do
                    target <- readArray arrST nextpos
                    when (isNothing target || getColor (fromJust target) /= color) (do
                        modifySTRef' ret (((oc, or), nextpos) : )
                        when (isNothing target && run) $ getMovesST'' p (oc, or) nextpos color prev ret (x, y) run arrST ) )

        inBounds :: Pos -> Bool
        inBounds (x, y) = x >= 'A' && x <= 'H' && y >= 1 && y <= 8


getScoreST :: BoardST s -> ST s Int
getScoreST (_, _, scoresST, _, _, _, _) = readSTRef scoresST >>= return . head

getScorePure :: BoardPure -> Int
getScorePure board = runST $ toBoardST board >>= getScoreST

getColorToMoveST :: BoardST s -> ST s Color
getColorToMoveST (_, _, _, _, _, _, colorST) = readSTRef colorST

getColorToMovePure :: BoardPure -> Color
getColorToMovePure b = runST $ toBoardST b >>= getColorToMoveST


(+-) :: Char -> Int -> Char
(+-) c n = chr (ord c + n)

boardInfo :: BoardPure -> IO ()
boardInfo (arr, moves, scores, deaths, castles, counter, color) = do
    putStr $ shwBoard arr
    putStrLn ("Move list: " ++ show (init moves))
    putStrLn ("Score list: " ++ show scores)
    putStrLn ("Deaths: " ++ show deaths)
    putStrLn ("Castle lists: " ++ show castles)
    putStrLn ("Number of moves: " ++ show counter)
    putStrLn ("To move: " ++ show color)

justArrST :: BoardST s -> ST s (STArray s (Char, Int) (Maybe Piece))
justArrST (arrST, _, _, _, _, _, _) = return arrST

shwST :: BoardST s -> ST s String
shwST boardST = justArrST boardST >>= freeze >>= return . shwBoard

shwBoard :: Board -> String
shwBoard b = concatMap (\r -> map (\c -> shwP $ b!(c, r)) "ABCDEFGH" ++ "\n") [8, 7..1]

shwBH :: BoardPure -> String
shwBH (arr, _, _, _, _, _, _) = shwBoard arr



