module Bot where



import Board2

depth :: Int
depth = 3

alfabeta :: Board -> Int -> Score -> Score -> Score
alfabeta b 0 _ _ = score b
alfabeta b _ _ _ | isgameover b = score b
alfabeta b n α β | colortomove b == White = forα α minBound (moves b White)
    where forα α v [] = v
          forα α v (m:ms) | α >= β = v
                          | otherwise = let value = max v $ alfabeta (move b m) (n-1) α β
                                        in  forα (max α value) value ms
alfabeta b n α β | colortomove b == Black = forβ β maxBound (moves b Black)
    where forβ β v [] = v
          forβ β v (m:ms) | α >= β = v
                          | otherwise = let value = min v $ alfabeta (move b m) (n-1) α β
                                        in  forβ (min β value) value ms





{-

module Bot (miniMax, alphaBeta) where

import Board (BoardST, BoardPure, Move, getMovesST, getLegalMovesST, getScoreST, goBackST, 
    moveST, toBoardST, immedeateValue, orderMoves, getColorToMoveST)
import Color (Color(..))
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM, forM_)
import Data.STRef (STRef, newSTRef, modifySTRef', readSTRef)


depth :: Int
depth = 4

initAlpha :: Int
initAlpha = minBound

initBeta  :: Int
initBeta  = maxBound




--Denne algoritmen bruker minimax med alfabeta-pruning, les om det her: https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
alphaBeta :: BoardPure -> Move
alphaBeta b = runST $ do
    boardST <- toBoardST b
    color <- getColorToMoveST boardST
    moves <- getLegalMovesST boardST
    --moves <- orderMoves boardST unorderedmoves
    if length moves == 1 then return (head moves)
    else if color == White then do
        bestMove <- newSTRef (initAlpha, head moves)
        forM_ moves $ \move -> do
            moveST boardST move
            score <- beta depth boardST initAlpha initBeta
            goBackST boardST
            modifySTRef' bestMove (max (score, move))
        readSTRef bestMove >>= return . snd
    else do
        bestMove <- newSTRef (initBeta, head moves)
        forM_ moves $ \move -> do
            moveST boardST move
            score <- alpha depth boardST initAlpha initBeta
            goBackST boardST
            modifySTRef' bestMove (min (score, move))
        readSTRef bestMove >>= return . snd



alpha :: Int -> BoardST s -> Int -> Int -> ST s Int
alpha 0 boardST _ _ = getScoreST boardST
alpha n boardST α β = getMovesST boardST >>= \moves -> forα moves boardST α β initAlpha

    where 
        forα :: [Move] -> BoardST s -> Int -> Int -> Int -> ST s Int
        forα [] boardST α β v = return v
        forα (move:moves) boardST α β v | α >= β = return v
                                        | otherwise = do
                                            moveST boardST move
                                            score <- beta (n-1) boardST α β
                                            goBackST boardST
                                            let value = max v score
                                            forα moves boardST (max value α) β value

beta :: Int -> BoardST s -> Int -> Int -> ST s Int
beta 0 boardST _ _ = getScoreST boardST
beta n boardST α β = getMovesST boardST >>= \moves -> forβ moves boardST α β initBeta

    where
        forβ :: [Move] -> BoardST s -> Int -> Int -> Int -> ST s Int
        forβ [] boardST α β v = return v        
        forβ (move:moves) boardST α β v | α >= β = return v
                                        | otherwise = do
                                            moveST boardST move
                                            score <- alpha (n-1) boardST α β
                                            goBackST boardST
                                            let value = min v score
                                            forβ moves boardST α (min value β) value







--Denne algoritmen bruker klassisk minimax, les om det her: https://en.wikipedia.org/wiki/Minimax#Minimax_algorithm_with_alternate_moves
miniMax :: BoardPure -> Move
miniMax board = runST $ do
    boardST <- toBoardST board
    c <- getColorToMoveST boardST
    if c == White then getWhiteMove boardST
    else getBlackMove boardST


getWhiteMove :: BoardST s -> ST s Move
getWhiteMove b = do
    moves <- getMovesST b
    bestMove <- newSTRef (initAlpha, (head moves))
    forM_ moves $ \move -> do
        moveST b move
        score <- mini depth b
        modifySTRef' bestMove (max (score, move))
        goBackST b
    readSTRef bestMove >>= return . snd

getBlackMove :: BoardST s -> ST s Move
getBlackMove b = do
    moves <- getMovesST b
    bestMove <- newSTRef (initBeta, (head moves))
    forM_ moves $ \move -> do
        moveST b move
        score <- mini depth b
        modifySTRef' bestMove (min (score, move))
        goBackST b
    readSTRef bestMove >>= return . snd

maxi :: Int -> BoardST s -> ST s Int
maxi 0 boardST = getScoreST boardST
maxi n boardST = do
    ret <- newSTRef (initAlpha)
    moves <- getMovesST boardST
    forM_ moves $ \move -> do
        moveST boardST move
        score <- mini (n-1) boardST
        modifySTRef' ret (max score)
        goBackST boardST
    readSTRef ret >>= return

mini :: Int -> BoardST s -> ST s Int
mini 0 boardST = getScoreST boardST
mini n boardST = do
    ret <- newSTRef (initBeta)
    moves <- getMovesST boardST
    forM_ moves $ \move -> do
        moveST boardST move
        score <- maxi (n-1) boardST
        modifySTRef' ret (min score)
        goBackST boardST
    readSTRef ret >>= return


-}