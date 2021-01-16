module Main where

import Bot (miniMax, alphaBeta)
import Board (BoardST, BoardPure, Move, Color, newboard, getLegalMovesPure, movePure, isLegalPure, 
    goBackPure, goBackST, toBoardST, toBoardPure, getMovesST, moveST, boardInfo, shwST)
import Text.Parsec (Parsec, ParseError, parse, oneOf)
import Data.Char (digitToInt, toUpper)
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Test.QuickCheck
import Debug.Trace

checkMoveAndGoBack :: Int -> Bool
checkMoveAndGoBack n = runST $ do
    boardST1 <- toBoardST newboard
    boardST2 <- toBoardST newboard
    forM_ [1..n] $ \_ -> do
        b <- shwST boardST1
        move:moves <- getMovesST boardST1
        moveST boardST1 move
    forM_ [1..n] $ \_ -> goBackST boardST1
    pure1 <- toBoardPure boardST1
    pure2 <- toBoardPure boardST2
    return (pure1 == pure2)

main = quickCheck (checkMoveAndGoBack)


main' :: IO ()
main' = inter newboard


inter :: BoardPure -> IO ()
inter board = do
    boardInfo board
    inn <- getLine
    if      inn == "moves" then print (getLegalMovesPure board) >> inter board
    else if inn == "back"  then inter (goBackPure board)
    else if inn == "bot"   then inter (movePure board (alphaBeta board))
    else case parseMove inn of
        Left err -> putStrLn ("Unrecognized command.") >> inter board
        Right move -> if isLegalPure board move then inter $ movePure board move
                      else putStrLn "Illegal move!" >> inter board


parseMove :: String -> Either ParseError Move
parseMove = parse parseMove' "(source)" . map toUpper . filter (/=' ')
    where 
        parseMove' :: Parsec String () Move
        parseMove' = do
            c1 <- oneOf ['A'..'H']
            r1 <- oneOf ['1'..'8']
            c2 <- oneOf ['A'..'H']
            r2 <- oneOf ['1'..'8']
            return ((c1, digitToInt r1), (c2, digitToInt r2))