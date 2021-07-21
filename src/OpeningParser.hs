module OpeningParser where

import Text.Parsec hiding (letter)
import Data.Char (toUpper, digitToInt)
import Control.Monad (forM_)
import Debug.Trace (trace)
import qualified Board2 as B
import qualified Piece as P

{-
Move => piece to | O-O | O-O-O | to = letter
piece => letter | letter file | letter rank
letter => R | N | B | Q | K | ε
to => x file rank | file rank
file => a | b | ... | h
rank => 1 | 2 | ... | 8
-}

-- 1.e4 e6 2.d4 d5 3.Nc3 Bb4 4.Ne2

parseMoveSequence :: String -> [B.Move]
parseMoveSequence text = case runParser moves B.newboard "(source)" text of
    Left err -> error (show err)
    Right ms -> ms

moves :: Parsec String B.Board [B.Move]
moves = try (do
        spaces
        n <- many1 digit
        d <- char '.'
        m <- move
        modifyState $ (flip B.move) m
        ms <- moves
        return (m:ms)
    ) <|> try (do
        spaces
        m <- move 
        modifyState $ (flip B.move) m
        ms <- moves
        return (m:ms)
    ) <|> return []



data FromInfo = JustLetter Char 
              | LetterFile Char Char 
              | LetterRank Char Int 
        deriving (Show, Eq)

parseMove :: String -> B.Move 
parseMove text = case runParser move B.newboard "(source)" text of
                    Left err -> error (show err)
                    Right mo -> mo

move :: Parsec String B.Board B.Move
move = try (do
        board <- getState
        from <- piece
        pos <- to
        let f:fs  = filter (matchFromInfo from board pos) (B.legalmoves board)
        if fs == [] then return f
        else trace ("Found ambiguous moves: " ++ show (f:fs) ++ "\n" ++ show board) (return f)
    ) <|> try (do
        char 'O' >> char '-' >> char 'O' >> char '-' >> char 'O'
        board <- getState
        if B.colortomove board == B.White then return (('E', 1), ('C', 1))
        else return (('E', 8), ('C', 8))
    ) <|> try (do
        char 'O' >> char '-' >> char 'O'
        board <- getState
        if B.colortomove board == B.White then return (('E', 1), ('G', 1))
        else return (('E', 8), ('G', 8))
    )



matchFromInfo :: FromInfo -> B.Board -> B.Position -> B.Move -> Bool
matchFromInfo (JustLetter l)   board pos (from, to) = to == pos && l == toUpper (P.showP (B.pieceat board from))
matchFromInfo (LetterFile l f) board pos (from, to) = to == pos && l == toUpper (P.showP (B.pieceat board from)) && fst from == toUpper f
matchFromInfo (LetterRank l r) board pos (from, to) = to == pos && l == toUpper (P.showP (B.pieceat board from)) && snd from == r

piece :: Parsec String B.Board FromInfo --En komplett beskrivelse av brikken som flyttet
piece = letter >>= \l -> 
    (rank >>= \r -> return (LetterRank l r)) --Om to like brikker kan gå dit, og de står på samme fil, spesifisierer vi med raden
    <|> try (file >>= \f -> (lookAhead file <|> lookAhead (char 'x')) >> return (LetterFile l f)) --Om to like brikker kan gå dit, spesifiserer vi med å legge til filen den flytter fra
    <|> return (JustLetter l)   --Om kun én brikke av den typen kan gå dit

letter :: Parsec String B.Board Char
letter = oneOf "RNBQK" <|> return 'P'

to :: Parsec String B.Board B.Position   --Hvor brikken skal flytte til.
to = do
        char 'x'
        f <- file
        r <- rank
        return (toUpper f, r)
    <|> do
        f <- file 
        r <- rank
        return (toUpper f, r)

file :: Parsec String B.Board Char
file = oneOf ['a'..'h']

rank :: Parsec String B.Board Int
rank = return . digitToInt =<< oneOf ['1'..'8']

