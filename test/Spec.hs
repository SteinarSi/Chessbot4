import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Data.Function

main :: IO ()
main = putStrLn "This is just a file for random testing, if you're seeing this please turn back."



testSTArray = runST $ do
    arr <- newArray (1, 5) 0 :: ST s (STArray s Int Int)
    readArray arr 10 >>= return

testArray = let a = array (1, 5) [(i, i ) | i<-[1..5]] in a ! 10


fib_mem :: Int -> Integer
fib_mem = (map fib [0..] !!)  
    where 
        fib 0 = 1        
        fib 1 = 1        
        fib n = fib_mem (n-2) + fib_mem (n-1)

fib_mem_arg :: Int -> Integer
fib_mem_arg x = map fib [0..] !! x 
    where 
        fib 0 = 1        
        fib 1 = 1        
        fib n = fib_mem_arg (n-2) + fib_mem_arg (n-1)

n :: Int
n = 30

matrix :: (Int, Int) -> Int
matrix = (array ((1, 1), (n, n)) [ ((n, m), if m == n then 1 else 0) | m <- [1..n], n <- [1..n]] !)

data Lex = Number Double Lex         
         | Plus Lex         
         | Times Lex         
         | End 
         
lexRPN :: String -> Lex
lexRPN = go . words  
    where 
        go ("*":rest) = Times (go rest)        
        go ("+":rest) = Plus (go rest)        
        go (num:rest) = Number (read num) (go rest)        
        go         [] = End

evalRPN :: Lex -> Double
evalRPN = go []  
    where    
        go stack (Number num rest)       = go (num : stack) rest    
        go (o1:o2:stack) (Plus rest)       = let r = o1 + o2 in r `seq` go (r : stack) rest    
        go (o1:o2:stack) (Times rest)       = let r = o1 * o2 in r `seq` go (r : stack) rest    
        go [res] End       = res