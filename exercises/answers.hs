
import Data.Char

fat :: Integer -> Integer
fat x
 | x == 0 = 1
  | otherwise = fat (x-1) * x
  

equal :: Int -> Int -> Bool
equal x y
 | x == y = True
  | otherwise = False
  

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w = (x == y) && (equal y z) && (equal z w)


equalInt :: Int -> Int -> Int
equalInt x y
 | x == y = 1
  | otherwise = 0
  
equalInt2 :: Int -> Int -> Int -> Int
equalInt2 x y z = (equalInt x x) +  (equalInt x y) + (equalInt x z)

equalCount :: Int -> Int -> Int -> Int
equalCount x y z = max (equalInt2 x y z) (max (equalInt2 y x z) (equalInt2 z x y))

vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15
vendas 3 = 43
vendas 4 = 12

igual :: Int -> Int -> Int
igual x y
 | x == y = 1
 | otherwise = 0

vendasIguais :: Int -> Int -> Int
vendasIguais s n
 | n == 0 = igual s (vendas 0)
 | otherwise = (igual s (vendas n)) + vendasIguais s (n-1)
