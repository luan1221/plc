
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

-- lesson #2 exercises

totalVendas :: Int -> Int
totalVendas x
 | x == 0 = vendas 0
 | otherwise = vendas x + totalVendas (x-1)

addEspacos :: Int -> String
addEspacos e
 | e == 0 = ""
 | otherwise = " " ++ addEspacos (e-1)

paraDireita :: Int -> String -> String
paraDireita i s = addEspacos i ++ s

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho
                  ++ imprimeSemanas n
                  ++ imprimeTotal n
                  ++ imprimeMedia n

cabecalho :: String
cabecalho = paraDireita 5 ("Semana" ++ addEspacos 4 ++ "Venda" ++ "\n")

imprimeSemanas :: Int -> String
imprimeSemanas x
 | x == 0 = "\t" ++ "0" ++ "\t" ++ (show (vendas 0)) ++ "\n"
 | otherwise = imprimeSemanas (x-1) ++ "\t" ++ (show x) ++ "\t" ++ (show (vendas x)) ++ "\n"

imprimeTotal :: Int -> String
imprimeTotal x = paraDireita 6 ("Total" ++ "\t" ++ show (totalVendas x) ++ "\n")

imprimeMedia :: Int -> String
imprimeMedia x = paraDireita 6 ("Média" ++ "\t" ++ show ( fromIntegral (totalVendas x )/ fromIntegral (x+1)) ++ "\n")

impressao :: Int -> IO()
impressao n = putStr (imprimeTabela n)

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (a,b)
  where
   a = min x (min y z)
   b = max x (max y z)

ordena2 :: (Int, Int) -> (Int, Int)
ordena2 (x, y)
 | x > y = (y, x)
 | otherwise = (x,y)

diff :: Int -> Int -> Int -> Int -> Int -> Int
diff x y z a b
 | x /= a && x /= b = x
 | y /= a && x /= b = y
 | z /= a && z /= b = z

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z) = (a, b, c)
  where
    a = min x (min y z)
    c = max x (max y z)
    b = diff x y z a c

type Ponto = (Float, Float)
type Reta  = (Ponto, Ponto)

fstPonto :: Ponto -> Float
fstPonto x = fst x

sndPonto :: Ponto -> Float
sndPonto (_,y) = y

retaVertical :: Reta -> Bool
retaVertical x 
  | fstPonto(fst x) == fstPonto(snd x) = True
  | otherwise = False
 
pontoY :: Float -> Reta -> Float
pontoY fl rt = (((sndPonto (snd rt) - sndPonto (fst rt))
                 *(fl - fstPonto (fst rt)))
                 /(fstPonto (snd rt) - fstPonto (fst rt))) + sndPonto (fst rt)
