module Tarea4 where

type Matriz a = [[a]]

-- 1. Suma de matrices
sumarMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
sumarMatrices = zipWith (zipWith (+))

-- 2. Igualdad de listas como conjuntos
igualesComoConjuntos :: Eq a => [a] -> [a] -> Bool
igualesComoConjuntos xs ys = all (`elem` ys) xs && all (`elem` xs) ys

-- 3a. Invertir lista con foldr
invertirFoldr :: [a] -> [a]
invertirFoldr = foldr (\x acc -> acc ++ [x]) []

-- 3b. Invertir lista con foldl
invertirFoldl :: [a] -> [a]
invertirFoldl = foldl (flip (:)) []

-- 4. Ordenar dos listas (unificadas en una sola lista ordenada)
ordenarDosListas :: Ord a => [a] -> [a] -> [a]
ordenarDosListas xs ys = qsort (xs ++ ys)
  where
    qsort []     = []
    qsort (p:ps) = qsort [x | x <- ps, x <= p] ++ [p] ++ qsort [x | x <- ps, x > p]
