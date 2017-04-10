module Sudoku
  (solve) where

type Row a = [a]
type Matrix a = [Row a]
type Digit = Char

type Grid = Matrix Digit

digits :: [Digit]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)

choice :: Digit -> [Digit]
choice d = if blank d then digits else [d]

valid :: Grid -> Bool
valid = const True

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
  where yss = cp xss
