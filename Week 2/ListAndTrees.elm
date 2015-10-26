module ListAndTrees where

import Graphics.Element as E
import List exposing ((::))
import List

-- Problem 1: Lists
{-
suffixes : List a -> List (List a)
suffixes list = 
    let suffixes' curList list = case curList of
        [] -> list
        _::curList' -> suffixes' curList' (list::curList')
    in
        suffixes' list [[]]
-}
-- Problem 2: Binary Trees
type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem x t = case t of
    Empty -> False
    Node y left right ->
        if | x == y -> True
           | x <  y -> mem x left
           | x >  y -> mem x right

fullTree : Int -> Int -> Tree
fullTree x h = if
    | h == 1 -> (Node x Empty Empty)
    | otherwise -> (Node x (fullTree x (h-1)) (fullTree x (h-1)))

balancedTree : Int -> Int -> Tree
balancedTree x n = if
    | n == 1 -> (Node x Empty Empty)
    | n%2 == 0 -> (Node x (balancedTree x (n//2)) (balancedTree x (n//2)))
    | otherwise -> case (create2 x ((n-1)//2)) in 
        (t1,t2) -> (Node x t1 t2)

create2 : Int -> Int -> (Tree, Tree)
create2 x m =
    ((balancedTree x (m)), (balancedTree x (m+1)))

balancedTrees : Int -> Int -> List Tree
balancedTrees x n =
    let balancedTrees' x n list = if
        | n == 1 -> (Node x Empty Empty)

main =
    -- E.show suffixes [1..4]
    E.show (fullTree 0 2)
