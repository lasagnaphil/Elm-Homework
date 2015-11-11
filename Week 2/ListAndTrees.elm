module ListAndTrees where

import Graphics.Element as E
import List exposing ((::))
import List

-- Problem 1: Lists
-- suffixes : List a -> List (List a)
suffixes list = 
    let suffixes' curList lst = case curList of
        [] -> lst
        _::curList' -> suffixes' curList' (lst::curList')
    in
        suffixes' list []

-- Problem 2: Binary Trees
type Tree = Empty | Node Int Tree Tree

-- 2.2.1
mem : Int -> Tree -> Bool
mem x t = case t of
    Empty -> False
    Node y left right ->
        if | x == y -> True
           | x <  y -> mem x left
           | x >  y -> mem x right

-- 2.2.2
fullTree : Int -> Int -> Tree
fullTree x h = if
    | h == 1 -> (Node x Empty Empty)
    | otherwise -> (Node x (fullTree x (h-1)) (fullTree x (h-1)))

-- 2.2.3
balancedTree : Int -> Int -> Tree
balancedTree x n = if
    | n == 1 -> (Node x Empty Empty)
    | n%2 == 0 -> (Node x (balancedTree x (n//2)) (balancedTree x (n//2)))
    | otherwise -> case (create2 x ((n-1)//2)) of
        (t1,t2) -> (Node x t1 t2)

create2 : Int -> Int -> (Tree, Tree)
create2 x m =
    ((balancedTree x (m)), (balancedTree x (m+1)))

-- 2.2.4
-- Note: https://wiki.haskell.org/99_questions/Solutions/55

{-
balancedTrees : Int -> Int -> List Tree
balancedTrees x n =
    let makeNode l r = (Node x l r)
        makeNode2 l r = [(Node x l r), (Node x r l)]
        balancedTrees' x n = 
        if | n == 1 -> [(Node x Empty Empty)]
           | n%2 == 1 -> List.map2 (makeNode) (balancedTrees' x (n-1)//2) (balancedTrees' x (n-1)//2)
           | otherwise -> List.concat [List.map2 (makeNode2) (balancedTrees' x (n-1)//2) (balancedTrees' x n//2)]
    in
        balancedTrees' x n 
-}
-- 2.2.5
-- completeTrees : Int -> Int -> List Tree
    {-
completeTrees x n =
    let makeNode l r = Node x l r
        makeRows =
    in
        List.map2 (++ (fullTree (n-1)) addRows
    -}

-- 2.2.6
-- almostCompleteTrees : Int -> Int -> List Tree









main =
    E.show suffixes [1..4]
    --E.show (List.map (List.length << balancedTrees 0) [0..20])
