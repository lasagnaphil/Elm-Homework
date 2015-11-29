module ListAndTrees where

import Graphics.Element as E
import List exposing ((::))
import List

-- Problem 1: Lists
suffixes : List a -> List (List a)
suffixes list = 
    let suffixes' curList lst = case curList of
        [] -> lst
        _::curList' -> suffixes' curList' (curList::lst)
    in
        List.reverse <| (suffixes' list [])

suffixes_2 : List a -> List (List a)
suffixes_2 xs = case xs of
    [] -> [[]]
    h::t -> xs :: suffixes_2 t


-- Problem 2: Binary Trees
type Tree = Empty | Node Int Tree Tree

-- 2.2.1
mem : Int -> Tree -> Bool
mem x t = case t of
    Empty -> False
    Node y left right ->
        if x == y then True
        else if x <  y then mem x left
        else mem x right

-- 2.2.2
fullTree : Int -> Int -> Tree
fullTree x h =
    if h == 1 then (Node x Empty Empty)
    else (Node x (fullTree x (h-1)) (fullTree x (h-1)))

-- 2.2.3
balancedTree : Int -> Int -> Tree
balancedTree x n =
    if n == 1 then (Node x Empty Empty)
    else if n%2 == 0 then (Node x (balancedTree x (n//2)) (balancedTree x (n//2)))
    else case (create2 x ((n-1)//2)) of
        (t1,t2) -> (Node x t1 t2)

create2 : Int -> Int -> (Tree, Tree)
create2 x m =
    ((balancedTree x (m)), (balancedTree x (m+1)))

-- 2.2.4
-- Note: https://wiki.haskell.org/99_questions/Solutions/55

cartesian : List Tree -> List Tree -> Int -> List Tree
cartesian l1 l2 x =
    List.concatMap (\a -> List.map (Node x a) l1) l2

balancedTrees : Int -> Int -> List Tree
balancedTrees x n =
    if n == 0 then [(Node x Empty Empty)]
    else if n%2 == 1 then
        cartesian (balancedTrees x ((n-1)//2)) (balancedTrees x ((n-1)//2)) x
    else
        (cartesian (balancedTrees x ((n-1)//2)) (balancedTrees x ((n)//2)) x) ++ (cartesian (balancedTrees x ((n)//2)) (balancedTrees x ((n-1)//2)) x)

-- 2.2.5
completeTrees : Int -> Int -> List Tree
completeTrees x n =
    let addRows tree j k = case tree of
        Empty -> if j < k then (Node x Empty Empty)
                 else Empty
        Node x l r -> Node x (addRows l j k) (addRows r (j+1) k)
    in
        List.map (addRows (fullTree x (n-1)) 0) [0..(2^(n-2)-1)]

-- 2.2.6
almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees x n =
    []
    --let makeNode l r = Node x l r











main =
    E.show (List.map (List.length << completeTrees 0) [0..5])
