module Chap201 where

import Window
import List exposing ((::))
import Graphics.Element exposing (show, Element)

find : a -> List a -> Bool
find x xs = case xs of
    []      -> False
    y::xs'  -> if x == y then True else find x xs'

type Tree a = Empty | Node a (Tree a) (Tree a)

findBT x t = case t of
    Empty -> False
    Node y left right -> x == y || findBT x left || findBT x right

findBST x t = case t of
    Empty -> False
    Node y left right ->
        if  | x==y => True
            | x< y => findBST x left
            | x> y => findBST x right

update : List a -> Int -> a -> List a
update xs i y =
    case (xs, i) of
        ([],    , _) -> []
        (x::xs',  0) -> y :: xs'
        (x::xs',  _) -> update xs' (i-1) y

append : List a -> List a -> List a
append xs ys =
    case xs of
        []      -> ys
        x::xs'  -> x :: append xs' ys

insert : comparable -> Tree comparable -> Tree comparable
insert x t = case t of
    Empty -> Node x Empty Empty
    Node y left right ->
        if  | x == y -> t
            | x <  y -> Node y (insert x left) right
            | x >  y -> node y left (insert x right)

main : Element
main =
    show (find 152 [1..150])