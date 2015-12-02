module FP where

import List exposing ((::), map, sum, length, concat, head, drop)
import Result exposing (map)

-- 1.1.1
digitsOfInt : Int -> List Int
digitsOfInt n =
    let digitsOfInt' n acc =
        if n == 0
            then acc
            else digitsOfInt' (n // 10) ((n % 10) :: acc)
    in 
        digitsOfInt' n []

-- 1.1.2
digitAdder : Int -> (Int, Int)
digitAdder n =
    let digitAdder' n trials =
        if n < 10
            then (n, trials)
            else digitAdder' (sum (digitsOfInt n)) (trials+1)
    in
        digitAdder' n 0

additivePersistence : Int -> Int
additivePersistence n = snd (digitAdder n)

digitalRoot : Int -> Int
digitalRoot n = fst (digitAdder n)

-- 1.1.3
subsequences : List a -> List (List a)
subsequences lst =
    case lst of
        [] -> [[]]
        h::t -> let st = subsequences t
                    in
                        st ++ List.map (\x -> h::x) st

-- 1.1.4
take : Int -> List a -> Result String (List a)
take n lst =
    if  | n < 0 -> Err "negative index"
        | (length lst) < n -> Err "not enough elements"
        | otherwise -> Ok (List.take n lst)
