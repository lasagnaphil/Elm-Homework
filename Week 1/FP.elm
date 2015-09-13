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
-- this is (almost) of copy of 1.1.2
{-
binaryDigits : Int -> List Int
binaryDigits n =
    let binaryDigits' n acc =
        if n == 0
            then acc
            else binaryDigits' (n // 2) ((n%2) :: acc)
    in
        binaryDigits' n []

allBinaryDigits : Int -> List (List Int)
allBinaryDigits n =
    let allBinaryDigits' count n acc =
        if count == n
            then acc
            else allBinaryDigits' count+1 n ((binaryDigits count) :: acc)
    in
        allBinaryDigits' 0 n 

subsequences : List a -> List (List a)
subsequences lst =
    let trials = 2^(length lst)
        originalList = repeat trials lst
        allBinaryDigits start end =
            if start == end 
    in
        map2 (*) (map (allBinaryDigits 0 trials)) ()
-}
{-
subsequences : List a -> List (List a)

subsequences lst =
    let addElement i lst1 lst2 = (head (drop i lst1)) :: lst2
        powerset i sublst lst =
        if i == (length lst)
            then sublst
            else concat (List.map (addElement i+1 lst) (powerset i+1 sublst lst)) (powerset i+1 sublst lst)
    in
        powerset 0 [[]] lst
-}

{-
subsequences : List a -> List (List a)
subsequences lst =
    let subsequences' count lst =
        if count == 0
            then lst
            else map (subsequences' (count-1)) [ lst, (tails lst) ] 
    in
        concat (subsequences' (length lst) lst)
-}


-- 1.1.4
take : Int -> List a -> Result String (List a)
take n lst =
    if  | n < 0 -> Err "negative index"
        | (length lst) < n -> Err "not enough elements"
        | otherwise -> Ok (List.take n lst)


