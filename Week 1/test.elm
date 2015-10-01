module Test where

import FP exposing (digitsOfInt, additivePersistence, digitalRoot, subsequences, take)

import Graphics.Element exposing (show, Element)

main : Element
main = show (subsequences [1,2,3,4])