module Test where

import FP exposing (digitsOfInt, additivePersistence, digitalRoot, take)

import Graphics.Element exposing (show, Element)

main : Element
main = show (digitsOfInt 15231253)