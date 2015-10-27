module DrawTrees where

import List exposing ((::))
import List
import ListandTrees exposing (..)
import Signal exposing ((<~),(~),Signal)
import Signal
import Window
import Mouse
import Text as T
import Graphics.Collage as C
import Graphics.Element as E
import Color

sampleListOn : Signal b -> List a -> Signal a
sampleListOn ticker xs =
    Signal.sampleOn ticker (\lst -> List.head lst)

view : (Int,Int) -> Tree -> E.Element
view _ _ =
    -- TODO
    E.spacer 0 0

signalTree : Signal Tree
signalTree =
    sampleListOn Mouse.clicks 

main : Signal E.Element
main =
    Signal.map2 view Window.dimensions signalTree
