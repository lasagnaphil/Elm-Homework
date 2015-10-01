module Chap202 where

import Window
import Signal
import List exposing ((::))
import Text as T
import Color
import Graphics.Collage as C
import Graphics.Element as E
import Graphics.Input exposing (button)

type alias State = Int
initState = 0
upstate _ i = i + 1

view (w,h) i =
    E.color Color.gray
 <| E.container w h E.middle
 <| E.flow E.down
        [ i |> toString |> T.fromString
        , button (Signal.send ch ()) "Increment"
        , button (Signal.send ch ()) "Clear"
        ]


ch : Signal.Mailbox ()
ch = Signal.mailbox ()


main =
    Signal.map2 view Window.dimensions
        (Signal.foldp upstate initState )
