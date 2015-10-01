module FRPTutorial where

import Signal exposing ((<~))
import Time
import Mouse
import Graphics.Element exposing (Element, show)

-- the model
type alias State = Int
initState : State
initState = 0

step : a -> State -> State
step _ i = i + 1

main : Signal Element
main =
    show <~ (Signal.foldp step initState (Time.every Time.second))