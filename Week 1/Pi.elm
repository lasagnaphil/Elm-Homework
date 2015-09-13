module Pi where

import Signal exposing ((<~), (~))
import Window
import Random
import Time exposing ()
import List exposing ((::), fst, snd)

type alias Point = { x:Float, y:Float }
type alias State = ((Int, List Point), (Int, List Point))

-- the model
upstate : Point -> State -> State
upstate point state =
    if (point.x)^2 + (point.y)^2 < 100.0
        then ((state |> fst |> fst + 1, (point.x, point.y) :: state |> fst |> snd),
            (state |> snd))
        else ((state |> fst),
            (state |> snd |> fst + 1, (point.x, point.y) :: state |> snd |> snd))

initState : State
initState = ((0,[]),(0,[]))

-- the controller
randPointGen : Generator (Float, Float)
randPointGen =
    pair (float -100, 100) (float -100 100)

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint seed =
    generate randPointGen seed

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
    genPoint <~ (Signal.foldp genPoint initial)
    --({x=0, y=0}, Random.initialSeed 17)


signalPoint : Signal Point
signalPoint =
    Time.every Time.second


-- the view
view : (Int, Int) -> State -> Element
view w h state =
    pointsToCircles <

pointsToCircles : Color.Color -> List.Point -> List Form
pointsToCircles color pointlst =

piApprox : State -> Float
piApprox =
    4 * (state|>fst|>fst / (state|>fst|>fst + state|>snd|>fst)

main : Signal Element
main =
    Signal.map2 view Window.dimensions
        (Signal.foldp upstate initState signalPoint)