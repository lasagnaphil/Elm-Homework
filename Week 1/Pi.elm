module Pi where

import Signal exposing ((<~), (~))
import Window
import Random
import Time
import List exposing ((::))

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Point = { x:Float, y:Float }
type alias State = ((Int, List Point), (Int, List Point))

-- the model
upstate : Point -> State -> State
upstate point ((n1,list1), (n2,list2)) =
    if (point.x)^2 + (point.y)^2 < 10000.0
        then ((n1 + 1, point::list1), (n2, list2))
        else ((n1, list1), (n2 + 1, point::list2))

initState : State
initState = ((0,[]),(0,[]))

-- the controller
floatGen : Random.Generator Float
floatGen =
    Random.float -100 100

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint seed =
    let (x, seed2) = Random.generate floatGen seed
        (y, seed3) = Random.generate floatGen seed2
    in
        ({x=x, y=y}, seed3)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
    Signal.foldp (\a (_, previousSeed) -> genPoint (previousSeed)) ({x=0, y=0}, Random.initialSeed 17) (Time.every (0.03*Time.second))

signalPoint : Signal Point
signalPoint =
    fst <~ signalPointSeed

-- the view
view : (Int, Int) -> State -> Element
view (w, h) ((n1,list1), (n2,list2)) =
    collage w h (
        ((toForm (show (piApprox ((n1,list1), (n2,list2))))) |> (move (0,-120))) ::
            (pointsToCircles (rgb 255 0 0) list1) ++
            (pointsToCircles (rgb 0 255 0) list2)
    )

pointsToCircles : Color.Color -> List Point -> List Form
pointsToCircles color pointlst =
    let drawCircle {x,y} =
        circle 5
        |> filled color
        |> move (x,y)
    in
    List.map drawCircle pointlst

piApprox : State -> Float
piApprox ((n1,list1),(n2,list2)) =
    4.0 * (toFloat n1) / (toFloat n1 + toFloat n2)

main : Signal Element
main =
    Signal.map2 view Window.dimensions
        (Signal.foldp upstate initState signalPoint)