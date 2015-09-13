module Pi where

import Signal exposing ((<~), (~))
import Window
import Random
import Random.Extra
import Time
import List exposing ((::))

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Point = { x:Float, y:Float }
type alias State = ((Int, List Point), (Int, List Point))

-- the model
upstate : Point -> State -> State
upstate point state =
    if (point.x)^2 + (point.y)^2 < 100.0
        then ((fst (fst state) + 1, point :: (snd (fst state))),
            (snd state))
        else ((fst state),
            (fst (snd state) + 1, point :: (snd (snd state))))

initState : State
initState = ((0,[]),(0,[]))

-- the controller
floatGen : Random.Generator Float
floatGen =
    Random.float -100 100


randPointGen : Random.Generator Point
randPointGen generator =
    Random.Extra.map Point floatGen floatGen

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint seed =
    Random.generate randPointGen seed

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
    Signal.foldp genPoint ({x=0, y=0}, Random.initialSeed 17) (Time.every Time.second)

signalPoint : Signal Point
signalPoint =
    fst <~ signalPointSeed

-- the view
view : (Int, Int) -> State -> Element
view w h state =
    collage w h [
        pointsToCircles (rgb 255 0 0) (state|>fst|>snd),
        pointsToCircles (rgb 0 255 0) (state|>snd|>snd),
        toForm (show piApprox state)
    ]

pointsToCircles : Color.Color -> List Point -> List Form
pointsToCircles color pointlst =
    List.map (\x -> move x (color (circle 5))) pointlst 

piApprox : State -> Float
piApprox state =
    4 * (state|>fst|>fst) / (state|>fst|>fst + state|>snd|>fst)

main : Signal Element
main =
    Signal.map2 view Window.dimensions
        (Signal.foldp upstate initState signalPoint)