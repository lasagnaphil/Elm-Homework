module FadingDots where

import List exposing ((::))
import List
import Time
import Signal exposing ((<~),(~),Signal)
import Signal
import Window
import Mouse
import Text as T
import Graphics.Collage as C
import Graphics.Element as E
import Color

type alias Click = (Time.Time, (Int, Int))
type alias State = (Time.Time, List Click)
type Update = NewTime Time.Time | NewClick Click

initState = (0, [])

tfade = 2 * Time.second
pruneOld now clicks = case clicks of
    [] -> []
    (t,xy) :: clicks' -> if
        | (now-t) > tfade -> []
        | otherwise -> (t,xy) :: pruneOld now clicks'

upstate u (_,clicks) = case u of
    NewTime t -> (t, pruneOld t clicks)
    NewClick (t,xy) -> (t, (t,xy) :: pruneOld t clicks)

setAlpha : Float -> Color.Color -> Color.Color
setAlpha a c =
    let rgb = Color.toRgb c in
    Color.rgba rgb.red rgb.green rgb.blue a

view (w,h) (now, clicks) =
    let str = case clicks of {[] -> E.show "..."; p::_ -> E.show p}
        color a = setAlpha a Color.darkBlue
        rad pct = 20 + 100*pct
        circ pct = C.filled (color (1-pct)) (C.circle (rad pct))
        (fw, fh) = (toFloat w, toFloat h)
        (dx, dy) = (-fw/2, fh/2)
        dots = 
            clicks |> List.map (\(t,(x,y)) ->
                let pct = (now-t) / tfade in
                circ pct |> C.move (toFloat x + dx, toFloat (-y) + dy)
            )
    in
        C.collage w h (dots ++ [C.toForm str])


time : Signal Time.Time
time = Signal.foldp (+) 0 (Time.fps 60)

clicks : Signal Click
clicks =
    let onClick = Signal.sampleOn Mouse.clicks in
    (,) <~ onClick time ~ onClick Mouse.position

state : Signal State
state = Signal.foldp upstate initState (Signal.merge (NewTime <~ time) (NewClick <~ clicks))

main =
    view <~ Window.dimensions ~ state
