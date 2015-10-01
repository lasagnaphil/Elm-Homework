import Graphics.Element exposing (show)
import Task exposing (Task)
import TaskTutorial exposing (print)
import Time exposing (second, Time)

clock : Signal Time
clock =
    Time.every second

printTasks : Signal (Task x ())
printTasks =
    Signal.map print clock

port runner : Signal (Task x ())
port runner =
    printTasks

main =
    show "Open your browser's Developer Console."