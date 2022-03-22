module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)



myShapes : Model -> List (Shape Msg)
myShapes _ = [

  ]


type Msg 
  = Tick Float GetKeyState
  
type alias Model = { 
    time : Float 
    }

      
update : Msg -> Model -> Model
update msg model 
  = case msg of
      Tick t _   -> {model | time = t}

      


init : Model
init = { 
        time = 0 
    }

main : GameApp Model Msg
main = gameApp Tick { model = init, view = view, update = update, title = "The Mandelbrot Fractal" }

view : Model -> Collage Msg
view model = collage 192 128 (myShapes model)



