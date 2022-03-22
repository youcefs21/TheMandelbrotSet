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



type alias Complex = (Float, Float)

-- (a + bi)(c + di) = ac + adi + cbi - db
multCmplx (r1, i1) (r2, i2) = (r1*r2 - i1*i2, r1*i2 + r2*i1)
addCmplx (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)


mandelbrot : Complex -> Complex -> Int -> Complex
mandelbrot z c n = 
    if n == 0 then
        z
    else
        mandelbrot (addCmplx (multCmplx z z) c) c (n - 1)


init : Model
init = { 
        time = 0 
    }

main : GameApp Model Msg
main = gameApp Tick { model = init, view = view, update = update, title = "The Mandelbrot Fractal" }

view : Model -> Collage Msg
view model = collage 192 128 (myShapes model)



