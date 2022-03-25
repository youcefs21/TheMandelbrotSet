module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes as Attrs exposing (..)
import Time


main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




type Msg = Tick Time.Posix | ZoomScroll String | XShiftScroll String | YShiftScroll String | ItrScroll String

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Tick t ->
      ( { model | time = t, col = if model.col == "orange" then "blue" else "orange" }
      , Cmd.none
      )
    ZoomScroll s ->
      ( { model | zoom = String.toFloat s |> Maybe.withDefault 80}
      , Cmd.none
      )
    XShiftScroll s ->
      ( { model | xShift = String.toFloat s |> Maybe.withDefault 0}
      , Cmd.none
      )
    YShiftScroll s ->
      ( { model | yShift = String.toFloat s |> Maybe.withDefault 100}
      , Cmd.none
      )
    ItrScroll s ->
      ( { model | itr = String.toInt s |> Maybe.withDefault 0}
      , Cmd.none
      )

pixels x y model = if x == 1 then
    [div [style "width" "2px", style "height" "2px", style "float" "left", style "background" (mandelbrotColor x y model)] []]
  else
    (div [style "width" "2px", style "height" "2px", style "float" "left", style "background" (mandelbrotColor x y model)] []) :: pixels (x - 1) y model

grid x y model = if y == 1 then
    pixels x y model
  else
    pixels x y model ++ grid x (y - 1) model


view : Model -> Html Msg
view model =
  div [align "center",  style "padding" "70px 0"] 
  [
    (div [style "width" "700px", style "height" "400px"]) (grid 350 200 model)
    , div []
      [
        Html.text <| "Zoom: "
        ,
        input
            [ type_ "range"
            , Attrs.min "80"
            , Attrs.max "1000"
            , value <| String.fromFloat model.zoom
            , onInput ZoomScroll
            ]
            []
        , Html.text <| String.fromFloat model.zoom
      ]        
        , div [] [
        Html.text <| " Iterations: "
        , input
            [ type_ "range"
            , Attrs.min "1"
            , Attrs.max "200"
            , value <| String.fromInt model.itr
            , onInput ItrScroll
            ]
            []
        , Html.text <| String.fromInt model.itr
        ]
      ,
      div []
      [
      Html.text <| "X-cord: " ++ String.fromFloat model.xShift
      , input
          [ type_ "range"
          , Attrs.min "-80"
          , Attrs.max "50"
          , value <| String.fromFloat (model.xShift - 30)
          , onInput XShiftScroll
          ]
          []
      , Html.text <| "Y-cord: " ++ String.fromFloat (model.yShift - 100)
      , input
          [ type_ "range"
          , Attrs.min "0"
          , Attrs.max "200"
          , value <| String.fromFloat model.yShift
          , onInput YShiftScroll
          ]
          []
      ]
  ]
    
type alias Complex = (Float, Float)

-- (a + bi)(c + di) = ac + adi + cbi - db
multCmplx (r1, i1) (r2, i2) = (r1*r2 - i1*i2, r1*i2 + r2*i1)
addCmplx (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

absCmplx (r1, i1) = sqrt (r1*r1 + i1*i1)


-- next = z^2 + c
mandelbrot : Complex -> Complex -> Int -> Int
mandelbrot z c n = 
  let
    newZ = (addCmplx (multCmplx z z) c)
  in

    if n == 0 || absCmplx(newZ) > 2 then
        n
    else
        mandelbrot newZ c (n - 1)



mandelbrotColor x y model =  if (mandelbrot (0,0) (model.xShift/50  -  (x/model.zoom),(y - model.yShift)/model.zoom) model.itr) == 0 then
  "black"
 else if (mandelbrot (0,0) (model.xShift/50  -  (x/model.zoom),(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 2)) then
  "Gold"
 else if (mandelbrot (0,0) (model.xShift/50  -  (x/model.zoom),(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 4)) then
  "red"
 else if (mandelbrot (0,0) (model.xShift/50  -  (x/model.zoom),(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 6)) then
  "orange"
 else if (mandelbrot (0,0) (model.xShift/50  -  (x/model.zoom),(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 8)) then
  "green"
 else
  "blue"


type alias Model =
  { col : String
  , time : Time.Posix
  , zoom : Float
  , xShift : Float
  , yShift : Float
  , itr : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "orange" (Time.millisToPosix 0) 80 30 100 100
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions _ = Time.every 50 Tick