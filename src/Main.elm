module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes as Attrs exposing (..)
import Time
import Platform.Sub exposing (batch)
import Json.Decode as Decode


main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




type Msg = 
  Tick Time.Posix     | 
  ZoomScroll String   | 
  ItrScroll String    | 
  CharacterKey Char   | 
  ControlKey String

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Tick t ->
      ( { model | time = t}
      , Cmd.none
      )
    ZoomScroll s ->
      ( { model | zoom = String.toFloat s |> Maybe.withDefault 80}
      , Cmd.none
      )
    ItrScroll s ->
      ( { model | itr = String.toInt s |> Maybe.withDefault 0}
      , Cmd.none
      )
    CharacterKey 'd' ->
        ( {model | xShift = model.xShift + 1}, Cmd.none )
    CharacterKey 'a' ->
        ( {model | xShift = model.xShift - 1}, Cmd.none )
    CharacterKey 's' ->
        ( {model | yShift = model.yShift + 1}, Cmd.none )
    CharacterKey 'w' ->
        ( {model | yShift = model.yShift - 1}, Cmd.none )
    CharacterKey 'q' ->
        ( {model | zoom = model.zoom - 1}, Cmd.none )
    CharacterKey 'e' ->
        ( {model | zoom = model.zoom + 1}, Cmd.none )
    _ ->
        ( model, Cmd.none )

pixels x y model = if x == 1 then
    [div [style "width" "4px", style "height" "4px", style "float" "left", style "background" (mandelbrotColor x y model)] []]
  else
    (div [style "width" "4px", style "height" "4px", style "float" "left", style "background" (mandelbrotColor x y model)] []) :: pixels (x - 1) y model

grid x y model = if y == 1 then
    pixels x y model
  else
    pixels x y model ++ grid x (y - 1) model

gX = 175
gY = 100


view : Model -> Html Msg
view model =
  div [align "center",  style "padding" "70px 0"] 
  [
    (div [style "width" "700px", style "height" "400px"]) (grid gX gY model)
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
    , div []
      [
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
        Html.text <| "X-cord: "
        , Html.text <| String.fromFloat (model.xShift - 30)
      ],
      div []
      [
        Html.text <| "Y-cord: "
        , Html.text <| String.fromFloat (model.yShift - 100)
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





mandelbrotColor x y model =  if (mandelbrot (0,0) ((model.xShift - x)/model.zoom,(y - model.yShift)/model.zoom) model.itr) == 0 then
  "black"
 else if (mandelbrot (0,0) ((model.xShift - x)/model.zoom,(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 2)) then
  "Gold"
 else if (mandelbrot (0,0) ((model.xShift - x)/model.zoom,(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 4)) then
  "red"
 else if (mandelbrot (0,0) ((model.xShift - x)/model.zoom,(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 6)) then
  "orange"
  else if (mandelbrot (0,0) ((model.xShift - x)/model.zoom,(y - model.yShift)/model.zoom) model.itr) < (model.itr - (model.itr // 8)) then
  "green"
 else
  "blue"


type alias Model =
  { time : Time.Posix
  , zoom : Float
  , xShift : Float
  , yShift : Float
  , itr : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( 
    {
      time = Time.millisToPosix 0,
      zoom = 40,
      xShift = 30,
      yShift = 100,
      itr = 10
    }
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions _ = batch [
    Time.every 50 Tick, 
    onKeyPress keyDecoder
  ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue