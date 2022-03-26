module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
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
  ItrScroll String    |
  ToggleDensity       |
  CharacterKey Char   | 
  ControlKey String

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Tick t ->
      ( { model | time = t, itr = model.itr+1}
      , Cmd.none
      )
    ItrScroll s ->
      ( { model | itr = String.toInt s |> Maybe.withDefault 0}
      , Cmd.none
      )
    ToggleDensity ->
      ( { model | density = if model.density == 4 then 2 else if model.density == 20 then 4 else 20}
      , Cmd.none
      )
    CharacterKey 'a' ->
        ( {model | xShift = model.xShift + 4/model.zoom}, Cmd.none )
    CharacterKey 'd' ->
        ( {model | xShift = model.xShift - 4/model.zoom}, Cmd.none )
    CharacterKey 'w' ->
        ( {model | yShift = model.yShift + 4/model.zoom}, Cmd.none )
    CharacterKey 's' ->
        ( {model | yShift = model.yShift - 4/model.zoom}, Cmd.none )
    CharacterKey 'q' ->
        ( {model | zoom = model.zoom * 0.9}, Cmd.none )
    CharacterKey 'e' ->
        ( {model | zoom = model.zoom * 1.1}, Cmd.none )
    _ ->
        ( model, Cmd.none )

pixels : Int -> Int -> Model -> List (Html Msg)
pixels x y model = 
  let
    pxS = ((String.fromInt model.density) ++ "px")
  in
    if x == Basics.min (model.density - 3) 1 then
      [div [style "width" pxS, style "height" pxS, style "float" "left", style "background" (mandelbrotColor x y model)] []]
    else
      (div [style "width" pxS, style "height" pxS, style "float" "left", style "background" (mandelbrotColor x y model)] []) :: pixels (x - 1) y model

grid : Int -> Int -> Model -> List (Html Msg)
grid x y model = if y == 1 then
    pixels x y model
  else
    pixels x y model ++ grid x (y - 1) model

gX = 700
gY = 400


view : Model -> Html Msg
view model =
  div [align "center",  style "padding" "70px 0"] 
  [
    (div [style "width" "700px", style "height" "400px"]) (grid (gX//model.density) (gY//model.density) model)
    , div []
      [
        Html.text <| " Iterations: "
        , input
            [ type_ "range"
            , Attrs.min "1"
            , Attrs.max "800"
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
        , Html.text <| String.fromFloat <| (toFloat <| round (model.xShift*1000))/1000
      ],
      div []
      [
        Html.text <| "Y-cord: "
        , Html.text <| String.fromFloat <| (toFloat <| round (model.yShift*1000))/1000
      ],
      button [onClick ToggleDensity] [Html.text "Resolution"],
      h2 []
      [
        Html.text <| "Controls "
      ],
      table [style "border" "1px solid black", style "width" "50%", style "text-align" "center"]
      [
        tr []
        [
          th [style "border" "1px solid black"] [Html.text <|"Button"],
          th [style "border" "1px solid black"] [Html.text <|"Description"]
        ],
        tr []
        [
          td [style "border" "1px solid black"] [Html.text <|"w, a, s, d"],
          td [style "border" "1px solid black"] [Html.text <|"move up, left, down, right respectivly"]
        ],
        tr []
        [
          td [style "border" "1px solid black"] [Html.text <|"q"],
          td [style "border" "1px solid black"] [Html.text <|"zoom out"]
        ],
        tr []
        [
          td [style "border" "1px solid black"] [Html.text <|"e"],
          td [style "border" "1px solid black"] [Html.text <|"zoom in"]
        ]
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

    if n == 0 || absCmplx(newZ) > 5 then
        n
    else
        mandelbrot newZ c (n - 1)



-- x is from 1 to gX
-- y is from 1 to gY
-- we want to fit x into -gX/zoom to gX/zoom
-- we want to fit y into -gY/zoom to gY/zoom
-- x = (x - gX/2)/zoom - xShift
-- y = (y - gY/2)/zoom - yShift


-- test cases:
-- -16 to 16             start       
-- -2 to 2               devide by 8
-- 4 to 8                add 6

-- -16 to 16                 start
-- -0.25 to 0.25               devide by 64
-- 5.75 to 6.25                add 6
-- range is maintaned!

mandelbrotColor : Int -> Int -> Model -> String
mandelbrotColor x y model = 
  let
    nX = (gX/(2 * toFloat model.density) - toFloat x)/(model.zoom / toFloat model.density) - model.xShift
    nY = (gY/(2 * toFloat model.density) - toFloat y)/(model.zoom / toFloat model.density) - model.yShift
    n = model.itr - (mandelbrot (0,0) (nX,nY) model.itr)
  in
    if n == model.itr then
      "black"
    else
      "hsl(" ++ (String.fromInt (modBy 360 <| round <| (sqrt (toFloat (300*n)) ))) ++ ", 100%, 70%)"


type alias Model =
  { time     : Time.Posix
  , zoom     : Float
  , xShift   : Float
  , yShift   : Float
  , itr      : Int
  , density  : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( 
    {
      time = Time.millisToPosix 0,
      zoom = 200,
      xShift = 0,
      yShift = 0,
      itr = 1,
      density = 4
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