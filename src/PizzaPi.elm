module PizzaPi exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL


type alias Model =
  { size : String
  , price : String
  }



init : Model
init =
  Model "30" "10"


type Msg
  = Size String
  | Price String
 

update : Msg -> Model -> Model
update msg model =
  case msg of
    Size size ->
      { model | size = size }

    Price price ->
      { model | price = price }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Size" model.size Size
    , viewInput "text" "Price" model.price Price
    , calculatePricePerCm2 model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


calculatePricePerCm2 : Model -> Html msg
calculatePricePerCm2 model =
    let
        pizzaprice =
            case (String.toFloat model.price) of
                Just price -> 
                    case (String.toInt model.size) of
                        Just size -> pricePerCm2 size price
                        Nothing -> 0.0
                Nothing -> 0.0

    in
        div [ style "color" "green" ] [
          text ("etwa " ++ (String.fromInt (round pizzaprice)) ++ " Euro") 
          ]

halfOf : Int -> Float
halfOf number =
  (toFloat number) / 2

square : Float -> Float
square number =
    number ^ 2

timesPi : Float -> Float
timesPi number =
    number * pi

pricePerCm2 : Int -> Float -> Float
pricePerCm2 size price =
    let
        circleArea =
            size
              |> halfOf
              |> square
              |> timesPi
        
    in
    price / circleArea * 100 * 100
