module Main exposing (..)

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
    div [ class "container p-2" ]
        [ div [ class "content" ]
            [ h1 [ class "header" ] [ text "Pizza Pi Calculator" ]
            , p [] [ text "made with elm 🌳❤️" ]
            , div [ class "columns" ]
                [ div [ class "column" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ]
                            [ text "Diameter in cm"
                            ]
                        ]
                    , div [ class "field" ]
                        [ viewInput "text" "Size" model.size "input" Size
                        , fillingFactor model
                        ]
                    ]
                , div [ class "column" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ]
                            [ text "Price in Euro"
                            ]
                        ]
                    , div [ class "field" ]
                        [ viewInput "text" "Price" model.price "input" Price
                        , calculatePricePerCm2 model
                        ]
                    ]
                ]
            ]
        ]


viewInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v c toMsg =
    input [ type_ t, placeholder p, value v, class c, onInput toMsg ] []


fillingFactor : Model -> Html msg
fillingFactor model =
    let
        pizzaSize =
            case String.toFloat model.size of
                Just size ->
                    pi * ((size / 100) / 2) ^ 2

                Nothing ->
                    0.0
    in
    div []
        [ formatFullness (fullnessFactor pizzaSize) ]


fullnessFactor : Float -> Int
fullnessFactor pizzaSize =
    -- the sweetspot area of a 30 cm diameter pizza
    let
        bestDiameterOfPizza =
            30

        sweetSpot =
            pi * ((bestDiameterOfPizza / 2) / 100) ^ 2
    in
    round ((pizzaSize / sweetSpot) * 100)


formatFullness : Int -> Html msg
formatFullness fullnessFactorResult =
    if fullnessFactorResult >= 100 then
        text ("You will feel full, " ++ String.fromInt fullnessFactorResult ++ " pct 😎")

    else
        text ("You will feel filled " ++ String.fromInt fullnessFactorResult ++ " pct 😖")


calculatePricePerCm2 : Model -> Html msg
calculatePricePerCm2 model =
    let
        pizzaprice =
            case String.toFloat model.price of
                Just price ->
                    case String.toInt model.size of
                        Just size ->
                            pricePerCm2 size price

                        Nothing ->
                            0.0

                Nothing ->
                    0.0
    in
    div [ style "color" "green" ]
        [ text ("1 m² costs approx. " ++ String.fromInt (round pizzaprice) ++ " Euro")
        ]


halfOf : Int -> Float
halfOf number =
    toFloat number / 2


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
