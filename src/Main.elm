module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



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
                        [ viewInput "number" "Size" model.size "input" Size
                        ]
                    ]
                , div [ class "column" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ]
                            [ text "Price in Euro"
                            ]
                        ]
                    , div [ class "field" ]
                        [ viewInput "number" "Price" model.price "input" Price
                        ]
                    ]
                ]
            , hr [] []
            , div [ class "columns" ]
                [ div [ class "column" ]
                    [ p [] [ calculatePricePerCm2 model ]
                    , p []
                        [ strong []
                            [ text "How you will feel: " ]
                        , span
                            []
                            [ fillingFactor model ]
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
    formatFullness (fullnessFactor pizzaSize)


areaCircle : Float -> Float
areaCircle diameter =
    pi * (diameter / 2) ^ 2


fullnessFactor : Float -> Int
fullnessFactor pizzaSize =
    -- the sweetspot area of a 30 cm diameter pizza
    let
        bestDiameterOfPizza =
            30

        bestDiameterOfPizzaSquareMeter =
            bestDiameterOfPizza / 100

        sweetSpot =
            areaCircle bestDiameterOfPizzaSquareMeter
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
    pi * number


pricePerCm2 : Int -> Float -> Float
pricePerCm2 sizeInCm price =
    let
        circleArea =
            sizeInCm
                |> halfOf
                |> square
                |> timesPi
    in
    price / sizeCm2ToM2 circleArea


sizeCm2ToM2 : number -> number
sizeCm2ToM2 n =
    n * 100 * 100



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
