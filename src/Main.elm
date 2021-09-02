module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MODEL


type alias Model =
    { size : String
    , price : String
    , perfectSize : String
    }


init : Model
init =
    { size = "28"
    , price = "10"
    , perfectSize = "28"
    }


type Msg
    = SizeChange String
    | PriceChange String
    | PerfectSizeChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SizeChange size ->
            { model | size = size }

        PriceChange price ->
            { model | price = price }

        PerfectSizeChange perfectSize ->
            { model | perfectSize = perfectSize }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container p-2" ]
        [ div [ class "content" ]
            [ h1 [ class "header" ] [ text "Pizza Pi Calculator" ]
            , small [] [ text "made with elm 🌳❤️" ]
            , div []
                [ div [ class "field-label is-normal" ]
                    [ label [ class "label" ]
                        [ text "Your sweetspot 🍕 size (diameter in cm)"
                        ]
                    ]
                , div [ class "field" ]
                    [ viewInput "number" "Perfect Size" model.perfectSize "input" PerfectSizeChange
                    ]
                ]
            , div [ class "columns" ]
                [ div [ class "column" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ]
                            [ text "Diameter in cm"
                            ]
                        ]
                    , div [ class "field" ]
                        [ viewInput "number" "Size" model.size "input" SizeChange
                        ]
                    ]
                , div [ class "column" ]
                    [ div [ class "field-label is-normal" ]
                        [ label [ class "label" ]
                            [ text "Price in Euro"
                            ]
                        ]
                    , div [ class "field" ]
                        [ viewInput "number" "Price" model.price "input" PriceChange
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
                            [ formatFullness <| fullnessFactor model ]
                        ]
                    ]
                ]
            ]
        ]


viewInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v c toMsg =
    input [ type_ t, placeholder p, value v, class c, onInput toMsg ] []


areaCircle : Float -> Float
areaCircle diameter =
    pi * (diameter / 2) ^ 2


fullnessFactor : Model -> Int
fullnessFactor model =
    -- the sweetspot area of a 30 cm diameter pizza
    let
        pizzaSize =
            case String.toFloat model.size of
                Just size ->
                    pi * ((size / 100) / 2) ^ 2

                Nothing ->
                    0.0

        bestDiameterOfPizza =
            String.toFloat model.perfectSize

        bestDiameterOfPizzaSquareMeter =
            case bestDiameterOfPizza of
                Just diameter ->
                    diameter / 100

                Nothing ->
                    0

        sweetSpotSizeCircleArea =
            areaCircle bestDiameterOfPizzaSquareMeter
    in
    round <| formatPercentage pizzaSize sweetSpotSizeCircleArea


formatPercentage : Float -> Float -> Float
formatPercentage x y =
    (x / y) * 100


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


sizeCm2ToM2 : Float -> Float
sizeCm2ToM2 f =
    100 * 100 * f


pricePerCm2 : Int -> Float -> Float
pricePerCm2 sizeInCm price =
    let
        circleArea =
            sizeInCm
                |> halfOf
                |> square
                |> timesPi
    in
    sizeCm2ToM2 <| price / circleArea



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
