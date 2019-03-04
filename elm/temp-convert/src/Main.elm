module Main exposing (main)

import Browser
import Element exposing (Color, Element, el, layout, none, paddingXY, rgb, row, spacingXY, text)
import Element.Border as Border
import Element.Input as Input
import Html



{-
   The interesting thing is how to handle invalid input. We need to keep it since the next character typed
   may turn into a valid input. Typically a negative number. The minus sign is not a valid number but when
   a digit follows, it is.

   Central to this the interdependence of the fields. If a number is entered in one field, the other is updated
   regardless of what was there before. We model this as a TemperatureInput and tag it with which temperature
   was updated.
-}
-- Business rules


fahrenheitFromCelsius : Float -> Float
fahrenheitFromCelsius celsius =
    celsius * (9 / 5) + 32


celsiusFromFahrenheit : Float -> Float
celsiusFromFahrenheit fahrenheit =
    (fahrenheit - 32) * (5 / 9)



-- Events


type Msg
    = CelsiusChanged String
    | FahrenheitChanged String



-- Inputs


type TemperatureInput
    = CelsiusInput String
    | FahrenheitInput String
    | NoInput



-- Valid state


type ValidField
    = InValid
    | Valid


type alias Model =
    { tempratureInput : TemperatureInput
    , celsiusFieldValid : ValidField
    , farenheitFieldValid : ValidField
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { tempratureInput = NoInput
    , celsiusFieldValid = Valid
    , farenheitFieldValid = Valid
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusChanged s ->
            { model | tempratureInput = CelsiusInput s, celsiusFieldValid = validate s }

        FahrenheitChanged s ->
            { model | tempratureInput = FahrenheitInput s, farenheitFieldValid = validate s }


validate : String -> ValidField
validate s =
    if String.toFloat s == Nothing then
        InValid

    else
        Valid



-- View


view : Model -> Html.Html Msg
view model =
    layout [] <|
        row
            [ spacingXY 5 5
            , paddingXY 20 20
            ]
            [ celsiusField model
            , el [] <| text <| "Celsius = "
            , fahrenheitField model
            , el [] <| text <| "Fahrenheit"
            ]


celsiusField : Model -> Element Msg
celsiusField model =
    Input.text
        (borderIndicatingError model.celsiusFieldValid)
        { onChange = CelsiusChanged
        , text = celsiusFieldValue model.tempratureInput
        , placeholder = Nothing
        , label = Input.labelAbove [] none
        }


fahrenheitField : Model -> Element Msg
fahrenheitField model =
    Input.text
        (borderIndicatingError model.farenheitFieldValid)
        { onChange = FahrenheitChanged
        , text = fahrenheitFieldValue model.tempratureInput
        , placeholder = Nothing
        , label = Input.labelAbove [] none
        }


celsiusFieldValue : TemperatureInput -> String
celsiusFieldValue input =
    case input of
        NoInput ->
            ""

        CelsiusInput s ->
            s

        FahrenheitInput s ->
            convert s celsiusFromFahrenheit


fahrenheitFieldValue : TemperatureInput -> String
fahrenheitFieldValue input =
    case input of
        NoInput ->
            ""

        CelsiusInput s ->
            convert s fahrenheitFromCelsius

        FahrenheitInput s ->
            s


convert : String -> (Float -> Float) -> String
convert s fn =
    case String.toFloat s of
        Nothing ->
            ""

        Just value ->
            String.fromFloat <| fn value


borderIndicatingError : ValidField -> List (Element.Attribute Msg)
borderIndicatingError inValid =
    [ case inValid of
        InValid ->
            Border.color red

        Valid ->
            Border.color black
    ]


red : Color
red =
    rgb 255 0 0


black : Color
black =
    rgb 0 0 0
