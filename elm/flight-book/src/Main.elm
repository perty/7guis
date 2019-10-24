module Main exposing (FlightDate, ValidatedDate(..), main, validate)

import Browser
import Html exposing (button, div, input, option, select, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Regex



-- Possible states: no valid input, one-way t1, return t1 t2, return invalid t2


type Msg
    = UpdateOption String
    | UpdateT1 String
    | UpdateT2 String
    | BookingRequested


type OptionSelection
    = OneWay
    | Return


type alias FlightDate =
    { day : Int
    , month : Int
    , year : Int
    }


type ValidatedDate
    = Invalid String
    | Valid FlightDate


type alias Model =
    { option : OptionSelection
    , t1 : ValidatedDate
    , t2 : ValidatedDate
    , message : String
    }


init : Model
init =
    { option = OneWay
    , t1 = Invalid ""
    , t2 = Invalid ""
    , message = ""
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateOption optionString ->
            if optionString == oneWayText then
                { model | option = OneWay }

            else
                { model | option = Return }

        UpdateT1 string ->
            { model | t1 = validate string }

        UpdateT2 string ->
            { model | t2 = validate string }

        BookingRequested ->
            { model | message = "Booking requested" }


validate : String -> ValidatedDate
validate string =
    case pickNumbers string of
        [ d, m, y ] ->
            Valid (FlightDate d m y)

        _ ->
            Invalid string


pickNumbers : String -> List Int
pickNumbers string =
    case Regex.fromString "\\d+" of
        Just r ->
            List.map .match (Regex.find r string) |> List.map (String.toInt >> Maybe.withDefault 0)

        Nothing ->
            []



-- View


view : Model -> Html.Html Msg
view model =
    div []
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "max-width" "150px"
            , style "margin" "5px"
            , style "border-style" "solid"
            , style "border-color" "lightGrey"
            ]
            [ select [ onInput UpdateOption, inheritMargin ]
                [ option [] [ text oneWayText ]
                , option [] [ text returnFlightText ]
                ]
            , input
                [ onInput UpdateT1
                , inheritMargin
                , borderIndicatingError model.t1
                ]
                []
            , case model.option of
                OneWay ->
                    div [] []

                Return ->
                    input
                        [ onInput UpdateT2
                        , inheritMargin
                        , borderIndicatingError model.t2
                        ]
                        []
            , button ([ onClick BookingRequested, inheritMargin ] ++ buttonEnabled model) [ text "Book" ]
            , div [ inheritMargin ]
                [ text model.message
                ]
            ]
        ]


oneWayText =
    "one-way flight"


returnFlightText =
    "return flight"


inheritMargin : Html.Attribute Msg
inheritMargin =
    style "margin" "inherit"


borderIndicatingError : ValidatedDate -> Html.Attribute Msg
borderIndicatingError inValid =
    case inValid of
        Valid _ ->
            validInputStyle

        Invalid s ->
            if s == "" then
                validInputStyle

            else
                inValidInputStyle


validInputStyle : Html.Attribute Msg
validInputStyle =
    style "border-color" "lightGrey"


inValidInputStyle : Html.Attribute Msg
inValidInputStyle =
    style "border-color" "red"


buttonEnabled : Model -> List (Html.Attribute Msg)
buttonEnabled model =
    case model.t1 of
        Invalid _ ->
            [ Html.Attributes.attribute "disabled" "disabled" ]

        Valid _ ->
            if model.option == OneWay then
                []

            else
                case model.t2 of
                    Invalid _ ->
                        [ Html.Attributes.attribute "disabled" "disabled" ]

                    Valid _ ->
                        []



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
