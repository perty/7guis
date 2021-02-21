module FlightBook exposing (FlightDate, ValidatedDate(..), after, main, validate)

import Browser
import Element exposing (centerX, column, el, fill, layout, paddingXY, paragraph, rgb, spacingXY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (button, div, input, option, p, select, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Regex



-- Possible states: no valid input, one-way t1, return t1 t2, return invalid t2


type Msg
    = UpdateSelection String
    | UpdateDeparture String
    | UpdateReturn String
    | BookingRequested
    | ChangeReturn Selection
    | NoOp String


type Selection
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


type ValidModelType
    = ValidModel
    | InvalidDeparture
    | InvalidReturn
    | ReturnNotAfterDeparture


type alias Model =
    { validModel : ValidModelType
    , option : Selection
    , departureDateString : String
    , returnDateString : String
    , departureDate : ValidatedDate
    , returnDate : ValidatedDate
    , message : String
    }


init : Model
init =
    { validModel = InvalidDeparture
    , option = OneWay
    , departureDateString = ""
    , returnDateString = ""
    , departureDate = Invalid ""
    , returnDate = Invalid ""
    , message = ""
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSelection optionString ->
            if optionString == oneWayText then
                validatedModel { model | option = OneWay }

            else
                validatedModel { model | option = Return }

        ChangeReturn option ->
            validatedModel { model | option = option }

        UpdateDeparture string ->
            validatedModel { model | departureDate = validate string, departureDateString = string, message = "" }

        UpdateReturn string ->
            validatedModel { model | returnDate = validate string, returnDateString = string, message = "" }

        BookingRequested ->
            validatedModel
                { model
                    | message =
                        case model.option of
                            OneWay ->
                                "Booking requested: " ++ model.departureDateString

                            Return ->
                                "Booking requested: " ++ model.departureDateString ++ ", " ++ model.returnDateString
                }

        NoOp _ ->
            model


validatedModel : Model -> Model
validatedModel model =
    case model.option of
        OneWay ->
            case model.departureDate of
                Invalid _ ->
                    { model | validModel = InvalidDeparture }

                Valid _ ->
                    { model | validModel = ValidModel }

        Return ->
            case model.departureDate of
                Invalid _ ->
                    { model | validModel = InvalidDeparture }

                Valid t1 ->
                    case model.returnDate of
                        Invalid _ ->
                            { model | validModel = InvalidReturn }

                        Valid t2 ->
                            if after t2 t1 then
                                { model | validModel = ValidModel }

                            else
                                { model | validModel = ReturnNotAfterDeparture }


after : FlightDate -> FlightDate -> Bool
after return departure =
    let
        t2Int =
            return.year * 10000 + return.month * 100 + return.day

        t1Int =
            departure.year * 10000 + departure.month * 100 + departure.day
    in
    t2Int > t1Int


validate : String -> ValidatedDate
validate string =
    case pickNumbers string of
        [ y, m, d ] ->
            validateDate d m y

        _ ->
            Invalid ("Format: " ++ dateformat)


dateformat : String
dateformat =
    "yyyy-mm-dd"


validateDate : Int -> Int -> Int -> ValidatedDate
validateDate day month year =
    if validYear year && validMonth month && validDay day then
        Valid (FlightDate day month year)

    else
        Invalid "Invalid date"


validYear : Int -> Bool
validYear year =
    year > 2018 && year < 2100


validMonth : Int -> Bool
validMonth month =
    month >= 1 && month <= 12


validDay : Int -> Bool
validDay day =
    day >= 1 && day <= 31


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
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        [ viewHtml model
        , viewElmUi model
        ]


viewElmUi : Model -> Html.Html Msg
viewElmUi model =
    let
        selector =
            Input.radioRow [ spacingXY 5 0 ]
                { onChange = ChangeReturn
                , options =
                    [ Input.option OneWay (Element.text oneWayText)
                    , Input.option Return (Element.text returnFlightText)
                    ]
                , selected = Just model.option
                , label = Input.labelHidden "hidden"
                }

        formatHint =
            Just <| Input.placeholder [] <| Element.text <| "yyyy-mm-dd"

        departureField =
            Input.text (fieldStyle model.departureDate)
                { onChange = UpdateDeparture
                , text = model.departureDateString
                , placeholder = formatHint
                , label = Input.labelAbove [ Font.size 12 ] <| Element.text "Departure date"
                }

        returnField =
            let
                enabled =
                    case model.departureDate of
                        Invalid _ ->
                            False

                        Valid _ ->
                            True
            in
            case model.option of
                Return ->
                    Input.text (fieldStyle model.returnDate)
                        { onChange =
                            if enabled then
                                UpdateReturn

                            else
                                NoOp
                        , text = model.returnDateString
                        , placeholder =
                            if enabled then
                                formatHint

                            else
                                Just <| Input.placeholder [] <| Element.text "enter departure date"
                        , label = Input.labelAbove [ Font.size 12 ] <| Element.text "Return date"
                        }

                OneWay ->
                    Element.none

        bookButton =
            case model.validModel of
                ValidModel ->
                    Input.button [ width fill, Border.width 1, Border.rounded 5, Background.color lightGrey ]
                        { onPress = Just BookingRequested
                        , label = el [ paddingXY 5 5, centerX ] <| Element.text <| "Book"
                        }

                _ ->
                    Input.button [ width fill, Border.width 1, Border.rounded 5 ]
                        { onPress = Nothing
                        , label = el [ paddingXY 5 5, centerX, Font.color lightGrey ] <| Element.text <| "(Invalid date)"
                        }

        footer =
            el [ width fill, Font.size 12 ] <| Element.text model.message

        fieldStyle field =
            case field of
                Valid _ ->
                    [ Border.width 1, Border.rounded 5, Border.color lightGrey ]

                Invalid _ ->
                    [ Border.width 1, Border.rounded 5, Border.color red ]
    in
    layout [ width fill ] <|
        column [ centerX, spacingXY 0 5 ]
            [ paragraph [ Font.center ] [ Element.text <| ("Date format " ++ dateformat) ]
            , column [ width fill, Border.width 1, Border.color lightGrey, spacingXY 0 5, paddingXY 5 5 ]
                [ selector
                , departureField
                , returnField
                , bookButton
                , footer
                ]
            ]


lightGrey : Element.Color
lightGrey =
    rgb 0.7 0.7 0.7


red : Element.Color
red =
    rgb 1 0 0


viewHtml : Model -> Html.Html Msg
viewHtml model =
    div []
        [ p [] [ text ("Date format " ++ dateformat) ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "max-width" "150px"
            , style "margin" "5px"
            , style "border-style" "solid"
            , style "border-color" "lightGrey"
            ]
            [ select [ onInput UpdateSelection, inheritMargin ]
                [ option [] [ text oneWayText ]
                , option [] [ text returnFlightText ]
                ]
            , input
                [ onInput UpdateDeparture
                , inheritMargin
                , borderIndicatingError model.departureDate
                , style "outline" "none"
                ]
                []
            , case model.option of
                OneWay ->
                    div [] []

                Return ->
                    input
                        [ onInput UpdateReturn
                        , inheritMargin
                        , borderIndicatingError model.returnDate
                        , style "outline" "none"
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
    case model.validModel of
        ValidModel ->
            []

        _ ->
            [ Html.Attributes.attribute "disabled" "disabled" ]



-- FlightBook


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
