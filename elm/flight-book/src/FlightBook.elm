module FlightBook exposing (FlightDate, ValidatedDate(..), after, main, validate)

import Browser
import Element exposing (centerX, column, el, fill, layout, paddingXY, paragraph, rgb, spacingXY, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (button, div, input, option, p, select, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Regex



-- Possible states: no valid input, one-way t1, return t1 t2, return invalid t2


type Msg
    = UpdateOption String
    | UpdateT1 String
    | UpdateT2 String
    | BookingRequested
    | ChangeReturn OptionSelection


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


type ValidModelType
    = ValidModel
    | InvalidT1
    | InvalidT2
    | T1NotAfterT2


type alias Model =
    { validModel : ValidModelType
    , option : OptionSelection
    , departureDateString : String
    , returnDateString : String
    , t1 : ValidatedDate
    , t2 : ValidatedDate
    , message : String
    }


init : Model
init =
    { validModel = InvalidT1
    , option = OneWay
    , departureDateString = ""
    , returnDateString = ""
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
                validatedModel { model | option = OneWay }

            else
                validatedModel { model | option = Return }

        ChangeReturn option ->
            validatedModel { model | option = option }

        UpdateT1 string ->
            validatedModel { model | t1 = validate string, departureDateString = string }

        UpdateT2 string ->
            validatedModel { model | t2 = validate string, returnDateString = string }

        BookingRequested ->
            validatedModel { model | message = "Booking requested" }


validatedModel : Model -> Model
validatedModel model =
    case model.option of
        OneWay ->
            case model.t1 of
                Invalid _ ->
                    { model | validModel = InvalidT1 }

                Valid _ ->
                    { model | validModel = ValidModel }

        Return ->
            case model.t1 of
                Invalid _ ->
                    { model | validModel = InvalidT1 }

                Valid t1 ->
                    case model.t2 of
                        Invalid _ ->
                            { model | validModel = InvalidT2 }

                        Valid t2 ->
                            if after t2 t1 then
                                { model | validModel = ValidModel }

                            else
                                { model | validModel = T1NotAfterT2 }


after : FlightDate -> FlightDate -> Bool
after t2 t1 =
    let
        t2Int =
            t2.year * 10000 + t2.month * 100 + t2.day

        t1Int =
            t1.year * 10000 + t1.month * 100 + t1.day
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
    div []
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

        departureField =
            Input.text []
                { onChange = UpdateT1
                , text = model.departureDateString
                , placeholder = Nothing
                , label = Input.labelHidden "hidden"
                }

        returnField =
            case model.option of
                Return ->
                    Input.text []
                        { onChange = UpdateT2
                        , text = model.returnDateString
                        , placeholder = Nothing
                        , label = Input.labelHidden "hidden"
                        }

                OneWay ->
                    Element.none

        bookButton =
            case model.validModel of
                ValidModel ->
                    Input.button [ width fill, Border.width 1, Border.rounded 5 ]
                        { onPress = Just BookingRequested
                        , label = el [ paddingXY 5 5, centerX ] <| Element.text <| "Book"
                        }

                _ ->
                    Input.button [ width fill, Border.width 1, Border.rounded 5 ]
                        { onPress = Nothing
                        , label = el [ paddingXY 5 5, centerX ] <| Element.text <| "Invalid date"
                        }

        footer =
            el [ width fill ] <| Element.text model.message
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
    rgb 0.5 0.5 0.5


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
            [ select [ onInput UpdateOption, inheritMargin ]
                [ option [] [ text oneWayText ]
                , option [] [ text returnFlightText ]
                ]
            , input
                [ onInput UpdateT1
                , inheritMargin
                , borderIndicatingError model.t1
                , style "outline" "none"
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
