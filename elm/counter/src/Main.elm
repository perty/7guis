module Main exposing (view)

import Browser
import Element exposing (Element, el, layout, paddingXY, px, row, spacingXY, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html


type Msg
    = Increment


type alias Model =
    { count : Int
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
    { count = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }


view : Model -> Html.Html Msg
view model =
    layout [] <|
        row
            [ spacingXY 5 5
            , paddingXY 20 20
            ]
            [ textBox model
            , countButton
            ]


textBox : Model -> Element Msg
textBox model =
    el
        [ Font.center
        , width (px 50)
        , paddingXY 5 5
        , Border.width 1
        ]
    <|
        text <|
            String.fromInt model.count


countButton : Element Msg
countButton =
    Input.button
        [ Font.bold
        , Border.width 1
        , Border.rounded 5
        , paddingXY 5 5
        ]
        { onPress = Just Increment
        , label = text "Count"
        }
