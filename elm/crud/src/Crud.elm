module Crud exposing (main)

import Browser
import Element exposing (column, el, fill, layout, maximum, paddingXY, row, spacingXY, text, width)
import Element.Border as Border
import Element.Input as Input
import Html


type Msg
    = UpdatePrefix String


type alias Person =
    { firstName : String
    , lastName : String
    }


type alias Model =
    { persons : List Person
    , prefix : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { persons = [], prefix = "" }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePrefix p ->
            ( { model | prefix = p }, Cmd.none )



-- View


view : Model -> Html.Html Msg
view _ =
    layout [] <|
        column
            [ spacingXY 5 5
            , paddingXY 20 20
            , Border.width 1
            , width (fill |> maximum 300)
            ]
            [ row []
                [ el [] <| text "Filter prefix:"
                , Input.text [] {}
                ]
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
