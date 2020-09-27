module Timer exposing (main)

import Browser
import Html exposing (Html, div, p, text)



-- Starter. Remove this comment when started.


type Msg
    = NoOp


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view _ =
    div []
        [ p []
            [ text "Hello world!"
            ]
        ]



-- Subscriptions. A timer.


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
