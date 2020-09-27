module CircleDraw exposing (main)

import Browser
import Html exposing (div, p, text)



-- Starter. Remove this comment when started.


type Msg
    = NoOp


type alias Model =
    {}


init : Model
init =
    {}



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- View


view : Model -> Html.Html Msg
view _ =
    div []
        [ p []
            [ text "Hello world!"
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
