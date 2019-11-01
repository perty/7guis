module Main exposing (main)

import Browser
import Html exposing (div, p, text)



-- Starter. Remove this comment when started.


type Msg
    = NoOp


type ConstantType
    = IntegerCell String
    | StringCell String


type alias FormulaType =
    { formula : String
    , dependencies : List Cell
    }


type CellType
    = Constant ConstantType
    | Formula FormulaType


type alias Cell =
    { cellType : CellType
    , cellCoordinates : { row : Int, col : Char }
    }


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
view model =
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
