module Main exposing (main)

import Array
import Browser
import Html exposing (div, p, text)
import Matrix exposing (Matrix)



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
    | EmtpyCell


type alias Cell =
    { cellType : CellType
    }


type alias Model =
    { sheet : Matrix.Matrix Cell }


init : Model
init =
    { sheet = Matrix.initialize 25 100 cellInit }


rows : List Int
rows =
    List.range 0 99


cellInit : Int -> Int -> Cell
cellInit _ _ =
    Cell EmtpyCell



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
        , displaySheet model.sheet
        ]


displaySheet : Matrix.Matrix Cell -> Html.Html Msg
displaySheet sheet =
    Html.table [] <|
        List.map (displayRow sheet) rows


displayRow : Matrix.Matrix Cell -> Int -> Html.Html Msg
displayRow sheet row =
    Html.tr []
        ([ Html.td [] [ text <| String.fromInt row ] ]
            ++ (Array.map displayCell (Matrix.getYs sheet row)
                    |> Array.toList
               )
        )


displayCell : Cell -> Html.Html Msg
displayCell cell =
    Html.td [] [ text "cell" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
