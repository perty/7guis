module Cells exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (Element, column, el, fill, height, layout, minimum, row, scrollbars, text, width)
import Element.Border as Border
import Element.Font as Font
import Html
import Matrix exposing (Matrix)



-- Starter. Remove this comment when started.


type Msg
    = CellInput Cell String


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
    , cellPos : ( Int, Int )
    }


type alias Model =
    { sheet : Matrix.Matrix Cell }


init : Model
init =
    { sheet = Matrix.initialize 25 100 cellInit }


cellInit : Int -> Int -> Cell
cellInit x y =
    Cell EmtpyCell ( x, y )



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellInput cell string ->
            handleInput cell string model


handleInput : Cell -> String -> Model -> Model
handleInput cell text model =
    let
        newCell =
            { cell | cellType = Constant (StringCell text) }

        newSheet =
            Matrix.set model.sheet cell.cellPos newCell
    in
    { model | sheet = newSheet }



-- View


view : Model -> Html.Html Msg
view model =
    layout [ width fill, height fill, scrollbars ] <|
        displaySheet model.sheet


displaySheet : Matrix.Matrix Cell -> Element Msg
displaySheet sheet =
    row [] ([ firstColumn 100 ] ++ (Array.toList <| Array.indexedMap displayColumn sheet))


firstColumn : Int -> Element Msg
firstColumn rows =
    column [] ([ el [ Font.center, width fill, height (fill |> minimum 20) ] <| text "" ] ++ List.map rowMarker (List.range 1 rows))


rowMarker : Int -> Element Msg
rowMarker n =
    el [] <| text <| String.fromInt n


displayColumn : Int -> Array Cell -> Element Msg
displayColumn index cells =
    column [] ([ el [ Font.center, width fill ] <| text <| col2letter index ] ++ (Array.toList <| Array.map displayCell cells))


displayCell : Cell -> Element Msg
displayCell cell =
    el [ Border.width 1, width (fill |> minimum 50), height (fill |> minimum 20) ] <| text <| cellToString cell


cellToString : Cell -> String
cellToString cell =
    case cell.cellType of
        Constant constantType ->
            case constantType of
                IntegerCell string ->
                    string

                StringCell string ->
                    string

        Formula formulaType ->
            formulaType.formula

        EmtpyCell ->
            ""


col2letter : Int -> String
col2letter n =
    Char.fromCode (65 + n) |> String.fromChar


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
