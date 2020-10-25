module Cells exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Element exposing (Element, column, el, fill, height, layout, px, row, scrollbars, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Matrix exposing (Matrix)
import Task



-- Starter. Remove this comment when started.


type Msg
    = CellInput Cell String
    | SelectCell Cell
    | NoOp


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
    { sheet : Matrix.Matrix Cell
    , selectedPos : ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sheet = Matrix.initialize 25 100 cellInit
      , selectedPos = ( -1, -1 )
      }
    , Cmd.none
    )


cellInit : Int -> Int -> Cell
cellInit x y =
    Cell EmtpyCell ( x, y )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellInput cell string ->
            ( handleInput cell string model, Cmd.none )

        SelectCell cell ->
            ( { model | selectedPos = cell.cellPos }, focusCell cell )

        NoOp ->
            ( model, Cmd.none )


handleInput : Cell -> String -> Model -> Model
handleInput cell text model =
    let
        newCell =
            { cell | cellType = Constant (StringCell text) }

        newSheet =
            Matrix.set model.sheet cell.cellPos newCell
    in
    { model | sheet = newSheet }


focusCell : Cell -> Cmd Msg
focusCell cell =
    let
        ( x, y ) =
            cell.cellPos
    in
    Task.attempt (\_ -> NoOp) (Dom.focus <| ("cell" ++ String.fromInt x ++ String.fromInt y))



-- View


view : Model -> Html.Html Msg
view model =
    layout [ width fill, height fill, scrollbars ] <|
        displaySheet model.selectedPos model.sheet


displaySheet : ( Int, Int ) -> Matrix.Matrix Cell -> Element Msg
displaySheet selectedPos sheet =
    row [] ([ firstColumn 100 ] ++ (Array.toList <| Array.indexedMap (displayColumn selectedPos) sheet))


firstColumn : Int -> Element Msg
firstColumn rows =
    column [] ([ el [ Font.center, width fill, height (px 50) ] <| text "" ] ++ List.map rowMarker (List.range 1 rows))


rowMarker : Int -> Element Msg
rowMarker n =
    el [ Font.center, width fill, height (px 40) ] <| text <| String.fromInt n


displayColumn : ( Int, Int ) -> Int -> Array Cell -> Element Msg
displayColumn selectedPos index cells =
    column [] ([ el [ Font.center, width fill ] <| text <| col2letter index ] ++ (Array.toList <| Array.map (displayCell selectedPos) cells))


displayCell : ( Int, Int ) -> Cell -> Element Msg
displayCell selectPos cell =
    let
        attr =
            [ Border.widthEach { top = 0, left = 0, right = 1, bottom = 1 }, width (px 110), height (px 40), htmlId cell.cellPos ]
    in
    if cell.cellPos == selectPos then
        Input.text attr
            { onChange = CellInput cell
            , text = cellToString cell
            , placeholder = Nothing
            , label = Input.labelHidden "cell"
            }

    else
        Input.button attr
            { onPress = Just <| SelectCell cell
            , label = el [] <| text <| cellToString cell
            }


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


htmlId : ( Int, Int ) -> Element.Attribute msg
htmlId ( x, y ) =
    Element.htmlAttribute (Html.Attributes.id ("cell" ++ String.fromInt x ++ String.fromInt y))


col2letter : Int -> String
col2letter n =
    Char.fromCode (65 + n) |> String.fromChar


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
