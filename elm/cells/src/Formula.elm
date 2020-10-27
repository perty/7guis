module Formula exposing (Cell, CellState(..), CellType(..), ConstantType(..), FormulaType, cellInit, cellValue, parseCell, sum)


type ConstantType
    = NumberCell Float
    | StringCell String


type alias FormulaType =
    { formula : String
    , dependencies : List Cell
    }


type CellType
    = Constant ConstantType
    | Formula FormulaType
    | EmtpyCell


type CellState
    = Ok
    | Error


type alias Cell =
    { cellType : CellType
    , cellPos : ( Int, Int )
    , cellInput : String
    , cellState : CellState
    }


cellInit : Int -> Int -> Cell
cellInit x y =
    Cell EmtpyCell ( x, y ) "" Ok


parseCell : Cell -> Cell
parseCell cell =
    if String.startsWith "=" cell.cellInput then
        parseFormula cell

    else
        parseConstant cell


parseFormula : Cell -> Cell
parseFormula cell =
    { cell | cellState = Error }


parseConstant : Cell -> Cell
parseConstant cell =
    case String.toFloat cell.cellInput of
        Just value ->
            { cell | cellType = Constant (NumberCell value), cellState = Ok }

        Nothing ->
            { cell | cellType = Constant (StringCell cell.cellInput), cellState = Ok }


sum : List Cell -> Float
sum cellList =
    List.map cellValue cellList |> List.foldr (+) 0


cellValue : Cell -> Float
cellValue cell =
    case cell.cellType of
        Constant constantType ->
            case constantType of
                NumberCell float ->
                    float

                StringCell _ ->
                    0

        Formula formulaType ->
            0

        EmtpyCell ->
            0
