module Main exposing (main)

import Array
import Browser
import Html exposing (div, p, text)
import Html.Attributes exposing (style, value)
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


cols : List Int
cols =
    List.range 0 24


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
            [ text "Hello spreadsheet!"
            ]
        , displaySheet model.sheet
        ]


tableAttr : List (Html.Attribute Msg)
tableAttr =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    ]


thAttr : List (Html.Attribute Msg)
thAttr =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    ]


tdAttr : List (Html.Attribute Msg)
tdAttr =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    ]


cellAttr : List (Html.Attribute Msg)
cellAttr =
    [ style "min-width" "30px"
    ]


displaySheet : Matrix.Matrix Cell -> Html.Html Msg
displaySheet sheet =
    Html.table tableAttr
        [ Html.thead thAttr ([ Html.th thAttr [] ] ++ List.map col2letter (List.range 0 24))
        , Html.tbody [] (List.map (displayRow sheet) (List.range 0 99))
        ]


col2letter : Int -> Html.Html Msg
col2letter n =
    Html.th thAttr [ Char.fromCode (65 + n) |> String.fromChar |> text ]


displayRow : Matrix.Matrix Cell -> Int -> Html.Html Msg
displayRow sheet row =
    Html.tr []
        ([ Html.td tdAttr [ text <| String.fromInt row ] ]
            ++ (Array.map displayCell (Matrix.getYs sheet row)
                    |> Array.toList
               )
        )


displayCell : Cell -> Html.Html Msg
displayCell cell =
    Html.td cellAttr [ cellContents cell ]


cellContents : Cell -> Html.Html Msg
cellContents cell =
    Html.input [ value "x" ] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
