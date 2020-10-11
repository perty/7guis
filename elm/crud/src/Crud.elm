module Crud exposing (main)

import Browser
import Element exposing (Element, column, el, fill, height, layout, maximum, paddingXY, row, spacingXY, text, width)
import Element.Border as Border
import Element.Input as Input
import Html


type Msg
    = UpdatePrefix String
    | Create
    | Update
    | Delete


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

        Create ->
            ( model, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    layout [] <|
        column
            [ spacingXY 5 5
            , paddingXY 20 20
            , Border.width 1
            , width (fill |> maximum 500)
            ]
            [ filterPrefix model
            , row []
                [ listPersons model
                , personView model
                ]
            , buttonRow model
            ]


filterPrefix : Model -> Element Msg
filterPrefix model =
    row []
        [ Input.text []
            { onChange = UpdatePrefix
            , text = model.prefix
            , placeholder = Nothing
            , label = Input.labelLeft [] (el [] <| text "Filter prefix:")
            }
        ]


listPersons model =
    column [ Border.width 1, width fill, height fill ] (List.map listPerson model.persons)


listPerson person =
    el [] <| text <| person.lastName ++ "," ++ person.firstName


personView model =
    column []
        [ Input.text []
            { onChange = UpdatePrefix
            , text = model.prefix
            , placeholder = Nothing
            , label = Input.labelLeft [] (el [] <| text "Name: ")
            }
        , Input.text []
            { onChange = UpdatePrefix
            , text = model.prefix
            , placeholder = Nothing
            , label = Input.labelLeft [] (el [] <| text "Surname: ")
            }
        ]


buttonRow model =
    row []
        [ Input.button []
            { onPress = Just Create
            , label = el [] <| text "Create"
            }
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
