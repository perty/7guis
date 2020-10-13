module Crud exposing (main)

import Browser
import CrudBackendApi exposing (Person, loadPersons)
import Element exposing (Element, column, el, fill, height, layout, maximum, minimum, paddingXY, row, spacingXY, text, width)
import Element.Border as Border
import Element.Input as Input
import Html


type Msg
    = UpdatePrefix String
    | Create
    | Update
    | Delete
    | PersonsLoaded (Result String (List Person))


type alias Model =
    { persons : List Person
    , prefix : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { persons = [], prefix = "" }, loadPersons PersonsLoaded )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonsLoaded (Ok persons) ->
            ( { model | persons = persons }, Cmd.none )

        PersonsLoaded (Err _) ->
            ( model, Cmd.none )

        UpdatePrefix p ->
            ( { model | prefix = p }, Cmd.none )

        Create ->
            ( model, Cmd.none )

        Update ->
            ( model, Cmd.none )

        Delete ->
            ( model, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    layout [ width (fill |> maximum 600) ] <|
        column [ width fill, Border.width 1, paddingXY 10 10 ]
            [ row [ width fill ]
                [ column [ spacingXY 5 5, paddingXY 20 20, width fill, height fill ]
                    [ filterPrefix model
                    , listPersons model
                    ]
                , personView model
                ]
            , buttonRow
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
    column [ Border.width 1, width fill, height (fill |> minimum 200) ] (List.map listPerson model.persons)


listPerson person =
    el [] <| text <| person.lastName ++ "," ++ person.firstName


personView model =
    column [ width fill ]
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


buttonRow =
    row [ spacingXY 5 5, width fill ]
        [ Input.button buttonAttr
            { onPress = Just Create
            , label = el [] <| text "Create"
            }
        , Input.button buttonAttr
            { onPress = Just Update
            , label = el [] <| text "Update"
            }
        , Input.button buttonAttr
            { onPress = Just Delete
            , label = el [] <| text "Delete"
            }
        ]


buttonAttr =
    [ Border.width 1, Border.rounded 5, paddingXY 5 5 ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
