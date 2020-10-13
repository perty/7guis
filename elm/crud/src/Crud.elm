module Crud exposing (main)

import Browser
import CrudBackendApi exposing (Database, Person, initDatabase, loadPersons)
import Element exposing (Element, column, el, fill, height, layout, maximum, minimum, paddingXY, row, spacingXY, text, width)
import Element.Border as Border
import Element.Input as Input
import Html


type Msg
    = UpdatePrefix String
    | UpdateFirstName String
    | UpdateLastName String
    | SelectPerson Person
    | Create
    | Update
    | Delete
    | PersonsLoaded (Result String (List Person))


type alias Model =
    { persons : List Person
    , prefix : String
    , selectedPerson : Person
    , database : Database
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { persons = []
      , prefix = ""
      , selectedPerson = Person -1 "" ""
      , database = []
      }
    , loadPersons initDatabase PersonsLoaded
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonsLoaded (Ok persons) ->
            ( { model | persons = persons }, Cmd.none )

        PersonsLoaded (Err _) ->
            ( model, Cmd.none )

        SelectPerson person ->
            ( { model | selectedPerson = person }, Cmd.none )

        UpdatePrefix p ->
            ( { model | prefix = p }, Cmd.none )

        UpdateFirstName firstName ->
            let
                oldPerson =
                    model.selectedPerson

                newPerson =
                    { oldPerson | firstName = firstName }
            in
            ( { model | selectedPerson = newPerson }, Cmd.none )

        UpdateLastName lastName ->
            let
                oldPerson =
                    model.selectedPerson

                newPerson =
                    { oldPerson | lastName = lastName }
            in
            ( { model | selectedPerson = newPerson }, Cmd.none )

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


listPersons : Model -> Element Msg
listPersons model =
    column [ Border.width 1, width fill, height (fill |> minimum 200) ] (List.map listPerson model.persons)


listPerson : Person -> Element Msg
listPerson person =
    Input.button []
        { onPress = Just (SelectPerson person)
        , label = el [] <| text <| person.lastName ++ "," ++ person.firstName
        }


personView : Model -> Element Msg
personView model =
    column [ width fill ]
        [ Input.text []
            { onChange = UpdateFirstName
            , text = model.selectedPerson.firstName
            , placeholder = Nothing
            , label = Input.labelLeft [] (el [] <| text "Name: ")
            }
        , Input.text []
            { onChange = UpdateLastName
            , text = model.selectedPerson.lastName
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
