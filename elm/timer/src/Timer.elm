module Timer exposing (main)

import Browser
import Element exposing (behindContent, centerX, centerY, column, el, fill, height, layout, maximum, none, paddingXY, px, rgb, row, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (defaultThumb)
import Html
import Time


type Msg
    = Update Float
    | Reset
    | Beat Time.Posix


type alias Model =
    { duration : Float
    , elapsed : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { duration = 5, elapsed = 0 }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update v ->
            ( { model | duration = v }, Cmd.none )

        Reset ->
            ( { model | elapsed = 0 }, Cmd.none )

        Beat _ ->
            if model.elapsed < model.duration then
                ( { model | elapsed = model.elapsed + 0.1 }, Cmd.none )

            else
                ( { model | elapsed = model.duration }, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    layout [] <|
        column
            [ spacingXY 5 5
            , paddingXY 20 20
            , Border.width 1
            , width (fill |> maximum 300)
            ]
            [ gauge model
            , elapsed model
            , durationSlider model
            , resetButton
            ]


gauge : Model -> Element.Element msg
gauge model =
    let
        elapsedPercent =
            Basics.round <| 100 * (model.elapsed / model.duration)
    in
    row [ width fill, spacingXY 5 0 ]
        [ text "Elapsed Time:"
        , row [ width (px 100), Border.width 1 ]
            [ el
                [ Border.width 1
                , Background.color (rgb 0.5 0.5 1.0)
                , width (px elapsedPercent)
                ]
              <|
                text " "
            , el
                [ Border.width 1
                , width (px (100 - elapsedPercent))
                ]
              <|
                text " "
            ]
        ]


elapsed : Model -> Element.Element msg
elapsed model =
    let
        up =
            Basics.round (model.elapsed * 10)

        down =
            Basics.toFloat up / 10
    in
    row [] [ text <| String.fromFloat down ++ "s" ]


durationSlider : { a | duration : Float } -> Element.Element Msg
durationSlider model =
    row [ width fill ]
        [ Input.slider
            [ width fill
            , behindContent
                (el
                    [ width Element.fill
                    , height (Element.px 2)
                    , centerY
                    , Background.color (rgb 0.1 0.1 0.1)
                    , Border.rounded 2
                    ]
                    none
                )
            ]
            { onChange = Update
            , label = Input.labelLeft [] (el [] <| text "Duration: ")
            , min = 0
            , max = 60
            , value = model.duration
            , thumb = defaultThumb
            , step = Just 0.1
            }
        ]


resetButton =
    row [ width fill, paddingXY 10 10 ]
        [ Input.button
            [ width fill
            , Border.width 1
            , Border.rounded 5
            , paddingXY 5 5
            ]
            { onPress = Just Reset
            , label = el [ centerX ] <| text "Reset"
            }
        ]



-- Subscriptions. A timer.


subscriptions : model -> Sub Msg
subscriptions _ =
    Time.every 100 Beat


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
