module DateValiidatorTest exposing (all)

import Expect
import Main exposing (FlightDate, ValidatedDate(..))
import Test exposing (..)


all : Test
all =
    describe "Test date validation"
        [ describe "Invalid input"
            [ test "Empty string is invalid" <|
                \_ -> Main.validate "" |> Expect.equal (Invalid formatMessage)
            , test "Extra number is invalid" <|
                \_ -> Main.validate "1.3.2019.3" |> Expect.equal (Invalid formatMessage)
            , test "Missing number is invalid" <|
                \_ -> Main.validate "1.3" |> Expect.equal (Invalid formatMessage)
            , test "Impossible date is invalid" <|
                \_ -> Main.validate "219-02-01" |> Expect.equal (Invalid "Invalid date")
            ]
        , describe "Valid input"
            [ test "First of February some year" <|
                \_ ->
                    Main.validate "2019-02-01"
                        |> Expect.equal (Valid (FlightDate 1 2 2019))
            ]
        , describe "Compare dates"
            [ test "After is a later date" <|
                \_ ->
                    Main.after (FlightDate 1 2 2019) (FlightDate 1 1 2019)
                        |> Expect.equal True
            , test "Not after with same date" <|
                \_ ->
                    Main.after (FlightDate 1 2 2019) (FlightDate 1 2 2019)
                        |> Expect.equal False
            , test "Not after with earlier date" <|
                \_ ->
                    Main.after (FlightDate 1 1 2019) (FlightDate 1 2 2019)
                        |> Expect.equal False
            ]
        ]


formatMessage =
    "Format: yyyy-mm-dd"
