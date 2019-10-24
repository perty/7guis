module DateValiidatorTest exposing (all)

import Expect
import Main exposing (FlightDate, ValidatedDate(..))
import Test exposing (..)


all : Test
all =
    describe "Test date validation"
        [ describe "Invalid input"
            [ test "Empty string is invalid" <|
                \_ -> Main.validate "" |> Expect.equal (Invalid "")
            , test "Extra number is invalid" <|
                \_ -> Main.validate "1.3.2019.3" |> Expect.equal (Invalid "1.3.2019.3")
            , test "Missing number is invalid" <|
                \_ -> Main.validate "1.3" |> Expect.equal (Invalid "1.3")
            ]
        , describe "Valid input"
            [ test "First of February some year" <|
                \_ ->
                    Main.validate "1.2.2019"
                        |> Expect.equal (Valid (FlightDate 1 2 2019))
            ]
        ]
