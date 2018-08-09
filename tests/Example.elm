module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, float, int, list, string)
import Iso8601
import Test exposing (..)
import Time


knownValues : Test
knownValues =
    describe "Epoch"
        [ test "fromTime 0 is January 1, 1970 at midnight" <|
            \_ ->
                Iso8601.fromTime (Time.millisToPosix 0)
                    |> Expect.equal "1970-01-01T00:00:00.000Z"
        , test "toTime \"1970-01-01T00:00:00.000Z\" gives me 0" <|
            \_ ->
                Iso8601.toTime "1970-01-01T00:00:00.000Z"
                    |> Expect.equal (Ok (Time.millisToPosix 0))
        , test "toTime \"1970-01-01T00:00:00Z\" gives me 0" <|
            \_ ->
                Iso8601.toTime "1970-01-01T00:00:00Z"
                    |> Expect.equal (Ok (Time.millisToPosix 0))
        ]


reflexive : Test
reflexive =
    fuzz int "(fromTime >> toTime) is a no-op" <|
        \num ->
            let
                time =
                    Time.millisToPosix num
            in
            Iso8601.fromTime time
                |> Iso8601.toTime
                |> Expect.equal (Ok time)
