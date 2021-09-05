module ZonedTimeSpec exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as JD
import Test exposing (Test, describe, fuzz, fuzz2)
import Time
import TimeZone
import ZonedTime exposing (ZonedTime)


suite : Test
suite =
    describe "test properties"
        [ testFromGregorianUtc
        , testSetToMidnight
        ]


testFromGregorianUtc : Test
testFromGregorianUtc =
    fuzz posixFuzzer "fromGregorianUtc should generate proper value" <|
        \posix ->
            let
                target : Maybe ZonedTime
                target =
                    ZonedTime.fromGregorianUtc
                        { year = Time.toYear Time.utc posix
                        , month = Time.toMonth Time.utc posix
                        , day = Time.toDay Time.utc posix
                        }
                        |> Maybe.map (ZonedTime.addHours (Time.toHour Time.utc posix))
                        |> Maybe.map (ZonedTime.addMinutes (Time.toMinute Time.utc posix))
                        |> Maybe.map (ZonedTime.addSeconds (Time.toSecond Time.utc posix))
                        |> Maybe.map (ZonedTime.addMillis (Time.toMillis Time.utc posix))
            in
            Expect.equal
                (Maybe.map ZonedTime.toPosix target)
                (Just posix)


testSetToMidnight : Test
testSetToMidnight =
    fuzz2 zoneFuzzer posixFuzzer "setToMidnight should generate midnight for given time zone." <|
        \zone posix ->
            let
                target : ZonedTime
                target =
                    ZonedTime.fromPosix zone posix
                        |> ZonedTime.setToMidnight
            in
            Expect.equal
                { day = ZonedTime.getDay target
                , hour = ZonedTime.getHour target
                , minute = ZonedTime.getMinute target
                , second = ZonedTime.getSecond target
                , millis = ZonedTime.getMillis target
                }
                { day = Time.toDay zone posix
                , hour = 0
                , minute = 0
                , second = 0
                , millis = 0
                }



-- Fuzzer


posixFuzzer : Fuzzer Time.Posix
posixFuzzer =
    Fuzz.map Time.millisToPosix Fuzz.int


zoneFuzzer : Fuzzer Time.Zone
zoneFuzzer =
    Fuzz.frequency
        [ ( 10, Fuzz.constant <| TimeZone.pacific__chatham () )
        , ( 10, Fuzz.constant <| TimeZone.pacific__marquesas () )
        , ( 10, Fuzz.constant <| TimeZone.africa__casablanca () )
        , ( 3, Fuzz.constant <| TimeZone.america__los_angeles () )
        , ( 1, Fuzz.constant <| TimeZone.asia__tokyo () )
        ]
