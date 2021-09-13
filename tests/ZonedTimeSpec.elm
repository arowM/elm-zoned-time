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
        [ describe "Test encode/decode behaviour."
            [ testFromGregorianUtc
            ]
        , describe "Test reset functions."
            [ testResetHour
            , testResetMinute
            , testResetSecond
            , testResetMillis
            , testSetToMidnight
            ]
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


testResetHour : Test
testResetHour =
    fuzz2 zoneFuzzer posixFuzzer "resetHour should generate proper time." <|
        \zone posix ->
            let
                target : ZonedTime
                target =
                    ZonedTime.fromPosix zone posix
                        |> ZonedTime.resetHour
            in
            Expect.equal
                { year = ZonedTime.getYear target
                , month = ZonedTime.getMonth target
                , day = ZonedTime.getDay target
                , hour = ZonedTime.getHour target
                , minute = ZonedTime.getMinute target
                , second = ZonedTime.getSecond target
                , millis = ZonedTime.getMillis target
                }
                { year = Time.toYear zone posix
                , month = Time.toMonth zone posix
                , day = Time.toDay zone posix
                , hour = 0
                , minute = Time.toMinute zone posix
                , second = Time.toSecond zone posix
                , millis = Time.toMillis zone posix
                }


testResetMinute : Test
testResetMinute =
    fuzz2 zoneFuzzer posixFuzzer "resetMinute should generate proper time." <|
        \zone posix ->
            let
                target : ZonedTime
                target =
                    ZonedTime.fromPosix zone posix
                        |> ZonedTime.resetMinute
            in
            Expect.equal
                { year = ZonedTime.getYear target
                , month = ZonedTime.getMonth target
                , day = ZonedTime.getDay target
                , hour = ZonedTime.getHour target
                , minute = ZonedTime.getMinute target
                , second = ZonedTime.getSecond target
                , millis = ZonedTime.getMillis target
                }
                { year = Time.toYear zone posix
                , month = Time.toMonth zone posix
                , day = Time.toDay zone posix
                , hour = Time.toHour zone posix
                , minute = 0
                , second = Time.toSecond zone posix
                , millis = Time.toMillis zone posix
                }


testResetSecond : Test
testResetSecond =
    fuzz2 zoneFuzzer posixFuzzer "resetSecond should generate proper time." <|
        \zone posix ->
            let
                target : ZonedTime
                target =
                    ZonedTime.fromPosix zone posix
                        |> ZonedTime.resetSecond
            in
            Expect.equal
                { year = ZonedTime.getYear target
                , month = ZonedTime.getMonth target
                , day = ZonedTime.getDay target
                , hour = ZonedTime.getHour target
                , minute = ZonedTime.getMinute target
                , second = ZonedTime.getSecond target
                , millis = ZonedTime.getMillis target
                }
                { year = Time.toYear zone posix
                , month = Time.toMonth zone posix
                , day = Time.toDay zone posix
                , hour = Time.toHour zone posix
                , minute = Time.toMinute zone posix
                , second = 0
                , millis = Time.toMillis zone posix
                }


testResetMillis : Test
testResetMillis =
    fuzz2 zoneFuzzer posixFuzzer "resetMillis should generate proper time." <|
        \zone posix ->
            let
                target : ZonedTime
                target =
                    ZonedTime.fromPosix zone posix
                        |> ZonedTime.resetMillis
            in
            Expect.equal
                { year = ZonedTime.getYear target
                , month = ZonedTime.getMonth target
                , day = ZonedTime.getDay target
                , hour = ZonedTime.getHour target
                , minute = ZonedTime.getMinute target
                , second = ZonedTime.getSecond target
                , millis = ZonedTime.getMillis target
                }
                { year = Time.toYear zone posix
                , month = Time.toMonth zone posix
                , day = Time.toDay zone posix
                , hour = Time.toHour zone posix
                , minute = Time.toMinute zone posix
                , second = Time.toSecond zone posix
                , millis = 0
                }


testSetToMidnight : Test
testSetToMidnight =
    fuzz2 zoneFuzzer posixFuzzer "setToMidnight should generate proper time." <|
        \zone posix ->
            let
                target : ZonedTime
                target =
                    ZonedTime.fromPosix zone posix
                        |> ZonedTime.setToMidnight
            in
            Expect.equal
                { year = ZonedTime.getYear target
                , month = ZonedTime.getMonth target
                , day = ZonedTime.getDay target
                , hour = ZonedTime.getHour target
                , minute = ZonedTime.getMinute target
                , second = ZonedTime.getSecond target
                , millis = ZonedTime.getMillis target
                }
                { year = Time.toYear zone posix
                , month = Time.toMonth zone posix
                , day = Time.toDay zone posix
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


zonedTimeFuzzer : Fuzzer ZonedTime
zonedTimeFuzzer =
    Fuzz.map2 ZonedTime.fromPosix zoneFuzzer posixFuzzer
