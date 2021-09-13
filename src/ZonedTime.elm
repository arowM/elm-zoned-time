module ZonedTime exposing
    ( ZonedTime
    , fromPosix
    , fromGregorianUtc
    , now
    , toPosix
    , toZone
    , overwriteZone
    , mapPosix
    , setToMidnight
    , addDays
    , addHours
    , addMinutes
    , addSeconds
    , addMillis
    , getYear
    , getMonth
    , getDay
    , getWeekday
    , getHour
    , getMinute
    , getSecond
    , getMillis
    , resetHour
    , resetMinute
    , resetSecond
    , resetMillis
    , isLeapYear
    , daysInYear
    , daysInMonth
    )

{-| A module to handle zoned time.


# Core

@docs ZonedTime
@docs fromPosix
@docs fromGregorianUtc
@docs now
@docs toPosix
@docs toZone


# Operators

@docs overwriteZone
@docs mapPosix
@docs setToMidnight
@docs addDays
@docs addHours
@docs addMinutes
@docs addSeconds
@docs addMillis


# Getters

@docs getYear
@docs getMonth
@docs getDay
@docs getWeekday
@docs getHour
@docs getMinute
@docs getSecond
@docs getMillis


# Reset functions

@docs resetHour
@docs resetMinute
@docs resetSecond
@docs resetMillis


# Utilities

@docs isLeapYear
@docs daysInYear
@docs daysInMonth

-}

import Task exposing (Task)
import Time exposing (Posix)



-- # Core


{-| -}
type ZonedTime
    = ZonedTime ZonedTime_


type alias ZonedTime_ =
    { zone : Time.Zone
    , time : Posix
    }


{-| -}
fromPosix : Time.Zone -> Posix -> ZonedTime
fromPosix zone time =
    ZonedTime
        { zone = zone
        , time = time
        }


{-| -}
now : Task x ZonedTime
now =
    Task.map2 ZonedTime_ Time.here Time.now
        |> Task.map ZonedTime


{-| -}
toPosix : ZonedTime -> Posix
toPosix (ZonedTime { time }) =
    time


{-| -}
toZone : ZonedTime -> Time.Zone
toZone (ZonedTime { zone }) =
    zone


{-| Construct `ZonedTime` from proleptic Gregorian calendar day in UTC.
Invalid values will return `Nothing`.

    import Time

    sample : Maybe ZonedTime
    sample =
        fromGregorianUtc
            { year = 2000
            , month = Time.Jun
            , day = 10
            }
            |> Maybe.map (addHours 22)
            |> Maybe.map (addMinutes 33)
            |> Maybe.map (addSeconds 44)
            |> Maybe.map (addMillis 55)

    Maybe.map toZone sample
    --> Just Time.utc

    Maybe.map getYear sample
    --> Just 2000

    Maybe.map getMonth sample
    --> Just Time.Jun

    Maybe.map getDay sample
    --> Just 10

    Maybe.map getHour sample
    --> Just 22

    Maybe.map getMinute sample
    --> Just 33

    Maybe.map getSecond sample
    --> Just 44

    Maybe.map getMillis sample
    --> Just 55

    fromGregorianUtc
        { year = 1999
        , month = Time.Feb
        , day = 29
        }
    --> Nothing

-}
fromGregorianUtc : { year : Int, month : Time.Month, day : Int } -> Maybe ZonedTime
fromGregorianUtc r =
    daysFromFirstDayInYear (isLeapYear r.year) r.month r.day
        |> Maybe.andThen (validateInRange 1 (daysInYear_ (isLeapYear r.year)))
        |> Maybe.map
            (\daysFromFirstDayInTheYear ->
                let
                    pastYear =
                        r.year - 1

                    daysFromOrigin =
                        daysFromFirstDayInTheYear + 365 * pastYear + pastYear // 4 - pastYear // 100 + pastYear // 400 - 1

                    posixForOrigin =
                        -62135596800000
                in
                daysFromOrigin
                    * 24
                    * 3600
                    * 1000
                    + posixForOrigin
                    |> Time.millisToPosix
                    |> fromPosix Time.utc
            )


daysFromFirstDayInYear : Bool -> Time.Month -> Int -> Maybe Int
daysFromFirstDayInYear isLeap month_ day_ =
    validateInRange 1 (daysInMonth_ isLeap month_) day_
        |> Maybe.map
            (\day ->
                let
                    month =
                        monthOrder month_

                    offset =
                        if month <= 2 then
                            0

                        else if isLeap then
                            -1

                        else
                            -2
                in
                (367 * month - 362) // 12 + offset + day
            )


{-| Return the number of days in the year specified as an argument.
-}
daysInYear : Int -> Int
daysInYear year =
    daysInYear_ (isLeapYear year)


daysInYear_ : Bool -> Int
daysInYear_ isLeap =
    if isLeap then
        366

    else
        365


{-| Return the number of days in the month specified by the given year and month arguments.
-}
daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    daysInMonth_ (isLeapYear year) month


daysInMonth_ : Bool -> Time.Month -> Int
daysInMonth_ isLeap month =
    case ( month, isLeap ) of
        ( Time.Jan, _ ) ->
            31

        ( Time.Feb, True ) ->
            29

        ( Time.Feb, False ) ->
            28

        ( Time.Mar, _ ) ->
            31

        ( Time.Apr, _ ) ->
            30

        ( Time.May, _ ) ->
            31

        ( Time.Jun, _ ) ->
            30

        ( Time.Jul, _ ) ->
            31

        ( Time.Aug, _ ) ->
            31

        ( Time.Sep, _ ) ->
            30

        ( Time.Oct, _ ) ->
            31

        ( Time.Nov, _ ) ->
            30

        ( Time.Dec, _ ) ->
            31


monthOrder : Time.Month -> Int
monthOrder month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Check if the year of given number is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear year =
    (remainderBy 4 year == 0) && ((remainderBy 400 year == 0) || not (remainderBy 100 year == 0))


validateInRange : Int -> Int -> Int -> Maybe Int
validateInRange a b n =
    if a <= n && n <= b then
        Just n

    else
        Nothing



-- # Operators


{-| -}
addDays : Int -> ZonedTime -> ZonedTime
addDays n =
    mapPosix <|
        \posix ->
            posix
                |> Time.posixToMillis
                |> (\millis ->
                        millis
                            + n
                            * 24
                            * 60
                            * 60
                            * 1000
                            |> Time.millisToPosix
                   )


{-| -}
addHours : Int -> ZonedTime -> ZonedTime
addHours n =
    mapPosix <|
        \posix ->
            posix
                |> Time.posixToMillis
                |> (\millis ->
                        millis
                            + n
                            * 3600
                            * 1000
                            |> Time.millisToPosix
                   )


{-| -}
addMinutes : Int -> ZonedTime -> ZonedTime
addMinutes n =
    mapPosix <|
        \posix ->
            posix
                |> Time.posixToMillis
                |> (\millis ->
                        millis
                            + n
                            * 60
                            * 1000
                            |> Time.millisToPosix
                   )


{-| -}
addSeconds : Int -> ZonedTime -> ZonedTime
addSeconds n =
    mapPosix <|
        \posix ->
            posix
                |> Time.posixToMillis
                |> (\millis ->
                        millis
                            + n
                            * 1000
                            |> Time.millisToPosix
                   )


{-| -}
addMillis : Int -> ZonedTime -> ZonedTime
addMillis n =
    mapPosix <|
        \posix ->
            posix
                |> Time.posixToMillis
                |> (\millis ->
                        millis
                            + n
                            |> Time.millisToPosix
                   )


{-| Overwrite time zone.
Make sure it will not change internal POSIX time value.

    import Time

    -- 1970-01-04T13:54:12.123Z
    original : ZonedTime
    original = fromPosix Time.utc (Time.millisToPosix 309252123)

    fooZone : Time.Zone
    fooZone = Time.customZone (9 * 60) []

    sample1 : ZonedTime
    sample1 =
        original
            |> overwriteZone fooZone

    sample2 : ZonedTime
    sample2 =
        original
            |> addHours -9
            |> overwriteZone fooZone

    toZone sample1
    --> fooZone

    toZone sample2
    --> fooZone

    getHour sample1
    --> 22

    getHour sample2
    --> 13

-}
overwriteZone : Time.Zone -> ZonedTime -> ZonedTime
overwriteZone zone (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | zone = zone
        }


{-| Modify internal POSIX time value.
-}
mapPosix : (Time.Posix -> Time.Posix) -> ZonedTime -> ZonedTime
mapPosix f (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | time = f zonedTime.time
        }


{-| Set time to midnight of the day in the time zone.
Shorthand for `resetMillis >> resetSecond >> resetMinute >> resetHour`.

    import Time

    -- 1970-01-04T13:54:12.123Z
    original : ZonedTime
    original = fromPosix Time.utc (Time.millisToPosix 309252123)

    midnight : ZonedTime
    midnight =
        setToMidnight original

    getYear original
    --> 1970

    getYear midnight
    --> 1970

    getMonth original
    --> Time.Jan

    getMonth midnight
    --> Time.Jan

    getDay original
    --> 4

    getDay midnight
    --> 4

    getHour midnight
    --> 0

    getMinute midnight
    --> 0

    getSecond midnight
    --> 0

    getMillis midnight
    --> 0

-}
setToMidnight : ZonedTime -> ZonedTime
setToMidnight =
    resetMillis >> resetSecond >> resetMinute >> resetHour



-- # Getters


{-| -}
getYear : ZonedTime -> Int
getYear =
    getHelper Time.toYear


{-| -}
getMonth : ZonedTime -> Time.Month
getMonth =
    getHelper Time.toMonth


{-| -}
getDay : ZonedTime -> Int
getDay =
    getHelper Time.toDay


{-| -}
getWeekday : ZonedTime -> Time.Weekday
getWeekday =
    getHelper Time.toWeekday


{-| -}
getHour : ZonedTime -> Int
getHour =
    getHelper Time.toHour


{-| -}
getMinute : ZonedTime -> Int
getMinute =
    getHelper Time.toMinute


{-| -}
getSecond : ZonedTime -> Int
getSecond =
    getHelper Time.toSecond


{-| -}
getMillis : ZonedTime -> Int
getMillis =
    getHelper Time.toMillis


getHelper : (Time.Zone -> Posix -> a) -> ZonedTime -> a
getHelper f (ZonedTime { zone, time }) =
    f zone time



-- Reset functions


{-| Reset the hour part of `ZonedTime` to zero.

Shorthand for `\time -> addHours (negate <| getHour time) time`.

-}
resetHour : ZonedTime -> ZonedTime
resetHour =
    \time -> addHours (negate <| getHour time) time


{-| Reset the minute part of `ZonedTime` to zero.

Shorthand for `\time -> addMinutes (negate <| getMinute time) time`.

-}
resetMinute : ZonedTime -> ZonedTime
resetMinute =
    \time -> addMinutes (negate <| getMinute time) time


{-| Reset the second part of `ZonedTime` to zero.

Shorthand for `\time -> addSeconds (negate <| getSecond time) time`.

-}
resetSecond : ZonedTime -> ZonedTime
resetSecond =
    \time -> addSeconds (negate <| getSecond time) time


{-| Reset the millis part of `ZonedTime` to zero.

Shorthand for `\time -> addMillis (negate <| getMillis time) time`.

-}
resetMillis : ZonedTime -> ZonedTime
resetMillis =
    \time -> addMillis (negate <| getMillis time) time
