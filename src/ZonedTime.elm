module ZonedTime exposing
    ( ZonedTime
    , fromPosix
    , now
    , toPosix
    , toZone
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
    )

{-| A brief module to handle zoned time.


# Core

@docs ZonedTime
@docs fromPosix
@docs now
@docs toPosix
@docs toZone


# Operators

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



-- # Operators


{-| -}
addDays : Int -> ZonedTime -> ZonedTime
addDays n (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | time =
                zonedTime.time
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
        }


{-| -}
addHours : Int -> ZonedTime -> ZonedTime
addHours n (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | time =
                zonedTime.time
                    |> Time.posixToMillis
                    |> (\millis ->
                            millis
                                + n
                                * 60
                                * 60
                                * 1000
                                |> Time.millisToPosix
                       )
        }


{-| -}
addMinutes : Int -> ZonedTime -> ZonedTime
addMinutes n (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | time =
                zonedTime.time
                    |> Time.posixToMillis
                    |> (\millis ->
                            millis
                                + n
                                * 60
                                * 1000
                                |> Time.millisToPosix
                       )
        }


{-| -}
addSeconds : Int -> ZonedTime -> ZonedTime
addSeconds n (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | time =
                zonedTime.time
                    |> Time.posixToMillis
                    |> (\millis ->
                            millis
                                + n
                                * 1000
                                |> Time.millisToPosix
                       )
        }


{-| -}
addMillis : Int -> ZonedTime -> ZonedTime
addMillis n (ZonedTime zonedTime) =
    ZonedTime
        { zonedTime
            | time =
                zonedTime.time
                    |> Time.posixToMillis
                    |> (\millis ->
                            millis
                                + n
                                |> Time.millisToPosix
                       )
        }


{-| Set time to midnight of the day in the time zone.

    import Time

    -- 1970-01-04T13:54:12.123Z
    original : ZonedTime
    original =
        fromPosix Time.utc
            <| Time.millisToPosix <|
                0
                    + 3 * 24 * 60 * 60 * 1000
                    + 13 * 60 * 60 * 1000
                    + 54 * 60 * 1000
                    + 12 * 1000
                    + 123

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
setToMidnight ((ZonedTime zonedTime_) as zonedTime) =
    ZonedTime
        { zonedTime_
            | time =
                zonedTime_.time
                    |> Time.posixToMillis
                    |> (\millis ->
                            millis
                                - getHour zonedTime
                                * 60
                                * 60
                                * 1000
                                - getMinute zonedTime
                                * 60
                                * 1000
                                - getSecond zonedTime
                                * 1000
                                - getMillis zonedTime
                                |> Time.millisToPosix
                       )
        }



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
