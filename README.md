# elm-zoned-time

[![Build Status](https://travis-ci.org/arowM/elm-zoned-time.svg?branch=main)](https://travis-ci.org/arowM/elm-zoned-time)

A library to handle zoned time.

# A Quick Example

## Construct `ZonedTime`

```elm
import Time

-- 1970-01-04T13:54:12.123Z
original : ZonedTime
original = fromPosix Time.utc (Time.millisToPosix 309252123)

midnight : ZonedTime
midnight = setToMidnight original
```

```elm
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
```

## Unwrap `ZonedTime`

```elm
import Time

posix : Maybe Time.Posix
posix =
    sample
        |> Maybe.map toPosix

zone : Maybe Time.Zone
zone =
    sample
        |> Maybe.map toZone
```
