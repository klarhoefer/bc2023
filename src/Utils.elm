module Utils exposing (filterAndOrderTracks, releaseName, secondsAsMinutes, sumUpSeconds)

import Data exposing (Track)


filterAndOrderTracks : List Track -> Int -> List Track
filterAndOrderTracks tracks trackNo =
    tracks
    |> List.filter (\t -> t.track == trackNo)
    |> List.sortBy (\t -> -t.release)


releaseName : Float -> String
releaseName release =
    if release > 84 && release < 85 then
        "United"
    else
        String.fromFloat release


secondsAsMinutes : Maybe Int -> String
secondsAsMinutes maybeSecs =
    case maybeSecs of
        Just secs ->
            let
                minutes = secs // 60
                seconds = modBy 60 secs
            in
                (String.fromInt minutes) ++ ":" ++ (if seconds < 10 then "0" else "") ++ (String.fromInt seconds)

        Nothing -> "?"


sumUpSeconds : List (Maybe Int) -> Maybe Int
sumUpSeconds maybeSecs =
    List.foldl sumSeconds (Just 0)  maybeSecs


sumSeconds : Maybe Int -> Maybe Int -> Maybe Int
sumSeconds a b =
    case a of
        Just x ->
            case b of
                Just y -> Just (x + y)
                _ -> Nothing
        _ -> Nothing
