module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Data exposing (..)
import Utils exposing (..)

type alias Model =
    { tracks : List Track
    , errorMessage : Maybe String
    }


type Msg = TracksLoaded (Result DataError (List Track))


main : Program () Model Msg
main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : () -> (Model, Cmd Msg)
init _ = (Model [] Nothing, loadTracks "https://klarhoefer.lima-city.de/bodycombatjson.php" TracksLoaded)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TracksLoaded (Err err) ->
            ({model | tracks = [], errorMessage = Just (dataErrorMessage err) }, Cmd.none)

        TracksLoaded (Ok tracks) ->
            ({model | tracks = tracks, errorMessage = Nothing}, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "BodyCombat Tracks" ]
        , div []
            (case model.errorMessage of
                Just msg -> [ text msg ]
                Nothing -> (List.map (\n -> viewTrackNumber (filterAndOrderTracks model.tracks n) n) (List.range 1 10))
            )
        ]


viewTrackNumber : List Track -> Int -> Html Msg
viewTrackNumber tracks trackNo =
    div [ class "tracknumber" ]
        [ div [] [ text <| "Track #" ++ (String.fromInt trackNo) ]
        , div [] (List.map viewTrack tracks)
        ]


viewTrack : Track -> Html Msg
viewTrack track =
    div [ class "track" ]
        [ div [] [ text track.title ]
        , div [] [ text track.artist ]
        , div [] [ text <| releaseName track.release ]
        , div [] [ text <| secondsAsMinutes track.seconds ]
        ]
