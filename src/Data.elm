module Data exposing (Track, DataError, loadTracks, dataErrorMessage)


import Http
import Json.Decode as D


type alias DataError = Http.Error

type alias Track =
    { title: String
    , artist: String
    , release: Float
    , track: Int
    , seconds: Maybe Int
    }


dataErrorMessage : DataError -> String
dataErrorMessage dataError =
    case dataError of
        Http.BadBody badBody -> "Error parsing document: " ++ badBody
        Http.BadUrl badUrl -> "Invalid URL: " ++ badUrl
        Http.BadStatus status -> "Response status: " ++ (String.fromInt status)
        Http.Timeout -> "Request timed out"
        Http.NetworkError -> "Network error"


loadTracks : String -> (Result DataError (List Track) -> msg) -> Cmd msg
loadTracks url msg =
    Http.get
        { url = url
        , expect = Http.expectJson msg trackListDecoder
        }


trackListDecoder : D.Decoder (List Track)
trackListDecoder = D.list trackDecoder


trackDecoder : D.Decoder Track
trackDecoder =
    D.map5 Track
        (D.field "title" D.string)
        (D.field "artist" D.string)
        (D.field "release" D.float)
        (D.field "track" D.int)
        (D.field "seconds" (D.nullable D.int))
