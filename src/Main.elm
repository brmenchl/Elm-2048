module Main exposing (main)

import Browser
import Grid exposing (viewGrid)
import Keyboard exposing (Key(..))
import Tile exposing (Tile, addRandomTile, move)


type alias Model =
    List Tile


type Msg
    = ArrowMsg (Maybe Key)
    | AddNewTile Tile


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( []
    , addRandomTile AddNewTile []
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrowMsg (Just arrow) ->
            let
                newTiles : List Tile
                newTiles =
                    move arrow model
            in
            ( newTiles, addRandomTile AddNewTile newTiles )

        AddNewTile tile ->
            ( tile :: model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 2048"
    , body = [ viewGrid model ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map ArrowMsg <| Keyboard.downs Keyboard.navigationKey
