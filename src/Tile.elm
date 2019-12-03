module Tile exposing (Tile, addRandomTile, baseTiles, move)

import Keyboard exposing (Key(..))
import Random
import Util


type alias Tile =
    { value : Int
    , position : Int
    }


row : Tile -> Int
row tile =
    tile.position // 4


col : Tile -> Int
col tile =
    modBy 4 tile.position


fromRowCol : { row : Int, col : Int } -> Int
fromRowCol gridPos =
    4 * gridPos.row + gridPos.col


setRow : Int -> Tile -> Tile
setRow newRow tile =
    { tile | position = fromRowCol { col = col tile, row = newRow } }


setCol : Int -> Tile -> Tile
setCol newCol tile =
    { tile | position = fromRowCol { row = row tile, col = newCol } }


newTile : Int -> Tile
newTile =
    Tile 2


isCellFilled : List Tile -> Int -> Bool
isCellFilled tiles pos =
    case Util.find (\tile -> tile.position == pos) tiles of
        Just _ ->
            True

        Nothing ->
            False


addTiles : Tile -> Tile -> Tile
addTiles a b =
    Tile (a.value + b.value) a.position


baseTiles : List Int
baseTiles =
    List.range 0 15


max : Int
max =
    3


addRandomTile : (Tile -> msg) -> List Tile -> Cmd msg
addRandomTile toMsg existingTiles =
    let
        validPositions =
            List.filter (not << isCellFilled existingTiles) baseTiles
    in
    case validPositions of
        [] ->
            Cmd.none

        [ x ] ->
            Random.generate toMsg <|
                Random.map newTile <|
                    Random.constant x

        x :: xs ->
            Random.generate toMsg <|
                Random.map newTile <|
                    Random.uniform x xs


move : Key -> List Tile -> List Tile
move key tiles =
    case key of
        ArrowUp ->
            moveByBy col moveCol tiles

        ArrowDown ->
            moveByBy col moveColReverse tiles

        ArrowLeft ->
            moveByBy row moveRow tiles

        ArrowRight ->
            moveByBy row moveRowReverse tiles

        _ ->
            tiles


moveByBy : (Tile -> comparable) -> (List Tile -> List Tile) -> List Tile -> List Tile
moveByBy by mover =
    List.concat
        << List.map mover
        << List.map combine
        << Util.partitionBy by


moveCol : List Tile -> List Tile
moveCol tiles =
    List.indexedMap setRow (List.sortBy row tiles)


moveColReverse : List Tile -> List Tile
moveColReverse tiles =
    List.indexedMap (\idx tile -> setRow (max - idx) tile) (Util.sortByDesc row tiles)


moveRow : List Tile -> List Tile
moveRow tiles =
    List.indexedMap setCol (List.sortBy col tiles)


moveRowReverse : List Tile -> List Tile
moveRowReverse tiles =
    List.indexedMap (\idx tile -> setCol (max - idx) tile) (Util.sortByDesc col tiles)


combine : List Tile -> List Tile
combine tiles =
    let
        ( subjects, rest ) =
            ( List.take 2 tiles, List.drop 2 tiles )
    in
    case subjects of
        [ a, b ] ->
            if a.value == b.value then
                addTiles a b :: combine rest

            else
                a :: combine (b :: rest)

        default ->
            default
