module Util exposing (find, partitionBy, partitionEvery, sortByDesc)

import List.Extra


find : (a -> Bool) -> List a -> Maybe a
find pred =
    List.head << List.filter pred


partitionEvery : Int -> List a -> List (List a)
partitionEvery count list =
    let
        ( partition, rest ) =
            ( List.take count list, List.drop count list )
    in
    if List.isEmpty partition then
        []

    else if List.isEmpty rest then
        [ partition ]

    else
        partition :: partitionEvery count rest


partitionBy : (a -> comparable) -> List a -> List (List a)
partitionBy comparator list =
    List.map
        (\( x, xs ) -> x :: xs)
    <|
        List.Extra.groupWhile (\a b -> comparator a == comparator b) <|
            List.sortBy comparator list


sortByDesc : (a -> comparable) -> List a -> List a
sortByDesc comp list =
    List.sortBy comp list |> List.reverse
