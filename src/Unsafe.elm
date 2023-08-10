module Unsafe exposing (constraint, packageName, project, version)

import Elm.Constraint
import Elm.Package
import Elm.Project
import Elm.Version
import Json.Decode


constraint : String -> Elm.Constraint.Constraint
constraint string =
    case Elm.Constraint.fromString string of
        Just a ->
            a

        Nothing ->
            unreachable ()


version : ( Int, Int, Int ) -> Elm.Version.Version
version triple =
    case Elm.Version.fromTuple triple of
        Just a ->
            a

        Nothing ->
            unreachable ()


packageName : String -> Elm.Package.Name
packageName string =
    case Elm.Package.fromString string of
        Just a ->
            a

        Nothing ->
            unreachable ()


project : String -> Elm.Project.Project
project string =
    case Json.Decode.decodeString Elm.Project.decoder string of
        Ok a ->
            a

        Err _ ->
            unreachable ()


{-| Be very careful when using this!
-}
unreachable : () -> a
unreachable () =
    let
        _ =
            causeStackOverflow 0
    in
    unreachable ()


causeStackOverflow : Int -> Int
causeStackOverflow value =
    causeStackOverflow value + 1
