module UpgradeElmUiTest exposing (all)

import Elm.Project
import Json.Encode
import Review.Project
import Review.Test
import Test exposing (Test, describe, test)
import Unsafe
import UpgradeElmUi exposing (rule)


project : Review.Project.Project
project =
    Review.Project.new |> Review.Project.addElmJson elmJsonToConstructManually


elmJson : Elm.Project.Project
elmJson =
    """{
   "type": "application",
   "source-directories": [
       "src"
   ],
   "elm-version": "0.19.1",
   "dependencies": {
       "direct": {
           "avh4/elm-color": "1.0.0",
           "elm/browser": "1.0.2",
           "elm/bytes": "1.0.8",
           "elm/core": "1.0.5",
           "elm/file": "1.0.5",
           "elm/html": "1.0.0",
           "elm/http": "2.0.0",
           "elm/json": "1.1.3",
           "elm/parser": "1.1.0",
           "elm/svg": "1.0.1",
           "elm/time": "1.0.0",
           "elm/url": "1.0.0",
           "mdgriffith/elm-ui": "1.1.7"
       },
       "indirect": {
       }
   },
   "test-dependencies": {
     "direct": {},
     "indirect": {}
   }
}"""
        |> Unsafe.project


elmJsonText =
    Elm.Project.encode elmJson |> Json.Encode.encode 4


elmJsonFixed : Elm.Project.Project
elmJsonFixed =
    """{
   "type": "application",
   "source-directories": [
       "src"
   ],
   "elm-version": "0.19.1",
   "dependencies": {
       "direct": {
           "avh4/elm-color": "1.0.0",
           "elm/browser": "1.0.2",
           "elm/bytes": "1.0.8",
           "elm/core": "1.0.5",
           "elm/file": "1.0.5",
           "elm/html": "1.0.0",
           "elm/http": "2.0.0",
           "elm/json": "1.1.3",
           "elm/parser": "1.1.0",
           "elm/svg": "1.0.1",
           "elm/time": "1.0.0",
           "elm/url": "1.0.0",
           "mdgriffith/elm-ui": "2.0.0"
       },
       "indirect": {
            "elm/virtual-dom": "1.0.0"
       }
   },
   "test-dependencies": {
     "direct": {},
     "indirect": {}
   }
}"""
        |> Unsafe.project


elmJsonFixedText =
    Json.Encode.encode 4 (Elm.Project.encode elmJsonFixed) ++ "\n"


elmJsonToConstructManually : { path : String, raw : String, project : Elm.Project.Project }
elmJsonToConstructManually =
    { path = "elm.json"
    , raw = elmJsonText
    , project = elmJson
    }


all : Test
all =
    describe "UpgradeElmUi"
        [ ruleTest "Remove width fill"
            """module A exposing (..)

import Element

a = 
    Element.row
        [ Element.spacing 24, Element.width Element.fill ]
        []
"""
            """module A exposing (..)

import Ui

a = 
    Ui.row
        [ Ui.spacing 24 ]
        []
"""
        , ruleTest "Add width shrink"
            """module A exposing (..)

import Element

a =
    Element.row
        [ Element.spacing 24 ]
        []
"""
            """module A exposing (..)

import Ui

a =
    Ui.row
        [ Ui.width Ui.shrink, Ui.spacing 24 ]
        []
"""
        ]


ruleTest : String -> String -> String -> Test
ruleTest name input output =
    test name <|
        \() ->
            input
                |> String.filter (\char -> char /= '\u{000D}')
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expect
                    [ Review.Test.elmJsonErrors
                        [ Review.Test.error
                            { message = "elm-ui 2 should replace elm-ui as a dependency"
                            , details = [ "Since were upgrading to elm-ui 2, elm-ui can be removed" ]
                            , under = elmJsonText
                            }
                            |> Review.Test.whenFixed elmJsonFixedText
                        ]
                    , Review.Test.moduleErrors
                        "A"
                        [ Review.Test.error
                            { message = "Module needs upgrading"
                            , details = [ "In order to upgrade to elm-ui 2, changes need to be made to this module." ]
                            , under = "module A exposing (..)"
                            }
                            |> Review.Test.whenFixed (String.filter (\char -> char /= '\u{000D}') output)
                        ]
                    ]
