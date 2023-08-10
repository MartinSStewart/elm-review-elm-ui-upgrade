module UpgradeElmUiTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UpgradeElmUi exposing (rule)


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


ruleTest name input output =
    test name <|
        \() ->
            input
                |> String.filter (\char -> char /= '\u{000D}')
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module needs upgrading"
                        , details = [ "In order to upgrade to elm-ui 2, changes need to be made to this module." ]
                        , under = "module A exposing (..)"
                        }
                        |> Review.Test.whenFixed (String.filter (\char -> char /= '\u{000D}') output)
                    ]
