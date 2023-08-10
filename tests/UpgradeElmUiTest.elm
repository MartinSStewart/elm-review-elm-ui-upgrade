module UpgradeElmUiTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UpgradeElmUi exposing (rule)


all : Test
all =
    describe "UpgradeElmUi"
        [ test "Remove width fill" <|
            \() ->
                """module A exposing (..)

import Element

a = 
    Element.row
        [ Element.spacing 24, Element.width Element.fill ]
        []
"""
                    |> String.filter (\char -> char /= '\u{000D}')
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module needs upgrading"
                            , details = [ "In order to upgrade to elm-ui 2, changes need to be made to this module." ]
                            , under = "module A exposing (..)"
                            }
                            |> Review.Test.whenFixed
                                (String.filter (\char -> char /= '\u{000D}')
                                    """module A exposing (..)

import Ui

a = 
    Element.row
        [ Element.spacing 24, Element.width Element.fill ]
        []
"""
                                )
                        ]

        --        , test "should report an error when REPLACEME" <|
        --            \() ->
        --                """module A exposing (..)
        --a = 1
        --"""
        --                    |> String.filter (\char -> char /= '\u{000D}')
        --                    |> Review.Test.run rule
        --                    |> Review.Test.expectErrors
        --                        [ Review.Test.error
        --                            { message = "REPLACEME"
        --                            , details = [ "REPLACEME" ]
        --                            , under = "REPLACEME"
        --                            }
        --                        ]
        ]
