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

import Ui

a = 
    Ui.row
        [ Ui.spacing 24, Ui.width Ui.fill ]
        []
"""
                    |> String.filter (\char -> char /= '\u{000D}')
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = []
                            , under = "REPLACEME"
                            }
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
