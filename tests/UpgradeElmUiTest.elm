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

a : Element.Element
a = 
    Element.row
        [ Element.spacing 24, Element.width Element.fill ]
        []
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim

a : Ui.Element
a = 
    Ui.row
        [ Ui.spacing 24 ]
        []
"""
        , ruleTest "Add width shrink"
            """module A exposing (..)

import Element

a : Element.Element
a =
    Element.row
        [ Element.spacing 24 ]
        []
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim

a : Ui.Element
a =
    Ui.row
        [ Ui.width Ui.shrink, Ui.spacing 24 ]
        []
"""
        , ruleTest "Handle unqualified types"
            """module A exposing (..)

import Description
import Element exposing (Element)
import FrontendUser exposing (FrontendUser)
import Name
import ProfileImage
import MyUi
import UserConfig exposing (UserConfig)

view : UserConfig -> FrontendUser -> Element msg
view userConfig user =
    Element.column
        (MyUi.pageContentAttributes ++ [ Element.spacing 32 ])
        [ Element.row
            [ Element.spacing 16 ]
            [ ProfileImage.image userConfig ProfileImage.defaultSize user.profileImage, MyUi.title (Name.toString user.name) ]
        , Description.toParagraph userConfig False user.description
        ]
"""
            """module A exposing (..)

import Description
import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim
import FrontendUser exposing (FrontendUser)
import Name
import ProfileImage
import MyUi
import UserConfig exposing (UserConfig)

view : UserConfig -> FrontendUser -> Ui.Element msg
view userConfig user =
    Ui.column
    -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes

        (MyUi.pageContentAttributes ++ [ Ui.spacing 32 ])
        [ Ui.row
            [ Ui.width Ui.shrink, Ui.spacing 16 ]
            [ ProfileImage.image userConfig ProfileImage.defaultSize user.profileImage, MyUi.title (Name.toString user.name) ]
        , Description.toParagraph userConfig False user.description
        ]
"""
        , ruleTest "Handle removing item from list"
            """module A exposing (..)

import Element exposing (Element)

a : Element msg
a =
    Element.column
        [ Element.width Element.fill, Element.spacing 16 ]
        Element.none

b : Element msg
b =
    Element.column
        [ Element.width Element.fill ]
        Element.none

c : Element msg
c =
    Element.column
        [ Element.width Element.fill
        ]
        Element.none

d : Element msg
d =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 16
        ]
        Element.none
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim

a : Ui.Element msg
a =
    Ui.column
        [Ui.spacing 16 ]
        Ui.none

b : Ui.Element msg
b =
    Ui.column
        [ ]
        Ui.none

c : Ui.Element msg
c =
    Ui.column
        [
        ]
        Ui.none

d : Ui.Element msg
d =
    Ui.column
        [Ui.spacing 16
        ]
        Ui.none
"""
        , ruleTest "Don't add width shrink if there's a width attribute"
            """module A exposing (..)

import Element exposing (Element)

a = 
    Element.image
        [ Element.width (Element.px 123)
        ]
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim

a = 
    Ui.image
        [ Ui.width (Ui.px 123)
        ]
"""
        , ruleTest "Function renaming"
            """module A exposing (..)

import Element exposing (Element)

a =
    Element.image
        [ Element.moveLeft 8
        , Element.moveUp 5
        ]
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim

a =
    Ui.image
        [ Ui.width Ui.shrink, Ui.left 8
        , Ui.up 5
        ]
"""
        , ruleTest "Fix range error for nested module names"
            """module A exposing (..)

import Element exposing (Element)

a = Element.Input.labelHidden ""
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim

a = Ui.Input.labelHidden ""
"""
        , ruleTest "Remove Attr"
            """module A exposing (..)

import Element exposing (Attr, Element)
import Element.Font

a : Attr decorative msg
a =
    Element.Font.size 28
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim
import Ui.Font

a : Ui.Attribute msg
a =
    Ui.Font.size 28
"""
        , ruleTest "Handle unqualified functions"
            """module A exposing (..)

import Element exposing (paragraph)
import Element.Input exposing (button)

a =
    paragraph [] []

b =
    button [] { onPress = Just NoOp, label = Element.text "abc" }
"""
            """module A exposing (..)

import Ui
import Ui.Prose
import Ui.Layout
import Ui.Anim
import Ui.Input
import Ui.Events


a =
    Ui.Prose.paragraph [ Ui.width Ui.shrink] []

b =
    Ui.el [ Ui.Events.onClick (NoOp)] (Element.text "abc")
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
