module UpgradeElmUi exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import List.Extra as List
import Review.Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports... REPLACEME

    config =
        [ UpgradeElmUi.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template MartinSStewart/elm-review-elm-ui-upgrade/example --rules UpgradeElmUi
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UpgradeElmUi" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> { lookupTable = lookupTable })
        |> Rule.withModuleNameLookupTable


importVisitor : Node Import -> List (Rule.Error {})
importVisitor (Node _ import2) =
    renameModules import2.moduleName
        ++ (case import2.moduleAlias of
                Just alias ->
                    renameModules alias

                Nothing ->
                    []
           )


moduleDefinitionVisitor : Node Module -> List (Rule.Error {})
moduleDefinitionVisitor (Node _ module2) =
    case module2 of
        NormalModule a ->
            renameModules a.moduleName

        PortModule a ->
            renameModules a.moduleName

        EffectModule a ->
            renameModules a.moduleName


renameModules : Node ModuleName -> List (Rule.Error {})
renameModules (Node range moduleName) =
    case moduleName of
        --[ "Ui" ] ->
        --    [ Rule.errorWithFix
        --        { message = "The Ui module name is used by elm-ui 2"
        --        , details = [ "You're Ui modules will be renamed to MyUi" ]
        --        }
        --        range
        --        [ Review.Fix.replaceRangeBy range "MyUi" ]
        --    ]
        [ "Element" ] ->
            fixError range "Ui"

        [ "Element", "Background" ] ->
            fixError range "Ui.Background"

        [ "Element", "Border" ] ->
            fixError range "Ui.Border"

        [ "Element", "Events" ] ->
            fixError range "Ui.Events"

        [ "Element", "Font" ] ->
            fixError range "Ui.Font"

        [ "Element", "Input" ] ->
            fixError range "Ui.Input"

        [ "Element", "Keyed" ] ->
            fixError range "Ui.Keyed"

        [ "Element", "Lazy" ] ->
            fixError range "Ui.Lazy"

        [ "Element", "Region" ] ->
            fixError range "Ui.Region"

        _ ->
            []


fixError : Range -> String -> List (Rule.Error {})
fixError range newModuleName =
    [ Rule.errorWithFix
        { message = "This module name is called " ++ newModuleName ++ " now"
        , details = []
        }
        range
        [ Review.Fix.replaceRangeBy range newModuleName ]
    ]


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor (Node range node) context =
    case node of
        FunctionOrValue moduleName _ ->
            ( renameModules
                (Node
                    { start = range.start
                    , end =
                        { column = range.start.column + String.length (String.join "." moduleName)
                        , row = range.start.row
                        }
                    }
                    moduleName
                )
            , context
            )

        Application ((Node range2 (FunctionOrValue _ function)) :: (Node param1Range (ListExpr list)) :: _) ->
            if
                (Review.ModuleNameLookupTable.moduleNameAt context.lookupTable range2 == Just [ "Element" ])
                    && Set.member function hasAttributeListParam
            then
                case List.filter (isWidthFill context.lookupTable) list of
                    [] ->
                        ( [ Rule.errorWithFix
                                { message = "The default is no longer Element.width Element.shrink"
                                , details = []
                                }
                                param1Range
                                [ Review.Fix.insertAt param1Range.start "Ui.width Ui.shrink" ]
                          ]
                        , context
                        )

                    widthFills ->
                        ( List.map
                            (\(Node widthFillRange _) ->
                                Rule.errorWithFix
                                    { message = "The default is no longer Element.width Element.shrink"
                                    , details = []
                                    }
                                    param1Range
                                    [ Review.Fix.removeRange widthFillRange ]
                            )
                            widthFills
                        , context
                        )

            else
                ( [], context )

        _ ->
            ( [], context )


isWidthFill : ModuleNameLookupTable -> Node Expression -> Bool
isWidthFill lookupTable expr =
    case Node.value expr of
        Application [ Node range1 (FunctionOrValue _ "width"), Node range2 (FunctionOrValue _ "fill") ] ->
            (Review.ModuleNameLookupTable.moduleNameAt lookupTable range1 == Just [ "Element" ])
                && (Review.ModuleNameLookupTable.moduleNameAt lookupTable range2 == Just [ "Element" ])

        _ ->
            False


hasAttributeListParam : Set String
hasAttributeListParam =
    Set.fromList
        [ "el"
        , "row"
        , "column"
        , "table"
        , "paragraph"
        , "wrappedRow"
        , "textColumn"
        , "indexedTable"
        ]
