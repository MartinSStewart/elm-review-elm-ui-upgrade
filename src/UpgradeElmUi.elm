module UpgradeElmUi exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import List.Extra as List
import Review.Fix exposing (Fix)
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
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable, ast : File }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable ast () -> { lookupTable = lookupTable, ast = ast })
        |> Rule.withModuleNameLookupTable
        |> Rule.withFullAst


importVisitor : Node Import -> List Fix
importVisitor (Node _ import2) =
    renameModules2 import2.moduleName
        ++ (case import2.moduleAlias of
                Just alias ->
                    renameModules2 alias

                Nothing ->
                    []
           )


moduleDefinitionVisitor : Node Module -> Context -> ( List (Rule.Error {}), Context )
moduleDefinitionVisitor (Node range _) context =
    let
        importFixes : List Fix
        importFixes =
            List.concatMap importVisitor context.ast.imports

        declarationFixes : List Fix
        declarationFixes =
            List.concatMap
                (\declaration ->
                    topLevelDeclarationVisitor context.lookupTable declaration
                )
                context.ast.declarations
    in
    ( case importFixes ++ declarationFixes of
        [] ->
            []

        _ ->
            [ Rule.errorWithFix
                { message = "Module needs upgrading"
                , details = [ "In order to upgrade to elm-ui 2, changes need to be made to this module." ]
                }
                range
                (importFixes ++ declarationFixes)
            ]
    , context
    )


topLevelDeclarationVisitor : ModuleNameLookupTable -> Node Declaration -> List Fix
topLevelDeclarationVisitor lookupTable declaration =
    case Node.value declaration of
        FunctionDeclaration function ->
            []

        AliasDeclaration typeAlias ->
            typeAnnotationVisitor lookupTable typeAlias.typeAnnotation

        CustomTypeDeclaration type2 ->
            List.concatMap
                (\(Node _ constructor) ->
                    List.concatMap
                        (typeAnnotationVisitor lookupTable)
                        constructor.arguments
                )
                type2.constructors

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []


typeAnnotationVisitor : ModuleNameLookupTable -> Node TypeAnnotation -> List Fix
typeAnnotationVisitor lookupTable (Node _ typeAnnotation) =
    case typeAnnotation of
        GenericType _ ->
            []

        Typed node nodes ->
            renameModules lookupTable (Node.map Tuple.first node)
                ++ List.concatMap (typeAnnotationVisitor lookupTable) nodes

        Unit ->
            []

        Tupled nodes ->
            List.concatMap (typeAnnotationVisitor lookupTable) nodes

        Record fields ->
            List.concatMap
                (\(Node _ ( _, typeAnnotation2 )) -> typeAnnotationVisitor lookupTable typeAnnotation2)
                fields

        GenericRecord _ (Node _ fields) ->
            List.concatMap
                (\(Node _ ( _, typeAnnotation2 )) -> typeAnnotationVisitor lookupTable typeAnnotation2)
                fields

        FunctionTypeAnnotation node1 node2 ->
            typeAnnotationVisitor lookupTable node1 ++ typeAnnotationVisitor lookupTable node2


renameModules : ModuleNameLookupTable -> Node ModuleName -> List Fix
renameModules lookupTable (Node range _) =
    case Review.ModuleNameLookupTable.moduleNameAt lookupTable range of
        Just realModuleName ->
            renameModules2 (Node range realModuleName)

        Nothing ->
            []


renameModules2 : Node ModuleName -> List Fix
renameModules2 (Node range moduleName) =
    case moduleName of
        [ "Element" ] ->
            [ replaceModuleName range "Ui" ]

        [ "Element", "Background" ] ->
            [ replaceModuleName range "Ui.Background" ]

        [ "Element", "Border" ] ->
            [ replaceModuleName range "Ui.Border" ]

        [ "Element", "Events" ] ->
            [ replaceModuleName range "Ui.Events" ]

        [ "Element", "Font" ] ->
            [ replaceModuleName range "Ui.Font" ]

        [ "Element", "Input" ] ->
            [ replaceModuleName range "Ui.Input" ]

        [ "Element", "Keyed" ] ->
            [ replaceModuleName range "Ui.Keyed" ]

        [ "Element", "Lazy" ] ->
            [ replaceModuleName range "Ui.Lazy" ]

        [ "Element", "Region" ] ->
            [ replaceModuleName range "Ui.Region" ]

        _ ->
            []


replaceModuleName : Range -> String -> Fix
replaceModuleName range newModuleName =
    Review.Fix.replaceRangeBy range newModuleName


expressionVisitor : Node Expression -> Context -> ( List Fix, Context )
expressionVisitor (Node range node) context =
    case node of
        FunctionOrValue moduleName _ ->
            ( renameModules
                context.lookupTable
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
                        ( [ Review.Fix.insertAt param1Range.start "Ui.width Ui.shrink"
                          ]
                        , context
                        )

                    widthFills ->
                        ( List.map
                            (\(Node widthFillRange _) ->
                                Review.Fix.removeRange widthFillRange
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
