module UpgradeElmUi exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Review.Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Upgrades your elm-ui dependency from elm-ui to elm-ui 2.
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


patternVisitor : ModuleNameLookupTable -> Node Pattern -> List Fix
patternVisitor lookupTable (Node range pattern) =
    case pattern of
        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []

        TuplePattern nodes ->
            List.concatMap (patternVisitor lookupTable) nodes

        RecordPattern _ ->
            []

        UnConsPattern node nodes ->
            patternVisitor lookupTable node ++ patternVisitor lookupTable nodes

        ListPattern nodes ->
            List.concatMap (patternVisitor lookupTable) nodes

        VarPattern _ ->
            []

        NamedPattern { moduleName } nodes ->
            renameModules
                lookupTable
                (Node
                    { start = range.start
                    , end =
                        { column = range.start.column + String.length (String.join "." moduleName)
                        , row = range.start.row
                        }
                    }
                    moduleName
                )
                ++ List.concatMap (patternVisitor lookupTable) nodes

        AsPattern node _ ->
            patternVisitor lookupTable node

        ParenthesizedPattern node ->
            patternVisitor lookupTable node


functionVisitor : ModuleNameLookupTable -> Function -> List Fix
functionVisitor lookupTable function =
    (case function.signature of
        Just (Node _ signature) ->
            typeAnnotationVisitor lookupTable signature.typeAnnotation

        _ ->
            []
    )
        ++ (function.declaration
                |> Node.value
                |> .arguments
                |> List.concatMap (patternVisitor lookupTable)
           )
        ++ (function.declaration
                |> Node.value
                |> .expression
                |> expressionVisitor lookupTable
           )


topLevelDeclarationVisitor : ModuleNameLookupTable -> Node Declaration -> List Fix
topLevelDeclarationVisitor lookupTable declaration =
    case Node.value declaration of
        FunctionDeclaration function ->
            functionVisitor lookupTable function

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


renameModules3 : ModuleNameLookupTable -> Range -> List String -> List Fix
renameModules3 lookupTable range moduleName =
    renameModules
        lookupTable
        (Node
            { start = range.start
            , end =
                { column = range.start.column + String.length (String.join "." moduleName)
                , row = range.start.row
                }
            }
            moduleName
        )


expressionVisitor : ModuleNameLookupTable -> Node Expression -> List Fix
expressionVisitor lookupTable (Node range expr) =
    case expr of
        FunctionOrValue moduleName _ ->
            renameModules3 lookupTable range moduleName

        Application ((Node range2 (FunctionOrValue moduleName function)) :: (Node param1Range (ListExpr list)) :: rest) ->
            renameModules3 lookupTable range2 moduleName
                ++ List.concatMap (expressionVisitor lookupTable) rest
                ++ (if
                        (Review.ModuleNameLookupTable.moduleNameAt lookupTable range2 == Just [ "Element" ])
                            && Set.member function hasAttributeListParam
                    then
                        let
                            insideListStart : Location
                            insideListStart =
                                { column = param1Range.start.column + 1, row = param1Range.start.row }
                        in
                        if List.any (isWidthFill lookupTable) list then
                            List.foldl
                                (\item { endOfPrevious, fixes } ->
                                    { fixes =
                                        if isWidthFill lookupTable item then
                                            Review.Fix.removeRange
                                                { start = endOfPrevious, end = Node.range item |> .end }
                                                :: fixes

                                        else
                                            expressionVisitor lookupTable item ++ fixes
                                    , endOfPrevious = Node.range item |> .end
                                    }
                                )
                                { endOfPrevious = insideListStart, fixes = [] }
                                list
                                |> .fixes

                        else
                            (" Ui.width Ui.shrink"
                                ++ (if List.isEmpty list then
                                        ""

                                    else
                                        ","
                                   )
                                |> Review.Fix.insertAt insideListStart
                            )
                                :: List.concatMap (expressionVisitor lookupTable) list

                    else
                        []
                   )

        Application list ->
            List.concatMap (expressionVisitor lookupTable) list

        UnitExpr ->
            []

        OperatorApplication _ _ left right ->
            expressionVisitor lookupTable left ++ expressionVisitor lookupTable right

        IfBlock condition ifTrue ifFalse ->
            expressionVisitor lookupTable condition
                ++ expressionVisitor lookupTable ifTrue
                ++ expressionVisitor lookupTable ifFalse

        PrefixOperator _ ->
            []

        Operator _ ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Negation node ->
            expressionVisitor lookupTable node

        Literal _ ->
            []

        CharLiteral _ ->
            []

        TupledExpression nodes ->
            List.concatMap (expressionVisitor lookupTable) nodes

        ParenthesizedExpression node ->
            expressionVisitor lookupTable node

        LetExpression letBlock ->
            expressionVisitor lookupTable letBlock.expression
                ++ List.concatMap
                    (\(Node _ letDeclaration) ->
                        case letDeclaration of
                            LetFunction function ->
                                functionVisitor lookupTable function

                            LetDestructuring pattern expression ->
                                patternVisitor lookupTable pattern ++ expressionVisitor lookupTable expression
                    )
                    letBlock.declarations

        CaseExpression caseBlock ->
            expressionVisitor lookupTable caseBlock.expression
                ++ List.concatMap
                    (\( pattern, expression ) ->
                        patternVisitor lookupTable pattern
                            ++ expressionVisitor lookupTable expression
                    )
                    caseBlock.cases

        LambdaExpression lambda ->
            List.concatMap (patternVisitor lookupTable) lambda.args
                ++ expressionVisitor lookupTable lambda.expression

        RecordExpr nodes ->
            List.concatMap (\(Node _ ( _, field )) -> expressionVisitor lookupTable field) nodes

        ListExpr nodes ->
            List.concatMap (expressionVisitor lookupTable) nodes

        RecordAccess node _ ->
            expressionVisitor lookupTable node

        RecordAccessFunction _ ->
            []

        RecordUpdateExpression _ nodes ->
            List.concatMap (\(Node _ ( _, field )) -> expressionVisitor lookupTable field) nodes

        GLSLExpression _ ->
            []


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
