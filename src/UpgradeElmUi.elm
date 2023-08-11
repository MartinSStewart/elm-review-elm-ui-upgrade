module UpgradeElmUi exposing (rule)

{-|

@docs rule

-}

import Elm.Constraint
import Elm.Package
import Elm.Project exposing (ApplicationInfo, PackageInfo)
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
import Elm.Version
import List.Extra as List
import Review.Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (ElmJsonKey, Rule)
import Set exposing (Set)
import Unsafe


{-| Upgrades your elm-ui dependency from elm-ui to elm-ui 2.
-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "UpgradeElmUi" { continueWithModules = False }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor
            (\schema ->
                Rule.withModuleDefinitionVisitor moduleDefinitionVisitor schema
                    |> Rule.providesFixesForModuleRule
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = initialContext
            , fromModuleToProject = initialContext2
            , foldProjectContexts = \_ next -> next
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { continueWithModules : Bool }


type alias Context =
    { lookupTable : ModuleNameLookupTable, ast : File, continueWithModules : Bool }


initialContext : Rule.ContextCreator ProjectContext Context
initialContext =
    Rule.initContextCreator
        (\lookupTable ast project ->
            { lookupTable = lookupTable
            , ast = ast
            , continueWithModules = project.continueWithModules
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withFullAst


initialContext2 : Rule.ContextCreator Context ProjectContext
initialContext2 =
    Rule.initContextCreator (\moduleContext -> { continueWithModules = moduleContext.continueWithModules })


constraintV2 : Elm.Constraint.Constraint
constraintV2 =
    Unsafe.constraint "2.0.0 <= v < 3.0.0"


version2 : Elm.Version.Version
version2 =
    Unsafe.version ( 2, 0, 0 )


dependenciesV2 : List ( Elm.Package.Name, Elm.Constraint.Constraint )
dependenciesV2 =
    [ ( Unsafe.packageName "avh4/elm-color", Unsafe.constraint "1.0.0 <= v < 2.0.0" )
    , ( Unsafe.packageName "elm/browser", Unsafe.constraint "1.0.2 <= v < 2.0.0" )
    , ( Unsafe.packageName "elm/core", Unsafe.constraint "1.0.0 <= v < 2.0.0" )
    , ( Unsafe.packageName "elm/html", Unsafe.constraint "1.0.0 <= v < 2.0.0" )
    , ( Unsafe.packageName "elm/json", Unsafe.constraint "1.0.0 <= v < 2.0.0" )
    , ( Unsafe.packageName "elm/time", Unsafe.constraint "1.0.0 <= v < 2.0.0" )
    , ( Unsafe.packageName "elm/virtual-dom", Unsafe.constraint "1.0.0 <= v < 2.0.0" )
    ]


elmUiName =
    Unsafe.packageName "mdgriffith/elm-ui"


getLowConstraint : Elm.Constraint.Constraint -> Elm.Version.Version
getLowConstraint constraint =
    case Elm.Constraint.toString constraint |> String.split " " |> List.head of
        Just lowVersionText ->
            case String.split "." lowVersionText |> List.filterMap String.toInt of
                [ major, minor, patch ] ->
                    Elm.Version.fromTuple ( major, minor, patch )
                        |> Maybe.withDefault Elm.Version.one

                _ ->
                    Elm.Version.one

        Nothing ->
            Elm.Version.one


type DependencyStatus
    = DependencyFoundAndIsValid
    | DependencyFoundButIsInvalid Elm.Version.Version
    | DependencyNotFound


elmJsonVisitor :
    Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project }
    -> ProjectContext
    -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
elmJsonVisitor maybeProject context =
    case maybeProject of
        Just { elmJsonKey, project } ->
            case project of
                Elm.Project.Package package ->
                    handlePackage elmJsonKey package context

                Elm.Project.Application application ->
                    handleApplication elmJsonKey application context

        Nothing ->
            ( [], { context | continueWithModules = False } )


handlePackage : ElmJsonKey -> PackageInfo -> ProjectContext -> ( List (Rule.Error scope), ProjectContext )
handlePackage elmJsonKey package context =
    let
        elmUiVersion : Maybe Int
        elmUiVersion =
            List.findMap
                (\( packageName, constraint ) ->
                    if packageName == elmUiName then
                        Elm.Constraint.toString constraint
                            |> String.split "."
                            |> List.head
                            |> Maybe.andThen String.toInt

                    else
                        Nothing
                )
                package.deps
    in
    case elmUiVersion of
        Just 1 ->
            ( [ Rule.errorForElmJsonWithFix
                    elmJsonKey
                    (\rawElmJson ->
                        { message = "elm-ui 2 should replace elm-ui as a dependency"
                        , details = [ "Since were upgrading to elm-ui 2, elm-ui can be removed" ]
                        , range = elmJsonRange rawElmJson
                        }
                    )
                    (\_ ->
                        { package
                            | deps =
                                List.map
                                    (\( packageName, version ) ->
                                        if packageName == elmUiName then
                                            ( packageName, constraintV2 )

                                        else
                                            ( packageName, version )
                                    )
                                    package.deps
                        }
                            |> Elm.Project.Package
                            |> Just
                    )
              ]
            , { context | continueWithModules = True }
            )

        Just _ ->
            ( [ Rule.errorForElmJson
                    elmJsonKey
                    (\rawElmJson ->
                        { message = "Wrong mdgriffith/elm-ui version"
                        , details = [ "This package only upgrades from mdgriffith/elm-ui V1 to V2" ]
                        , range = elmJsonRange rawElmJson
                        }
                    )
              ]
            , { context | continueWithModules = False }
            )

        Nothing ->
            ( [ Rule.errorForElmJson
                    elmJsonKey
                    (\rawElmJson ->
                        { message = "mdgriffith/elm-ui dependency missing"
                        , details = [ "This package only upgrades from mdgriffith/elm-ui V1 to V2" ]
                        , range = elmJsonRange rawElmJson
                        }
                    )
              ]
            , { context | continueWithModules = False }
            )


elmJsonRange : String -> Range
elmJsonRange text =
    { start = { column = 0, row = 1 }
    , end =
        { column =
            case String.split "\n" text |> List.last of
                Just last ->
                    String.length last

                Nothing ->
                    0
        , row = String.toList text |> List.count ((==) '\n') |> (+) 1
        }
    }


checkDependency : ApplicationInfo -> ( Elm.Package.Name, Elm.Constraint.Constraint ) -> DependencyStatus
checkDependency application ( name2, constraint ) =
    case
        List.findMap
            (\( name, version ) ->
                if name == name2 then
                    Just version

                else
                    Nothing
            )
            (application.depsIndirect ++ application.depsDirect)
    of
        Just version ->
            if Elm.Constraint.check version constraint then
                DependencyFoundAndIsValid

            else
                DependencyFoundButIsInvalid version

        Nothing ->
            DependencyNotFound


handleApplication : ElmJsonKey -> ApplicationInfo -> ProjectContext -> ( List (Rule.Error scope), ProjectContext )
handleApplication elmJsonKey application context =
    let
        elmUiVersion : Maybe Int
        elmUiVersion =
            List.findMap
                (\( packageName, constraint ) ->
                    if Just packageName == Elm.Package.fromString "mdgriffith/elm-ui" then
                        Elm.Version.toTuple constraint
                            |> (\( major, _, _ ) -> major)
                            |> Just

                    else
                        Nothing
                )
                application.depsDirect
    in
    case elmUiVersion of
        Just 1 ->
            let
                a :
                    { invalidDependencies :
                        List
                            { name : Elm.Package.Name
                            , version : Elm.Version.Version
                            , constraint : Elm.Constraint.Constraint
                            }
                    , missingDependencies : List ( Elm.Package.Name, Elm.Version.Version )
                    }
                a =
                    List.foldl
                        (\( name, constraint ) state ->
                            case checkDependency application ( name, constraint ) of
                                DependencyNotFound ->
                                    { state
                                        | missingDependencies =
                                            ( name, getLowConstraint constraint ) :: state.missingDependencies
                                    }

                                DependencyFoundAndIsValid ->
                                    state

                                DependencyFoundButIsInvalid version ->
                                    { state
                                        | invalidDependencies =
                                            { name = name, version = version, constraint = constraint }
                                                :: state.invalidDependencies
                                    }
                        )
                        { invalidDependencies = [], missingDependencies = [] }
                        dependenciesV2
            in
            case a.invalidDependencies of
                [] ->
                    ( [ Rule.errorForElmJsonWithFix
                            elmJsonKey
                            (\rawElmJson ->
                                { message = "elm-ui 2 should replace elm-ui as a dependency"
                                , details = [ "Since were upgrading to elm-ui 2, elm-ui can be removed" ]
                                , range = elmJsonRange rawElmJson
                                }
                            )
                            (\_ ->
                                { application
                                    | depsDirect =
                                        List.map
                                            (\( packageName, version ) ->
                                                if Just packageName == Elm.Package.fromString "mdgriffith/elm-ui" then
                                                    ( packageName, version2 )

                                                else
                                                    ( packageName, version )
                                            )
                                            application.depsDirect
                                    , depsIndirect =
                                        application.depsIndirect ++ a.missingDependencies
                                }
                                    |> Elm.Project.Application
                                    |> Just
                            )
                      ]
                    , { context | continueWithModules = True }
                    )

                _ ->
                    ( [ Rule.errorForElmJson
                            elmJsonKey
                            (\rawElmJson ->
                                { message = "Incompatible package dependencies"
                                , details =
                                    [ "The following packages are required by elm-ui v2 but couldn't be installed due to them already being installed with an incompatible version"
                                    , List.map
                                        (\{ name, version, constraint } ->
                                            Elm.Package.toString name
                                                ++ " "
                                                ++ Elm.Constraint.toString constraint
                                                ++ " (you have "
                                                ++ Elm.Version.toString version
                                                ++ ")"
                                        )
                                        a.invalidDependencies
                                        |> String.join "\n"
                                    ]
                                , range = elmJsonRange rawElmJson
                                }
                            )
                      ]
                    , { context | continueWithModules = False }
                    )

        Just _ ->
            ( [ Rule.errorForElmJson
                    elmJsonKey
                    (\rawElmJson ->
                        { message = "Wrong mdgriffith/elm-ui version"
                        , details = [ "This package only upgrades from mdgriffith/elm-ui V1 to V2" ]
                        , range = elmJsonRange rawElmJson
                        }
                    )
              ]
            , { context | continueWithModules = False }
            )

        Nothing ->
            ( [ Rule.errorForElmJson
                    elmJsonKey
                    (\rawElmJson ->
                        { message = "mdgriffith/elm-ui dependency missing"
                        , details = [ "This package only upgrades from mdgriffith/elm-ui V1 to V2" ]
                        , range = elmJsonRange rawElmJson
                        }
                    )
              ]
            , { context | continueWithModules = False }
            )


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

        Typed (Node range ( moduleName, _ )) nodes ->
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
renameModules lookupTable (Node range moduleName) =
    if moduleName == [] then
        []

    else
        case Review.ModuleNameLookupTable.moduleNameAt lookupTable range of
            Just realModuleName ->
                renameModules2 (Node range realModuleName)

            Nothing ->
                []


renameModules2 : Node ModuleName -> List Fix
renameModules2 (Node range moduleName) =
    case moduleName of
        --[ "Ui" ] ->
        --    [ replaceModuleName range "MyUi" ]
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
                                (\item { endOfPrevious, fixes, isFirst } ->
                                    { fixes =
                                        if isWidthFill lookupTable item then
                                            Review.Fix.removeRange
                                                { start = endOfPrevious
                                                , end =
                                                    case ( isFirst, List.getAt 1 list ) of
                                                        ( True, Just (Node rangeNext _) ) ->
                                                            rangeNext.start

                                                        _ ->
                                                            Node.range item |> .end
                                                }
                                                :: fixes

                                        else
                                            expressionVisitor lookupTable item ++ fixes
                                    , endOfPrevious = Node.range item |> .end
                                    , isFirst = False
                                    }
                                )
                                { endOfPrevious = insideListStart, fixes = [], isFirst = True }
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
