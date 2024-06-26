module UpgradeElmUi exposing (rule)

{-|

@docs rule

-}

import Canonicalize
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
import Elm.Writer
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
    Rule.initContextCreator
        (\moduleContext ->
            { continueWithModules = moduleContext.continueWithModules
            }
        )


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
            ( [ Rule.globalError
                    { message = "elm.json not available"
                    , details = [ "This rule can't run without an elm.json file." ]
                    }
              ]
            , { context | continueWithModules = False }
            )


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

        Just 2 ->
            ( [], { context | continueWithModules = False } )

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

        Just 2 ->
            ( [], { context | continueWithModules = False } )

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
importVisitor (Node range import2) =
    let
        fix text =
            Review.Fix.replaceRangeBy (Node.range import2.moduleName) text
                :: (case import2.exposingList of
                        Just (Node exposingRange _) ->
                            let
                                start =
                                    exposingRange.start
                            in
                            [ Review.Fix.removeRange { exposingRange | start = { start | column = start.column - 1 } } ]

                        Nothing ->
                            []
                   )
    in
    case Node.value import2.moduleName of
        [ "Element", "Background" ] ->
            [ Review.Fix.removeRange range ]

        [ "Element", "Border" ] ->
            [ Review.Fix.removeRange range ]

        [ "Element" ] ->
            Review.Fix.insertAt range.end "\nimport Ui.Prose\nimport Ui.Layout\nimport Ui.Anim"
                :: fix "Ui"

        [ "Element", "Events" ] ->
            fix "Ui.Events"

        [ "Element", "Font" ] ->
            fix "Ui.Font"

        [ "Element", "Input" ] ->
            Review.Fix.insertAt range.end "\nimport Ui.Events\n" :: fix "Ui.Input"

        [ "Element", "Keyed" ] ->
            fix "Ui.Keyed"

        [ "Element", "Lazy" ] ->
            fix "Ui.Lazy"

        [ "Element", "Region" ] ->
            fix "Ui.Accessibility"

        _ ->
            []


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


patternVisitor : Node Pattern -> List Fix
patternVisitor (Node range pattern) =
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
            List.concatMap patternVisitor nodes

        RecordPattern _ ->
            []

        UnConsPattern node nodes ->
            patternVisitor node ++ patternVisitor nodes

        ListPattern nodes ->
            List.concatMap patternVisitor nodes

        VarPattern _ ->
            []

        NamedPattern { moduleName, name } nodes ->
            renameFunctions
                (Node
                    { start = range.start
                    , end =
                        { column = range.end.column + String.length name - 1
                        , row = range.start.row
                        }
                    }
                    ( moduleName, name )
                )
                ++ List.concatMap patternVisitor nodes

        AsPattern node _ ->
            patternVisitor node

        ParenthesizedPattern node ->
            patternVisitor node


functionVisitor : Function -> List Fix
functionVisitor function =
    (case function.signature of
        Just (Node _ signature) ->
            typeAnnotationVisitor signature.typeAnnotation

        _ ->
            []
    )
        ++ (function.declaration |> Node.value |> .arguments |> List.concatMap patternVisitor)
        ++ (function.declaration |> Node.value |> .expression |> expressionVisitor)


topLevelDeclarationVisitor : ModuleNameLookupTable -> Node Declaration -> List Fix
topLevelDeclarationVisitor lookupTable declaration =
    case Node.value declaration of
        FunctionDeclaration function ->
            Canonicalize.function lookupTable function |> functionVisitor

        AliasDeclaration typeAlias ->
            Canonicalize.typeAnnotation lookupTable typeAlias.typeAnnotation |> typeAnnotationVisitor

        CustomTypeDeclaration type2 ->
            List.concatMap
                (\(Node _ constructor) ->
                    List.concatMap
                        (Canonicalize.typeAnnotation lookupTable >> typeAnnotationVisitor)
                        constructor.arguments
                )
                type2.constructors

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []

        Destructuring _ _ ->
            []


typeAnnotationVisitor : Node TypeAnnotation -> List Fix
typeAnnotationVisitor (Node _ typeAnnotation) =
    case typeAnnotation of
        GenericType _ ->
            []

        Typed node nodes ->
            case ( node, nodes ) of
                ( Node attrRange ( [ "Element" ], "Attr" ), [ Node range2 _, _ ] ) ->
                    [ Review.Fix.replaceRangeBy { attrRange | end = range2.end } "Ui.Attribute" ]

                _ ->
                    renameFunctions node ++ List.concatMap typeAnnotationVisitor nodes

        Unit ->
            []

        Tupled nodes ->
            List.concatMap typeAnnotationVisitor nodes

        Record fields ->
            List.concatMap
                (\(Node _ ( _, typeAnnotation2 )) -> typeAnnotationVisitor typeAnnotation2)
                fields

        GenericRecord _ (Node _ fields) ->
            List.concatMap
                (\(Node _ ( _, typeAnnotation2 )) -> typeAnnotationVisitor typeAnnotation2)
                fields

        FunctionTypeAnnotation node1 node2 ->
            typeAnnotationVisitor node1 ++ typeAnnotationVisitor node2


renameFunctions : Node ( ModuleName, String ) -> List Fix
renameFunctions (Node range ( moduleName, function )) =
    let
        fix : String -> List Fix
        fix text =
            [ Review.Fix.replaceRangeBy range text ]
    in
    case moduleName ++ [ function ] of
        [ "Element", "Background", "color" ] ->
            fix "Ui.background"

        [ "Element", "Border", "color" ] ->
            fix "Ui.borderColor"

        [ "Element", "Border", "rounded" ] ->
            fix "Ui.rounded"

        [ "Element", "Border", "roundEach" ] ->
            fix "Ui.roundedWith"

        [ "Element", "Border", "width" ] ->
            fix "Ui.border"

        [ "Element", "Border", "widthEach" ] ->
            fix "Ui.borderWith"

        [ "Element", "Border", "shadow" ] ->
            fix "Ui.shadow"

        [ "Element", "Events", name ] ->
            fix ("Ui.Events." ++ name)

        [ "Element", "Font", name ] ->
            fix ("Ui.Font." ++ name)

        [ "Element", "Input", name ] ->
            fix ("Ui.Input." ++ name)

        [ "Element", "Keyed", name ] ->
            fix ("Ui.Keyed." ++ name)

        [ "Element", "Lazy", name ] ->
            fix ("Ui.Lazy." ++ name)

        [ "Element", "Region", name ] ->
            fix ("Ui.Accessibility." ++ name)

        [ "Element", "wrappedRow" ] ->
            fix "Ui.Layout.row { wrap = True, align = (Ui.Layout.left, Ui.Layout.top) }"

        [ "Element", "moveLeft" ] ->
            fix "Ui.left"

        [ "Element", "moveRight" ] ->
            fix "Ui.right"

        [ "Element", "moveUp" ] ->
            fix "Ui.up"

        [ "Element", "moveDown" ] ->
            fix "Ui.down"

        [ "Element", "table" ] ->
            fix "Ui.Table.column"

        [ "Element", "indexedTable" ] ->
            fix "Ui.Table.indexedTable"

        [ "Element", "paragraph" ] ->
            fix "Ui.Prose.paragraph"

        [ "Element", "paddingEach" ] ->
            fix "Ui.paddingWith"

        [ "Element", "alpha" ] ->
            fix "Ui.opacity"

        [ "Element", "fillPortion" ] ->
            fix "Ui.portion"

        [ "Element", "rgb255" ] ->
            fix "Ui.rgb"

        [ "Element", name ] ->
            fix ("Ui." ++ name)

        _ ->
            []


addListItem : String -> Range -> List a -> Fix
addListItem text listRange list =
    let
        insideListStart : Location
        insideListStart =
            { column = listRange.start.column + 1, row = listRange.start.row }
    in
    " "
        ++ text
        ++ (if List.isEmpty list then
                ""

            else
                ","
           )
        |> Review.Fix.insertAt insideListStart


writeExpression expr =
    Elm.Writer.write (Elm.Writer.writeExpression expr)
        |> String.replace "\n" ("\n" ++ String.repeat 100 " ")


handleAttributeList moduleName function listRange list =
    if isLayoutElement moduleName function then
        if List.any isWidthAttribute list then
            List.foldl
                (\item { endOfPrevious, fixes, isFirst } ->
                    { fixes =
                        if isWidthFill item then
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
                            expressionVisitor item ++ fixes
                    , endOfPrevious = Node.range item |> .end
                    , isFirst = False
                    }
                )
                { endOfPrevious = { column = listRange.start.column + 1, row = listRange.start.row }
                , fixes = []
                , isFirst = True
                }
                list
                |> .fixes

        else
            addListItem "Ui.width Ui.shrink" listRange list
                :: List.concatMap expressionVisitor list

    else
        List.concatMap expressionVisitor list


removeParens : Node Expression -> Node Expression
removeParens expr =
    case Node.value expr of
        ParenthesizedExpression node ->
            removeParens node

        _ ->
            expr


expressionVisitor : Node Expression -> List Fix
expressionVisitor (Node range expr) =
    case expr of
        FunctionOrValue moduleName function ->
            renameFunctions (Node range ( moduleName, function ))

        Application [ Node range2 (FunctionOrValue [ "Element" ] "link"), Node listRange (ListExpr list), Node recordRange (RecordExpr [ Node _ ( Node _ "label", label ), Node _ ( Node _ "url", url ) ]) ] ->
            [ Review.Fix.replaceRangeBy range2 "Ui.el"
            , addListItem ("Ui.link (" ++ writeExpression url ++ ")") listRange list
            , Review.Fix.replaceRangeBy recordRange ("(" ++ writeExpression label ++ ")")
            ]

        Application [ Node range2 (FunctionOrValue [ "Element" ] "newTabLink"), Node listRange (ListExpr list), Node recordRange (RecordExpr [ Node _ ( Node _ "label", label ), Node _ ( Node _ "url", url ) ]) ] ->
            [ Review.Fix.replaceRangeBy range2 "Ui.el"
            , addListItem ("Ui.linkNewTab (" ++ writeExpression url ++ ")") listRange list
            , Review.Fix.replaceRangeBy recordRange ("(" ++ writeExpression label ++ ")")
            ]

        Application [ Node range2 (FunctionOrValue [ "Element" ] "download"), Node listRange (ListExpr list), Node recordRange (RecordExpr [ Node _ ( Node _ "label", label ), Node _ ( Node _ "url", url ) ]) ] ->
            [ Review.Fix.replaceRangeBy range2 "Ui.el"
            , addListItem ("Ui.download (" ++ writeExpression url ++ ")") listRange list
            , Review.Fix.replaceRangeBy recordRange ("(" ++ writeExpression label ++ ")")
            ]

        Application [ Node range2 (FunctionOrValue [ "Element" ] "image"), _, Node _ (RecordExpr [ Node _ ( Node srcRange "src", _ ), _ ]) ] ->
            [ Review.Fix.replaceRangeBy range2 "Ui.image"
            , Review.Fix.replaceRangeBy srcRange "source"
            ]

        Application [ Node _ (FunctionOrValue [ "Element", "Region" ] "heading"), Node _ (Integer value) ] ->
            if value > 0 && value < 7 then
                [ Review.Fix.replaceRangeBy range ("Ui.Accessibility.h" ++ String.fromInt value) ]

            else
                []

        Application ((Node range2 (FunctionOrValue [ "Element" ] "mouseOver")) :: rest) ->
            Review.Fix.replaceRangeBy range2 "Ui.Anim.hovered (Ui.Anim.ms 0)"
                :: fixMouseOverAttributes rest

        Application [ Node _ (FunctionOrValue [ "Element" ] "rgb"), red, green, blue ] ->
            let
                handleColor color =
                    case Node.value color of
                        Floatable float ->
                            float * 255 |> round |> String.fromInt

                        Integer int ->
                            int * 255 |> String.fromInt

                        _ ->
                            "(round (255 * (" ++ writeExpression color ++ ")))"
            in
            [ "Ui.rgb "
                ++ handleColor red
                ++ " "
                ++ handleColor green
                ++ " "
                ++ handleColor blue
                |> Review.Fix.replaceRangeBy range
            ]

        Application [ Node range2 (FunctionOrValue [ "Element", "Input" ] "button"), Node listRange (ListExpr list), Node recordRange (RecordExpr [ Node _ ( Node _ "label", label ), Node _ ( Node _ "onPress", onPress ) ]) ] ->
            let
                fixButton msg =
                    [ Review.Fix.replaceRangeBy range2 "Ui.el"
                    , addListItem ("Ui.Events.onClick (" ++ writeExpression msg ++ ")") listRange list
                    , Review.Fix.replaceRangeBy recordRange ("(" ++ writeExpression label ++ ")")
                    ]
            in
            case removeParens onPress |> Node.value of
                Application [ Node _ (FunctionOrValue _ "Just"), msg ] ->
                    fixButton msg

                OperatorApplication "<|" _ (Node _ (FunctionOrValue _ "Just")) msg ->
                    fixButton msg

                OperatorApplication "|>" _ msg (Node _ (FunctionOrValue _ "Just")) ->
                    fixButton msg

                _ ->
                    fixButton onPress

        Application ((Node range2 (FunctionOrValue moduleName function)) :: (Node listRange (ListExpr list)) :: rest) ->
            renameFunctions (Node range2 ( moduleName, function ))
                ++ List.concatMap expressionVisitor rest
                ++ handleAttributeList moduleName function listRange list

        Application ((Node range2 (FunctionOrValue moduleName function)) :: second :: rest) ->
            (if isLayoutElement moduleName function then
                let
                    leftPadding =
                        String.repeat (range2.start.column - 1) " "
                in
                [ Review.Fix.insertAt
                    range2.end
                    ("\n" ++ leftPadding ++ "-- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes\n" ++ leftPadding)
                ]

             else
                []
            )
                ++ renameFunctions (Node range2 ( moduleName, function ))
                ++ List.concatMap expressionVisitor (second :: rest)

        Application list ->
            List.concatMap expressionVisitor list

        UnitExpr ->
            []

        OperatorApplication _ _ left right ->
            expressionVisitor left ++ expressionVisitor right

        IfBlock condition ifTrue ifFalse ->
            expressionVisitor condition
                ++ expressionVisitor ifTrue
                ++ expressionVisitor ifFalse

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
            expressionVisitor node

        Literal _ ->
            []

        CharLiteral _ ->
            []

        TupledExpression nodes ->
            List.concatMap expressionVisitor nodes

        ParenthesizedExpression node ->
            expressionVisitor node

        LetExpression letBlock ->
            expressionVisitor letBlock.expression
                ++ List.concatMap
                    (\(Node _ letDeclaration) ->
                        case letDeclaration of
                            LetFunction function ->
                                functionVisitor function

                            LetDestructuring pattern expression ->
                                patternVisitor pattern ++ expressionVisitor expression
                    )
                    letBlock.declarations

        CaseExpression caseBlock ->
            expressionVisitor caseBlock.expression
                ++ List.concatMap
                    (\( pattern, expression ) ->
                        patternVisitor pattern
                            ++ expressionVisitor expression
                    )
                    caseBlock.cases

        LambdaExpression lambda ->
            List.concatMap patternVisitor lambda.args
                ++ expressionVisitor lambda.expression

        RecordExpr nodes ->
            List.concatMap (\(Node _ ( _, field )) -> expressionVisitor field) nodes

        ListExpr nodes ->
            List.concatMap expressionVisitor nodes

        RecordAccess node _ ->
            expressionVisitor node

        RecordAccessFunction _ ->
            []

        RecordUpdateExpression _ nodes ->
            List.concatMap (\(Node _ ( _, field )) -> expressionVisitor field) nodes

        GLSLExpression _ ->
            []


fixMouseOverAttributes : List (Node Expression) -> List Fix
fixMouseOverAttributes rest =
    case rest of
        [ Node _ (ListExpr list) ] ->
            List.concatMap
                (\(Node _ item) ->
                    case item of
                        Application [ Node range3 (FunctionOrValue [ "Element", "Background" ] "color"), _ ] ->
                            [ Review.Fix.replaceRangeBy range3 "Ui.Anim.backgroundColor" ]

                        Application [ Node range3 (FunctionOrValue [ "Element", "Border" ] "color"), _ ] ->
                            [ Review.Fix.replaceRangeBy range3 "Ui.Anim.borderColor" ]

                        Application [ Node range3 (FunctionOrValue [ "Element", "Font" ] "color"), _ ] ->
                            [ Review.Fix.replaceRangeBy range3 "Ui.Anim.fontColor" ]

                        Application [ Node range3 (FunctionOrValue [ "Element" ] "alpha"), _ ] ->
                            [ Review.Fix.replaceRangeBy range3 "Ui.Anim.opacity" ]

                        _ ->
                            []
                )
                list

        _ ->
            []


isWidthFill : Node Expression -> Bool
isWidthFill expr =
    case Node.value expr of
        Application [ Node _ (FunctionOrValue moduleName1 "width"), Node _ (FunctionOrValue moduleName2 "fill") ] ->
            (moduleName1 == [ "Element" ])
                && (moduleName2 == [ "Element" ])

        _ ->
            False


isWidthAttribute : Node Expression -> Bool
isWidthAttribute expr =
    case Node.value expr of
        Application [ Node _ (FunctionOrValue moduleName "width"), _ ] ->
            moduleName == [ "Element" ]

        _ ->
            False


isLayoutElement : List String -> String -> Bool
isLayoutElement moduleName function =
    Set.member
        ( moduleName
        , function
        )
        hasAttributeListParam


hasAttributeListParam : Set ( List String, String )
hasAttributeListParam =
    Set.fromList
        [ ( [ "Element" ], "el" )
        , ( [ "Element" ], "row" )
        , ( [ "Element" ], "column" )
        , ( [ "Element" ], "table" )
        , ( [ "Element" ], "indexedTable" )
        , ( [ "Element" ], "paragraph" )
        , ( [ "Element" ], "wrappedRow" )
        , ( [ "Element" ], "textColumn" )
        , ( [ "Element" ], "indexedTable" )
        , ( [ "Element" ], "image" )
        , ( [ "Element" ], "newTabLink" )
        , ( [ "Element" ], "link" )
        , ( [ "Element" ], "download" )
        , ( [ "Element" ], "downloadAs" )
        , ( [ "Element", "Input" ], "button" )
        , ( [ "Element", "Input" ], "text" )
        , ( [ "Element", "Input" ], "checkbox" )
        , ( [ "Element", "Input" ], "multiline" )
        , ( [ "Element", "Input" ], "placeholder" )
        , ( [ "Element", "Input" ], "username" )
        , ( [ "Element", "Input" ], "newPassword" )
        , ( [ "Element", "Input" ], "currentPassword" )
        , ( [ "Element", "Input" ], "email" )
        , ( [ "Element", "Input" ], "search" )
        , ( [ "Element", "Input" ], "spellChecked" )
        , ( [ "Element", "Input" ], "slider" )
        , ( [ "Element", "Input" ], "radio" )
        , ( [ "Element", "Input" ], "radioRow" )
        , ( [ "Element", "Input" ], "labelAbove" )
        , ( [ "Element", "Input" ], "labelBelow" )
        , ( [ "Element", "Input" ], "labelLeft" )
        , ( [ "Element", "Input" ], "labelRight" )
        ]
