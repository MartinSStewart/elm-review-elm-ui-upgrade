module Canonicalize exposing (function, typeAnnotation)

import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


typeAnnotation : ModuleNameLookupTable -> Node TypeAnnotation -> Node TypeAnnotation
typeAnnotation lookupTable (Node range typeAnnotation2) =
    (case typeAnnotation2 of
        GenericType a ->
            GenericType a

        Typed (Node range2 ( moduleName, name )) nodes ->
            Typed
                (Node range2 ( canonicalizeModuleName lookupTable moduleName range, name ))
                (List.map (typeAnnotation lookupTable) nodes)

        Unit ->
            Unit

        Tupled nodes ->
            List.map (typeAnnotation lookupTable) nodes |> Tupled

        Record fields ->
            List.map
                (Node.map (Tuple.mapSecond (typeAnnotation lookupTable)))
                fields
                |> Record

        GenericRecord variable fields ->
            GenericRecord
                variable
                (Node.map
                    (List.map (Node.map (Tuple.mapSecond (typeAnnotation lookupTable))))
                    fields
                )

        FunctionTypeAnnotation node1 node2 ->
            FunctionTypeAnnotation
                (typeAnnotation lookupTable node1)
                (typeAnnotation lookupTable node2)
    )
        |> Node range


canonicalizeModuleName : ModuleNameLookupTable -> ModuleName -> Range -> List String
canonicalizeModuleName lookupTable moduleName range =
    if moduleName == [] then
        []

    else
        Review.ModuleNameLookupTable.moduleNameAt lookupTable range
            |> Maybe.withDefault moduleName


pattern : ModuleNameLookupTable -> Node Pattern -> Node Pattern
pattern lookupTable (Node range pattern2) =
    (case pattern2 of
        AllPattern ->
            AllPattern

        UnitPattern ->
            UnitPattern

        CharPattern a ->
            CharPattern a

        StringPattern a ->
            StringPattern a

        IntPattern a ->
            IntPattern a

        HexPattern a ->
            HexPattern a

        FloatPattern a ->
            FloatPattern a

        TuplePattern nodes ->
            List.map (pattern lookupTable) nodes |> TuplePattern

        RecordPattern a ->
            RecordPattern a

        UnConsPattern node nodes ->
            UnConsPattern
                (pattern lookupTable node)
                (pattern lookupTable nodes)

        ListPattern nodes ->
            List.map (pattern lookupTable) nodes |> ListPattern

        VarPattern a ->
            VarPattern a

        NamedPattern { moduleName, name } nodes ->
            NamedPattern
                { moduleName = canonicalizeModuleName lookupTable moduleName range
                , name = name
                }
                (List.map (pattern lookupTable) nodes)

        AsPattern node variable ->
            AsPattern (pattern lookupTable node) variable

        ParenthesizedPattern node ->
            pattern lookupTable node |> ParenthesizedPattern
    )
        |> Node range


function : ModuleNameLookupTable -> Function -> Function
function lookupTable function2 =
    { documentation = function2.documentation
    , signature =
        case function2.signature of
            Just node ->
                Node.map
                    (\signature ->
                        { name = signature.name
                        , typeAnnotation = typeAnnotation lookupTable signature.typeAnnotation
                        }
                    )
                    node
                    |> Just

            Nothing ->
                Nothing
    , declaration =
        Node.map
            (\implementation ->
                { name = implementation.name
                , arguments = List.map (pattern lookupTable) implementation.arguments
                , expression = expression lookupTable implementation.expression
                }
            )
            function2.declaration
    }


expression : ModuleNameLookupTable -> Node Expression -> Node Expression
expression lookupTable (Node range expr) =
    (case expr of
        FunctionOrValue moduleName name ->
            FunctionOrValue
                (canonicalizeModuleName lookupTable moduleName range)
                name

        Application list ->
            List.map (expression lookupTable) list |> Application

        UnitExpr ->
            UnitExpr

        OperatorApplication operator direction left right ->
            OperatorApplication
                operator
                direction
                (expression lookupTable left)
                (expression lookupTable right)

        IfBlock condition ifTrue ifFalse ->
            IfBlock
                (expression lookupTable condition)
                (expression lookupTable ifTrue)
                (expression lookupTable ifFalse)

        PrefixOperator a ->
            PrefixOperator a

        Operator a ->
            Operator a

        Integer a ->
            Integer a

        Hex a ->
            Hex a

        Floatable a ->
            Floatable a

        Negation node ->
            expression lookupTable node |> Negation

        Literal a ->
            Literal a

        CharLiteral a ->
            CharLiteral a

        TupledExpression nodes ->
            List.map (expression lookupTable) nodes |> TupledExpression

        ParenthesizedExpression node ->
            expression lookupTable node |> ParenthesizedExpression

        LetExpression letBlock ->
            LetExpression
                { expression = expression lookupTable letBlock.expression
                , declarations =
                    List.map
                        (Node.map
                            (\letDeclaration ->
                                case letDeclaration of
                                    LetFunction function2 ->
                                        function lookupTable function2 |> LetFunction

                                    LetDestructuring pattern2 expression2 ->
                                        LetDestructuring
                                            (pattern lookupTable pattern2)
                                            (expression lookupTable expression2)
                            )
                        )
                        letBlock.declarations
                }

        CaseExpression caseBlock ->
            CaseExpression
                { expression = expression lookupTable caseBlock.expression
                , cases =
                    List.map
                        (\( pattern2, expression2 ) ->
                            ( pattern lookupTable pattern2
                            , expression lookupTable expression2
                            )
                        )
                        caseBlock.cases
                }

        LambdaExpression lambda ->
            LambdaExpression
                { args = List.map (pattern lookupTable) lambda.args
                , expression = expression lookupTable lambda.expression
                }

        RecordExpr nodes ->
            List.map (Node.map (Tuple.mapSecond (expression lookupTable))) nodes
                |> RecordExpr

        ListExpr nodes ->
            List.map (expression lookupTable) nodes |> ListExpr

        RecordAccess node field ->
            RecordAccess
                (expression lookupTable node)
                field

        RecordAccessFunction a ->
            RecordAccessFunction a

        RecordUpdateExpression variable nodes ->
            RecordUpdateExpression
                variable
                (List.map (Node.map (Tuple.mapSecond (expression lookupTable))) nodes)

        GLSLExpression a ->
            GLSLExpression a
    )
        |> Node range
