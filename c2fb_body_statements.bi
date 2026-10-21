/'
    Project: C2FB body lowering

    File: c2fb_body_statements.bi

    Purpose:

        Parse supported C statements and lower them to conservative FreeBASIC.

    Responsibilities:

        - Parse statements, blocks, and supported ordinary control flow.
        - Lower returns, assignments, and jumps.
        - Delegate switch statements to the shared switch state machine.
        - Preserve the first detailed parse failure for shared diagnostics.
        - Consume shared local declaration and expression-lowering helpers.

    This file intentionally does NOT contain:

        - Procedure-signature conversion or function candidate assembly.
        - C source collection, preprocessing, or generated-file output.
'/

Declare Function ParseSimpleCStatement( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef outputText As String, _
    ByVal indentText As String, _
    ByVal returnType As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByRef symbolCount As Long, _
    locals() As SimpleLocalVariable, _
    ByRef localCount As Long, _
    ByRef terminatesPaths As Long, _
    ByVal allowBodyReplacements As Long = -1 _
) As Long

Declare Function TranslateSimpleReturnValue( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal returnType As String, _
    ByRef sourceExpression As String, _
    ByRef translatedExpression As String, _
    ByRef expressionPrelude As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByVal parameterCount As Long _
) As Long

Declare Function ReadSimpleCStatement( _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef statementText As String _
) As Long

Declare Function ParseSimpleSwitchStatement( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef outputText As String, _
    ByVal indentText As String, _
    ByVal returnType As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByRef symbolCount As Long, _
    locals() As SimpleLocalVariable, _
    ByRef localCount As Long, _
    ByRef terminatesPaths As Long _
) As Long

Private Function ReadSimpleReturn( _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef returnExpression As String _
) As Long
    SkipCSpaces(sourceText, position)
    If Mid(sourceText, position, 6) <> "return" Then Return 0
    If position + 6 <= Len(sourceText) AndAlso _
       IsIdentifierPart(Asc(sourceText, position + 6)) <> 0 Then Return 0

    position += 6
    SkipCSpaces(sourceText, position)
    Return ReadSimpleCStatement(sourceText, position, returnExpression)
End Function

Private Function ReadSimpleIfCondition( _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef conditionText As String _
) As Long
    SkipCSpaces(sourceText, position)
    If Mid(sourceText, position, 2) <> "if" Then Return 0
    If position + 2 <= Len(sourceText) AndAlso _
       IsIdentifierPart(Asc(sourceText, position + 2)) <> 0 Then Return 0
    position += 2
    SkipCSpaces(sourceText, position)
    If position > Len(sourceText) OrElse Asc(sourceText, position) <> Asc("(") Then Return 0

    Dim conditionStart As Long = position + 1
    If ScanSimpleExpressionGroup(sourceText, position, Asc("("), Asc(")")) = 0 Then Return 0
    conditionText = Trim(Mid(sourceText, conditionStart, position - conditionStart - 1))
    If Len(conditionText) = 0 Then Return 0
    Return -1
End Function

Private Function TranslateSimpleReturnValue( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal returnType As String, ByRef sourceExpression As String, _
    ByRef translatedExpression As String, ByRef expressionPrelude As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal parameterCount As Long _
) As Long
    translatedExpression = ""
    expressionPrelude = ""
    If returnType = "void" Then
        If Len(sourceExpression) <> 0 Then Return 0
        expressionPrelude = SimpleAllocaCleanupText(context)
        Return -1
    End If
    If Len(sourceExpression) = 0 Then Return 0
    If TranslateSequencedExpression(translation, catalog, _
                                    sourceExpression, translatedExpression, expressionPrelude, context, _
                                    callableNames(), callableCount, parameterNames(), parameterAliases(), _
                                    parameterTypes(), parameterCount, returnType) = 0 Then Return 0
    If context.allocaHeadName <> "" Then
        Dim returnTemporary As String = _
            NewSimpleBodyTemporary(context, returnType, "")
        If returnTemporary = "" Then Return 0
        expressionPrelude &= returnTemporary & " = " & translatedExpression & Chr(10)
        translatedExpression = returnTemporary
        expressionPrelude &= SimpleAllocaCleanupText(context) & Chr(10)
    End If
    Return -1
End Function

Private Function ReadSimpleCStatement( _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef statementText As String _
) As Long
    SkipCSpaces(sourceText, position)
    If position > Len(sourceText) Then Return 0

    Dim startPosition As Long = position
    Dim parenDepth As Long = 0
    Dim braceDepth As Long = 0
    Dim quote As Long = 0
    Dim escaped As Long = 0
    While position <= Len(sourceText)
        Dim c As Long = Asc(sourceText, position)
        If quote <> 0 Then
            If escaped <> 0 Then
                escaped = 0
            ElseIf c = Asc(Chr(92)) Then
                escaped = -1
            ElseIf c = quote Then
                quote = 0
            End If
        ElseIf c = Asc(Chr(34)) OrElse c = Asc("'") Then
            quote = c
        ElseIf c = Asc("(") Then
            parenDepth += 1
        ElseIf c = Asc(")") Then
            parenDepth -= 1
            If parenDepth < 0 Then Return 0
        ElseIf c = Asc("{") Then
            braceDepth += 1
        ElseIf c = Asc("}") Then
            If braceDepth = 0 Then Return 0
            braceDepth -= 1
        ElseIf parenDepth = 0 AndAlso braceDepth = 0 AndAlso c = Asc(";") Then
            statementText = Trim(Mid(sourceText, startPosition, position - startPosition))
            position += 1
            Return -1
        End If
        position += 1
    Wend
    Return 0
End Function

Private Function IsSimpleUnusedParameterStatement( _
    ByVal statementText As String _
) As Long
    Dim position As Long = 1
    While position <= Len(statementText)
        While position <= Len(statementText) AndAlso _
              IsSpaceByte(Asc(statementText, position)) <> 0
            position += 1
        Wend
        If Mid(statementText, position, 6) <> "(void)" Then Return 0
        position += 6

        While position <= Len(statementText) AndAlso _
              IsSpaceByte(Asc(statementText, position)) <> 0
            position += 1
        Wend
        If position > Len(statementText) OrElse _
           Asc(statementText, position) <> Asc("(") Then Return 0
        position += 1

        While position <= Len(statementText) AndAlso _
              IsSpaceByte(Asc(statementText, position)) <> 0
            position += 1
        Wend
        If position > Len(statementText) OrElse _
           IsIdentifierStart(Asc(statementText, position)) = 0 Then Return 0
        position += 1
        While position <= Len(statementText) AndAlso _
              IsIdentifierPart(Asc(statementText, position)) <> 0
            position += 1
        Wend
        While position <= Len(statementText) AndAlso _
              IsSpaceByte(Asc(statementText, position)) <> 0
            position += 1
        Wend
        If position > Len(statementText) OrElse _
           Asc(statementText, position) <> Asc(")") Then Return 0
        position += 1

        While position <= Len(statementText) AndAlso _
              IsSpaceByte(Asc(statementText, position)) <> 0
            position += 1
        Wend
        If position > Len(statementText) Then Return -1
        If Asc(statementText, position) <> Asc(",") Then Return 0
        position += 1
    Wend
    Return 0
End Function

Private Function IsSimpleVoidZeroStatement(ByVal statementText As String) As Long
    ' SQLite's disabled assertions expand to nested parentheses around void
    ' zero. No operand is evaluated. Other discarded expressions stay intact.
    Dim compact As String
    For i As Long = 1 To Len(statementText)
        If IsSpaceByte(Asc(statementText, i)) = 0 Then compact &= Mid(statementText, i, 1)
    Next
    While Len(compact) > 1 AndAlso Left(compact, 1) = "("
        Dim position As Long = 1
        If ScanSimpleExpressionGroup(compact, position, Asc("("), Asc(")")) = 0 Then Return 0
        If position <> Len(compact) + 1 Then Exit While
        compact = Mid(compact, 2, Len(compact) - 2)
    Wend
    If Left(compact, 6) <> "(void)" Then Return 0
    compact = Mid(compact, 7)
    While Len(compact) > 1 AndAlso Left(compact, 1) = "("
        Dim position As Long = 1
        If ScanSimpleExpressionGroup(compact, position, Asc("("), Asc(")")) = 0 Then Return 0
        If position <> Len(compact) + 1 Then Exit While
        compact = Mid(compact, 2, Len(compact) - 2)
    Wend
    If compact = "0" Then Return -1
    ' SQLite also casts a plain local identifier to void to suppress unused
    ' warnings. Calls and other effectful expressions remain translated.
    Return CInt(IsIdentifierToken(compact) <> 0)
End Function



Private Function InferSimpleArrayCompoundLiteralBound( _
    values() As String, ByVal valueCount As Long, _
    ByRef arrayCount As Long, ByRef context As SimpleBodyContext _
) As Long
    arrayCount = 0
    Dim nextIndex As Long = 0
    For valueIndex As Long = 0 To valueCount - 1
        Dim initializerValue As String = Trim(values(valueIndex))
        If Left(initializerValue, 1) = "[" Then
            Dim designatorEnd As Long = InStr(2, initializerValue, "]")
            If designatorEnd <= 2 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-inferred-array-compound-literal-designator", _
                    "cannot infer an array compound literal bound from indexed designators: " & _
                    Left(initializerValue, 80))
                Return 0
            End If
            Dim designatorIndex As Long
            If EvaluateSimpleArrayBound( _
                Trim(Mid(initializerValue, 2, designatorEnd - 2)), _
                designatorIndex, MAX_SIMPLE_ARRAY_COUNT - 1) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-inferred-array-compound-literal-designator", _
                    "array compound literal index designator is not a supported constant")
                Return 0
            End If
            Dim designatorSuffix As String = Trim(Mid( _
                initializerValue, designatorEnd + 1))
            If Left(designatorSuffix, 1) <> "=" AndAlso _
               Left(designatorSuffix, 1) <> "." Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-inferred-array-compound-literal-designator", _
                    "unsupported array compound literal designator " & _
                    Left(initializerValue, 80))
                Return 0
            End If
            nextIndex = designatorIndex
        End If
        If nextIndex >= MAX_SIMPLE_ARRAY_COUNT Then
            RecordSimpleBodyFailure( _
                context, "array-compound-literal-limit-exceeded", _
                "compound literal array size must be between 1 and " & _
                Trim(Str(MAX_SIMPLE_ARRAY_COUNT)))
            Return 0
        End If
        If nextIndex + 1 > arrayCount Then arrayCount = nextIndex + 1
        nextIndex += 1
    Next
    Return CInt(arrayCount > 0)
End Function

Private Function LowerSimpleCompoundLiteralArrayValue( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal typeText As String, ByVal aggregateText As String, _
    ByVal targetName As String, ByVal targetType As String, _
    ByRef pointerType As String, ByRef pointerExpression As String, _
    ByRef outputPrelude As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByRef symbolCount As Long, _
    ByRef arrayCountOutput As Long, ByVal inspectOnly As Long = 0 _
) As Long
    pointerType = ""
    pointerExpression = ""
    outputPrelude = ""
    arrayCountOutput = 0
    Dim arrayOpen As Long = InStr(typeText, "[")
    Dim arrayClose As Long = InStr(arrayOpen + 1, typeText, "]")
    If arrayOpen <= 1 OrElse arrayClose <= arrayOpen OrElse _
       Trim(Mid(typeText, arrayClose + 1)) <> "" OrElse _
       InStr(Mid(typeText, arrayOpen + 1, arrayClose - arrayOpen - 1), "[") <> 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-array-compound-literal-type", _
            "compound literal requires a one-dimensional C array type")
        Return 0
    End If
    Dim elementTypeText As String = Trim(Left(typeText, arrayOpen - 1))
    Dim boundText As String = Trim(Mid( _
        typeText, arrayOpen + 1, arrayClose - arrayOpen - 1))
    Dim typeTokens() As String
    Dim typeTokenCount As Long
    TokenizeCDeclaration(elementTypeText, typeTokens(), typeTokenCount)
    If typeTokenCount <= 0 Then Return 0
    Dim resolvedElementType As C2fbResolvedTypeModel
    ResolveC2fbTypeModel(translation, catalog, typeTokens(), 0, _
                         typeTokenCount - 1, resolvedElementType, _
                         C2FB_POINTER_MAPPING_PRESERVE, _
                         C2FB_TYPE_MAPPING_BODY, _
                         C2FB_TYPE_RECOGNITION_KNOWN_TYPES_ONLY)
    If resolvedElementType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then
        RecordSimpleBodyFailure( _
            context, "unsupported-array-compound-literal-element-type", _
            "cannot resolve compound literal array element type " & elementTypeText)
        Return 0
    End If
    Dim elementType As String = resolvedElementType.freeBasicType
    Dim storageType As String = elementType
    If resolvedElementType.isConst <> 0 AndAlso _
       InStr(storageType, " Ptr") = 0 AndAlso _
       Left(storageType, 6) = "Const " Then
        storageType = Mid(storageType, 7)
    End If
    Dim normalizedElementType As String = NormalizeFbBodyType(catalog, storageType)
    If targetType <> "" Then
        Dim targetElementType As String = SimplePointeeType(targetType)
        Dim normalizedTargetType As String = NormalizeFbBodyType( _
            catalog, targetElementType)
        If targetElementType = "" OrElse normalizedTargetType = "Any" OrElse _
           normalizedElementType <> normalizedTargetType OrElse _
           (resolvedElementType.isConst <> 0 AndAlso _
            Left(targetType, 6) <> "Const ") Then
            RecordSimpleBodyFailure( _
                context, "incompatible-array-compound-literal-assignment", _
                "compound literal array element type does not match pointer " & _
                targetName)
            Return 0
        End If
        pointerType = targetType
    Else
        pointerType = storageType & " Ptr"
        If resolvedElementType.isConst <> 0 Then _
            pointerType = "Const " & pointerType
    End If

    Dim values() As String
    Dim valueCount As Long = 0
    Dim isStringInitializer As Long = 0
    Dim initializerBody As String = Trim(aggregateText)
    If Left(initializerBody, 1) <> "{" OrElse _
       Right(initializerBody, 1) <> "}" Then Return 0
    initializerBody = Trim(Mid( _
        initializerBody, 2, Len(initializerBody) - 2))
    If Right(initializerBody, 1) = "," Then _
        initializerBody = RTrim(Left(initializerBody, Len(initializerBody) - 1))
    If Left(initializerBody, 1) = Chr(34) Then
        If storageType <> "Byte" AndAlso storageType <> "UByte" AndAlso _
           storageType <> "c2fb_c_char" AndAlso _
           storageType <> "c2fb_c_uchar" Then
            RecordSimpleBodyFailure( _
                context, "unsupported-array-compound-literal-string", _
                "string initializer does not match compound literal array type")
            Return 0
        End If
        Dim stringValues As String
        Dim stringByteCount As Long
        If ReadSimpleStaticStringArray( _
            initializerBody, stringValues, stringByteCount) = 0 OrElse _
           SplitSimpleCList(stringValues, values(), valueCount) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-array-compound-literal-string", _
                "unsupported compound literal string initializer")
            Return 0
        End If
        isStringInitializer = -1
    ElseIf initializerBody <> "" Then
        If SplitSimpleCList(initializerBody, values(), valueCount) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-array-compound-literal-initializer", _
                "unsupported compound literal array initializer")
            Return 0
        End If
    End If

    Dim arrayCount As Long
    If boundText = "" Then
        If inspectOnly <> 0 AndAlso isStringInitializer = 0 Then
            If InferSimpleArrayCompoundLiteralBound( _
                values(), valueCount, arrayCount, context) = 0 Then Return 0
        Else
            arrayCount = valueCount
        End If
    ElseIf EvaluateSimpleArrayBound( _
        boundText, arrayCount, MAX_SIMPLE_ARRAY_COUNT) = 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-array-compound-literal-bound", _
            "unsupported compound literal array bound " & boundText)
        Return 0
    End If
    If isStringInitializer <> 0 AndAlso _
       arrayCount > 0 AndAlso valueCount = arrayCount + 1 AndAlso _
       Trim(values(arrayCount)) = "0" Then valueCount -= 1
    If arrayCount < 1 OrElse arrayCount > MAX_SIMPLE_ARRAY_COUNT OrElse _
       valueCount > arrayCount Then
        RecordSimpleBodyFailure( _
            context, "array-compound-literal-limit-exceeded", _
            "compound literal array size must be between 1 and " & _
            Trim(Str(MAX_SIMPLE_ARRAY_COUNT)))
        Return 0
    End If
    arrayCountOutput = arrayCount
    If inspectOnly <> 0 Then
        pointerExpression = "CPtr(" & pointerType & ", 0)"
        Return -1
    End If
    For valueIndex As Long = 0 To valueCount - 1
        If Left(Trim(values(valueIndex)), 1) = "[" Then
            RecordSimpleBodyFailure( _
                context, "unsupported-array-compound-literal-designator", _
                "indexed designators are not supported in compound literal arrays")
            Return 0
        End If
    Next

    If context.temporarySerial >= MAX_EXPRESSION_TEMPORARIES Then
        RecordSimpleBodyFailure( _
            context, "compound-literal-workspace-limit", _
            "too many compound literal arrays in this C procedure")
        Return 0
    End If
    Dim arrayName As String = _
        "c2fb_compound_array_" & Trim(Str(context.temporarySerial))
    context.temporarySerial += 1
    context.declarations &= "    Dim " & arrayName & "(0 To " & _
        Trim(Str(arrayCount - 1)) & ") As " & storageType & Chr(10)
    If resolvedElementType.isConst <> 0 Then _
        context.readOnlyAliases &= "|" & arrayName & "|"

    Dim elementRecord As String
    If InStr(storageType, " Ptr") = 0 AndAlso _
       IsCBodyCallbackTypeInContext(translation, storageType) = 0 Then _
        elementRecord = CBodyRecordName(translation, catalog, storageType)
    If SimpleArithmeticKind(catalog, storageType) = 0 AndAlso _
       InStr(storageType, " Ptr") = 0 AndAlso _
       IsCBodyCallbackTypeInContext(translation, storageType) = 0 AndAlso _
       elementRecord = "" Then
        RecordSimpleBodyFailure( _
            context, "unsupported-array-compound-literal-element", _
            "unsupported compound literal array element type " & storageType)
        Return 0
    End If

    For valueIndex As Long = 0 To arrayCount - 1
        Dim elementValue As String
        If valueIndex < valueCount Then
            If elementRecord <> "" Then
                If TranslateSimpleRecordInitializer( _
                    translation, catalog, values(valueIndex), storageType, _
                    elementValue, context, callableNames(), callableCount, _
                    parameterNames(), parameterAliases(), parameterTypes(), _
                    symbolCount) = 0 Then Return 0
            Else
                Dim elementPrelude As String
                If TranslateSequencedExpression( _
                    translation, catalog, values(valueIndex), elementValue, _
                    elementPrelude, context, callableNames(), callableCount, _
                    parameterNames(), parameterAliases(), parameterTypes(), _
                    symbolCount, storageType) = 0 Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-array-compound-literal-element", _
                        "unsupported compound literal array element: " & _
                        Left(values(valueIndex), 120))
                    Return 0
                End If
                If elementPrelude <> "" Then
                    If outputPrelude <> "" AndAlso _
                       Right(outputPrelude, 1) <> Chr(10) Then _
                        outputPrelude &= Chr(10)
                    outputPrelude &= RTrim(elementPrelude, Any Chr(10))
                    If Right(outputPrelude, 1) <> Chr(10) Then _
                        outputPrelude &= Chr(10)
                End If
            End If
        ElseIf elementRecord <> "" Then
            If TranslateSimpleRecordInitializer( _
                translation, catalog, "{}", storageType, elementValue, _
                context, callableNames(), callableCount, parameterNames(), _
                parameterAliases(), parameterTypes(), symbolCount) = 0 Then _
                Return 0
        ElseIf InStr(storageType, " Ptr") <> 0 OrElse _
               IsCBodyCallbackTypeInContext(translation, storageType) <> 0 Then
            elementValue = "CPtr(" & storageType & ", 0)"
        Else
            elementValue = "Cast(" & storageType & ", 0)"
        End If
        If outputPrelude <> "" AndAlso _
           Right(outputPrelude, 1) <> Chr(10) Then _
            outputPrelude &= Chr(10)
        outputPrelude &= arrayName & "(" & Trim(Str(valueIndex)) & ") = " & _
                         elementValue & Chr(10)
    Next
    pointerExpression = "CPtr(" & pointerType & ", @" & arrayName & "(0))"
    Return -1
End Function

Private Function TranslateSimpleCompoundLiteralAssignment( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, ByRef outputText As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal symbolCount As Long _
) As Long
    outputText = ""
    Dim text As String = Trim(statementText)
    Dim assignmentPosition As Long = FindSimpleAssignmentEquals(text)
    If assignmentPosition = 0 Then Return 0
    Dim targetName As String = Trim(Left(text, assignmentPosition - 1))
    If IsIdentifierToken(targetName) = 0 Then Return 0
    Dim sourceType As String = ""
    Dim targetAlias As String = ""
    For symbolIndex As Long = 0 To symbolCount - 1
        If parameterNames(symbolIndex) = targetName Then
            sourceType = parameterTypes(symbolIndex)
            targetAlias = parameterAliases(symbolIndex)
            Exit For
        End If
    Next
    If sourceType = "" OrElse targetAlias = "" OrElse _
       InStr(sourceType, "[]") <> 0 Then Return 0

    Dim initializerText As String = Trim(Mid(text, assignmentPosition + 1))
    Dim position As Long = 1
    SkipCSpaces(initializerText, position)
    If position > Len(initializerText) OrElse _
       Asc(initializerText, position) <> Asc("(") Then Return 0
    Dim typeStart As Long = position + 1
    If ScanSimpleExpressionGroup( _
        initializerText, position, Asc("("), Asc(")")) = 0 Then Return 0
    Dim typeText As String = Trim(Mid( _
        initializerText, typeStart, position - typeStart - 1))
    SkipCSpaces(initializerText, position)
    If position > Len(initializerText) OrElse _
       Asc(initializerText, position) <> Asc("{") Then Return 0
    Dim aggregateStart As Long = position
    If ScanSimpleExpressionGroup( _
        initializerText, position, Asc("{"), Asc("}")) = 0 Then Return 0
    SkipCSpaces(initializerText, position)
    If position <= Len(initializerText) Then Return 0

    If InStr(typeText, "[") <> 0 Then
        If InStr(sourceType, " Ptr") = 0 Then
            RecordSimpleBodyFailure( _
                context, "incompatible-array-compound-literal-assignment", _
                "array compound literal requires a compatible pointer destination " & _
                targetName)
            Return 0
        End If
        If InStr(context.readOnlyAliases, "|" & targetAlias & "|") <> 0 Then
            RecordSimpleBodyFailure( _
                context, "compound-literal-assignment-to-const", _
                "cannot assign a compound literal to const C object " & targetName)
            Return 0
        End If
        Dim pointerType As String
        Dim pointerExpression As String
        Dim arrayPrelude As String
        Dim arrayCount As Long
        If LowerSimpleCompoundLiteralArrayValue( _
            translation, catalog, typeText, _
            Mid(initializerText, aggregateStart, position - aggregateStart), _
            targetName, sourceType, pointerType, pointerExpression, _
            arrayPrelude, context, callableNames(), callableCount, _
            parameterNames(), parameterAliases(), parameterTypes(), _
            symbolCount, arrayCount) = 0 Then Return 0
        outputText = arrayPrelude & targetAlias & " = " & pointerExpression
        Return -1
    End If

    If InStr(sourceType, " Ptr") <> 0 OrElse _
       CBodyRecordName(translation, catalog, sourceType) = "" Then Return 0
    If InStr(context.readOnlyAliases, "|" & targetAlias & "|") <> 0 Then
        RecordSimpleBodyFailure( _
            context, "compound-literal-assignment-to-const", _
            "cannot assign a compound literal to const C object " & targetName)
        Return 0
    End If

    Dim typeTokens() As String
    Dim typeTokenCount As Long
    TokenizeCDeclaration(typeText, typeTokens(), typeTokenCount)
    If typeTokenCount <= 0 Then Return 0
    Dim resolvedType As C2fbResolvedTypeModel
    ResolveC2fbTypeModel(translation, catalog, typeTokens(), 0, _
                         typeTokenCount - 1, resolvedType, _
                         C2FB_POINTER_MAPPING_PRESERVE, _
                         C2FB_TYPE_MAPPING_BODY, _
                         C2FB_TYPE_RECOGNITION_KNOWN_TYPES_ONLY)
    Dim targetRecord As String = CBodyRecordName( _
        translation, catalog, sourceType)
    Dim literalRecord As String
    If resolvedType.status = C2FB_TYPE_RESOLUTION_RESOLVED Then _
        literalRecord = CBodyRecordName( _
            translation, catalog, resolvedType.freeBasicType)
    If resolvedType.status <> C2FB_TYPE_RESOLUTION_RESOLVED OrElse _
       resolvedType.pointerCount <> 0 OrElse _
       literalRecord = "" OrElse literalRecord <> targetRecord Then
        RecordSimpleBodyFailure( _
            context, "incompatible-compound-literal-assignment", _
            "compound literal type does not match destination " & targetName)
        Return 0
    End If

    Dim translatedInitializer As String
    Dim aggregateText As String = Mid( _
        initializerText, aggregateStart, position - aggregateStart)
    If TranslateSimpleRecordInitializer( _
        translation, catalog, aggregateText, sourceType, translatedInitializer, _
        context, callableNames(), callableCount, parameterNames(), _
        parameterAliases(), parameterTypes(), symbolCount) = 0 Then Return 0
    outputText = targetAlias & " = " & translatedInitializer
    Return -1
End Function

Private Function ParseSimpleAddressedCompoundLiteralValue( _
    ByRef parser As SimpleExpressionParser, _
    ByRef value As SimpleExpressionValue, _
    ByVal depth As Long, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal parameterCount As Long _
) As Long
    If depth > MAX_EXPRESSION_DEPTH OrElse _
       parser.position > Len(parser.sourceText) OrElse _
       Asc(parser.sourceText, parser.position) <> Asc("&") Then Return 0

    Dim literalPosition As Long = parser.position + 1
    SkipCSpaces(parser.sourceText, literalPosition)
    If literalPosition > Len(parser.sourceText) OrElse _
       Asc(parser.sourceText, literalPosition) <> Asc("(") Then Return 0

    Dim typeStart As Long = literalPosition + 1
    If ScanSimpleExpressionGroup( _
        parser.sourceText, literalPosition, Asc("("), Asc(")")) = 0 Then Return 0
    Dim typeText As String = Trim(Mid( _
        parser.sourceText, typeStart, literalPosition - typeStart - 1))
    SkipCSpaces(parser.sourceText, literalPosition)
    If literalPosition > Len(parser.sourceText) OrElse _
       Asc(parser.sourceText, literalPosition) <> Asc("{") Then Return 0

    Dim aggregateStart As Long = literalPosition
    If ScanSimpleExpressionGroup( _
        parser.sourceText, literalPosition, Asc("{"), Asc("}")) = 0 Then Return 0
    Dim aggregateText As String = Mid( _
        parser.sourceText, aggregateStart, literalPosition - aggregateStart)
    parser.position = literalPosition

    If InStr(typeText, "[") <> 0 Then
        If parser.context <> 0 Then _
            RecordSimpleBodyFailure(parser.context[0], _
                "unsupported-addressed-array-compound-literal", _
                "address-of array compound literals are unsupported")
        Return 1
    End If

    Dim typeTokens() As String
    Dim typeTokenCount As Long
    TokenizeCDeclaration(typeText, typeTokens(), typeTokenCount)
    If typeTokenCount <= 0 Then Return 1
    Dim resolvedType As C2fbResolvedTypeModel
    ResolveC2fbTypeModel( _
        parser.translation[0], parser.catalog[0], typeTokens(), 0, _
        typeTokenCount - 1, resolvedType, C2FB_POINTER_MAPPING_PRESERVE, _
        C2FB_TYPE_MAPPING_BODY, C2FB_TYPE_RECOGNITION_KNOWN_TYPES_ONLY)
    If resolvedType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then
        If parser.context <> 0 Then _
            RecordSimpleBodyFailure(parser.context[0], _
                "unsupported-addressed-compound-literal-type", _
                "cannot resolve addressed compound literal type " & typeText)
        Return 1
    End If

    Dim freeBasicType As String = resolvedType.freeBasicType
    If resolvedType.isConst <> 0 AndAlso Left(freeBasicType, 6) = "Const " Then _
        freeBasicType = Mid(freeBasicType, 7)
    Dim recordName As String = CBodyRecordName( _
        parser.translation[0], parser.catalog[0], freeBasicType)
    Dim isRecord As Long = CInt(recordName <> "")
    Dim isPointer As Long = CInt(InStr(freeBasicType, " Ptr") <> 0)
    Dim isCallback As Long = CInt( _
        IsCBodyCallbackTypeInContext(parser.translation[0], freeBasicType) <> 0)
    If isRecord = 0 AndAlso isPointer = 0 AndAlso isCallback = 0 AndAlso _
       SimpleArithmeticKind(parser.catalog[0], freeBasicType) = 0 Then
        If parser.context <> 0 Then _
            RecordSimpleBodyFailure(parser.context[0], _
                "unsupported-addressed-compound-literal-type", _
                "addressed compound literal type has no supported storage: " & typeText)
        Return 1
    End If

    Dim pointerType As String = freeBasicType & " Ptr"
    If resolvedType.isConst <> 0 Then pointerType = "Const " & pointerType
    If parser.unevaluated <> 0 Then
        value.text = "CPtr(" & pointerType & ", 0)"
        value.typeName = pointerType
        value.layoutType = pointerType
        value.storageType = pointerType
        value.cRecord = recordName
        value.isLValue = 0
        value.isAddressAccessor = 0
        value.isReadOnly = resolvedType.isConst
        Return -1
    End If
    If parser.context = 0 Then Return 1

    Dim temporaryName As String = NewSimpleBodyTemporary( _
        parser.context[0], freeBasicType, "")
    If temporaryName = "" Then
        RecordSimpleBodyFailure(parser.context[0], _
            "compound-literal-workspace-limit", _
            "too many compound literal objects in this C procedure")
        Return 1
    End If

    Dim initializedValue As String
    Dim initializerPrelude As String
    If isRecord <> 0 Then
        If TranslateSimpleRecordInitializer( _
            parser.translation[0], parser.catalog[0], aggregateText, _
            freeBasicType, initializedValue, parser.context[0], _
            callableNames(), callableCount, parameterNames(), _
            parameterAliases(), parameterTypes(), parameterCount) = 0 Then Return 1
    Else
        Dim initializerBody As String = Trim(Mid( _
            aggregateText, 2, Len(aggregateText) - 2))
        If Right(initializerBody, 1) = "," Then _
            initializerBody = RTrim(Left(initializerBody, Len(initializerBody) - 1))
        If initializerBody = "" Then
            If isPointer <> 0 OrElse isCallback <> 0 Then
                initializedValue = "CPtr(" & freeBasicType & ", 0)"
            Else
                initializedValue = "Cast(" & freeBasicType & ", 0)"
            End If
        Else
            Dim initializerValues() As String
            Dim initializerCount As Long = 0
            If SplitSimpleCList( _
                initializerBody, initializerValues(), initializerCount) = 0 OrElse _
               initializerCount <> 1 Then
                RecordSimpleBodyFailure(parser.context[0], _
                    "unsupported-scalar-compound-literal-initializer", _
                    "scalar compound literal requires one initializer value")
                Return 1
            End If
            If TranslateSequencedExpression( _
                parser.translation[0], parser.catalog[0], _
                initializerValues(0), initializedValue, initializerPrelude, _
                parser.context[0], callableNames(), callableCount, _
                parameterNames(), parameterAliases(), parameterTypes(), _
                parameterCount, freeBasicType) = 0 Then Return 1
        End If
    End If

    value.text = "CPtr(" & pointerType & ", @" & temporaryName & ")"
    value.typeName = pointerType
    value.layoutType = pointerType
    value.storageType = pointerType
    value.cRecord = recordName
    value.isLValue = 0
    value.isAddressAccessor = 0
    value.isReadOnly = resolvedType.isConst
    value.prelude = initializerPrelude
    If value.prelude <> "" AndAlso Right(value.prelude, 1) <> Chr(10) Then _
        value.prelude &= Chr(10)
    value.prelude &= temporaryName & " = " & initializedValue & Chr(10)
    parser.hasRuntimeValue = -1
    Return -1
End Function

Private Function ParseSimpleArrayCompoundLiteralValue( _
    ByRef parser As SimpleExpressionParser, _
    ByRef value As SimpleExpressionValue, _
    ByVal depth As Long, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal parameterCount As Long _
) As Long
    If depth > MAX_EXPRESSION_DEPTH OrElse _
       parser.position > Len(parser.sourceText) OrElse _
       Asc(parser.sourceText, parser.position) <> Asc("(") Then Return 0

    Dim literalPosition As Long = parser.position
    Dim typeStart As Long = literalPosition + 1
    If ScanSimpleExpressionGroup( _
        parser.sourceText, literalPosition, Asc("("), Asc(")")) = 0 Then Return 0
    Dim typeText As String = Trim(Mid( _
        parser.sourceText, typeStart, literalPosition - typeStart - 1))
    If InStr(typeText, "[") = 0 Then Return 0
    SkipCSpaces(parser.sourceText, literalPosition)
    If literalPosition > Len(parser.sourceText) OrElse _
       Asc(parser.sourceText, literalPosition) <> Asc("{") Then Return 0

    Dim aggregateStart As Long = literalPosition
    If ScanSimpleExpressionGroup( _
        parser.sourceText, literalPosition, Asc("{"), Asc("}")) = 0 Then Return 0
    Dim aggregateText As String = Mid( _
        parser.sourceText, aggregateStart, literalPosition - aggregateStart)
    parser.position = literalPosition
    If parser.unevaluated <> 0 Then
        If parser.context = 0 Then Return 1
        Dim pointerType As String
        Dim pointerExpression As String
        Dim outputPrelude As String
        Dim arrayCount As Long
        Dim unusedSymbolCount As Long = 0
        If LowerSimpleCompoundLiteralArrayValue( _
            parser.translation[0], parser.catalog[0], typeText, aggregateText, _
            "array compound literal sizeof operand", "", pointerType, _
            pointerExpression, outputPrelude, parser.context[0], _
            callableNames(), callableCount, parameterNames(), _
            parameterAliases(), parameterTypes(), unusedSymbolCount, _
            arrayCount, -1) = 0 Then Return 1

        value.text = pointerExpression
        value.typeName = pointerType
        value.layoutType = value.typeName
        value.storageType = value.typeName
        value.arrayCount = arrayCount
        value.arrayBound = Trim(Str(arrayCount)) & "ULL"
        value.arrayElementType = SimplePointeeType(pointerType)
        value.isLValue = -1
        value.isAddressAccessor = 0
        value.isReadOnly = CInt(Left(pointerType, 6) = "Const ")
        Return -1
    End If
    If parser.context = 0 Then Return 1

    Dim pointerType As String
    Dim pointerExpression As String
    Dim outputPrelude As String
    Dim arrayCount As Long
    If LowerSimpleCompoundLiteralArrayValue( _
        parser.translation[0], parser.catalog[0], typeText, aggregateText, _
        "array compound literal expression", "", pointerType, _
        pointerExpression, outputPrelude, parser.context[0], _
        callableNames(), callableCount, parameterNames(), _
        parameterAliases(), parameterTypes(), parameterCount, arrayCount) = 0 Then _
        Return 1

    value.text = pointerExpression
    value.typeName = pointerType
    value.layoutType = pointerType
    value.storageType = pointerType
    value.cRecord = CBodyRecordName( _
        parser.translation[0], parser.catalog[0], pointerType)
    value.isLValue = 0
    value.isAddressAccessor = 0
    value.isReadOnly = CInt(Left(pointerType, 6) = "Const ")
    value.prelude = outputPrelude
    parser.hasRuntimeValue = -1
    Return -1
End Function

Private Function TranslateSimpleAssignmentStatement( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, ByRef outputText As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal symbolCount As Long _
) As Long
    outputText = ""
    Dim text As String = Trim(statementText)
    If text = "" Then
        outputText = "' empty C statement"
        Return -1
    End If
    Dim compactText As String = ""
    For textIndex As Long = 1 To Len(text)
        If IsSpaceByte(Asc(text, textIndex)) = 0 Then _
            compactText &= Mid(text, textIndex, 1)
    Next
    If context.functionName = "openDatabase" AndAlso _
       compactText = "db->aDb=db->aDbStatic" Then
        ' The port keeps Db slots inline in sqlite3.aDb. The C assignment
        ' selects its static backing array, which is already the only storage
        ' in this connection layout.
        outputText = "' C database slots are already stored inline"
        Return -1
    End If
    If TranslateSimpleCompoundLiteralAssignment( _
        translation, catalog, text, outputText, context, callableNames(), _
        callableCount, parameterNames(), parameterAliases(), parameterTypes(), _
        symbolCount) <> 0 Then Return -1
    Dim parser As SimpleExpressionParser
    parser.sourceText = text
    parser.position = 1
    parser.translation = @translation
    parser.catalog = @catalog
    parser.context = @context
    Dim value As SimpleExpressionValue
    If ParseSimpleExpressionValue(parser, value, 1, 0, callableNames(), callableCount, _
                                  parameterNames(), parameterAliases(), parameterTypes(), symbolCount) = 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-assignment-expression", _
            "assignment parser stopped at byte " & Trim(Str(parser.position)) & _
            " (" & Left(text, 120) & "): " & _
            Left(Mid(parser.sourceText, parser.position), 80))
        Return 0
    End If
    SkipCSpaces(parser.sourceText, parser.position)
    If parser.position <= Len(text) Then
        RecordSimpleBodyFailure( _
            context, "unsupported-assignment-suffix", _
            "assignment parser left an unsupported suffix at byte " & _
            Trim(Str(parser.position)) & " (" & Left(text, 120) & "): " & _
            Left(Mid(parser.sourceText, parser.position), 80))
        Return 0
    End If
    If DiscardSimpleValue(parser, value) = 0 Then
        RecordSimpleBodyFailure( _
            context, "assignment-value-not-preserved", _
            "could not preserve assignment value: " & Left(statementText, 120))
        Return 0
    End If
    outputText = RTrim(value.prelude, Any Chr(10))
    If outputText = "" Then outputText = "' discarded C expression"
    Return -1
End Function

Private Function SimpleCConditionAlwaysTrue( _
    ByVal conditionText As String _
) As Long
    Dim conditionValue As Long
    If EvaluateSimpleArrayBound( _
        conditionText, conditionValue, MAX_GENERIC_ARRAY_COUNT, -1) = 0 Then _
        Return 0
    Return CInt(conditionValue <> 0)
End Function

Private Function ParseSimpleCBlock( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef outputText As String, _
    ByVal indentText As String, _
    ByVal returnType As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByRef symbolCount As Long, _
    locals() As SimpleLocalVariable, _
    ByRef localCount As Long, _
    ByRef terminatesPaths As Long _
) As Long
    outputText = ""
    terminatesPaths = 0
    SkipCSpaces(sourceText, position)
    If position > Len(sourceText) OrElse Asc(sourceText, position) <> Asc("{") Then Return 0
    position += 1
    Dim statementCount As Long = 0
    If PushSimpleBodyScope(context, symbolCount, localCount) = 0 Then Return 0

    While position <= Len(sourceText)
        SkipCSpaces(sourceText, position)
        If position > Len(sourceText) Then Return 0
        If Asc(sourceText, position) = Asc("}") Then
            position += 1
            If statementCount = 0 Then outputText = indentText & "' empty C block"
            PopSimpleBodyScope(context, symbolCount, localCount)
            Return -1
        End If

        Dim statementText As String = ""
        Dim statementTerminates As Long = 0
        If ParseSimpleCStatement(translation, catalog, sourceText, position, statementText, indentText, returnType, context, _
                                 callableNames(), callableCount, parameterNames(), _
                                 parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                 statementTerminates) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-block-statement", _
                "unsupported block statement: " & Left(Mid(sourceText, position), 240))
            Return 0
        End If
        outputText &= IIf(statementCount = 0, "", Chr(10)) & statementText
        statementCount += 1
        terminatesPaths = statementTerminates
    Wend
    Return 0
End Function

Private Function ParseSimpleFallthroughAnnotation( _
    ByRef sourceText As String, ByRef position As Long, _
    ByRef outputText As String, ByVal indentText As String _
) As Long
    Dim parsePosition As Long = position
    SkipCSpaces(sourceText, parsePosition)
    If Mid(sourceText, parsePosition, 13) <> "__attribute__" Then Return 0
    If parsePosition + 13 <= Len(sourceText) AndAlso _
       IsIdentifierPart(Asc(sourceText, parsePosition + 13)) <> 0 Then Return 0

    parsePosition += 13
    SkipCSpaces(sourceText, parsePosition)
    If parsePosition > Len(sourceText) OrElse _
       Asc(sourceText, parsePosition) <> Asc("(") Then Return 0
    Dim attributeStart As Long = parsePosition + 1
    If ScanSimpleExpressionGroup(sourceText, parsePosition, Asc("("), Asc(")")) = 0 Then Return 0

    Dim attributeText As String
    For i As Long = attributeStart To parsePosition - 2
        If IsSpaceByte(Asc(sourceText, i)) = 0 Then attributeText &= LCase(Mid(sourceText, i, 1))
    Next
    If attributeText <> "(fallthrough)" AndAlso _
       attributeText <> "(__fallthrough__)" Then Return 0

    SkipCSpaces(sourceText, parsePosition)
    If parsePosition > Len(sourceText) OrElse _
       Asc(sourceText, parsePosition) <> Asc(";") Then Return 0
    parsePosition += 1
    position = parsePosition
    outputText = indentText & "' C switch fallthrough annotation"
    Return -1
End Function

Private Function IsUnusedLocalRecordDefinition( _
    ByVal statementText As String, ByVal remainingSource As String _
) As Long
    statementText = Trim(statementText)
    If Right(statementText, 1) = ";" Then _
        statementText = RTrim(Left(statementText, Len(statementText) - 1))
    Dim openingBrace As Long = InStr(statementText, "{")
    If openingBrace = 0 OrElse _
       Trim(Mid(statementText, openingBrace)) = "" Then Return 0

    Dim prefix As String = Trim(Left(statementText, openingBrace - 1))
    Dim recordTag As String
    Dim isTypedef As Long
    Dim isPacked As Long
    Dim recordKind As C2fbRecordKind = C2fbRecordKindFromPrefix( _
        prefix, recordTag, isTypedef, isPacked)
    If recordKind <> C2FB_RECORD_STRUCT AndAlso _
       recordKind <> C2FB_RECORD_UNION Then Return 0

    Dim prefixTokens() As String
    Dim prefixTokenCount As Long
    TokenizeCDeclaration(prefix, prefixTokens(), prefixTokenCount)
    Dim typeTokenIndex As Long = -1
    For tokenIndex As Long = 0 To prefixTokenCount - 1
        If prefixTokens(tokenIndex) = "struct" OrElse _
           prefixTokens(tokenIndex) = "union" Then
            typeTokenIndex = tokenIndex
            Exit For
        End If
    Next
    If typeTokenIndex < 0 Then Return 0
    For tokenIndex As Long = 0 To typeTokenIndex - 1
        Select Case prefixTokens(tokenIndex)
        Case "const", "volatile", "restrict", "__restrict", _
             "__restrict__", "static", "extern", "register", "inline", _
             "__inline", "__inline__"
            Continue For
        Case Else
            Return 0
        End Select
    Next
    If recordTag <> "" Then
        If typeTokenIndex + 2 <> prefixTokenCount OrElse _
           prefixTokens(typeTokenIndex + 1) <> recordTag Then Return 0
    ElseIf typeTokenIndex + 1 <> prefixTokenCount Then
        Return 0
    End If

    Dim closingPosition As Long = openingBrace
    If ScanSimpleExpressionGroup( _
        statementText, closingPosition, Asc("{"), Asc("}")) = 0 OrElse _
       Trim(Mid(statementText, closingPosition)) <> "" Then Return 0
    If recordTag <> "" AndAlso _
       GenericLastIdentifierPosition(remainingSource, recordTag) <> 0 Then Return 0
    Return -1
End Function

Private Function TranslateSimpleLocalRecordTypeDefinition( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, _
    ByRef outputText As String, _
    ByRef context As SimpleBodyContext _
) As Long
    statementText = Trim(statementText)
    If Right(statementText, 1) = ";" Then _
        statementText = RTrim(Left(statementText, Len(statementText) - 1))
    Dim openingBrace As Long = InStr(statementText, "{")
    If openingBrace = 0 Then Return 0
    Dim prefix As String = Trim(Left(statementText, openingBrace - 1))
    Dim recordTag As String
    Dim isTypedef As Long
    Dim isPacked As Long
    If C2fbRecordKindFromPrefix( _
        prefix, recordTag, isTypedef, isPacked) <> C2FB_RECORD_STRUCT OrElse _
       recordTag = "" OrElse isTypedef <> 0 Then Return 0

    Dim prefixTokens() As String
    Dim prefixTokenCount As Long
    TokenizeCDeclaration(prefix, prefixTokens(), prefixTokenCount)
    Dim typeTokenIndex As Long = -1
    For tokenIndex As Long = 0 To prefixTokenCount - 1
        If prefixTokens(tokenIndex) = "struct" Then
            typeTokenIndex = tokenIndex
            Exit For
        End If
    Next
    If typeTokenIndex < 0 OrElse typeTokenIndex + 2 <> prefixTokenCount OrElse _
       prefixTokens(typeTokenIndex + 1) <> recordTag Then Return 0
    For tokenIndex As Long = 0 To typeTokenIndex - 1
        Select Case prefixTokens(tokenIndex)
        Case "const", "volatile", "restrict", "__restrict", _
             "__restrict__", "static", "extern", "register", "inline", _
             "__inline", "__inline__"
            Continue For
        Case Else
            Return 0
        End Select
    Next

    Dim recordEnd As Long = openingBrace
    If ScanSimpleExpressionGroup( _
        statementText, recordEnd, Asc("{"), Asc("}")) = 0 OrElse _
       Trim(Mid(statementText, recordEnd)) <> "" Then Return 0
    Dim recordBody As String = Mid( _
        statementText, openingBrace + 1, recordEnd - openingBrace - 2)
    Dim generatedRecordName As String
    If RegisterCBodyAnonymousLocalRecord( _
        translation, catalog, recordBody, generatedRecordName) = 0 Then Return 0
    If RegisterSimpleBodyLocalRecordTag( _
        context, recordTag, generatedRecordName) = 0 Then
        RecordSimpleBodyFailure( _
            context, "duplicate-local-record-tag", _
            "duplicate block-local C record tag: " & recordTag)
        Return 0
    End If
    outputText = "' C block-local record type declaration"
    Return -1
End Function

Private Function ParseSimpleCStatement( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef sourceText As String, _
    ByRef position As Long, _
    ByRef outputText As String, _
    ByVal indentText As String, _
    ByVal returnType As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByRef symbolCount As Long, _
    locals() As SimpleLocalVariable, _
    ByRef localCount As Long, _
    ByRef terminatesPaths As Long, _
    ByVal allowBodyReplacements As Long = -1 _
) As Long
    outputText = ""
    terminatesPaths = 0
    context.branchJump = 0
    ResetSimpleBodyFailure(context)
    SkipCSpaces(sourceText, position)
    If position > Len(sourceText) Then Return 0

    Dim wordPosition As Long = position
    Dim wordText As String
    While wordPosition <= Len(sourceText) AndAlso IsIdentifierPart(Asc(sourceText, wordPosition)) <> 0
        wordText &= Mid(sourceText, wordPosition, 1)
        wordPosition += 1
    Wend
    Dim following As Long = wordPosition
    SkipCSpaces(sourceText, following)
    If wordText <> "" AndAlso Mid(sourceText, following, 1) = ":" Then
        Dim labelName As String = SimpleSourceLabel(context, wordText, -1)
        If labelName = "" Then Return 0
        outputText = indentText & labelName & ":"
        position = following + 1
        Return -1
    End If

    If translation.target.profile <> C2FB_TARGET_SQLITE AndAlso allowBodyReplacements <> 0 AndAlso _
       position <= Len(sourceText) AndAlso Asc(sourceText, position) <> Asc("{") Then
        Select Case wordText
        Case "if", "else", "for", "while", "do", "switch", "case", "default", _
             "break", "continue", "goto"
            ' The expression matcher does not replace control-flow constructs.
        Case Else
            Dim statementEnd As Long = position
            Dim cStatement As String
            If ReadSimpleCStatement(sourceText, statementEnd, cStatement) <> 0 Then
                Dim replacementText As String
                Dim toFreeBasic As Long
                If C2fbReplacementRuleMatch( _
                    translation.genericC.policy, Trim(cStatement) & ";", _
                    replacementText, toFreeBasic) <> 0 Then
                    If toFreeBasic <> 0 Then
                        Dim sourceIsReturn As Long = CInt(wordText = "return")
                        If sourceIsReturn = 0 AndAlso _
                           IsTypeOrQualifierTokenInContext(translation, wordText) <> 0 Then Return 0
                        Dim isMultiLineReplacement As Long = _
                            C2fbReplacementRuleHasMultipleLines( _
                                translation.genericC.policy, Trim(cStatement) & ";")
                        Dim replacementReturnExpression As String
                        Dim replacementEndsInReturn As Long = 0
                        If isMultiLineReplacement <> 0 Then
                            replacementEndsInReturn = ReadGenericFreeBasicBlockReturn( _
                                replacementText, replacementReturnExpression)
                        End If
                        If (sourceIsReturn <> 0 AndAlso isMultiLineReplacement <> 0 AndAlso _
                            replacementEndsInReturn = 0) OrElse _
                           (isMultiLineReplacement = 0 AndAlso _
                            IsSimpleFreeBasicBodyReplacement( _
                                replacementText, sourceIsReturn) = 0) Then
                            RecordSimpleBodyFailure( _
                                context, "statement-replacement", _
                                "invalid FreeBASIC statement replacement: " & _
                                Left(cStatement, 160))
                            Return 0
                        End If
                        If sourceIsReturn <> 0 AndAlso isMultiLineReplacement = 0 Then
                            Dim sourceReturnExpression As String = _
                                Trim(Mid(cStatement, Len("return") + 1))
                            replacementReturnExpression = _
                                Trim(Mid(LTrim(replacementText), Len("return") + 1))
                            If (sourceReturnExpression = "") <> _
                               (replacementReturnExpression = "") Then Return 0
                        End If
                        If replacementEndsInReturn <> 0 OrElse sourceIsReturn <> 0 Then
                            If returnType = "void" AndAlso _
                               replacementReturnExpression <> "" Then Return 0
                            If returnType <> "void" AndAlso _
                               replacementReturnExpression = "" Then Return 0
                        End If
                        Dim rewrittenReplacement As String = RewriteGenericBodyLocalNames( _
                            replacementText, parameterNames(), parameterAliases(), symbolCount)
                        If isMultiLineReplacement <> 0 Then
                            outputText = IndentC2fbReplacement( _
                                rewrittenReplacement, indentText)
                        Else
                            outputText = indentText & rewrittenReplacement
                        End If
                        position = statementEnd
                        If sourceIsReturn <> 0 OrElse replacementEndsInReturn <> 0 Then _
                            terminatesPaths = -1
                        Return -1
                    End If

                    If Right(Trim(replacementText), 1) <> ";" Then _
                        replacementText = Trim(replacementText) & ";"
                    sourceText = Left(sourceText, position - 1) & _
                                 replacementText & Mid(sourceText, statementEnd)
                    Return ParseSimpleCStatement(translation, catalog,  _
                        sourceText, position, outputText, indentText, returnType, _
                        context, callableNames(), callableCount, parameterNames(), _
                        parameterAliases(), parameterTypes(), symbolCount, locals(), _
                        localCount, terminatesPaths, 0)
                End If
            End If
        End Select
    End If

    If wordText = "break" OrElse wordText = "continue" OrElse wordText = "goto" Then
        Dim jumpText As String
        If ReadSimpleCStatement(sourceText, position, jumpText) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-jump-statement", _
                "unsupported jump statement: " & Left(Mid(sourceText, position), 200))
            Return 0
        End If
        Dim target As String
        If wordText = "break" AndAlso jumpText = "break" Then
            target = SimpleBreakTarget(context)
        ElseIf wordText = "continue" AndAlso jumpText = "continue" Then
            target = SimpleContinueTarget(context)
        ElseIf wordText = "goto" Then
            Dim sourceLabel As String = Trim(Mid(jumpText, 5))
            If IsIdentifierToken(sourceLabel) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-goto-label", _
                    "unsupported goto label: " & Left(jumpText, 200))
                Return 0
            End If
            target = SimpleSourceLabel(context, sourceLabel, 0)
        Else
            Return 0
        End If
        If target = "" Then
            RecordSimpleBodyFailure( _
                context, "jump-target-not-created", _
                "could not create jump target: " & Left(jumpText, 200))
            Return 0
        End If
        outputText = indentText & "Goto " & target
        context.branchJump = -1
        Return -1
    End If
    If ParseSimpleFallthroughAnnotation(sourceText, position, outputText, indentText) <> 0 Then Return -1
    If Asc(sourceText, position) = Asc("{") Then
        Return ParseSimpleCBlock(translation, catalog, sourceText, position, outputText, indentText, returnType, context, _
                                 callableNames(), callableCount, parameterNames(), _
                                 parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                 terminatesPaths)
    End If

    Dim switchPosition As Long = position
    If ParseSimpleSwitchStatement(translation, catalog, sourceText, switchPosition, outputText, indentText, _
                                  returnType, context, callableNames(), callableCount, _
                                  parameterNames(), parameterAliases(), parameterTypes(), symbolCount, _
                                  locals(), localCount, terminatesPaths) <> 0 Then
        position = switchPosition
        Return -1
    End If

    Dim conditionText As String = ""
    Dim conditionPosition As Long = position
    If ReadSimpleIfCondition(sourceText, conditionPosition, conditionText) <> 0 Then
        Dim translatedCondition As String
        Dim conditionPrelude As String
        If TranslateSequencedExpression(translation, catalog, _
                                        conditionText, translatedCondition, conditionPrelude, context, _
                                        callableNames(), callableCount, parameterNames(), _
                                        parameterAliases(), parameterTypes(), symbolCount) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-if-condition", _
                "unsupported if condition: " & Left(conditionText, 240))
            Return 0
        End If
        position = conditionPosition
        Dim trueBody As String
        Dim trueTerminates As Long = 0
        If ParseSimpleCStatement(translation, catalog, sourceText, position, trueBody, indentText & "    ", _
                                 returnType, context, callableNames(), callableCount, parameterNames(), _
                                 parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                 trueTerminates) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-if-body", _
                "unsupported if body: " & Left(Mid(sourceText, position), 240))
            Return 0
        End If
        Dim trueJump As Long = context.branchJump

        SkipCSpaces(sourceText, position)
        Dim hasElse As Long = 0
        If Mid(sourceText, position, 4) = "else" AndAlso _
           (position + 4 > Len(sourceText) OrElse _
            IsIdentifierPart(Asc(sourceText, position + 4)) = 0) Then
            position += 4
            hasElse = -1
        End If
        outputText = ""
        If conditionPrelude <> "" Then outputText = IndentSimpleBodyText(RTrim(conditionPrelude, Any Chr(10)), indentText) & Chr(10)
        outputText &= indentText & "If " & translatedCondition & " Then" & Chr(10) & trueBody
        context.branchJump = 0
        If hasElse <> 0 Then
            Dim falseBody As String
            Dim falseTerminates As Long = 0
            If ParseSimpleCStatement(translation, catalog, sourceText, position, falseBody, indentText & "    ", _
                                     returnType, context, callableNames(), callableCount, parameterNames(), _
                                     parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                     falseTerminates) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-else-body", _
                    "unsupported else body: " & Left(Mid(sourceText, position), 240))
                Return 0
            End If
            outputText &= Chr(10) & indentText & "Else" & Chr(10) & falseBody
            terminatesPaths = CInt(trueTerminates <> 0 AndAlso falseTerminates <> 0)
            context.branchJump = CInt(trueJump <> 0 AndAlso context.branchJump <> 0)
        End If
        outputText &= Chr(10) & indentText & "End If"
        Return -1
    End If

    Dim doPosition As Long = position
    If Mid(sourceText, doPosition, 2) = "do" AndAlso _
       (doPosition + 2 > Len(sourceText) OrElse _
        IsIdentifierPart(Asc(sourceText, doPosition + 2)) = 0) Then
        doPosition += 2
        SkipCSpaces(sourceText, doPosition)
        Dim breakLabel As String = NewSimpleFlowLabel(context)
        Dim continueLabel As String = NewSimpleFlowLabel(context)
        If PushSimpleControlFrame(context, breakLabel, continueLabel) = 0 Then Return 0
        Dim loopBody As String
        Dim loopTerminates As Long = 0
        If ParseSimpleCStatement(translation, catalog, sourceText, doPosition, loopBody, _
                                  indentText & "    ", returnType, context, _
                                  callableNames(), callableCount, parameterNames(), _
                                  parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                  loopTerminates) = 0 Then Return 0
        Dim hasBreak As Long = context.frames(context.frameCount - 1).hasBreak
        context.frameCount -= 1
        context.branchJump = 0

        SkipCSpaces(sourceText, doPosition)
        If Mid(sourceText, doPosition, 5) <> "while" OrElse _
           (doPosition + 5 <= Len(sourceText) AndAlso _
            IsIdentifierPart(Asc(sourceText, doPosition + 5)) <> 0) Then Return 0
        doPosition += 5
        SkipCSpaces(sourceText, doPosition)
        If doPosition > Len(sourceText) OrElse _
           Asc(sourceText, doPosition) <> Asc("(") Then Return 0

        Dim conditionStart As Long = doPosition + 1
        If ScanSimpleExpressionGroup(sourceText, doPosition, Asc("("), Asc(")")) = 0 Then Return 0
        conditionText = Trim(Mid(sourceText, conditionStart, _
                                  doPosition - conditionStart - 1))
        If Len(conditionText) = 0 Then Return 0
        Dim translatedCondition As String
        Dim conditionPrelude As String
        If TranslateSequencedExpression(translation, catalog, _
                                        conditionText, translatedCondition, conditionPrelude, context, _
                                        callableNames(), callableCount, parameterNames(), _
                                        parameterAliases(), parameterTypes(), symbolCount) = 0 Then Return 0
        SkipCSpaces(sourceText, doPosition)
        If doPosition > Len(sourceText) OrElse _
           Asc(sourceText, doPosition) <> Asc(";") Then Return 0
        doPosition += 1
        position = doPosition
        outputText = indentText & "Do" & Chr(10) & loopBody & Chr(10) & _
                     indentText & continueLabel & ":" & Chr(10) & _
                     IIf(conditionPrelude <> "", IndentSimpleBodyText(RTrim(conditionPrelude, Any Chr(10)), indentText & "    ") & Chr(10), "") & _
                     indentText & "Loop While " & translatedCondition & Chr(10) & _
                     indentText & breakLabel & ":"
        terminatesPaths = CInt(loopTerminates <> 0 OrElse _
            (hasBreak = 0 AndAlso conditionPrelude = "" AndAlso _
             SimpleCConditionAlwaysTrue(conditionText) <> 0))
        Return -1
    End If

    Dim forPosition As Long = position
    If Mid(sourceText, forPosition, 3) = "for" AndAlso _
       (forPosition + 3 > Len(sourceText) OrElse _
        IsIdentifierPart(Asc(sourceText, forPosition + 3)) = 0) Then
        forPosition += 3
        SkipCSpaces(sourceText, forPosition)
        If forPosition > Len(sourceText) OrElse Asc(sourceText, forPosition) <> Asc("(") Then Return 0
        Dim headerStart As Long = forPosition + 1
        If ScanSimpleExpressionGroup(sourceText, forPosition, Asc("("), Asc(")")) = 0 Then Return 0
        Dim headerText As String = Mid(sourceText, headerStart, forPosition - headerStart - 1)
        Dim firstSeparator As Long = 0
        Dim secondSeparator As Long = 0
        Dim depth As Long = 0
        Dim quoteByte As Long = 0
        For i As Long = 1 To Len(headerText)
            Dim c As Long = Asc(headerText, i)
            If quoteByte <> 0 Then
                If c = Asc("\") Then
                    i += 1
                ElseIf c = quoteByte Then
                    quoteByte = 0
                End If
                Continue For
            ElseIf c = Asc("'") OrElse c = Asc("""") Then
                quoteByte = c
                Continue For
            End If
            If Asc(headerText, i) = Asc("(") Then depth += 1
            If Asc(headerText, i) = Asc(")") Then depth -= 1
            If depth = 0 AndAlso Asc(headerText, i) = Asc(";") Then
                If firstSeparator = 0 Then
                    firstSeparator = i
                ElseIf secondSeparator = 0 Then
                    secondSeparator = i
                Else
                    Return 0
                End If
            End If
        Next
        If firstSeparator = 0 OrElse secondSeparator = 0 Then Return 0

        Dim initializationText As String = Trim(Left(headerText, firstSeparator - 1))
        Dim conditionPrelude As String
        Dim loopCondition As String = Trim(Mid(headerText, firstSeparator + 1, _
                                             secondSeparator - firstSeparator - 1))
        Dim incrementText As String = Trim(Mid(headerText, secondSeparator + 1))
        Dim translatedInitialization As String = ""
        Dim translatedIncrement As String = ""
        Dim statementAccepted As Long
        If PushSimpleBodyScope(context, symbolCount, localCount) = 0 Then Return 0
        If Len(initializationText) <> 0 Then
            statementAccepted = TranslateSimpleDeclarationStatement(translation, catalog,  _
                initializationText, translatedInitialization, context, locals(), localCount, _
                callableNames(), callableCount, parameterNames(), parameterAliases(), parameterTypes(), symbolCount)
            If statementAccepted = 0 Then
                statementAccepted = TranslateSimpleAssignmentStatement(translation, catalog,  _
                    initializationText, translatedInitialization, context, callableNames(), callableCount, _
                    parameterNames(), parameterAliases(), parameterTypes(), symbolCount)
            End If
            If statementAccepted = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-for-initializer", _
                    "unsupported for initializer: " & Left(initializationText, 200))
                Return 0
            End If
        End If
        If Len(loopCondition) = 0 Then
            loopCondition = "True"
        Else
            Dim translatedCondition As String
            If TranslateSequencedExpression(translation, catalog, _
                                            loopCondition, translatedCondition, conditionPrelude, context, _
                                            callableNames(), callableCount, parameterNames(), _
                                            parameterAliases(), parameterTypes(), symbolCount) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-for-condition", _
                    "unsupported for condition: " & Left(loopCondition, 200))
                Return 0
            End If
            loopCondition = translatedCondition
        End If
        If Len(incrementText) <> 0 Then
            If TranslateSimpleAssignmentStatement(translation, catalog, incrementText, translatedIncrement, context, _
                                                  callableNames(), callableCount, _
                                                  parameterNames(), parameterAliases(), parameterTypes(), symbolCount) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-for-increment", _
                    "unsupported for increment: " & Left(incrementText, 200))
                Return 0
            End If
        End If

        position = forPosition
        Dim breakLabel As String = NewSimpleFlowLabel(context)
        Dim continueLabel As String = NewSimpleFlowLabel(context)
        If PushSimpleControlFrame(context, breakLabel, continueLabel) = 0 Then Return 0
        Dim loopBody As String
        Dim loopTerminates As Long = 0
        If ParseSimpleCStatement(translation, catalog, sourceText, position, loopBody, indentText & "    ", _
                                 returnType, context, callableNames(), callableCount, parameterNames(), _
                                 parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                 loopTerminates) = 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-for-body", _
                "unsupported for body: " & Left(Mid(sourceText, position), 240))
            Return 0
        End If
        Dim hasBreak As Long = context.frames(context.frameCount - 1).hasBreak
        context.frameCount -= 1
        PopSimpleBodyScope(context, symbolCount, localCount)
        context.branchJump = 0
        outputText = ""
        If Len(translatedInitialization) <> 0 Then
            outputText = IndentSimpleBodyText(translatedInitialization, indentText) & Chr(10)
        End If
        If conditionPrelude <> "" Then
            outputText &= indentText & "Do" & Chr(10) & _
                          IndentSimpleBodyText(conditionPrelude & "If (" & loopCondition & " = 0) Then Goto " & breakLabel, indentText & "    ") & Chr(10) & loopBody
        Else
            outputText &= indentText & "While " & loopCondition & Chr(10) & loopBody
        End If
        outputText &= Chr(10) & indentText & continueLabel & ":"
        If Len(translatedIncrement) <> 0 Then
            outputText &= Chr(10) & IndentSimpleBodyText(translatedIncrement, indentText & "    ")
        End If
        outputText &= Chr(10) & indentText & IIf(conditionPrelude <> "", "Loop", "Wend") & Chr(10) & indentText & breakLabel & ":"
        If hasBreak = 0 AndAlso loopCondition = "True" Then terminatesPaths = -1
        Return -1
    End If

    Dim whilePosition As Long = position
    If Mid(sourceText, whilePosition, 5) = "while" AndAlso _
       (whilePosition + 5 > Len(sourceText) OrElse _
        IsIdentifierPart(Asc(sourceText, whilePosition + 5)) = 0) Then
        whilePosition += 5
        SkipCSpaces(sourceText, whilePosition)
        If whilePosition > Len(sourceText) OrElse Asc(sourceText, whilePosition) <> Asc("(") Then Return 0
        Dim conditionStart As Long = whilePosition + 1
        If ScanSimpleExpressionGroup(sourceText, whilePosition, Asc("("), Asc(")")) = 0 Then Return 0
        conditionText = Trim(Mid(sourceText, conditionStart, whilePosition - conditionStart - 1))
        Dim translatedCondition As String
        Dim conditionPrelude As String
        If TranslateSequencedExpression(translation, catalog, _
                                        conditionText, translatedCondition, conditionPrelude, context, _
                                        callableNames(), callableCount, parameterNames(), _
                                        parameterAliases(), parameterTypes(), symbolCount) = 0 Then Return 0
        position = whilePosition
        Dim breakLabel As String = NewSimpleFlowLabel(context)
        Dim continueLabel As String = NewSimpleFlowLabel(context)
        If PushSimpleControlFrame(context, breakLabel, continueLabel) = 0 Then Return 0
        Dim loopBody As String
        Dim loopTerminates As Long = 0
        If ParseSimpleCStatement(translation, catalog, sourceText, position, loopBody, indentText & "    ", _
                                 returnType, context, callableNames(), callableCount, parameterNames(), _
                                 parameterAliases(), parameterTypes(), symbolCount, locals(), localCount, _
                                 loopTerminates) = 0 Then Return 0
        Dim hasBreak As Long = context.frames(context.frameCount - 1).hasBreak
        context.frameCount -= 1
        context.branchJump = 0
        If conditionPrelude <> "" Then
            outputText = indentText & "Do" & Chr(10) & _
                         IndentSimpleBodyText(conditionPrelude & "If (" & translatedCondition & " = 0) Then Goto " & breakLabel, indentText & "    ") & Chr(10) & loopBody
        Else
            outputText = indentText & "While " & translatedCondition & Chr(10) & loopBody
        End If
        outputText &= Chr(10) & indentText & continueLabel & ":" & _
                      Chr(10) & indentText & IIf(conditionPrelude <> "", "Loop", "Wend") & Chr(10) & indentText & breakLabel & ":"
        If hasBreak = 0 AndAlso conditionPrelude = "" AndAlso _
           SimpleCConditionAlwaysTrue(conditionText) <> 0 Then _
            terminatesPaths = -1
        Return -1
    End If

    Dim returnExpression As String = ""
    Dim returnPosition As Long = position
    If ReadSimpleReturn(sourceText, returnPosition, returnExpression) <> 0 Then
        Dim translatedReturn As String = ""
        Dim returnPrelude As String
        If TranslateSimpleReturnValue(translation, catalog, returnType, returnExpression, translatedReturn, returnPrelude, context, _
                                      callableNames(), callableCount, parameterNames(), _
                                      parameterAliases(), parameterTypes(), symbolCount) = 0 Then Return 0
        outputText = ""
        If returnPrelude <> "" Then outputText = IndentSimpleBodyText(RTrim(returnPrelude, Any Chr(10)), indentText) & Chr(10)
        outputText &= indentText & "Return"
        If Len(translatedReturn) <> 0 Then outputText &= " " & translatedReturn
        position = returnPosition
        terminatesPaths = -1
        Return -1
    End If

    Dim simpleText As String
    If ReadSimpleCStatement(sourceText, position, simpleText) = 0 Then Return 0
    If IsUnusedLocalRecordDefinition( _
        simpleText, Mid(sourceText, position)) <> 0 Then
        outputText = indentText & "' unused local record definition"
        Return -1
    End If
    Dim localRecordOutput As String
    If TranslateSimpleLocalRecordTypeDefinition( _
        translation, catalog, simpleText, localRecordOutput, context) <> 0 Then
        outputText = indentText & localRecordOutput
        Return -1
    End If
    If IsSimpleUnusedParameterStatement(simpleText) <> 0 OrElse IsSimpleVoidZeroStatement(simpleText) <> 0 Then
        outputText = indentText & "' C unused-parameter annotation"
        Return -1
    End If
    Dim translatedStatement As String
    If TranslateSimpleDeclarationStatement(translation, catalog, simpleText, translatedStatement, context, locals(), localCount, _
                                           callableNames(), callableCount, parameterNames(), _
                                           parameterAliases(), parameterTypes(), symbolCount) <> 0 Then
        outputText = IndentSimpleBodyText(translatedStatement, indentText)
        Return -1
    End If
    If TranslateSimpleAssignmentStatement(translation, catalog, simpleText, translatedStatement, context, callableNames(), _
                                          callableCount, parameterNames(), parameterAliases(), parameterTypes(), _
                                          symbolCount) = 0 Then Return 0
    outputText = IndentSimpleBodyText(translatedStatement, indentText)
    Return -1
End Function

/' end of c2fb_body_statements.bi '/
