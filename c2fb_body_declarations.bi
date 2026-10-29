/'
    Project: C2FB body lowering

    File: c2fb_body_declarations.bi

    Purpose:

        Lower C local declarations and initializers into body-local bindings.

    Responsibilities:

        - Translate local scalar, array, string, and record declarations.
        - Split comma-separated local declarations with shared token ranges.
        - Validate supported static and nested record initializers.
        - Register local names and types in the current procedure scope.
        - Preserve the first detailed failure for the body rejection report.

    This file intentionally does NOT contain:

        - Statement and control-flow parsing or procedure construction.
        - C source collection, preprocessing, or generated-file output.
'/

Private Function ReadSimpleStaticStringArray( _
    ByVal sourceText As String, ByRef values As String, ByRef byteCount As Long _
) As Long
    values = ""
    byteCount = 0
    Dim position As Long = 1
    Do
        SkipCSpaces(sourceText, position)
        If Mid(sourceText, position, 1) <> Chr(34) Then Return 0
        position += 1
        Dim hasClosingQuote As Long = 0
        While position <= Len(sourceText)
            Dim byteValue As Long = Asc(sourceText, position)
            If byteValue = Asc(Chr(34)) Then
                position += 1
                hasClosingQuote = -1
                Exit While
            ElseIf byteValue = Asc(Chr(92)) Then
                Dim parser As SimpleExpressionParser
                parser.sourceText = sourceText
                parser.position = position
                If ReadSimpleStringEscape(parser, byteValue) = 0 Then Return 0
                position = parser.position
            Else
                If byteValue = 0 OrElse byteValue = 10 OrElse byteValue = 13 Then Return 0
                position += 1
            End If
            If byteCount >= 65534 Then Return 0
            If values <> "" Then values &= ", "
            values &= Trim(Str(byteValue))
            byteCount += 1
        Wend
        If hasClosingQuote = 0 Then Return 0
        SkipCSpaces(sourceText, position)
        If Mid(sourceText, position, 1) <> Chr(34) Then Exit Do
    Loop
    If values <> "" Then values &= ", "
    values &= "0" ' C string arrays include the terminating NUL byte.
    byteCount += 1
    Return -1
End Function


Private Function ReadSimpleStaticStringMatrix( _
    ByVal initializerText As String, ByVal rowWidth As Long, _
    ByRef values As String, ByRef rowCount As Long, _
    ByRef byteCount As Long _
) As Long
    values = ""
    rowCount = 0
    byteCount = 0
    initializerText = Trim(initializerText)
    If rowWidth < 1 OrElse Left(initializerText, 1) <> "{" OrElse _
       Right(initializerText, 1) <> "}" Then Return 0

    Dim initialText As String = Trim(Mid( _
        initializerText, 2, Len(initializerText) - 2))
    If Right(initialText, 1) = "," Then _
        initialText = RTrim(Left(initialText, Len(initialText) - 1))
    Dim rows() As String
    If SplitSimpleCList(initialText, rows(), rowCount) = 0 OrElse _
       rowCount < 1 OrElse rowCount > MAX_SIMPLE_ARRAY_COUNT OrElse _
       rowCount > MAX_SIMPLE_ARRAY_COUNT \ rowWidth Then Return 0

    For rowIndex As Long = 0 To rowCount - 1
        Dim rowValues As String
        Dim rowByteCount As Long
        If ReadSimpleStaticStringArray( _
            Trim(rows(rowIndex)), rowValues, rowByteCount) = 0 OrElse _
           rowByteCount > rowWidth Then Return 0
        If values <> "" Then values &= ", "
        values &= rowValues
        For columnIndex As Long = rowByteCount To rowWidth - 1
            values &= ", 0"
        Next
    Next
    byteCount = rowCount * rowWidth
    Return -1
End Function



' Local enum values use the same bounded integer grammar as generic headers.
' Resolve visible body constants first so a local name can shadow a header enum.
Private Function EvaluateSimpleBodyEnumIntegerExpression( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, _
    locals() As SimpleLocalVariable, ByVal localCount As Long, _
    parameterNames() As String, ByVal parameterCount As Long, _
    ByRef integerValue As Long _
) As Long
    Dim numericExpression As String
    Dim position As Long = 1
    While position <= Len(sourceExpression)
        Dim currentByte As Long = Asc(sourceExpression, position)
        If IsIdentifierStart(currentByte) <> 0 Then
            Dim tokenStart As Long = position
            position += 1
            While position <= Len(sourceExpression) AndAlso _
                  IsIdentifierPart(Asc(sourceExpression, position)) <> 0
                position += 1
            Wend
            Dim sourceName As String = Mid( _
                sourceExpression, tokenStart, position - tokenStart)
            Dim visibleSymbol As Long = -1
            For symbolIndex As Long = parameterCount - 1 To 0 Step -1
                If parameterNames(symbolIndex) = sourceName Then
                    visibleSymbol = symbolIndex
                    Exit For
                End If
            Next
            If visibleSymbol >= 0 Then
                Dim localIndex As Long = -1
                For candidateIndex As Long = localCount - 1 To 0 Step -1
                    If locals(candidateIndex).name = sourceName Then
                        localIndex = candidateIndex
                        Exit For
                    End If
                Next
                If localIndex < 0 OrElse _
                   locals(localIndex).isIntegerConstant = 0 Then Return 0
                numericExpression &= Trim(Str(locals(localIndex).integerValue))
            Else
                Dim enumIndex As Long = _
                    GenericEnumMemberIndexInContext(translation, sourceName)
                If enumIndex < 0 OrElse _
                   translation.genericC.header.enumMembers( _
                       enumIndex).integerValueKnown = 0 Then Return 0
                numericExpression &= Trim(Str( _
                    translation.genericC.header.enumMembers( _
                        enumIndex).integerValue))
            End If
        ElseIf currentByte >= Asc("0") AndAlso currentByte <= Asc("9") Then
            Dim tokenStart As Long = position
            position += 1
            While position <= Len(sourceExpression) AndAlso _
                  IsIdentifierPart(Asc(sourceExpression, position)) <> 0
                position += 1
            Wend
            numericExpression &= Mid( _
                sourceExpression, tokenStart, position - tokenStart)
        Else
            numericExpression &= Chr(currentByte)
            position += 1
        End If
    Wend

    Dim parser As SimpleArrayBoundParser
    parser.sourceText = numericExpression
    parser.position = 1
    Dim value As LongInt
    If ReadSimpleArrayBoundShift(parser, value, 0) = 0 Then Return 0
    SkipCSpaces(parser.sourceText, parser.position)
    If parser.position <= Len(parser.sourceText) OrElse _
       value < -MAX_GENERIC_ARRAY_BOUND_VALUE OrElse _
       value > MAX_GENERIC_ARRAY_BOUND_VALUE Then Return 0
    integerValue = CLng(value)
    Return -1
End Function


' Local enums have block scope but no runtime storage. Give each enumerator a
' private compile-time alias and retain its value for later members.
Private Function TranslateSimpleLocalEnumDefinition( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, ByRef outputText As String, _
    ByRef context As SimpleBodyContext, _
    locals() As SimpleLocalVariable, ByRef localCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByRef symbolCount As Long _
) As Long
    outputText = ""
    If translation.target.profile <> C2FB_TARGET_GENERIC Then Return 0

    statementText = Trim(statementText)
    If Left(statementText, Len("enum")) <> "enum" OrElse _
       (Len(statementText) > Len("enum") AndAlso _
        IsIdentifierPart(Asc(statementText, Len("enum") + 1)) <> 0) Then _
        Return 0

    Dim position As Long = Len("enum") + 1
    SkipCSpaces(statementText, position)
    If position <= Len(statementText) AndAlso _
       Asc(statementText, position) <> Asc("{") Then
        If IsIdentifierStart(Asc(statementText, position)) = 0 Then Return 0
        While position <= Len(statementText) AndAlso _
              IsIdentifierPart(Asc(statementText, position)) <> 0
            position += 1
        Wend
        SkipCSpaces(statementText, position)
    End If
    If position > Len(statementText) OrElse _
       Asc(statementText, position) <> Asc("{") Then Return 0
    Dim openingBrace As Long = position
    Dim closingBrace As Long = position
    If ScanSimpleExpressionGroup( _
        statementText, closingBrace, Asc("{"), Asc("}")) = 0 Then Return 0
    Dim enumBody As String = Mid( _
        statementText, openingBrace + 1, closingBrace - openingBrace - 2)
    position = closingBrace
    SkipCSpaces(statementText, position)
    If position <= Len(statementText) AndAlso _
       Asc(statementText, position) = Asc(";") Then position += 1
    SkipCSpaces(statementText, position)
    If position <= Len(statementText) Then Return 0

    Dim enumParts() As String
    Dim enumPartCount As Long
    If SplitSimpleCList( _
        enumBody, enumParts(), enumPartCount, -1) = 0 OrElse _
       enumPartCount < 1 OrElse enumPartCount > MAX_SIMPLE_VARIABLES Then _
        Return 0

    Dim scopeStart As Long = 0
    If context.scopeDepth > 0 Then _
        scopeStart = context.scopeSymbols(context.scopeDepth - 1)
    Dim previousValue As Long = 0
    Dim previousValueKnown As Long = -1
    Dim previousAlias As String
    Dim callableNames(0 To 0) As String
    For memberIndex As Long = 0 To enumPartCount - 1
        Dim memberText As String = Trim(enumParts(memberIndex))
        Dim equalsPosition As Long = FindSimpleAssignmentEquals(memberText)
        Dim memberNameText As String = memberText
        If equalsPosition > 0 Then _
            memberNameText = Trim(Left(memberText, equalsPosition - 1))

        Dim memberTokens() As String
        Dim memberTokenCount As Long
        TokenizeCDeclaration( _
            memberNameText, memberTokens(), memberTokenCount)
        If memberTokenCount <> 1 OrElse _
           IsIdentifierToken(memberTokens(0)) = 0 Then Return 0
        Dim memberName As String = memberTokens(0)
        For symbolIndex As Long = scopeStart To symbolCount - 1
            If parameterNames(symbolIndex) = memberName Then Return 0
        Next

        Dim value As Long
        Dim valueKnown As Long = 0
        Dim freeBasicValue As String
        If equalsPosition > 0 Then
            Dim valueExpression As String = Trim( _
                Mid(memberText, equalsPosition + 1))
            If valueExpression = "" Then Return 0
            If EvaluateSimpleBodyEnumIntegerExpression( _
                translation, valueExpression, locals(), localCount, _
                parameterNames(), symbolCount, value) <> 0 Then
                valueKnown = -1
                freeBasicValue = Trim(Str(value))
            Else
                Dim hasBooleanOperator As Long
                Dim hasFunctionCall As Long
                If TranslateSimpleExpression( _
                    translation, catalog, valueExpression, freeBasicValue, _
                    hasBooleanOperator, hasFunctionCall, callableNames(), 0, _
                    parameterNames(), parameterAliases(), parameterTypes(), _
                    symbolCount, "c2fb_c_int", 0, -1) = 0 OrElse _
                   hasFunctionCall <> 0 Then Return 0
            End If
        ElseIf memberIndex = 0 Then
            value = 0
            valueKnown = -1
            freeBasicValue = "0"
        ElseIf previousValueKnown <> 0 Then
            If previousValue >= MAX_GENERIC_ARRAY_BOUND_VALUE Then Return 0
            value = previousValue + 1
            valueKnown = -1
            freeBasicValue = Trim(Str(value))
        Else
            freeBasicValue = previousAlias & " + 1"
        End If

        If context.localSerial >= MAX_SIMPLE_VARIABLES OrElse _
           localCount >= MAX_SIMPLE_VARIABLES OrElse _
           symbolCount >= MAX_SIMPLE_VARIABLES Then Return 0
        Dim aliasName As String = "local" & Trim(Str(context.localSerial))
        context.localSerial += 1
        context.declarations &= "    Const " & aliasName & _
            " As c2fb_c_int = " & freeBasicValue & Chr(10)
        context.readOnlyAliases &= "|" & aliasName & "|"
        locals(localCount).name = memberName
        locals(localCount).typeName = "c2fb_c_int"
        locals(localCount).isConst = -1
        locals(localCount).sizeofBytes = CLng(SizeOf(Long))
        locals(localCount).hasKnownSize = -1
        locals(localCount).isIntegerConstant = valueKnown
        locals(localCount).integerValue = value
        localCount += 1
        parameterNames(symbolCount) = memberName
        parameterAliases(symbolCount) = aliasName
        parameterTypes(symbolCount) = "c2fb_c_int"
        symbolCount += 1
        previousValue = value
        previousValueKnown = valueKnown
        previousAlias = aliasName
    Next
    outputText = "' C local enum constants"
    Return -1
End Function


Private Function CBodySourceRecordKindInContext( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceRecord As String _
) As C2fbRecordKind
    For declarationIndex As Long = 0 To _
        translation.sourceUnit.declarationCount - 1
        With translation.sourceUnit.declarations(declarationIndex)
            If .recordKind = C2FB_RECORD_KIND_NONE Then Continue For
            If .recordTag = sourceRecord OrElse _
               .recordAlias = sourceRecord Then Return .recordKind
        End With
    Next
    For bindingIndex As Long = 0 To translation.body.recordBindingCount - 1
        With translation.body.recordBindings(bindingIndex)
            ' Source-unit anonymous aggregates use owner.field keys.
            If (.isGenerated <> 0 OrElse InStr(.sourceName, ".") <> 0) AndAlso _
               .sourceName = sourceRecord Then _
                Return .recordKind
        End With
    Next
    Return C2FB_RECORD_KIND_NONE
End Function

Private Function RegisterCBodyAnonymousLocalRecord( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal recordBody As String, _
    ByRef generatedRecordName As String, _
    ByVal recordKind As C2fbRecordKind = C2FB_RECORD_STRUCT _
) As Long
    generatedRecordName = ""
    If (recordKind <> C2FB_RECORD_STRUCT AndAlso _
        recordKind <> C2FB_RECORD_UNION) OrElse _
       Trim(recordBody) = "" OrElse InStr(recordBody, "{") <> 0 OrElse _
       InStr(recordBody, "}") <> 0 Then Return 0
    If translation.body.localRecordSerial >= MAX_SIMPLE_VARIABLES Then Return 0

    Dim sourceRecordName As String = "c2fb_local_record_" & _
        Trim(Str(translation.body.localRecordSerial))
    For i As Long = 0 To translation.body.recordBindingCount - 1
        If LCase(translation.body.recordBindings(i).sourceName) = _
           LCase(sourceRecordName) Then Return 0
    Next
    For i As Long = 0 To catalog.bodyFieldCount - 1
        If LCase(catalog.bodyFields(i).ownerName) = _
           LCase(sourceRecordName) Then Return 0
    Next

    Dim savedRecordFieldCount As Long = translation.body.recordFieldCount
    Dim savedRecordBindingCount As Long = translation.body.recordBindingCount
    Dim savedBodyFieldCount As Long = catalog.bodyFieldCount
    Dim localDeclaration As C2fbDeclarationModel
    localDeclaration.entryKind = C2FB_DECLARATION_RECORD
    localDeclaration.recordKind = recordKind
    localDeclaration.recordTag = sourceRecordName
    localDeclaration.recordBody = recordBody
    BuildC2fbRecordFieldSources(localDeclaration)
    If localDeclaration.recordFieldsComplete = 0 OrElse _
       localDeclaration.recordFieldCount = 0 Then Return 0
    If RecordCBodyFieldsFromModel( _
        translation, catalog, sourceRecordName, localDeclaration, 0) = 0 Then
        translation.body.recordFieldCount = savedRecordFieldCount
        RebuildCBodyRecordFieldIndexes(translation)
        translation.body.recordBindingCount = savedRecordBindingCount
        Return 0
    End If

    Dim registeredFieldCount As Long = _
        translation.body.recordFieldCount - savedRecordFieldCount
    If registeredFieldCount <> localDeclaration.recordFieldCount Then
        translation.body.recordFieldCount = savedRecordFieldCount
        RebuildCBodyRecordFieldIndexes(translation)
        translation.body.recordBindingCount = savedRecordBindingCount
        Return 0
    End If

    Dim recordAccepted As Long = -1
    For fieldIndex As Long = savedRecordFieldCount To _
        translation.body.recordFieldCount - 1
        With translation.body.recordFields(fieldIndex)
            If .ownerName <> sourceRecordName OrElse _
               IsIdentifierToken(.name) = 0 OrElse .bitWidth >= 0 Then
                recordAccepted = 0
                Exit For
            End If
            For priorField As Long = savedRecordFieldCount To fieldIndex - 1
                If LCase(translation.body.recordFields(priorField).name) = _
                   LCase(.name) Then
                    recordAccepted = 0
                    Exit For
                End If
            Next
            If recordAccepted = 0 Then Exit For

            Dim fieldType As String = CBodyFieldType( _
                translation, catalog, fieldIndex)
            If fieldType = "" Then
                recordAccepted = 0
                Exit For
            End If
            Dim storedFieldType As String = fieldType
            If .isArray <> 0 Then
                Dim arrayCount As Long
                If EvaluateSimpleArrayBound( _
                    .arrayBound, arrayCount) = 0 OrElse arrayCount < 1 OrElse _
                   arrayCount > MAX_SIMPLE_ARRAY_COUNT Then
                    recordAccepted = 0
                    Exit For
                End If
                If IsCBodyCallbackTypeInContext( _
                    translation, fieldType) = 0 Then
                    storedFieldType = SimplePointeeType(fieldType)
                End If
                If storedFieldType = "" Then
                    recordAccepted = 0
                    Exit For
                End If
            Else
                Dim nestedRecord As String = CBodyRecordName( _
                    translation, catalog, fieldType)
                If SimpleArithmeticKind(catalog, fieldType) = 0 AndAlso _
                   InStr(fieldType, " Ptr") = 0 AndAlso _
                   IsCBodyCallbackTypeInContext(translation, fieldType) = 0 AndAlso _
                   (nestedRecord = "" OrElse _
                    CBodyRecordSizeIsKnown( _
                        translation, catalog, nestedRecord) = 0) Then
                    recordAccepted = 0
                    Exit For
                End If
            End If

            If catalog.bodyFieldCount >= MAX_C2FB_BODY_CATALOG_FIELDS Then _
                ReportError("FreeBASIC record fields exceed the configured limit")
            If catalog.bodyFieldCount > UBound(catalog.bodyFields) Then _
                ReDim Preserve catalog.bodyFields( _
                    0 To UBound(catalog.bodyFields) * 2 + 1)
            With catalog.bodyFields(catalog.bodyFieldCount)
                .ownerName = sourceRecordName
                .name = translation.body.recordFields(fieldIndex).freeBasicName
                If .name = "" Then _
                    .name = translation.body.recordFields(fieldIndex).name
                .typeName = storedFieldType
                .isArray = translation.body.recordFields(fieldIndex).isArray
                .isUnionMember = 0
                .headerName = ""
            End With
            catalog.bodyFieldCount += 1
            IndexC2fbBodyField(catalog, catalog.bodyFieldCount - 1)
        End With
        If recordAccepted = 0 Then Exit For
    Next
    If recordAccepted = 0 Then
        translation.body.recordFieldCount = savedRecordFieldCount
        RebuildCBodyRecordFieldIndexes(translation)
        translation.body.recordBindingCount = savedRecordBindingCount
        catalog.bodyFieldCount = savedBodyFieldCount
        RebuildC2fbBodyFieldIndex(catalog)
        Return 0
    End If

    If translation.body.recordBindingCount >= _
       MAX_C2FB_BODY_RECORD_BINDINGS Then _
        ReportError("generated local records exceed the configured limit")
    If translation.body.recordBindingCount > UBound(translation.body.recordBindings) Then _
        ReDim Preserve translation.body.recordBindings( _
            0 To UBound(translation.body.recordBindings) * 2 + 1)
    With translation.body.recordBindings(translation.body.recordBindingCount)
        .sourceName = sourceRecordName
        .fbName = sourceRecordName
        .recordKind = recordKind
        .isGenerated = -1
    End With
    translation.body.recordBindingCount += 1
    translation.body.localRecordSerial += 1
    generatedRecordName = sourceRecordName
    Return -1
End Function

Private Function ConvertSimpleRecordInitializerFieldStorage( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal sourceRecord As String, ByVal fieldName As String, _
    ByVal fieldType As String, ByVal fbFieldIndex As Long, _
    ByRef fieldValue As String, _
    ByRef context As SimpleBodyContext _
) As Long
    If fbFieldIndex < 0 Then Return 0
    Dim storedFieldType As String = NormalizeFbBodyType( _
        catalog, catalog.bodyFields(fbFieldIndex).typeName)
    Dim convertedFieldType As String = NormalizeFbBodyType( _
        catalog, fieldType)
    If storedFieldType = convertedFieldType Then Return -1

    If InStr(storedFieldType, " Ptr") <> 0 OrElse _
       IsCBodyCallbackTypeInContext(translation, storedFieldType) <> 0 Then
        fieldValue = "CPtr(" & storedFieldType & ", " & fieldValue & ")"
        Return -1
    End If
    If SimpleArithmeticKind(catalog, storedFieldType) > 0 AndAlso _
       SimpleArithmeticKind(catalog, fieldType) > 0 Then
        fieldValue = "Cast(" & storedFieldType & ", " & fieldValue & ")"
        Return -1
    End If

    RecordSimpleBodyFailure( _
        context, "static-record-field-storage-mismatch", _
        "static record field storage mismatch " & sourceRecord & "." & _
        fieldName & ": C type=" & fieldType & _
        ", FreeBASIC type=" & storedFieldType)
    Return 0
End Function

Private Function ParseSimpleCMemberDesignator( _
    ByVal initializerText As String, _
    ByRef fieldName As String, _
    ByRef fieldInitializer As String _
) As Long
    fieldName = ""
    fieldInitializer = ""
    initializerText = Trim(initializerText)
    If Len(initializerText) < 4 OrElse Left(initializerText, 1) <> "." Then Return 0
    Dim pathSegments(0 To MAX_TYPE_WORDS - 1) As String
    Dim pathSegmentCount As Long = 0
    Dim position As Long = 1
    Dim foundInitializer As Long = 0
    While position <= Len(initializerText)
        If Asc(initializerText, position) <> Asc(".") Then Return 0
        position += 1
        SkipCSpaces(initializerText, position)
        If position > Len(initializerText) OrElse _
           IsIdentifierStart(Asc(initializerText, position)) = 0 OrElse _
           pathSegmentCount >= MAX_TYPE_WORDS Then Return 0
        Dim nameStart As Long = position
        position += 1
        While position <= Len(initializerText) AndAlso _
              IsIdentifierPart(Asc(initializerText, position)) <> 0
            position += 1
        Wend
        pathSegments(pathSegmentCount) = _
            Mid(initializerText, nameStart, position - nameStart)
        pathSegmentCount += 1
        SkipCSpaces(initializerText, position)
        If position > Len(initializerText) Then Return 0
        If Asc(initializerText, position) = Asc("=") Then
            position += 1
            fieldInitializer = Trim(Mid(initializerText, position))
            foundInitializer = -1
            Exit While
        ElseIf Asc(initializerText, position) <> Asc(".") Then
            Return 0
        End If
    Wend
    If foundInitializer = 0 OrElse pathSegmentCount = 0 OrElse _
       fieldInitializer = "" Then Return 0

    fieldName = pathSegments(0)
    For pathIndex As Long = pathSegmentCount - 1 To 1 Step -1
        fieldInitializer = "{ ." & pathSegments(pathIndex) & " = " & _
            fieldInitializer & " }"
    Next
    Return -1
End Function

Private Function MergeSimpleCMemberDesignatorInitializers( _
    ByVal firstText As String, ByVal secondText As String, _
    ByRef mergedText As String, ByVal depth As Long = 0 _
) As Long
    mergedText = ""
    If depth >= MAX_BODY_NESTING Then Return 0
    firstText = Trim(firstText)
    secondText = Trim(secondText)
    If Left(firstText, 1) <> "{" OrElse Right(firstText, 1) <> "}" OrElse _
       Left(secondText, 1) <> "{" OrElse Right(secondText, 1) <> "}" Then Return 0

    Dim firstBody As String = Trim(Mid(firstText, 2, Len(firstText) - 2))
    Dim secondBody As String = Trim(Mid(secondText, 2, Len(secondText) - 2))
    If firstBody = "" OrElse secondBody = "" Then Return 0
    Dim initializerLists(0 To 1) As String
    initializerLists(0) = firstBody
    initializerLists(1) = secondBody

    Dim fieldNames(0 To MAX_TYPE_WORDS - 1) As String
    Dim fieldValues(0 To MAX_TYPE_WORDS - 1) As String
    Dim fieldCount As Long = 0
    For listIndex As Long = 0 To 1
        Dim values() As String
        Dim valueCount As Long
        If SplitSimpleCList( _
            initializerLists(listIndex), values(), valueCount) = 0 Then Return 0
        For valueIndex As Long = 0 To valueCount - 1
            Dim fieldName As String
            Dim fieldInitializer As String
            If ParseSimpleCMemberDesignator( _
                values(valueIndex), fieldName, fieldInitializer) = 0 Then Return 0
            Dim existingField As Long = -1
            For fieldIndex As Long = 0 To fieldCount - 1
                If fieldNames(fieldIndex) = fieldName Then
                    existingField = fieldIndex
                    Exit For
                End If
            Next
            If existingField >= 0 Then
                Dim nestedInitializer As String
                If MergeSimpleCMemberDesignatorInitializers( _
                    fieldValues(existingField), fieldInitializer, _
                    nestedInitializer, depth + 1) = 0 Then Return 0
                fieldValues(existingField) = nestedInitializer
            Else
                If fieldCount >= MAX_TYPE_WORDS Then Return 0
                fieldNames(fieldCount) = fieldName
                fieldValues(fieldCount) = fieldInitializer
                fieldCount += 1
            End If
        Next
    Next

    If fieldCount = 0 Then Return 0
    mergedText = "{ "
    For fieldIndex As Long = 0 To fieldCount - 1
        If fieldIndex > 0 Then mergedText &= ", "
        mergedText &= "." & fieldNames(fieldIndex) & " = " & _
            fieldValues(fieldIndex)
    Next
    mergedText &= " }"
    Return -1
End Function

Private Function SimpleInitializerMayHaveSideEffects( _
    ByVal initializerText As String _
) As Long
    initializerText = Trim(initializerText)
    Dim position As Long = 1
    Dim quoteByte As Long = 0
    Dim escaped As Long = 0
    While position <= Len(initializerText)
        Dim currentByte As Long = Asc(initializerText, position)
        If quoteByte <> 0 Then
            If escaped <> 0 Then
                escaped = 0
            ElseIf currentByte = Asc(Chr(92)) Then
                escaped = -1
            ElseIf currentByte = quoteByte Then
                quoteByte = 0
            End If
        ElseIf Mid(initializerText, position, 2) = "/*" Then
            Dim commentEnd As Long = InStr(position + 2, initializerText, "*/")
            If commentEnd = 0 Then Return 0
            position = commentEnd + 2
            Continue While
        ElseIf Mid(initializerText, position, 2) = "//" Then
            Dim lineEnd As Long = InStr(position + 2, initializerText, Chr(10))
            If lineEnd = 0 Then Return 0
            position = lineEnd + 1
            Continue While
        ElseIf currentByte = Asc(Chr(34)) OrElse currentByte = Asc("'") Then
            quoteByte = currentByte
        ElseIf Mid(initializerText, position, 2) = "++" OrElse _
               Mid(initializerText, position, 2) = "--" Then
            Return -1
        End If
        position += 1
    Wend
    If Left(initializerText, 1) = "{" Then
        If Right(initializerText, 1) <> "}" Then Return -1
        Dim initializerBody As String = Trim(Mid( _
            initializerText, 2, Len(initializerText) - 2))
        If Right(initializerBody, 1) = "," Then _
            initializerBody = RTrim(Left(initializerBody, Len(initializerBody) - 1))
        Dim values() As String
        Dim valueCount As Long
        If initializerBody = "" Then Return 0
        If SplitSimpleCList(initializerBody, values(), valueCount) = 0 Then Return -1
        For valueIndex As Long = 0 To valueCount - 1
            Dim valueText As String = Trim(values(valueIndex))
            If Left(valueText, 1) = "." Then
                Dim fieldName As String
                Dim fieldInitializer As String
                If ParseSimpleCMemberDesignator( _
                    valueText, fieldName, fieldInitializer) = 0 Then Return -1
                valueText = fieldInitializer
            End If
            If SimpleInitializerMayHaveSideEffects(valueText) <> 0 Then Return -1
        Next
        Return 0
    End If
    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(initializerText, tokens(), tokenCount)
    For tokenIndex As Long = 0 To tokenCount - 1
        Select Case tokens(tokenIndex)
        Case "++", "--", "=", "+=", "-=", "*=", "/=", "%=", _
             "&=", "|=", "^=", "<<=", ">>="
            Return -1
        End Select
    Next
    Return 0
End Function

Private Function TranslateSimpleRecordArrayInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal initializerText As String, _
    ByVal elementType As String, ByVal arrayBound As String, _
    ByVal fieldLabel As String, _
    ByRef translatedValue As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal parameterCount As Long, _
    ByVal requireConstant As Long = 0 _
) As Long
    translatedValue = ""
    Dim arrayCount As Long
    If EvaluateSimpleArrayBound( _
        arrayBound, arrayCount, MAX_SIMPLE_ARRAY_COUNT) = 0 OrElse _
       arrayCount < 1 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-record-array-bound", _
            "unsupported fixed array member bound for " & fieldLabel)
        Return 0
    End If

    Dim values() As String
    Dim valueCount As Long = 0
    Dim isStringInitializer As Long = 0
    initializerText = Trim(initializerText)
    If initializerText <> "" Then
        If Left(initializerText, 1) = Chr(34) Then
            If elementType <> "Byte" AndAlso elementType <> "UByte" AndAlso _
               elementType <> "c2fb_c_char" AndAlso _
               elementType <> "c2fb_c_uchar" Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-record-array-string", _
                    "string initializer does not match array member " & fieldLabel)
                Return 0
            End If
            Dim stringValues As String
            Dim stringByteCount As Long
            If ReadSimpleStaticStringArray( _
                initializerText, stringValues, stringByteCount) = 0 OrElse _
               SplitSimpleCList( _
                   stringValues, values(), valueCount) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-record-array-string", _
                    "unsupported string initializer for array member " & fieldLabel)
                Return 0
            End If
            isStringInitializer = -1
        ElseIf Left(initializerText, 1) = "{" AndAlso _
               Right(initializerText, 1) = "}" Then
            Dim initializerBody As String = Trim(Mid( _
                initializerText, 2, Len(initializerText) - 2))
            If Right(initializerBody, 1) = "," Then _
                initializerBody = RTrim(Left(initializerBody, Len(initializerBody) - 1))
            If initializerBody <> "" AndAlso _
               SplitSimpleCList( _
                   initializerBody, values(), valueCount) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-record-array-initializer", _
                    "unsupported array initializer for member " & fieldLabel)
                Return 0
            End If
        Else
            RecordSimpleBodyFailure( _
                context, "unsupported-record-array-initializer", _
                "array member requires a brace or string initializer: " & fieldLabel)
            Return 0
        End If
    End If
    If isStringInitializer <> 0 AndAlso valueCount = arrayCount + 1 AndAlso _
       Trim(values(arrayCount)) = "0" Then valueCount -= 1
    If valueCount > arrayCount Then
        RecordSimpleBodyFailure( _
            context, "record-array-initializer-too-large", _
            "too many values in array member initializer " & fieldLabel)
        Return 0
    End If

    Dim nestedRecord As String
    If InStr(elementType, " Ptr") = 0 AndAlso _
       IsCBodyCallbackTypeInContext(translation, elementType) = 0 Then
        nestedRecord = CBodyRecordName(translation, catalog, elementType)
    End If
    If SimpleArithmeticKind(catalog, elementType) = 0 AndAlso _
       InStr(elementType, " Ptr") = 0 AndAlso _
       IsCBodyCallbackTypeInContext(translation, elementType) = 0 AndAlso _
       nestedRecord = "" Then
        RecordSimpleBodyFailure( _
            context, "unsupported-record-array-element", _
            "unsupported array member element type " & fieldLabel & " As " & elementType)
        Return 0
    End If

    Dim resultValues As String
    For valueIndex As Long = 0 To arrayCount - 1
        Dim elementValue As String
        If valueIndex >= valueCount Then
            If nestedRecord <> "" Then
                If TranslateSimpleRecordInitializer( _
                    translation, catalog, "{}", elementType, elementValue, _
                    context, callableNames(), callableCount, parameterNames(), _
                    parameterAliases(), parameterTypes(), parameterCount, _
                    requireConstant) = 0 Then _
                    Return 0
            ElseIf InStr(elementType, " Ptr") <> 0 OrElse _
                   IsCBodyCallbackTypeInContext(translation, elementType) <> 0 Then
                elementValue = "CPtr(" & elementType & ", 0)"
            Else
                elementValue = "Cast(" & elementType & ", 0)"
            End If
        ElseIf nestedRecord <> "" Then
            If TranslateSimpleRecordInitializer( _
                translation, catalog, values(valueIndex), elementType, _
                elementValue, context, callableNames(), callableCount, _
                parameterNames(), parameterAliases(), parameterTypes(), _
                parameterCount, requireConstant) = 0 Then Return 0
        Else
            Dim hasBoolean As Long
            Dim hasFunctionCall As Long
            If TranslateSimpleExpression( _
                translation, catalog, values(valueIndex), elementValue, _
                hasBoolean, hasFunctionCall, callableNames(), callableCount, _
                parameterNames(), parameterAliases(), parameterTypes(), _
                parameterCount, elementType, -1, requireConstant) = 0 OrElse _
               hasFunctionCall <> 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-record-array-element", _
                    "unsupported array initializer element for " & fieldLabel & ": " & _
                    Left(values(valueIndex), 120))
                Return 0
            End If
        End If
        If resultValues <> "" Then resultValues &= ", "
        resultValues &= elementValue
    Next
    translatedValue = "{" & resultValues & "}"
    Return -1
End Function

Private Function TranslateSimpleNestedRecordValue( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal initializerText As String, ByVal freeBasicType As String, _
    ByRef translatedValue As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal parameterCount As Long, _
    ByVal requireConstant As Long _
) As Long
    initializerText = Trim(initializerText)
    If Left(initializerText, 1) = "{" Then _
        Return TranslateSimpleRecordInitializer( _
            translation, catalog, initializerText, freeBasicType, _
            translatedValue, context, callableNames(), callableCount, _
            parameterNames(), parameterAliases(), parameterTypes(), _
            parameterCount, requireConstant)

    ' A nested C record may be copied from an existing object. Restrict this
    ' case to an identifier so a Basic constructor cannot reorder side effects.
    If IsIdentifierToken(initializerText) = 0 Then Return 0
    Dim hasBoolean As Long
    Dim hasFunctionCall As Long
    If TranslateSimpleExpression( _
        translation, catalog, initializerText, translatedValue, _
        hasBoolean, hasFunctionCall, callableNames(), callableCount, _
        parameterNames(), parameterAliases(), parameterTypes(), _
        parameterCount, freeBasicType, -1, requireConstant) = 0 OrElse _
       hasFunctionCall <> 0 Then
        Return 0
    End If
    Return -1
End Function

Private Function AppendSimpleBodyUnionInitializerHelper( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef context As SimpleBodyContext, _
    ByVal unionType As String, ByVal fieldType As String, _
    ByVal freeBasicFieldName As String, ByRef helperName As String _
) As Long
    helperName = ""
    If unionType = "" OrElse fieldType = "" OrElse _
       freeBasicFieldName = "" OrElse IsIdentifierToken(freeBasicFieldName) = 0 OrElse _
       Left(unionType, 6) = "Const " Then Return 0

    Dim serial As Long = translation.body.generatedAggregateInitializerSerial
    Do
        helperName = "c2fb_union_initializer_" & Trim(Str(serial))
        serial += 1
    Loop While CBodyProcedureIndex(translation, helperName) >= 0 OrElse _
               CBodyExternalObjectIndex(translation, helperName) >= 0
    translation.body.generatedAggregateInitializerSerial = serial

    context.generatedAggregateInitializerHelpers &= _
        "' C union initialization selects this member explicitly" & Chr(10) & _
        "Private Function " & helperName & _
        " (ByVal c2fb_union_value As " & fieldType & ") As " & unionType & Chr(10) & _
        "    Dim c2fb_union_result As " & unionType & Chr(10) & _
        "    c2fb_union_result." & freeBasicFieldName & _
        " = c2fb_union_value" & Chr(10) & _
        "    Return c2fb_union_result" & Chr(10) & _
        "End Function" & Chr(10) & Chr(10)
    Return -1
End Function

Private Function AppendSimpleBodyRecordInitializerHelper( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef context As SimpleBodyContext, _
    ByVal recordType As String, _
    ByVal parameters As String, ByVal assignments As String, _
    ByRef helperName As String _
) As Long
    helperName = ""
    If recordType = "" OrElse parameters = "" OrElse assignments = "" OrElse _
       Left(recordType, 6) = "Const " Then Return 0

    Dim serial As Long = translation.body.generatedAggregateInitializerSerial
    Do
        helperName = "c2fb_record_initializer_" & Trim(Str(serial))
        serial += 1
    Loop While CBodyProcedureIndex(translation, helperName) >= 0 OrElse _
               CBodyExternalObjectIndex(translation, helperName) >= 0
    translation.body.generatedAggregateInitializerSerial = serial

    context.generatedAggregateInitializerHelpers &= _
        "' C record initialization assigns union members through their active view" & Chr(10) & _
        "Private Function " & helperName & " (" & parameters & _
        ") As " & recordType & Chr(10) & _
        "    Dim c2fb_record_result As " & recordType & Chr(10) & _
        assignments & _
        "    Return c2fb_record_result" & Chr(10) & _
        "End Function" & Chr(10) & Chr(10)
    Return -1
End Function

Private Function TranslateSimpleRecordInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal initializerText As String, _
    ByVal freeBasicType As String, _
    ByRef translatedValue As String, _
    ByRef context As SimpleBodyContext, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, _
    parameterTypes() As String, ByVal parameterCount As Long, _
    ByVal requireConstant As Long = 0 _
) As Long
    translatedValue = ""
    initializerText = Trim(initializerText)
    If Left(initializerText, 1) <> "{" OrElse Right(initializerText, 1) <> "}" Then Return 0

    Dim sourceRecord As String = CBodyRecordName( _
        translation, catalog, freeBasicType)
    If sourceRecord = "" OrElse CBodyRecordSizeIsKnown( _
        translation, catalog, sourceRecord) = 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-static-record-type", _
            "unsupported static record type " & freeBasicType)
        Return 0
    End If
    Dim sourceRecordKind As C2fbRecordKind = _
        CBodySourceRecordKindInContext(translation, sourceRecord)
    Dim initializerBody As String = Trim(Mid(initializerText, 2, Len(initializerText) - 2))
    If Right(initializerBody, 1) = "," Then _
        initializerBody = RTrim(Left(initializerBody, Len(initializerBody) - 1))

    Dim values() As String
    Dim valueCount As Long = 0
    If initializerBody <> "" Then
        If SplitSimpleCList(initializerBody, values(), valueCount) = 0 Then Return 0
    End If

    Dim isDesignatedInitializer As Long = 0
    For valueIndex As Long = 0 To valueCount - 1
        If Left(Trim(values(valueIndex)), 1) = "." Then _
            isDesignatedInitializer = -1
    Next
    Dim designatedValues() As String
    Dim designatedFields() As Long
    Dim sourceFieldIndexes() As Long
    Dim sourceFieldCount As Long = 0
    If isDesignatedInitializer <> 0 Then
        If translation.body.recordFieldCount <= 0 Then Return 0
        ReDim designatedValues(0 To translation.body.recordFieldCount - 1)
        ReDim designatedFields(0 To translation.body.recordFieldCount - 1)
        ReDim sourceFieldIndexes(0 To translation.body.recordFieldCount - 1)
        For cField As Long = 0 To translation.body.recordFieldCount - 1
            If translation.body.recordFields(cField).ownerName <> sourceRecord Then Continue For
            sourceFieldIndexes(sourceFieldCount) = cField
            sourceFieldCount += 1
        Next
        If sourceFieldCount = 0 Then Return 0
        Dim nextFieldOrdinal As Long = 0
        Dim previousFieldOrdinal As Long = -1
        Dim hasInitializerReordering As Long = 0
        For valueIndex As Long = 0 To valueCount - 1
            Dim fieldOrdinal As Long
            Dim fieldInitializer As String
            Dim fieldName As String
            If Left(Trim(values(valueIndex)), 1) = "." Then
                If ParseSimpleCMemberDesignator( _
                    values(valueIndex), fieldName, fieldInitializer) = 0 Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-record-designator", _
                        "unsupported record member designator: " & _
                        Left(values(valueIndex), 120))
                    Return 0
                End If
                fieldOrdinal = -1
                For sourceFieldOrdinal As Long = 0 To sourceFieldCount - 1
                    Dim sourceFieldIndex As Long = _
                        sourceFieldIndexes(sourceFieldOrdinal)
                    If translation.body.recordFields(sourceFieldIndex).name = fieldName Then
                        fieldOrdinal = sourceFieldOrdinal
                        Exit For
                    End If
                Next
                If fieldOrdinal < 0 Then
                    RecordSimpleBodyFailure( _
                        context, "unknown-record-designator", _
                        "unknown record member designator " & sourceRecord & "." & _
                        fieldName)
                    Return 0
                End If
                nextFieldOrdinal = fieldOrdinal + 1
            Else
                If nextFieldOrdinal >= sourceFieldCount Then
                    RecordSimpleBodyFailure( _
                        context, "record-initializer-field-count-mismatch", _
                        "too many values in record initializer " & sourceRecord)
                    Return 0
                End If
                fieldOrdinal = nextFieldOrdinal
                fieldInitializer = Trim(values(valueIndex))
                nextFieldOrdinal += 1
            End If
            If previousFieldOrdinal >= 0 AndAlso _
               fieldOrdinal < previousFieldOrdinal Then _
                hasInitializerReordering = -1
            previousFieldOrdinal = fieldOrdinal
            Dim fieldIndex As Long = sourceFieldIndexes(fieldOrdinal)
            If designatedFields(fieldIndex) <> 0 Then
                Dim mergedInitializer As String
                If MergeSimpleCMemberDesignatorInitializers( _
                    designatedValues(fieldIndex), fieldInitializer, _
                    mergedInitializer) = 0 Then
                    RecordSimpleBodyFailure( _
                        context, "duplicate-record-initializer-field", _
                        "record field initialized more than once: " & sourceRecord & "." & _
                        translation.body.recordFields(fieldIndex).name)
                    Return 0
                End If
                designatedValues(fieldIndex) = mergedInitializer
            Else
                designatedValues(fieldIndex) = fieldInitializer
                designatedFields(fieldIndex) = -1
            End If
        Next
        If hasInitializerReordering <> 0 Then
            For cField As Long = 0 To translation.body.recordFieldCount - 1
                If designatedFields(cField) = 0 Then Continue For
                If SimpleInitializerMayHaveSideEffects( _
                    designatedValues(cField)) <> 0 Then
                    RecordSimpleBodyFailure( _
                        context, "designated-initializer-order-not-preserved", _
                        "cannot preserve side effects in reordered record initializer " & _
                        sourceRecord)
                    Return 0
                End If
            Next
        End If
    End If

    Dim fieldCount As Long = 0
    Dim resultFields As String = ""
    Dim recordHelperParameters As String = ""
    Dim recordHelperArguments As String = ""
    Dim recordHelperAssignments As String = ""
    Dim hasUnionStorageField As Long = 0
    Dim canUseRecordHelper As Long = -1
    If sourceRecord = "FuncDef.u" Then
        If LookupFbBodyField( _
            translation, catalog, freeBasicType, "pHash") < 0 OrElse _
           valueCount > 1 OrElse (valueCount = 1 AndAlso Trim(values(0)) <> "0") Then Return 0
        ' The port's union members are opaque pointers, and this source table
        ' explicitly initializes the union to null before insertion.
        Dim unionValue As String = "Cast(Any Ptr, 0)"
        translatedValue = "Type<" & freeBasicType & ">( " & unionValue & " )"
        Return -1
    End If
    If sourceRecordKind = C2FB_RECORD_UNION Then
        If valueCount > 1 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-static-union-initializer", _
                "C union initializer must select its first member: " & sourceRecord)
            Return 0
        End If
        Dim firstField As Long = -1
        Dim firstUnionMember As Long = -1
        Dim selectedFieldCount As Long = 0
        For cField As Long = 0 To translation.body.recordFieldCount - 1
            If translation.body.recordFields(cField).ownerName = sourceRecord Then
                If firstField < 0 Then
                    firstField = cField
                    firstUnionMember = cField
                End If
                If isDesignatedInitializer <> 0 Then
                    If designatedFields(cField) <> 0 Then
                        firstField = cField
                        selectedFieldCount += 1
                    End If
                ElseIf firstField = cField Then
                    Exit For
                End If
            End If
        Next
        If isDesignatedInitializer <> 0 AndAlso selectedFieldCount <> 1 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-union-designator", _
                "union initializer must select exactly one member: " & sourceRecord)
            Return 0
        End If
        If firstField < 0 OrElse _
           translation.body.recordFields(firstField).isArray <> 0 Then
            RecordSimpleBodyFailure( _
                context, "unsupported-static-union-initializer", _
                "unsupported first C union member: " & sourceRecord)
            Return 0
        End If
        Dim fieldName As String = translation.body.recordFields(firstField).name
        Dim fieldType As String = CBodyFieldType( _
            translation, catalog, firstField)
        Dim fbFieldIndex As Long = LookupFbBodyField( _
            translation, catalog, freeBasicType, fieldName, firstField)
        If fieldType = "" OrElse fbFieldIndex < 0 Then Return 0
        Dim nestedRecord As String
        If InStr(fieldType, " Ptr") = 0 AndAlso _
           IsCBodyCallbackTypeInContext(translation, fieldType) = 0 Then
            nestedRecord = CBodyRecordName(translation, catalog, fieldType)
        End If
        If SimpleArithmeticKind(catalog, fieldType) = 0 AndAlso _
           InStr(fieldType, " Ptr") = 0 AndAlso _
           IsCBodyCallbackTypeInContext(translation, fieldType) = 0 AndAlso _
           nestedRecord = "" Then Return 0

        Dim unionFieldValue As String
        Dim unionInitializer As String
        If valueCount > 0 Then
            If isDesignatedInitializer <> 0 Then
                unionInitializer = designatedValues(firstField)
            Else
                unionInitializer = values(0)
            End If
        End If
        If valueCount = 0 Then
            If nestedRecord <> "" Then
                If TranslateSimpleRecordInitializer( _
                    translation, catalog, "{}", fieldType, unionFieldValue, _
                    context, callableNames(), callableCount, parameterNames(), _
                    parameterAliases(), parameterTypes(), parameterCount, _
                    requireConstant) = 0 Then _
                    Return 0
            ElseIf InStr(fieldType, " Ptr") <> 0 OrElse _
                   IsCBodyCallbackTypeInContext(translation, fieldType) <> 0 Then
                unionFieldValue = "CPtr(" & fieldType & ", 0)"
            Else
                unionFieldValue = "Cast(" & fieldType & ", 0)"
            End If
        ElseIf nestedRecord <> "" Then
            If TranslateSimpleNestedRecordValue( _
                translation, catalog, unionInitializer, fieldType, unionFieldValue, _
                context, callableNames(), callableCount, parameterNames(), _
                parameterAliases(), parameterTypes(), parameterCount, _
                requireConstant) = 0 Then _
                Return 0
        Else
            Dim hasBoolean As Long
            Dim hasFunctionCall As Long
            If TranslateSimpleExpression( _
                translation, catalog, unionInitializer, unionFieldValue, hasBoolean, _
                hasFunctionCall, callableNames(), callableCount, _
                parameterNames(), parameterAliases(), parameterTypes(), _
                parameterCount, fieldType, -1, requireConstant) = 0 OrElse _
               hasFunctionCall <> 0 Then _
                Return 0
        End If
        If ConvertSimpleRecordInitializerFieldStorage( _
            translation, catalog, sourceRecord, fieldName, fieldType, _
            fbFieldIndex, unionFieldValue, context) = 0 Then Return 0
        If isDesignatedInitializer <> 0 Then
            If requireConstant <> 0 Then
                If firstField <> firstUnionMember Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-static-designated-union-initializer", _
                        "static C union initializer selects a non-first member: " & _
                        sourceRecord)
                    Return 0
                End If
                translatedValue = "Type<" & freeBasicType & ">( " & _
                    unionFieldValue & " )"
                Return -1
            End If
            Dim helperName As String
            Dim unionStorageType As String = NormalizeFbBodyType( _
                catalog, catalog.bodyFields(fbFieldIndex).typeName)
            If unionStorageType = "" Then Return 0
            If AppendSimpleBodyUnionInitializerHelper( _
                translation, catalog, context, freeBasicType, unionStorageType, _
                catalog.bodyFields(fbFieldIndex).name, helperName) = 0 Then _
                Return 0
            translatedValue = helperName & "(" & unionFieldValue & ")"
        Else
            translatedValue = "Type<" & freeBasicType & ">( " & _
                unionFieldValue & " )"
        End If
        Return -1
    End If
    For cField As Long = 0 To translation.body.recordFieldCount - 1
        If translation.body.recordFields(cField).ownerName <> sourceRecord Then Continue For
        Dim isArrayField As Long = translation.body.recordFields(cField).isArray
        Dim fieldType As String
        If isArrayField <> 0 Then
            fieldType = CBodyFieldType(translation, catalog, cField, 0)
        Else
            fieldType = CBodyFieldType(translation, catalog, cField)
        End If
        Dim nestedRecord As String
        Dim fbFieldIndex As Long = LookupFbBodyField( _
            translation, catalog, freeBasicType, _
            translation.body.recordFields(cField).name, cField)
        Dim isFuncDefUnionStorage As Long = 0
        If sourceRecord = "FuncDef" AndAlso _
           translation.body.recordFields(cField).name = "u" AndAlso fbFieldIndex >= 0 AndAlso _
           NormalizeFbBodyType( _
               catalog, _
               catalog.bodyFields(fbFieldIndex).typeName) = "Any Ptr" Then
            ' The C union is represented by the same pointer-sized slot in
            ' this port so static table initialization stays a scalar value.
            fieldType = "Any Ptr"
            isFuncDefUnionStorage = -1
        ElseIf InStr(fieldType, " Ptr") = 0 AndAlso _
               IsCBodyCallbackTypeInContext(translation, fieldType) = 0 Then
            nestedRecord = CBodyRecordName( _
                translation, catalog, fieldType)
        End If
        If SimpleArithmeticKind(catalog, fieldType) = 0 AndAlso _
           InStr(fieldType, " Ptr") = 0 AndAlso _
           IsCBodyCallbackTypeInContext(translation, fieldType) = 0 AndAlso nestedRecord = "" Then
            RecordSimpleBodyFailure( _
                context, "unsupported-static-record-field-type", _
                "unsupported static record field type " & sourceRecord & "." & _
                translation.body.recordFields(cField).name & " As " & fieldType)
            Return 0
        End If
        If LookupFbBodyField( _
            translation, catalog, freeBasicType, _
            translation.body.recordFields(cField).name, cField) < 0 Then
            RecordSimpleBodyFailure( _
                context, "missing-mapped-static-record-field", _
                "missing mapped static record field " & freeBasicType & "." & _
                translation.body.recordFields(cField).name)
            Return 0
        End If
        If isArrayField <> 0 AndAlso _
           catalog.bodyFields(fbFieldIndex).isArray = 0 Then
            RecordSimpleBodyFailure( _
                context, "record-array-field-layout-mismatch", _
                "C array member is not an array in the FreeBASIC layout: " & _
                sourceRecord & "." & translation.body.recordFields(cField).name)
            Return 0
        End If
        Dim fieldValue As String
        Dim fieldInitializer As String
        Dim hasFieldInitializer As Long = 0
        If isDesignatedInitializer <> 0 Then
            If designatedFields(cField) <> 0 Then
                fieldInitializer = designatedValues(cField)
                hasFieldInitializer = -1
            End If
        ElseIf fieldCount < valueCount Then
            fieldInitializer = values(fieldCount)
            hasFieldInitializer = -1
        End If
        If isArrayField <> 0 Then
            canUseRecordHelper = 0
            Dim arrayInitializer As String
            If hasFieldInitializer <> 0 Then arrayInitializer = fieldInitializer
            If TranslateSimpleRecordArrayInitializer( _
                translation, catalog, arrayInitializer, fieldType, _
                translation.body.recordFields(cField).arrayBound, _
                sourceRecord & "." & translation.body.recordFields(cField).name, _
                fieldValue, context, callableNames(), callableCount, _
                parameterNames(), parameterAliases(), parameterTypes(), _
                parameterCount, requireConstant) = 0 Then Return 0
        ElseIf hasFieldInitializer <> 0 Then
            If isFuncDefUnionStorage <> 0 Then
                Dim funcDefUnionInitializer As String = Trim(fieldInitializer)
                If funcDefUnionInitializer <> "{0}" AndAlso _
                   funcDefUnionInitializer <> "{}" Then Return 0
                fieldValue = "Cast(Any Ptr, 0)"
            ElseIf nestedRecord <> "" Then
                If TranslateSimpleNestedRecordValue(translation, catalog, _
                    fieldInitializer, fieldType, fieldValue, context, _
                    callableNames(), callableCount, parameterNames(), parameterAliases(), _
                    parameterTypes(), parameterCount, requireConstant) = 0 Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-nested-record-value-initializer", _
                        "unsupported nested record initializer " & sourceRecord & "." & _
                        translation.body.recordFields(cField).name & " from " & fieldInitializer)
                    Return 0
                End If
			ElseIf IsCBodyCallbackTypeInContext(translation, fieldType) <> 0 Then
				If TranslateCBodyCallbackInitializer( _
				    translation, catalog, fieldInitializer, fieldType, fieldValue) = 0 Then
					RecordSimpleBodyFailure( _
					    context, "unsupported-static-record-callback-initializer", _
					    "unsupported static callback initializer " & sourceRecord & "." & _
					    translation.body.recordFields(cField).name & " from " & fieldInitializer)
					Return 0
				End If
            Else
                Dim hasBoolean As Long
                Dim hasFunctionCall As Long
                If TranslateSimpleExpression(translation, catalog, _
                                             fieldInitializer, fieldValue, hasBoolean, hasFunctionCall, _
                                             callableNames(), callableCount, parameterNames(), _
                                             parameterAliases(), parameterTypes(), parameterCount, _
                                             fieldType, -1, requireConstant) = 0 OrElse _
                   hasFunctionCall <> 0 Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-static-record-initializer", _
                        "unsupported static record initializer " & sourceRecord & "." & _
                        translation.body.recordFields(cField).name & " from " & fieldInitializer)
                    Return 0
                End If
            End If
        Else
            If nestedRecord <> "" Then
                If TranslateSimpleRecordInitializer(translation, catalog, "{}", fieldType, fieldValue, context, _
                    callableNames(), callableCount, parameterNames(), parameterAliases(), _
                    parameterTypes(), parameterCount, requireConstant) = 0 Then Return 0
            ElseIf IsCBodyCallbackTypeInContext(translation, fieldType) <> 0 Then
                fieldValue = "CPtr(" & fieldType & ", 0)"
            Else
                fieldValue = "Cast(" & fieldType & ", 0)"
            End If
        End If
        If freeBasicType = "SqliteFuncDef" AndAlso _
           translation.body.recordFields(cField).name = "funcFlags" Then
            ' SqliteFuncDef keeps C's alignment padding as an explicit field.
            ' C aggregate initialization omits this unnamed padding member.
            If resultFields <> "" Then resultFields &= ", "
            resultFields &= "Cast(u16, 0)"
            canUseRecordHelper = 0
        End If
        If ConvertSimpleRecordInitializerFieldStorage( _
            translation, catalog, sourceRecord, _
            translation.body.recordFields(cField).name, fieldType, _
            fbFieldIndex, fieldValue, context) = 0 Then Return 0
        If resultFields <> "" Then resultFields &= ", "
        resultFields &= fieldValue
        Dim freeBasicFieldName As String = _
            catalog.bodyFields(fbFieldIndex).name
        If freeBasicFieldName = "" OrElse _
           IsIdentifierToken(freeBasicFieldName) = 0 Then
            canUseRecordHelper = 0
        Else
            Dim helperArgumentName As String = _
                "c2fb_record_value_" & Trim(Str(fieldCount))
            If recordHelperParameters <> "" Then
                recordHelperParameters &= ", "
                recordHelperArguments &= ", "
            End If
            recordHelperParameters &= "ByVal " & helperArgumentName & _
                " As " & fieldType
            recordHelperArguments &= fieldValue
            recordHelperAssignments &= _
                "    c2fb_record_result." & freeBasicFieldName & _
                " = " & helperArgumentName & Chr(10)
            If catalog.bodyFields(fbFieldIndex).isUnionMember <> 0 Then _
                hasUnionStorageField = -1
        End If
        fieldCount += 1
    Next
    Dim initializerFieldCount As Long = valueCount
    If isDesignatedInitializer <> 0 Then
        initializerFieldCount = 0
        For cField As Long = 0 To translation.body.recordFieldCount - 1
            If designatedFields(cField) <> 0 Then initializerFieldCount += 1
        Next
    End If
    If fieldCount = 0 OrElse initializerFieldCount > fieldCount Then
        RecordSimpleBodyFailure( _
            context, "static-record-field-count-mismatch", _
            "static record field count mismatch " & sourceRecord & _
            ": source fields=" & Trim(Str(fieldCount)) & _
            ", initializer values=" & Trim(Str(initializerFieldCount)))
        Return 0
    End If
    If hasUnionStorageField <> 0 AndAlso canUseRecordHelper <> 0 AndAlso _
       requireConstant = 0 Then
        Dim helperName As String
        If AppendSimpleBodyRecordInitializerHelper( _
            translation, catalog, context, freeBasicType, _
            recordHelperParameters, recordHelperAssignments, helperName) = 0 Then _
            Return 0
        translatedValue = helperName & "(" & recordHelperArguments & ")"
    Else
        translatedValue = "Type<" & freeBasicType & ">( " & resultFields & " )"
    End If
    Return -1
End Function

Private Function NormalizeCInlineRecordArrayDeclaration( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef context As SimpleBodyContext, _
    ByVal declarationText As String _
) As String
    ' Reuse mapped C tags, or synthesize a private FreeBASIC type for a simple
    ' anonymous local record before ordinary declaration lowering runs.
    Dim structureStart As Long = InStr(declarationText, "struct ")
    Dim unionStart As Long = InStr(declarationText, "union ")
    Dim recordStart As Long = structureStart
    Dim recordKeyword As String = "struct "
    Dim recordKind As C2fbRecordKind = C2FB_RECORD_STRUCT
    If unionStart > 0 AndAlso (recordStart = 0 OrElse unionStart < recordStart) Then
        recordStart = unionStart
        recordKeyword = "union "
        recordKind = C2FB_RECORD_UNION
    End If
    If recordStart = 0 Then
        Dim aliasStart As Long = 1
        SkipCSpaces(declarationText, aliasStart)
        Dim aliasEnd As Long = aliasStart
        While aliasEnd <= Len(declarationText) AndAlso _
              IsIdentifierPart(Asc(declarationText, aliasEnd)) <> 0
            aliasEnd += 1
        Wend
        If aliasEnd > aliasStart Then
            Dim aliasType As String = SimpleBodyLocalRecordType( _
                context, Mid(declarationText, aliasStart, aliasEnd - aliasStart))
            Dim objectDeclaration As String = Trim(Mid(declarationText, aliasEnd))
            If aliasType <> "" AndAlso objectDeclaration <> "" Then _
                Return aliasType & " " & objectDeclaration
        End If
        Return declarationText
    End If
    Dim tagStart As Long = recordStart + Len(recordKeyword)
    Dim tagEnd As Long = tagStart
    While tagEnd <= Len(declarationText) AndAlso _
          IsIdentifierPart(Asc(declarationText, tagEnd)) <> 0
        tagEnd += 1
    Wend
    If tagEnd = tagStart Then
        Dim anonymousBrace As Long = tagEnd
        While anonymousBrace <= Len(declarationText) AndAlso _
              IsSpaceByte(Asc(declarationText, anonymousBrace)) <> 0
            anonymousBrace += 1
        Wend
        If anonymousBrace > Len(declarationText) OrElse _
           Asc(declarationText, anonymousBrace) <> Asc("{") Then _
            Return declarationText
        Dim recordEnd As Long = anonymousBrace
        If ScanSimpleExpressionGroup( _
            declarationText, recordEnd, Asc("{"), Asc("}")) = 0 Then _
            Return declarationText
        Dim recordBody As String = Mid( _
            declarationText, anonymousBrace + 1, _
            recordEnd - anonymousBrace - 2)
        Dim objectDeclaration As String = Trim(Mid(declarationText, recordEnd))
        If objectDeclaration = "" Then Return declarationText
        Dim objectParts() As String
        Dim objectPartCount As Long
        If SplitSimpleCList( _
            objectDeclaration, objectParts(), objectPartCount) = 0 OrElse _
           objectPartCount <> 1 Then Return declarationText
        Dim generatedRecordName As String
        If RegisterCBodyAnonymousLocalRecord( _
            translation, catalog, recordBody, generatedRecordName, _
            recordKind) = 0 Then _
            Return declarationText
        Dim prefix As String = Trim(Left(declarationText, recordStart - 1))
        If prefix <> "" Then prefix &= " "
        Return prefix & generatedRecordName & " " & objectDeclaration
    End If

    Dim localTagEnd As Long = tagEnd
    While localTagEnd <= Len(declarationText) AndAlso _
          IsSpaceByte(Asc(declarationText, localTagEnd)) <> 0
        localTagEnd += 1
    Wend
    If localTagEnd <= Len(declarationText) AndAlso _
       Asc(declarationText, localTagEnd) = Asc("{") Then
        Dim recordEnd As Long = localTagEnd
        If ScanSimpleExpressionGroup( _
            declarationText, recordEnd, Asc("{"), Asc("}")) = 0 Then _
            Return declarationText
        Dim recordBody As String = Mid( _
            declarationText, localTagEnd + 1, _
            recordEnd - localTagEnd - 2)
        Dim objectDeclaration As String = Trim(Mid(declarationText, recordEnd))
        If objectDeclaration = "" Then Return declarationText
        Dim objectParts() As String
        Dim objectPartCount As Long
        If SplitSimpleCList( _
            objectDeclaration, objectParts(), objectPartCount) = 0 OrElse _
           objectPartCount <> 1 Then Return declarationText
        Dim generatedRecordName As String
        If RegisterCBodyAnonymousLocalRecord( _
            translation, catalog, recordBody, generatedRecordName, _
            recordKind) = 0 OrElse _
           RegisterSimpleBodyLocalRecordTag( _
               context, Mid(declarationText, tagStart, tagEnd - tagStart), _
               generatedRecordName) = 0 Then Return declarationText
        Dim prefix As String = Trim(Left(declarationText, recordStart - 1))
        If prefix <> "" Then prefix &= " "
        Return prefix & generatedRecordName & " " & objectDeclaration
    End If

    Dim fbRecord As String = SimpleBodyLocalRecordType( _
        context, Mid(declarationText, tagStart, tagEnd - tagStart))
    If fbRecord = "" Then
        fbRecord = MappedFbBodyRecord( _
            translation, catalog, _
            Mid(declarationText, tagStart, tagEnd - tagStart))
    End If
    If fbRecord = "" Then Return declarationText
    While tagEnd <= Len(declarationText) AndAlso IsSpaceByte(Asc(declarationText, tagEnd)) <> 0
        tagEnd += 1
    Wend
    If tagEnd > Len(declarationText) Then Return declarationText
    If Asc(declarationText, tagEnd) <> Asc("{") Then
        ' Also normalize a separate file-scope record type used by a local
        ' variable, for example "struct unixFile *pFile". Pointer and array
        ' operators remain in the declarator for the ordinary C type parser.
        Dim objectDeclaration As String = Trim(Mid(declarationText, tagEnd))
        If objectDeclaration = "" Then Return declarationText
        Dim prefix As String = Trim(Left(declarationText, recordStart - 1))
        If prefix <> "" Then prefix &= " "
        Return prefix & fbRecord & " " & objectDeclaration
    End If
    Dim recordEnd As Long = tagEnd
    If ScanSimpleExpressionGroup(declarationText, recordEnd, Asc("{"), Asc("}")) = 0 Then _
        Return declarationText
    Dim objectDeclaration As String = Trim(Mid(declarationText, recordEnd))
    If objectDeclaration = "" Then Return declarationText
    Dim prefix As String = Trim(Left(declarationText, recordStart - 1))
    If prefix <> "" Then prefix &= " "
    Return prefix & fbRecord & " " & objectDeclaration
End Function

Private Function IsSupportedSimpleStaticPointerInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, _
    parameterNames() As String, ByVal symbolCount As Long, _
    ByVal variableType As String _
) As Long
    sourceExpression = GenericMacroStripOuterParentheses(Trim(sourceExpression))
    If sourceExpression = "" Then Return 0
    If GenericNullPointerConstantText(sourceExpression) <> 0 Then Return -1
    If Left(sourceExpression, 1) = Chr(34) Then Return -1

    Dim targetPointeeType As String = SimplePointeeType(variableType)
    If targetPointeeType = "Byte" OrElse targetPointeeType = "UByte" OrElse _
       targetPointeeType = "ZString" OrElse targetPointeeType = "Const ZString" OrElse _
       targetPointeeType = "c2fb_c_char" OrElse _
       targetPointeeType = "c2fb_c_uchar" Then
        Dim stringPosition As Long = InStr(sourceExpression, Chr(34))
        If stringPosition > 0 Then
            Dim prefix As String = Trim(Left(sourceExpression, stringPosition - 1))
            Dim stringValues As String
            Dim stringByteCount As Long
            If Left(prefix, 1) = "(" AndAlso Right(prefix, 1) = ")" AndAlso _
               ReadSimpleStaticStringArray( _
                   Mid(sourceExpression, stringPosition), _
                   stringValues, stringByteCount) <> 0 Then
                Dim typeText As String = Trim( _
                    Mid(prefix, 2, Len(prefix) - 2))
                Dim typeTokens() As String
                Dim typeTokenCount As Long
                TokenizeCDeclaration(typeText, typeTokens(), typeTokenCount)
                Dim sawCharacterType As Long = 0
                Dim castPointerCount As Long = 0
                Dim validCastType As Long = CInt(typeTokenCount > 0)
                For typeIndex As Long = 0 To typeTokenCount - 1
                    Select Case typeTokens(typeIndex)
                    Case "char"
                        sawCharacterType = -1
                    Case "signed", "unsigned", "const", "volatile", _
                         "restrict", "__restrict", "__restrict__"
                    Case "*"
                        castPointerCount += 1
                    Case Else
                        validCastType = 0
                    End Select
                Next
                If validCastType <> 0 AndAlso sawCharacterType <> 0 AndAlso _
                   castPointerCount = 1 Then Return -1
            End If
        End If
    End If

    Dim isAddress As Long = 0
    If Left(sourceExpression, 1) = "&" Then
        isAddress = -1
        sourceExpression = GenericMacroStripOuterParentheses( _
            Trim(Mid(sourceExpression, 2)))
    End If
    If IsIdentifierToken(sourceExpression) = 0 Then Return 0

    For i As Long = 0 To symbolCount - 1
        If parameterNames(i) = sourceExpression Then Return 0
    Next

    Dim externalObjectIndex As Long = _
        CBodyExternalObjectIndex(translation, sourceExpression)
    If externalObjectIndex >= 0 Then
        If isAddress <> 0 Then Return -1
        If translation.body.externalObjects(externalObjectIndex).isArray <> 0 Then _
            Return -1
        Return 0
    End If
    Return 0
End Function

Private Function IsSupportedSimpleStaticCallbackInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, _
    parameterNames() As String, ByVal symbolCount As Long, _
    callableNames() As String, ByVal callableCount As Long _
) As Long
    sourceExpression = GenericMacroStripOuterParentheses(Trim(sourceExpression))
    If sourceExpression = "" Then Return 0
    If GenericNullPointerConstantText(sourceExpression) <> 0 Then Return -1

    If Left(sourceExpression, 1) = "&" Then
        sourceExpression = GenericMacroStripOuterParentheses( _
            Trim(Mid(sourceExpression, 2)))
    End If
    If IsIdentifierToken(sourceExpression) = 0 Then Return 0

    For i As Long = 0 To symbolCount - 1
        If parameterNames(i) = sourceExpression Then Return 0
    Next
    If C2fbNameListContains( _
        callableNames(), callableCount, sourceExpression) <> 0 OrElse _
       CBodyProcedureIndex(translation, sourceExpression) >= 0 Then Return -1
    Return 0
End Function

Private Function ParseSimpleArrayDesignator( _
    ByRef translation As C2fbTranslationContext, _
    ByVal initializerText As String, _
    ByRef elementIndex As Long, _
    ByRef elementValue As String _
) As Long
    initializerText = Trim(initializerText)
    elementValue = initializerText
    If Left(initializerText, 1) <> "[" Then Return 0

    Dim bracketEnd As Long = 1
    If ScanSimpleExpressionGroup( _
        initializerText, bracketEnd, Asc("["), Asc("]")) = 0 Then Return 0
    Dim indexExpression As String = Trim(Mid( _
        initializerText, 2, bracketEnd - 3))
    Dim position As Long = bracketEnd
    SkipCSpaces(initializerText, position)
    If position > Len(initializerText) OrElse _
       Asc(initializerText, position) <> Asc("=") Then Return 0
    position += 1
    elementValue = Trim(Mid(initializerText, position))
    If indexExpression = "" OrElse elementValue = "" Then Return 0

    If EvaluateSimpleArrayBound( _
           indexExpression, elementIndex, MAX_SIMPLE_ARRAY_COUNT, -1) = 0 AndAlso _
       EvaluateGenericBodyArrayBound( _
           translation, indexExpression, elementIndex, _
           MAX_SIMPLE_ARRAY_COUNT) = 0 Then Return 0
    If elementIndex < 0 OrElse _
       elementIndex >= MAX_SIMPLE_ARRAY_COUNT Then Return 0
    Return -1
End Function

Private Function NormalizeGenericBoundLocalSizeof( _
    ByVal sourceExpression As String, _
    locals() As SimpleLocalVariable, ByVal localCount As Long, _
    ByRef normalizedExpression As String _
) As Long
    normalizedExpression = ""
    Dim position As Long = 1
    While position <= Len(sourceExpression)
        If Mid(sourceExpression, position, Len("sizeof")) = "sizeof" AndAlso _
           (position = 1 OrElse _
            IsIdentifierPart(Asc(sourceExpression, position - 1)) = 0) Then
            Dim operandPosition As Long = position + Len("sizeof")
            If operandPosition <= Len(sourceExpression) AndAlso _
               IsIdentifierPart(Asc(sourceExpression, operandPosition)) <> 0 Then
                normalizedExpression &= Mid(sourceExpression, position, 1)
                position += 1
                Continue While
            End If
            SkipCSpaces(sourceExpression, operandPosition)
            Dim operandName As String
            Dim operandEnd As Long = operandPosition
            If operandPosition <= Len(sourceExpression) AndAlso _
               Asc(sourceExpression, operandPosition) = Asc("(") Then
                If ScanSimpleExpressionGroup( _
                    sourceExpression, operandEnd, Asc("("), Asc(")")) <> 0 Then
                    Dim operandText As String = Trim(Mid( _
                        sourceExpression, operandPosition + 1, _
                        operandEnd - operandPosition - 2))
                    If IsIdentifierToken(operandText) <> 0 Then _
                        operandName = operandText
                End If
            ElseIf operandPosition <= Len(sourceExpression) AndAlso _
                   IsIdentifierStart(Asc(sourceExpression, operandPosition)) <> 0 Then
                operandEnd += 1
                While operandEnd <= Len(sourceExpression) AndAlso _
                      IsIdentifierPart(Asc(sourceExpression, operandEnd)) <> 0
                    operandEnd += 1
                Wend
                operandName = Mid( _
                    sourceExpression, operandPosition, _
                    operandEnd - operandPosition)
            End If
            If operandName <> "" Then
                For localIndex As Long = localCount - 1 To 0 Step -1
                    If locals(localIndex).name <> operandName OrElse _
                       locals(localIndex).hasKnownSize = 0 Then Continue For
                    normalizedExpression &= Trim(Str( _
                        locals(localIndex).sizeofBytes))
                    position = operandEnd
                    Exit For
                Next
                If position = operandEnd Then Continue While
            End If
        End If
        normalizedExpression &= Mid(sourceExpression, position, 1)
        position += 1
    Wend
    Return -1
End Function

Private Function TranslateSingleDeclarationStatementWithTokens( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, _
    sourceTokens() As String, ByVal sourceTokenCount As Long, _
    ByRef outputText As String, _
    ByRef context As SimpleBodyContext, _
    locals() As SimpleLocalVariable, _
    ByRef localCount As Long, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByRef symbolCount As Long _
) As Long
    outputText = ""
    If Len(statementText) = 0 Then Return 0
    Dim equalsPosition As Long = FindSimpleAssignmentEquals(statementText)
    Dim declarationText As String = statementText
    If equalsPosition > 0 Then declarationText = Trim(Left(statementText, equalsPosition - 1))
    Dim originalDeclarationText As String = declarationText
    declarationText = NormalizeCInlineRecordArrayDeclaration( _
        translation, catalog, context, declarationText)

    Dim tokens() As String
    ' Array bounds and initializers can use the full declaration token limit.
    If sourceTokenCount < 2 OrElse _
       sourceTokenCount >= MAX_C_DECLARATION_TOKENS Then Return 0
    ReDim tokens(0 To sourceTokenCount - 1)
    For tokenIndex As Long = 0 To sourceTokenCount - 1
        tokens(tokenIndex) = sourceTokens(tokenIndex)
    Next
    ' The original statement keeps its initializer text. Only declarator tokens
    ' enter the bounded type and name analysis buffers.
    Dim tokenCount As Long = sourceTokenCount
    If tokens(tokenCount - 1) = ";" Then tokenCount -= 1
    Dim assignmentToken As Long
    If FindC2fbTopLevelAssignmentToken( _
        tokens(), 0, tokenCount - 1, assignmentToken) = 0 Then Return 0
    If assignmentToken >= 0 Then tokenCount = assignmentToken
    If declarationText <> originalDeclarationText Then
        TokenizeCDeclaration(declarationText, tokens(), tokenCount)
    End If
    If tokenCount < 2 Then Return 0
    Dim callbackResult As C2fbCallbackTypeResultModel
    ResolveCBodyCallbackTypeInContext( _
        translation, catalog, tokens(), 0, tokenCount - 1, _
        callbackResult, 0, C2FB_TYPE_MAPPING_BODY)
    If callbackResult.status = C2FB_CALLBACK_TYPE_UNSUPPORTED Then Return 0
    If callbackResult.status = C2FB_CALLBACK_TYPE_CONVERTED AndAlso _
       callbackResult.callbackName <> "" Then
        Dim callbackName As String = callbackResult.callbackName
        Dim callbackType As String = callbackResult.freeBasicType
        Dim isStaticCallback As Long = _
            CInt(InStr(declarationText, "static") <> 0)
        If callbackResult.pointerDepth <> 1 Then Return 0
        Dim callbackInitializer As String = ""
        If equalsPosition > 0 Then
            callbackInitializer = Trim(Mid(statementText, equalsPosition + 1))
            If isStaticCallback <> 0 AndAlso _
               IsSupportedSimpleStaticCallbackInitializer( _
                   translation, callbackInitializer, parameterNames(), _
                   symbolCount, callableNames(), callableCount) = 0 Then
                RecordSimpleBodyFailure( _
                    context, "unsupported-static-callback-initializer", _
                    "unsupported static callback initializer: " & _
                    Left(statementText, 160))
                Return 0
            End If
            Dim translatedInitializer As String
            Dim initializerHasBoolean As Long
            Dim initializerHasCall As Long
            If TranslateSimpleExpression( _
                translation, catalog, callbackInitializer, translatedInitializer, _
                initializerHasBoolean, initializerHasCall, callableNames(), _
                callableCount, parameterNames(), parameterAliases(), _
                parameterTypes(), symbolCount, callbackType) = 0 OrElse _
               initializerHasCall <> 0 Then Return 0
            callbackInitializer = translatedInitializer
        End If
        Dim scopeStart As Long = context.scopeSymbols(context.scopeDepth - 1)
        For i As Long = scopeStart To symbolCount - 1
            If parameterNames(i) = callbackName Then Return 0
        Next
        If context.localSerial >= MAX_SIMPLE_VARIABLES OrElse _
           localCount >= MAX_SIMPLE_VARIABLES OrElse _
           symbolCount >= MAX_SIMPLE_VARIABLES Then Return 0
        locals(localCount).name = callbackName
        locals(localCount).typeName = callbackType
        locals(localCount).isConst = 0
        locals(localCount).sizeofBytes = CLng(SizeOf(Any Ptr))
        locals(localCount).hasKnownSize = -1
        locals(localCount).isIntegerConstant = 0
        locals(localCount).integerValue = 0
        localCount += 1
        parameterNames(symbolCount) = callbackName
        parameterAliases(symbolCount) = "local" & Trim(Str(context.localSerial))
        context.localSerial += 1
        parameterTypes(symbolCount) = callbackType
        If isStaticCallback <> 0 Then
            context.declarations &= "    Static " & parameterAliases(symbolCount) & _
                " As " & callbackType
            If equalsPosition > 0 Then _
                context.declarations &= " = " & callbackInitializer
            context.declarations &= Chr(10)
        Else
            context.declarations &= "    Dim " & parameterAliases(symbolCount) & _
                " As " & callbackType & Chr(10)
        End If
        symbolCount += 1
        If isStaticCallback = 0 AndAlso equalsPosition > 0 Then
            ' Automatic C initializers run when control reaches the declaration.
            ' Keep the storage declaration at procedure scope, but execute the
            ' callback assignment at its original statement position.
            outputText = parameterAliases(symbolCount - 1) & " = " & _
                callbackInitializer
        Else
            outputText = IIf( _
                isStaticCallback <> 0, "' C static callback storage", _
                "' C automatic callback storage declaration")
        End If
        Return -1
    End If
    Dim nameIndex As Long = tokenCount - 1
    Dim arrayCount As Long = 0
    Dim arrayBound As String
    Dim arrayRank As Long = 0
    Dim arrayDimensions As String
    Dim arrayDimensionBounds() As String
    Dim arrayDimensionCounts() As Long
    Dim isSymbolicArrayBound As Long = 0
    Dim isDynamicLocalArray As Long = 0
    Dim localArrayTypedefAliasIndex As Long = -1
    Dim dynamicBoundName As String
    Dim dynamicBoundExpression As String
    Dim dynamicBoundPrelude As String
    Dim isStaticLocal As Long = 0
    Dim isConstLocal As Long = 0
    Dim staticStringValues As String
    Dim staticStringByteCount As Long = 0
    Dim staticStringInitializer As Long = 0
    For i As Long = 0 To tokenCount - 1
        If tokens(i) = "static" Then isStaticLocal = -1
        If tokens(i) = "const" Then isConstLocal = -1
    Next
    Dim openingBracket As Long = -1
    For i As Long = 0 To tokenCount - 1
        If tokens(i) = "[" Then
            openingBracket = i
            Exit For
        End If
    Next
    If openingBracket >= 0 Then
        nameIndex = openingBracket - 1
        Dim bracketPosition As Long = InStr(declarationText, "[")
        Dim bracketEndPosition As Long = bracketPosition
        If bracketPosition = 0 OrElse _
           ScanSimpleExpressionGroup( _
               declarationText, bracketEndPosition, Asc("["), Asc("]")) = 0 Then _
            Return 0
        Dim countText As String = Trim(Mid( _
            declarationText, bracketPosition + 1, _
            bracketEndPosition - bracketPosition - 2))
        Dim arrayDimensionCount As Long = 0
        Dim dimensionOpenToken As Long = openingBracket
        While dimensionOpenToken < tokenCount AndAlso _
              tokens(dimensionOpenToken) = "["
            Dim closingBracketToken As Long = -1
            Dim nestedBracketDepth As Long = 0
            For tokenIndex As Long = dimensionOpenToken + 1 To tokenCount - 1
                If tokens(tokenIndex) = "[" Then
                    nestedBracketDepth += 1
                ElseIf tokens(tokenIndex) = "]" Then
                    If nestedBracketDepth = 0 Then
                        closingBracketToken = tokenIndex
                        Exit For
                    End If
                    nestedBracketDepth -= 1
                End If
            Next
            If closingBracketToken < 0 Then Return 0
            arrayDimensionCount += 1
            dimensionOpenToken = closingBracketToken + 1
        Wend
        If arrayDimensionCount <= 0 OrElse dimensionOpenToken <> tokenCount OrElse _
           arrayDimensionCount > MAX_GENERIC_ARRAY_RANK Then Return 0

        If arrayDimensionCount > 1 Then
            If isConstLocal <> 0 AndAlso isStaticLocal = 0 Then Return 0
            Dim canInferStringRowCount As Long = CInt( _
                arrayDimensionCount = 2 AndAlso isStaticLocal <> 0 AndAlso _
                equalsPosition <> 0)
            Dim hasCharElementType As Long = 0
            For typeToken As Long = 0 To openingBracket - 1
                If tokens(typeToken) = "char" Then hasCharElementType = -1
                If tokens(typeToken) = "*" Then canInferStringRowCount = 0
            Next
            canInferStringRowCount = CInt( _
                canInferStringRowCount <> 0 AndAlso hasCharElementType <> 0)
            Dim hasUnsizedLeadingDimension As Long = 0
            ReDim arrayDimensionBounds(0 To arrayDimensionCount - 1)
            ReDim arrayDimensionCounts(0 To arrayDimensionCount - 1)
            Dim dimensionTextPosition As Long = bracketPosition
            Dim totalArrayCount As LongInt = 1
            Dim hasDynamicDimension As Long = 0
            For dimensionIndex As Long = 0 To arrayDimensionCount - 1
                SkipCSpaces(declarationText, dimensionTextPosition)
                Dim dimensionEndPosition As Long = dimensionTextPosition
                If ScanSimpleExpressionGroup( _
                    declarationText, dimensionEndPosition, _
                    Asc("["), Asc("]")) = 0 Then Return 0
                Dim dimensionText As String = Trim(Mid( _
                    declarationText, dimensionTextPosition + 1, _
                    dimensionEndPosition - dimensionTextPosition - 2))
                Dim normalizedDimensionText As String
                NormalizeGenericBoundLocalSizeof( _
                    dimensionText, locals(), localCount, normalizedDimensionText)
                dimensionText = normalizedDimensionText
                Dim dimensionCount As Long
                If dimensionText = "" Then
                    If dimensionIndex <> 0 OrElse canInferStringRowCount = 0 Then Return 0
                    hasUnsizedLeadingDimension = -1
                    arrayDimensionCounts(dimensionIndex) = 0
                    arrayDimensionBounds(dimensionIndex) = ""
                ElseIf EvaluateSimpleArrayBound( _
                    dimensionText, dimensionCount, MAX_GENERIC_ARRAY_COUNT) <> 0 OrElse _
                   EvaluateGenericBodyArrayBound( _
                       translation, dimensionText, dimensionCount, _
                       MAX_GENERIC_ARRAY_COUNT) <> 0 Then
                    arrayDimensionCounts(dimensionIndex) = dimensionCount
                    arrayDimensionBounds(dimensionIndex) = _
                        Trim(Str(dimensionCount)) & "ULL"
                    If hasDynamicDimension = 0 Then
                        If totalArrayCount > _
                           (MAX_GENERIC_ARRAY_COUNT \ dimensionCount) Then Return 0
                        totalArrayCount *= dimensionCount
                    End If
                Else
                    If isStaticLocal <> 0 OrElse equalsPosition <> 0 Then Return 0
                    ' Keep multidimensional bounds free of calls and side-effect
                    ' preludes; this pass does not choose their C evaluation order.
                    Dim loweredDimensionBound As String
                    Dim dimensionHasBoolean As Long
                    Dim dimensionHasCall As Long
                    If TranslateSimpleExpression( _
                        translation, catalog, dimensionText, _
                        loweredDimensionBound, dimensionHasBoolean, _
                        dimensionHasCall, callableNames(), callableCount, _
                        parameterNames(), parameterAliases(), parameterTypes(), _
                        symbolCount, "LongInt") = 0 OrElse _
                       dimensionHasCall <> 0 Then Return 0
                    Dim dimensionBoundName As String = NewSimpleBodyTemporary( _
                        context, "LongInt", "")
                    If dimensionBoundName = "" Then Return 0
                    dynamicBoundPrelude &= dimensionBoundName & " = " & _
                        loweredDimensionBound & Chr(10) & _
                        "If " & dimensionBoundName & " <= 0 OrElse " & _
                        dimensionBoundName & " > " & _
                        Trim(Str(MAX_GENERIC_ARRAY_COUNT)) & " Then Error 5" & Chr(10)
                    arrayDimensionBounds(dimensionIndex) = dimensionBoundName
                    arrayDimensionCounts(dimensionIndex) = 0
                    hasDynamicDimension = -1
                End If
                If dimensionIndex > 0 Then arrayDimensions &= ","
                arrayDimensions &= arrayDimensionBounds(dimensionIndex)
                dimensionTextPosition = dimensionEndPosition
            Next
            arrayRank = arrayDimensionCount
            If hasDynamicDimension <> 0 Then
                If equalsPosition <> 0 Then Return 0
                dynamicBoundName = NewSimpleBodyTemporary(context, "LongInt", "")
                If dynamicBoundName = "" Then Return 0
                dynamicBoundPrelude &= dynamicBoundName & " = " & _
                    arrayDimensionBounds(0) & Chr(10)
                For dimensionIndex As Long = 1 To arrayDimensionCount - 1
                    dynamicBoundPrelude &= _
                        "If " & dynamicBoundName & " > " & _
                        Trim(Str(MAX_GENERIC_ARRAY_COUNT)) & " \ " & _
                        arrayDimensionBounds(dimensionIndex) & _
                        " Then Error 5" & Chr(10) & _
                        dynamicBoundName & " *= " & _
                        arrayDimensionBounds(dimensionIndex) & Chr(10)
                Next
                dynamicBoundExpression = ""
                arrayBound = dynamicBoundName
                isSymbolicArrayBound = -1
                isDynamicLocalArray = -1
            ElseIf hasUnsizedLeadingDimension <> 0 Then
                If arrayDimensionCounts(1) < 1 Then Return 0
                Dim initializerText As String = Trim(Mid( _
                    statementText, equalsPosition + 1))
                Dim stringRowCount As Long
                Dim stringMatrixByteCount As Long
                If ReadSimpleStaticStringMatrix( _
                    initializerText, arrayDimensionCounts(1), _
                    staticStringValues, stringRowCount, _
                    stringMatrixByteCount) = 0 Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-static-string-matrix", _
                        "unsupported static string-matrix initializer: " & _
                        Left(statementText, 160))
                    Return 0
                End If
                arrayDimensionCounts(0) = stringRowCount
                arrayDimensionBounds(0) = Trim(Str(stringRowCount)) & "ULL"
                arrayDimensions = arrayDimensionBounds(0) & "," & _
                                 arrayDimensionBounds(1)
                arrayCount = stringMatrixByteCount
                arrayBound = Trim(Str(arrayCount)) & "ULL"
                staticStringByteCount = arrayCount
                staticStringInitializer = -1
            ElseIf totalArrayCount <= MAX_SIMPLE_ARRAY_COUNT Then
                arrayCount = CLng(totalArrayCount)
                arrayBound = Trim(Str(arrayCount)) & "ULL"
            Else
                If isStaticLocal <> 0 OrElse equalsPosition <> 0 Then Return 0
                dynamicBoundExpression = Trim(Str(totalArrayCount))
                dynamicBoundName = NewSimpleBodyTemporary( _
                    context, "LongInt", "")
                If dynamicBoundName = "" Then Return 0
                arrayBound = dynamicBoundName
                isSymbolicArrayBound = -1
                isDynamicLocalArray = -1
            End If
        Else
            If countText = "" Then
                If equalsPosition = 0 Then Return 0
                Dim initializerText As String = Trim(Mid(statementText, equalsPosition + 1))
                If Left(initializerText, 1) = Chr(34) Then
                    If isStaticLocal = 0 OrElse isConstLocal = 0 Then Return 0
                    If ReadSimpleStaticStringArray(initializerText, staticStringValues, _
                                                   staticStringByteCount) = 0 Then Return 0
                    staticStringInitializer = -1
                    arrayCount = staticStringByteCount
                ElseIf Left(initializerText, 1) = "{" AndAlso Right(initializerText, 1) = "}" Then
                    Dim initialValues() As String
                    Dim initialValueCount As Long
                    Dim initialText As String = Trim(Mid(initializerText, 2, Len(initializerText) - 2))
                    If Right(initialText, 1) = "," Then _
                        initialText = RTrim(Left(initialText, Len(initialText) - 1))
                    If SplitSimpleCList(initialText, initialValues(), initialValueCount) = 0 Then Return 0
                    Dim nextElementIndex As Long = 0
                    Dim maximumElementIndex As Long = -1
                    For valueIndex As Long = 0 To initialValueCount - 1
                        Dim currentElementIndex As Long = nextElementIndex
                        Dim elementValue As String = Trim(initialValues(valueIndex))
                        If Left(elementValue, 1) = "[" Then
                            Dim designatedValue As String
                            If ParseSimpleArrayDesignator( _
                                translation, elementValue, currentElementIndex, _
                                designatedValue) = 0 Then Return 0
                        End If
                        If currentElementIndex > maximumElementIndex Then _
                            maximumElementIndex = currentElementIndex
                        nextElementIndex = currentElementIndex + 1
                    Next
                    arrayCount = maximumElementIndex + 1
                    If arrayCount <= 0 Then Return 0
                Else
                    Return 0
                End If
            Else
                Dim normalizedCountText As String
                NormalizeGenericBoundLocalSizeof( _
                    countText, locals(), localCount, normalizedCountText)
                countText = normalizedCountText
                If EvaluateSimpleArrayBound(countText, arrayCount) = 0 AndAlso _
                   EvaluateGenericBodyArrayBound( _
                       translation, countText, arrayCount) = 0 Then
                Dim oversizedConstantArrayCount As Long
                If EvaluateSimpleArrayBound( _
                    countText, oversizedConstantArrayCount, _
                    MAX_GENERIC_ARRAY_COUNT) <> 0 Then
                    If oversizedConstantArrayCount <= MAX_SIMPLE_ARRAY_COUNT OrElse _
                       equalsPosition <> 0 OrElse isStaticLocal <> 0 OrElse _
                       isConstLocal <> 0 Then Return 0
                    ' Large fixed automatic arrays use managed storage instead
                    ' of exhausting the compiler's bounded inline-array path.
                    dynamicBoundExpression = Trim(Str(oversizedConstantArrayCount))
                Else
                    Dim loweredSourceBound As String = LCase(countText)
                    If InStr(loweredSourceBound, "sizeof") = 0 AndAlso _
                       InStr(loweredSourceBound, "__builtin_offsetof") = 0 Then
                        Dim boundTokens() As String
                        Dim boundTokenCount As Long
                        TokenizeCDeclaration( _
                            countText, boundTokens(), boundTokenCount)
                        Dim hasIdentifierBound As Long = 0
                        Dim previousTokenIsInteger As Long = 0
                        For boundTokenIndex As Long = 0 To boundTokenCount - 1
                            Dim boundToken As String = boundTokens(boundTokenIndex)
                            Dim lowerBoundToken As String = LCase(boundToken)
                            If IsIdentifierToken(boundToken) <> 0 Then
                                Select Case lowerBoundToken
                                Case "u", "l", "ll", "ul", "lu", "ull", "llu"
                                    If previousTokenIsInteger <> 0 Then
                                        previousTokenIsInteger = 0
                                        Continue For
                                    End If
                                End Select
                                hasIdentifierBound = -1
                            End If
                            Dim firstBoundByte As Long = Asc(boundToken, 1)
                            previousTokenIsInteger = CInt( _
                                firstBoundByte >= Asc("0") AndAlso _
                                firstBoundByte <= Asc("9"))
                        Next
                        If hasIdentifierBound = 0 Then Return 0
                    End If
                    ' A C VLA evaluates its bound at the declaration point.
                    ' Retain that single result for later sizeof operations.
                    If equalsPosition <> 0 OrElse isStaticLocal <> 0 OrElse _
                       isConstLocal <> 0 Then Return 0
                    If TranslateSequencedExpression( _
                        translation, catalog, countText, _
                        dynamicBoundExpression, dynamicBoundPrelude, context, _
                        callableNames(), callableCount, parameterNames(), _
                        parameterAliases(), parameterTypes(), symbolCount, "LongInt") = 0 Then _
                        Return 0
                End If
                dynamicBoundName = NewSimpleBodyTemporary(context, "LongInt", "")
                If dynamicBoundName = "" Then Return 0
                arrayBound = dynamicBoundName
                isSymbolicArrayBound = -1
                isDynamicLocalArray = -1
                End If
            End If
            arrayRank = 1
            arrayDimensions = arrayBound
        End If
        If isSymbolicArrayBound = 0 Then
            If arrayCount < 1 OrElse arrayCount > MAX_SIMPLE_ARRAY_COUNT Then Return 0
            arrayBound = Trim(Str(arrayCount)) & "ULL"
        ElseIf arrayBound = "" Then
            Return 0
        End If
        If arrayDimensionCount = 1 Then arrayDimensions = arrayBound
    End If
    Dim localArrayTypeName As String = ""
    Dim localArrayTypeNameIsAmbiguous As Long = 0
    For typeIndex As Long = 0 To nameIndex - 1
        Select Case tokens(typeIndex)
        Case "const"
            isConstLocal = -1
        Case "volatile", "restrict", "__restrict", "__restrict__", _
             "static", "auto", "register"
            Continue For
        Case Else
            If localArrayTypeName <> "" Then
                localArrayTypeNameIsAmbiguous = -1
                Exit For
            End If
            localArrayTypeName = tokens(typeIndex)
        End Select
    Next
    If localArrayTypeNameIsAmbiguous = 0 AndAlso _
       localArrayTypeName <> "" Then
        localArrayTypedefAliasIndex = _
            CBodyArrayTypeAliasIndexInContext( _
                translation, localArrayTypeName)
    End If
    If localArrayTypedefAliasIndex >= 0 Then
        With translation.body.aliases(localArrayTypedefAliasIndex)
            If .arrayCount < 1 OrElse _
               .arrayCount > MAX_SIMPLE_ARRAY_COUNT Then Return 0
            If .isConst <> 0 Then isConstLocal = -1
            If openingBracket >= 0 Then
                If isSymbolicArrayBound <> 0 OrElse isDynamicLocalArray <> 0 OrElse _
                   arrayRank <= 0 OrElse arrayRank >= MAX_GENERIC_ARRAY_RANK OrElse _
                   arrayCount <= 0 OrElse _
                   arrayCount > MAX_SIMPLE_ARRAY_COUNT \ .arrayCount Then Return 0
                Dim priorArrayRank As Long = arrayRank
                ReDim Preserve arrayDimensionBounds(0 To priorArrayRank)
                ReDim Preserve arrayDimensionCounts(0 To priorArrayRank)
                arrayDimensionBounds(priorArrayRank) = Trim(Str(.arrayCount)) & "ULL"
                arrayDimensionCounts(priorArrayRank) = .arrayCount
                arrayDimensions &= "," & arrayDimensionBounds(priorArrayRank)
                arrayCount *= .arrayCount
                arrayRank = priorArrayRank + 1
            Else
                arrayCount = .arrayCount
                arrayRank = 1
                arrayBound = Trim(Str(.arrayCount)) & "ULL"
                arrayDimensions = arrayBound
                ReDim arrayDimensionBounds(0 To 0)
                ReDim arrayDimensionCounts(0 To 0)
                arrayDimensionBounds(0) = arrayBound
                arrayDimensionCounts(0) = .arrayCount
            End If
        End With
    End If
    If nameIndex <= 0 OrElse IsIdentifierToken(tokens(nameIndex)) = 0 OrElse _
       IsTypeOrQualifierTokenInContext(translation, tokens(nameIndex)) <> 0 Then Return 0
    For i As Long = 0 To nameIndex - 1
        If tokens(i) = "," OrElse tokens(i) = "extern" Then Return 0
    Next
    Dim resolvedVariableType As C2fbResolvedTypeModel
    If localArrayTypedefAliasIndex >= 0 Then
        With translation.body.aliases(localArrayTypedefAliasIndex)
            resolvedVariableType.status = C2FB_TYPE_RESOLUTION_RESOLVED
            resolvedVariableType.freeBasicType = .arrayElementType
            resolvedVariableType.pointerCount = 0
            resolvedVariableType.isConst = CInt( _
                .isConst <> 0 OrElse isConstLocal <> 0) * -1
        End With
    Else
        ResolveC2fbTypeModel( _
            translation, catalog, tokens(), 0, nameIndex - 1, _
            resolvedVariableType)
    End If
    If resolvedVariableType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then Return 0
    Dim variableType As String = resolvedVariableType.freeBasicType
    Dim pointerCount As Long = resolvedVariableType.pointerCount
    Dim isConst As Long = resolvedVariableType.isConst
    Dim sourceRecordType As String
    For typeIndex As Long = nameIndex - 1 To 0 Step -1
        If IsIdentifierToken(tokens(typeIndex)) = 0 Then Continue For
        Dim sourceRecordKind As C2fbRecordKind = _
            CBodySourceRecordKindInContext(translation, tokens(typeIndex))
        If sourceRecordKind = C2FB_RECORD_STRUCT OrElse _
           sourceRecordKind = C2FB_RECORD_UNION Then
            sourceRecordType = tokens(typeIndex)
            Exit For
        End If
    Next
    If isStaticLocal <> 0 AndAlso isConstLocal <> 0 AndAlso _
       arrayRank > 1 AndAlso pointerCount = 0 Then
        isConst = -1
        If Left(variableType, Len("Const ")) <> "Const " Then _
            variableType = "Const " & variableType
    End If
    If variableType = "void" Then Return 0
    Dim isRecordArray As Long = 0
    Dim isRecordLocal As Long = 0
    Dim variableRecord As String = CBodyRecordName( _
        translation, catalog, variableType)
    ' An explicit map is the caller's storage assertion; its FreeBASIC type
    ' can be supplied by an include outside the indexed source directory.
    Dim hasExplicitSourceRecordTypeMap As Long = CInt( _
        GenericTypeOverrideInPolicy( _
            translation.genericC.policy, sourceRecordType) <> "")
    If sourceRecordType <> "" AndAlso pointerCount = 0 AndAlso _
       variableRecord = "" AndAlso hasExplicitSourceRecordTypeMap = 0 Then
        RecordSimpleBodyFailure( _
            context, "incomplete-local-record-layout", _
            "C record has no complete FreeBASIC storage layout: " & _
            sourceRecordType)
        Return 0
    End If
    ' Generated aliases can exist without a usable layout. For SQLite's
    ' stricter size-parity policy, also allow a complete catalog Type when
    ' the C record is not on the fixed-layout whitelist.
    If sourceRecordType <> "" AndAlso pointerCount = 0 AndAlso _
       variableRecord <> "" AndAlso hasExplicitSourceRecordTypeMap = 0 AndAlso _
       CBodyRecordSizeIsKnown( _
           translation, catalog, variableRecord) = 0 AndAlso _
       FbBodyRecordHasFieldModel(catalog, variableType) = 0 Then
        RecordSimpleBodyFailure( _
            context, "incomplete-local-record-layout", _
            "C record has no complete FreeBASIC storage layout: " & _
            sourceRecordType)
        Return 0
    End If
    If arrayRank > 1 AndAlso variableRecord <> "" AndAlso _
       pointerCount = 0 Then Return 0
    If isSymbolicArrayBound <> 0 AndAlso _
       (isStaticLocal <> 0 OrElse _
        (variableRecord <> "" AndAlso pointerCount = 0)) Then Return 0
    If variableRecord <> "" AndAlso pointerCount = 0 AndAlso _
       openingBracket < 0 AndAlso localArrayTypedefAliasIndex < 0 AndAlso _
       CBodyRecordSizeIsKnown( _
           translation, catalog, variableRecord) <> 0 Then
        isRecordLocal = -1
    ElseIf variableRecord <> "" AndAlso _
       (openingBracket >= 0 OrElse localArrayTypedefAliasIndex >= 0) AndAlso _
       pointerCount = 0 Then
        If arrayCount = 0 OrElse CBodyRecordSizeIsKnown( _
            translation, catalog, variableRecord) = 0 Then Return 0
        isRecordArray = -1
    ElseIf localArrayTypedefAliasIndex < 0 AndAlso _
       C2fbTypeRecognitionStatusInContext( _
           translation, catalog, tokens(), 0, nameIndex - 1) <> _
           C2FB_TYPE_RESOLUTION_RESOLVED AndAlso _
           pointerCount = 0 Then
        If isStaticLocal = 0 OrElse isConstLocal = 0 OrElse _
           openingBracket < 0 OrElse arrayCount = 0 Then Return 0
        RecordSimpleBodyFailure( _
            context, "unsupported-local-aggregate-declaration", _
            "unsupported local aggregate declaration: " & Left(statementText, 160))
        Return 0
    End If
    ' Opaque C pointers use Any Ptr in the port ABI as well as in local
    ' storage. If the body later dereferences an unmapped record, the
    ' expression parser still rejects that field access; the declaration
    ' itself does not need to be rejected just because its pointee is
    ' intentionally opaque (for example sqlite3_mutex*).
    Dim isTopLevelPointerConst As Long = 0
    If openingBracket >= 0 AndAlso pointerCount > 0 Then
        Dim finalPointerToken As Long = -1
        For tokenIndex As Long = 0 To nameIndex - 1
            If tokens(tokenIndex) = "*" Then finalPointerToken = tokenIndex
        Next
        For tokenIndex As Long = finalPointerToken + 1 To nameIndex - 1
            If tokens(tokenIndex) = "const" Then isTopLevelPointerConst = -1
        Next
    End If
    Dim staticPointerInitializerText As String
    If equalsPosition > 0 Then _
        staticPointerInitializerText = Trim(Mid(statementText, equalsPosition + 1))
    Dim staticPointerElementType As String = SimplePointeeType(variableType)
    Dim isStaticStringPointerArray As Long = CInt( _
        isStaticLocal <> 0 AndAlso openingBracket >= 0 AndAlso _
        (variableType = "Const ZString Ptr" OrElse _
         (pointerCount = 1 AndAlso InStr( _
             staticPointerInitializerText, Chr(34)) <> 0 AndAlso _
          (staticPointerElementType = "Byte" OrElse _
           staticPointerElementType = "UByte" OrElse _
           staticPointerElementType = "c2fb_c_char" OrElse _
           staticPointerElementType = "c2fb_c_uchar"))))
    Dim isStaticCallbackScalar As Long = CInt( _
        isStaticLocal <> 0 AndAlso openingBracket < 0 AndAlso _
        localArrayTypedefAliasIndex < 0 AndAlso _
        (IsCBodyCallbackTypeInContext(translation, variableType) <> 0 OrElse _
         callbackResult.status = C2FB_CALLBACK_TYPE_CONVERTED))
    If isStaticLocal <> 0 AndAlso isRecordLocal <> 0 AndAlso _
       equalsPosition > 0 Then
        Dim recordInitializer As String = _
            Trim(Mid(statementText, equalsPosition + 1))
        Dim recordKind As C2fbRecordKind = _
            CBodySourceRecordKindInContext(translation, variableRecord)
        If Left(recordInitializer, 1) <> "{" OrElse _
           (recordKind <> C2FB_RECORD_STRUCT AndAlso _
            recordKind <> C2FB_RECORD_UNION AndAlso _
            variableRecord <> "FuncDef.u") Then
            RecordSimpleBodyFailure( _
                context, "unsupported-static-record-initializer", _
                "unsupported static record initializer: " & _
                Left(statementText, 160))
            Return 0
        End If
    End If
    If isStaticLocal <> 0 AndAlso pointerCount <> 0 AndAlso _
       openingBracket >= 0 AndAlso _
       isStaticStringPointerArray = 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-static-local", _
            "unsupported static local: " & Left(statementText, 160))
        Return 0
    End If
    If isStaticCallbackScalar <> 0 AndAlso equalsPosition > 0 AndAlso _
       IsSupportedSimpleStaticCallbackInitializer( _
           translation, Trim(Mid(statementText, equalsPosition + 1)), _
           parameterNames(), symbolCount, callableNames(), callableCount) = 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-static-callback-initializer", _
            "unsupported static callback initializer: " & Left(statementText, 160))
        Return 0
    End If
    If isStaticLocal <> 0 AndAlso pointerCount <> 0 AndAlso _
       openingBracket < 0 AndAlso isStaticCallbackScalar = 0 AndAlso _
       equalsPosition > 0 AndAlso _
       IsSupportedSimpleStaticPointerInitializer( _
           translation, Trim(Mid(statementText, equalsPosition + 1)), _
           parameterNames(), symbolCount, variableType) = 0 Then
        RecordSimpleBodyFailure( _
            context, "unsupported-static-local-initializer", _
            "unsupported static pointer initializer: " & Left(statementText, 160))
        Return 0
    End If
    Dim isStaticScalar As Long = CInt( _
        isStaticLocal <> 0 AndAlso openingBracket < 0 AndAlso _
        localArrayTypedefAliasIndex < 0)
    Dim variableName As String = tokens(nameIndex)
    Dim scopeStart As Long = context.scopeSymbols(context.scopeDepth - 1)
    For i As Long = scopeStart To symbolCount - 1
        If parameterNames(i) = variableName Then Return 0
    Next
    If context.localSerial >= MAX_SIMPLE_VARIABLES Then Return 0
    If localCount >= MAX_SIMPLE_VARIABLES OrElse symbolCount >= MAX_SIMPLE_VARIABLES Then Return 0

    Dim initializer As String = ""
    Dim initializerPrelude As String
    Dim initializerSymbolCount As Long = symbolCount
    Dim initializerHasSelfName As Long = 0
    If equalsPosition > 0 AndAlso openingBracket < 0 AndAlso _
       localArrayTypedefAliasIndex < 0 Then
        ' C makes the declared name visible inside its initializer. Expose a
        ' temporary typed alias so unevaluated forms such as sizeof(*p) parse.
        parameterNames(symbolCount) = variableName
        parameterAliases(symbolCount) = "local" & Trim(Str(context.localSerial))
        parameterTypes(symbolCount) = variableType
        symbolCount += 1
        initializerHasSelfName = -1
    End If
    If isStaticScalar <> 0 AndAlso equalsPosition = 0 AndAlso _
       isRecordLocal = 0 Then initializer = "0"
    If equalsPosition > 0 AndAlso arrayCount = 0 Then
        initializer = Trim(Mid(statementText, equalsPosition + 1))
        If Len(initializer) = 0 Then
            If initializerHasSelfName <> 0 Then _
                symbolCount = initializerSymbolCount
            Return 0
        End If
        Dim translatedInitializer As String
        If isRecordLocal <> 0 AndAlso Left(initializer, 1) = "{" Then
            If TranslateSimpleRecordInitializer(translation, catalog,  _
                initializer, variableType, translatedInitializer, context, _
                callableNames(), callableCount, parameterNames(), _
                parameterAliases(), parameterTypes(), symbolCount, _
                CInt(isStaticLocal <> 0)) = 0 Then
                If initializerHasSelfName <> 0 Then _
                    symbolCount = initializerSymbolCount
                Return 0
            End If
        ElseIf isStaticScalar <> 0 Then
            Dim hasBoolean As Long
            Dim hasFunctionCall As Long
            Dim requiresFloatConstant As Long = CInt( _
                SimpleArithmeticKind(catalog, variableType) > 4)
            If TranslateSimpleExpression(translation, catalog, _
                                         initializer, translatedInitializer, hasBoolean, hasFunctionCall, _
                                         callableNames(), callableCount, parameterNames(), parameterAliases(), _
                                         parameterTypes(), symbolCount, variableType, 0, _
                                         requiresFloatConstant) = 0 OrElse _
               hasFunctionCall <> 0 Then
                If initializerHasSelfName <> 0 Then _
                    symbolCount = initializerSymbolCount
                Return 0
            End If
        Else
            If TranslateSequencedExpression(translation, catalog, _
                                            initializer, translatedInitializer, initializerPrelude, context, _
                                            callableNames(), callableCount, parameterNames(), parameterAliases(), _
                                            parameterTypes(), symbolCount, variableType) = 0 Then
                If initializerHasSelfName <> 0 Then _
                    symbolCount = initializerSymbolCount
                Return 0
            End If
        End If
        If initializerHasSelfName <> 0 Then _
            symbolCount = initializerSymbolCount
        initializer = translatedInitializer
    End If

    Dim localSizeofBytes As Long
    Dim localHasKnownSize As Long = 0
    Dim localSourceType As String
    For typeTokenIndex As Long = 0 To nameIndex - 1
        Select Case tokens(typeTokenIndex)
        Case "auto", "extern", "register", "static", "typedef"
            Continue For
        End Select
        If localSourceType <> "" Then localSourceType &= " "
        localSourceType &= tokens(typeTokenIndex)
    Next
    Dim localElementBytes As Long
    If localSourceType <> "" AndAlso _
       GenericArrayBoundTypeSizeInContext( _
           translation, localSourceType, localElementBytes) <> 0 Then
        Dim localObjectCount As Long = 1
        If arrayRank > 0 Then
            If isDynamicLocalArray <> 0 OrElse arrayCount <= 0 Then
                localObjectCount = 0
            Else
                localObjectCount = arrayCount
            End If
        End If
        If localObjectCount > 0 AndAlso localElementBytes > 0 AndAlso _
           localObjectCount <= MAX_GENERIC_ARRAY_COUNT \ localElementBytes Then
            localSizeofBytes = localObjectCount * localElementBytes
            localHasKnownSize = -1
        End If
    End If

    locals(localCount).name = variableName
    locals(localCount).typeName = variableType
    locals(localCount).isConst = isConst
    locals(localCount).sizeofBytes = localSizeofBytes
    locals(localCount).hasKnownSize = localHasKnownSize
    locals(localCount).isIntegerConstant = 0
    locals(localCount).integerValue = 0
    localCount += 1
    parameterNames(symbolCount) = variableName
    parameterAliases(symbolCount) = "local" & Trim(Str(context.localSerial))
    context.localSerial += 1
    parameterTypes(symbolCount) = variableType
    If isStaticStringPointerArray <> 0 AndAlso _
       isTopLevelPointerConst <> 0 Then _
        context.readOnlyAliases &= "|" & parameterAliases(symbolCount) & "|"
    If arrayCount > 0 OrElse isSymbolicArrayBound <> 0 Then _
        parameterTypes(symbolCount) &= "[]" & arrayBound & "|" & arrayDimensions
    ' Const scalar and array storage is immutable. Pointer pointee qualifiers are
    ' carried by variableType; a trailing top-level pointer const does not
    ' make the pointed-to page bytes read-only.
    If isConst <> 0 AndAlso pointerCount = 0 Then _
        context.readOnlyAliases &= "|" & parameterAliases(symbolCount) & "|"
    symbolCount += 1
    ' Hoist storage, not initialization. Scalar/pointer locals have no BASIC
    ' destructor; C block names stay distinct even when a goto crosses a block.
    If isStaticScalar <> 0 Then
        ' Static local storage has process lifetime. Leave a record without
        ' an initializer here so FreeBASIC zero-fills its fields.
        context.declarations &= "    Static " & parameterAliases(symbolCount - 1) & _
            " As " & variableType
        If initializer <> "" Then context.declarations &= " = " & initializer
        context.declarations &= Chr(10)
        outputText = "' C static scalar storage"
        Return -1
    End If
    If arrayCount > 0 OrElse isSymbolicArrayBound <> 0 Then
        Dim arrayName As String = parameterAliases(symbolCount - 1)
        If isDynamicLocalArray <> 0 Then
            ' FreeBASIC allocates dynamic arrays on the heap. ReDim at the C
            ' declaration point preserves repeated block-entry bounds.
            context.declarations &= "    Dim " & arrayName & "(Any) As " & _
                variableType & Chr(10)
            If dynamicBoundPrelude <> "" Then _
                outputText = RTrim(dynamicBoundPrelude, Any Chr(10)) & Chr(10)
            If dynamicBoundExpression <> "" Then _
                outputText &= dynamicBoundName & " = " & _
                    dynamicBoundExpression & Chr(10)
            outputText &= _
                "' C local array bounds must fit FreeBASIC's signed array range" & Chr(10) & _
                "If " & dynamicBoundName & " <= 0 OrElse " & _
                dynamicBoundName & " > " & _
                Trim(Str(MAX_GENERIC_ARRAY_COUNT)) & " Then Error 5" & Chr(10) & _
                "ReDim " & arrayName & "(0 To " & dynamicBoundName & _
                " - 1) As " & variableType
            Return -1
        End If
        If isStaticLocal <> 0 Then
            If equalsPosition = 0 Then
                ' C static arrays without an initializer have zero-filled
                ' storage for the lifetime of the process.
                context.declarations &= "    Static " & arrayName & "(0 To " & _
                    Trim(Str(arrayCount - 1)) & ") As " & variableType & Chr(10)
                outputText = "' C static array storage"
                Return -1
            End If
            Dim initializerValues As String = staticStringValues
            If staticStringInitializer = 0 Then
                Dim initializerText As String = Trim(Mid(statementText, equalsPosition + 1))
                If Left(initializerText, 1) <> "{" OrElse Right(initializerText, 1) <> "}" Then
                    RecordSimpleBodyFailure( _
                        context, "unsupported-static-array-initializer", _
                        "unsupported static array initializer: " & Left(statementText, 160))
                    Return 0
                End If
                Dim values() As String
                Dim valueCount As Long = 0
                If arrayRank > 1 Then
                    ReDim values(0 To arrayCount - 1)
                    If FlattenSimpleCArrayInitializer( _
                        initializerText, arrayDimensionCounts(), arrayRank, _
                        0, values(), valueCount, arrayCount) = 0 OrElse _
                       valueCount <> arrayCount Then
                        RecordSimpleBodyFailure( _
                            context, "unsupported-static-array-initializer", _
                            "unsupported multidimensional static array initializer: " & _
                            Left(statementText, 160))
                        Return 0
                    End If
                Else
                    initializerText = Trim(Mid(initializerText, 2, Len(initializerText) - 2))
                    If Right(initializerText, 1) = "," Then _
                        initializerText = RTrim(Left(initializerText, Len(initializerText) - 1))
                    If SplitSimpleCList( _
                        initializerText, values(), valueCount) = 0 OrElse _
                       valueCount > arrayCount Then
                        RecordSimpleBodyFailure( _
                            context, "unsupported-static-array-initializer", _
                            "unsupported static array initializer: " & Left(statementText, 160))
                        Return 0
                    End If
                End If
                If arrayRank = 1 Then
                    Dim hasArrayDesignator As Long = 0
                    For i As Long = 0 To valueCount - 1
                        If Left(Trim(values(i)), 1) = "[" Then
                            hasArrayDesignator = -1
                            Exit For
                        End If
                    Next
                    If hasArrayDesignator <> 0 Then
                        Dim sourceValues() As String
                        Dim sourceValueCount As Long = valueCount
                        If sourceValueCount > 0 Then
                            ReDim sourceValues(0 To sourceValueCount - 1)
                            For i As Long = 0 To sourceValueCount - 1
                                sourceValues(i) = values(i)
                            Next
                        End If
                        ReDim values(0 To arrayCount - 1)
                        Dim initializedElements() As Long
                        ReDim initializedElements(0 To arrayCount - 1)
                        Dim nextElementIndex As Long = 0
                        For sourceIndex As Long = 0 To sourceValueCount - 1
                            Dim currentElementIndex As Long = nextElementIndex
                            Dim elementValue As String = Trim(sourceValues(sourceIndex))
                            If Left(elementValue, 1) = "[" Then
                                Dim designatedValue As String
                                If ParseSimpleArrayDesignator( _
                                    translation, elementValue, currentElementIndex, _
                                    designatedValue) = 0 Then
                                    RecordSimpleBodyFailure( _
                                        context, "unsupported-static-array-designator", _
                                        "unsupported static array designator: " & _
                                        Left(elementValue, 120))
                                    Return 0
                                End If
                                elementValue = designatedValue
                            End If
                            If currentElementIndex < 0 OrElse _
                               currentElementIndex >= arrayCount OrElse _
                               initializedElements(currentElementIndex) <> 0 Then
                                RecordSimpleBodyFailure( _
                                    context, "invalid-static-array-designator", _
                                    "out-of-range or repeated static array index: " & _
                                    Trim(Str(currentElementIndex)))
                                Return 0
                            End If
                            values(currentElementIndex) = elementValue
                            initializedElements(currentElementIndex) = -1
                            nextElementIndex = currentElementIndex + 1
                        Next
                        For i As Long = 0 To arrayCount - 1
                            If initializedElements(i) = 0 Then values(i) = ""
                        Next
                        valueCount = arrayCount
                    End If
                End If
                For i As Long = 0 To valueCount - 1
                    Dim translatedValue As String
                    If Trim(values(i)) = "" Then
                        translatedValue = "Cast(" & variableType & ", 0)"
                    ElseIf isRecordArray <> 0 Then
                        If TranslateSimpleRecordInitializer(translation, catalog,  _
                            values(i), variableType, translatedValue, context, _
                            callableNames(), callableCount, parameterNames(), _
                            parameterAliases(), parameterTypes(), symbolCount) = 0 Then
                            RecordSimpleBodyFailure( _
                                context, "unsupported-static-record-initializer", _
                                "unsupported static record initializer: " & Left(values(i), 160))
                            Return 0
                        End If
                    Else
                        Dim hasBoolean As Long
                        Dim hasCall As Long
                        Dim requiresFloatConstant As Long = CInt( _
                            SimpleArithmeticKind(catalog, variableType) > 4)
                        If TranslateSimpleExpression(translation, catalog, _
                                                     values(i), translatedValue, hasBoolean, hasCall, _
                                                     callableNames(), callableCount, parameterNames(), _
                                                     parameterAliases(), parameterTypes(), symbolCount, _
                                                     "", -1, requiresFloatConstant) = 0 OrElse _
                           hasCall <> 0 Then
                            RecordSimpleBodyFailure( _
                                context, "unsupported-static-array-element", _
                                "unsupported static array element: " & Left(values(i), 120))
                            Return 0
                        End If
                        translatedValue = "Cast(" & variableType & ", " & translatedValue & ")"
                    End If
                    If initializerValues <> "" Then
                        If isRecordArray <> 0 Then
                            initializerValues &= ", _" & Chr(10) & "        "
                        Else
                            initializerValues &= ", "
                        End If
                    End If
                    initializerValues &= translatedValue
                Next
                For i As Long = valueCount To arrayCount - 1
                    If initializerValues <> "" Then
                        If isRecordArray <> 0 Then
                            initializerValues &= ", _" & Chr(10) & "        "
                        Else
                            initializerValues &= ", "
                        End If
                    End If
                    If isRecordArray <> 0 Then
                        Dim zeroRecord As String
                        If TranslateSimpleRecordInitializer(translation, catalog,  _
                            "{}", variableType, zeroRecord, context, callableNames(), _
                            callableCount, parameterNames(), parameterAliases(), _
                            parameterTypes(), symbolCount) = 0 Then Return 0
                        initializerValues &= zeroRecord
                    Else
                        initializerValues &= "Cast(" & variableType & ", 0)"
                    End If
                Next
            Else
                Dim initialValues() As String
                Dim initialValueCount As Long
                If SplitSimpleCList(initializerValues, initialValues(), initialValueCount) = 0 OrElse _
                   initialValueCount > arrayCount Then Return 0
                For i As Long = initialValueCount To arrayCount - 1
                    If initializerValues <> "" Then initializerValues &= ", "
                    initializerValues &= "0"
                Next
            End If
            If isRecordArray <> 0 Then
                context.declarations &= "    Static " & arrayName & "(0 To " & _
                    Trim(Str(arrayCount - 1)) & ") As " & variableType & _
                    " = { _" & Chr(10) & "        " & initializerValues & "}" & Chr(10)
            Else
                context.declarations &= "    Static " & arrayName & "(0 To " & _
                    Trim(Str(arrayCount - 1)) & ") As " & variableType & _
                    " = {" & initializerValues & "}" & Chr(10)
            End If
            outputText = "' C static array storage"
            Return -1
        End If
        Dim arrayLastIndex As String = Trim(Str(arrayCount - 1))
        If isSymbolicArrayBound <> 0 Then _
            arrayLastIndex = "(" & arrayBound & ") - 1"
        context.declarations &= "    Dim " & arrayName & "(0 To " & _
            arrayLastIndex & ") As " & variableType & Chr(10)
        If equalsPosition = 0 Then
            outputText = "' C automatic array storage"
            Return -1
        End If
        Dim initializerText As String = Trim(Mid(statementText, equalsPosition + 1))
        If Left(initializerText, 1) <> "{" OrElse Right(initializerText, 1) <> "}" Then Return 0
        initializerText = Trim(Mid(initializerText, 2, Len(initializerText) - 2))
        If Right(initializerText, 1) = "," Then initializerText = RTrim(Left(initializerText, Len(initializerText) - 1))
        Dim values() As String
        Dim valueCount As Long
        If arrayRank > 1 Then
            If isDynamicLocalArray <> 0 OrElse arrayCount < 1 OrElse _
               arrayCount > MAX_SIMPLE_ARRAY_COUNT Then Return 0
            ReDim values(0 To arrayCount - 1)
            valueCount = 0
            If FlattenSimpleCArrayInitializer( _
                Trim(Mid(statementText, equalsPosition + 1)), _
                arrayDimensionCounts(), arrayRank, 0, values(), valueCount, _
                arrayCount) = 0 OrElse valueCount <> arrayCount Then Return 0
        ElseIf SplitSimpleCList(initializerText, values(), valueCount) = 0 OrElse _
               valueCount > arrayCount Then
            Return 0
        End If
        ' ERASE clears a fixed BASIC array without freeing it. Repeat the clear
        ' at the declaration site so omitted C initializer elements reset even
        ' when that declaration is reached again in a loop.
        outputText = "Erase " & arrayName
        For i As Long = 0 To valueCount - 1
            If values(i) = "" Then Continue For
            Dim converted As String
            Dim valuePrelude As String
            If isRecordArray <> 0 Then
                If TranslateSimpleRecordInitializer( _
                    translation, catalog, values(i), variableType, _
                    converted, context, callableNames(), callableCount, _
                    parameterNames(), parameterAliases(), parameterTypes(), _
                    symbolCount) = 0 Then Return 0
            ElseIf TranslateSequencedExpression(translation, catalog, _
                values(i), converted, valuePrelude, context, _
                callableNames(), callableCount, parameterNames(), _
                parameterAliases(), parameterTypes(), symbolCount, _
                variableType) = 0 Then
                Return 0
            End If
            If valuePrelude <> "" Then outputText &= Chr(10) & RTrim(valuePrelude, Any Chr(10))
            outputText &= Chr(10) & arrayName & "(" & Trim(Str(i)) & ") = " & converted
        Next
        Return -1
    End If
    context.declarations &= "    Dim " & parameterAliases(symbolCount - 1) & " As " & variableType & Chr(10)
    If initializer <> "" Then
        outputText = initializerPrelude & parameterAliases(symbolCount - 1) & " = " & initializer
    Else
        outputText = "' C automatic storage declaration"
    End If
    Return -1
End Function

Private Function TranslateSingleDeclarationStatement( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, _
    ByRef outputText As String, _
    ByRef context As SimpleBodyContext, _
    locals() As SimpleLocalVariable, _
    ByRef localCount As Long, _
    callableNames() As String, _
    ByVal callableCount As Long, _
    parameterNames() As String, _
    parameterAliases() As String, _
    parameterTypes() As String, _
    ByRef symbolCount As Long _
) As Long
    Dim declarationPrefix As String = statementText
    Dim initializerEquals As Long = FindSimpleAssignmentEquals(statementText)
    If initializerEquals > 0 Then
        declarationPrefix = Trim(Left(statementText, initializerEquals - 1))
    End If
    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(declarationPrefix, tokens(), tokenCount)
    Return TranslateSingleDeclarationStatementWithTokens( _
        translation, catalog, statementText, tokens(), tokenCount, outputText, _
        context, locals(), localCount, callableNames(), callableCount, _
        parameterNames(), parameterAliases(), parameterTypes(), symbolCount)
End Function


Private Function StripSimpleUnusedLeadingAttribute( _
    ByVal statementText As String _
) As String
    Dim parsePosition As Long = 1
    SkipCSpaces(statementText, parsePosition)
    Const attributeName As String = "__attribute__"
    If Mid(statementText, parsePosition, Len(attributeName)) <> attributeName Then _
        Return statementText
    parsePosition += Len(attributeName)
    If parsePosition <= Len(statementText) AndAlso _
       IsIdentifierPart(Asc(statementText, parsePosition)) <> 0 Then _
        Return statementText
    SkipCSpaces(statementText, parsePosition)
    If parsePosition > Len(statementText) OrElse _
       Asc(statementText, parsePosition) <> Asc("(") Then Return statementText

    Dim attributeEnd As Long = parsePosition
    If ScanSimpleExpressionGroup( _
        statementText, attributeEnd, Asc("("), Asc(")")) = 0 Then _
        Return statementText
    Dim attributeContents As String
    For position As Long = parsePosition + 1 To attributeEnd - 2
        If IsSpaceByte(Asc(statementText, position)) = 0 Then _
            attributeContents &= LCase(Mid(statementText, position, 1))
    Next
    If attributeContents <> "(__unused__)" AndAlso _
       attributeContents <> "(unused)" Then Return statementText

    Dim declarationText As String = Trim(Mid(statementText, attributeEnd))
    If declarationText = "" Then Return statementText
    Return declarationText
End Function

Private Function TranslateSimpleDeclarationStatement( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal statementText As String, ByRef outputText As String, _
    ByRef context As SimpleBodyContext, locals() As SimpleLocalVariable, ByRef localCount As Long, _
    callableNames() As String, ByVal callableCount As Long, _
    parameterNames() As String, parameterAliases() As String, parameterTypes() As String, _
    ByRef symbolCount As Long _
) As Long
    statementText = StripSimpleUnusedLeadingAttribute(statementText)
    Dim parts() As String
    Dim partCount As Long
    If SplitSimpleCList(statementText, parts(), partCount) = 0 Then Return 0

    If partCount = 1 Then
        Return TranslateSingleDeclarationStatement( _
            translation, catalog, Trim(parts(0)), outputText, context, _
            locals(), localCount, callableNames(), callableCount, _
            parameterNames(), parameterAliases(), parameterTypes(), symbolCount)
    End If

    Dim sourceTokens() As String
    Dim sourceTokenCount As Long
    TokenizeCDeclaration(statementText, sourceTokens(), sourceTokenCount)
    Dim sourceRanges() As C2fbTokenRangeModel
    Dim sourceRangeCount As Long = 0
    ' Use shared token ranges when they agree with the retained source parts.
    If sourceTokenCount > 0 AndAlso _
       SplitC2fbTokenList( _
           sourceTokens(), 0, sourceTokenCount - 1, _
           sourceRanges(), sourceRangeCount) <> 0 AndAlso _
       sourceRangeCount = partCount Then
        If sourceRangeCount = 1 Then
            Dim singleTokens() As String
            Dim singleTokenCount As Long
            If BuildC2fbDeclaratorPartTokens( _
                sourceTokens(), sourceRanges(), sourceRangeCount, 0, 0, _
                singleTokens(), singleTokenCount) <> 0 Then
                Return TranslateSingleDeclarationStatementWithTokens( _
                    translation, catalog, Trim(parts(0)), singleTokens(), _
                    singleTokenCount, outputText, context, locals(), localCount, _
                    callableNames(), callableCount, parameterNames(), _
                    parameterAliases(), parameterTypes(), symbolCount)
            End If
        End If
        Dim firstTokens() As String
        Dim firstTokenCount As Long = 0
        If BuildC2fbDeclaratorPartTokens( _
            sourceTokens(), sourceRanges(), sourceRangeCount, 0, 0, _
            firstTokens(), firstTokenCount) <> 0 Then
            If firstTokenCount > 0 AndAlso _
               firstTokens(firstTokenCount - 1) = ";" Then _
                firstTokenCount -= 1
            Dim tokenAssignmentIndex As Long
            If firstTokenCount >= 2 AndAlso _
               FindC2fbTopLevelAssignmentToken( _
                   firstTokens(), 0, firstTokenCount - 1, _
                   tokenAssignmentIndex) <> 0 Then
                Dim typeDeclarationTokenCount As Long = firstTokenCount
                If tokenAssignmentIndex >= 0 Then _
                    typeDeclarationTokenCount = tokenAssignmentIndex
                Dim tokenNameIndex As Long = typeDeclarationTokenCount - 1
                For tokenIndex As Long = 0 To typeDeclarationTokenCount - 1
                    If firstTokens(tokenIndex) = "[" Then
                        tokenNameIndex = tokenIndex - 1
                        Exit For
                    End If
                Next
                Dim tokenTypeEnd As Long = tokenNameIndex - 1
                While tokenTypeEnd >= 0 AndAlso _
                      (firstTokens(tokenTypeEnd) = "*" OrElse _
                       IsQualifierToken(firstTokens(tokenTypeEnd)) <> 0)
                    tokenTypeEnd -= 1
                Wend

                If tokenTypeEnd >= 0 Then
                    Dim tokenBaseType As String
                    For tokenIndex As Long = 0 To tokenTypeEnd
                        If tokenIndex > 0 Then tokenBaseType &= " "
                        tokenBaseType &= firstTokens(tokenIndex)
                    Next
                    Dim tokenCommonTypeCount As Long = tokenTypeEnd + 1
                    Dim canBuildAllParts As Long = -1
                    For validationPartIndex As Long = 0 To partCount - 1
                        Dim validationTokens() As String
                        Dim validationTokenCount As Long
                        If BuildC2fbDeclaratorPartTokens( _
                            sourceTokens(), sourceRanges(), sourceRangeCount, _
                            validationPartIndex, tokenCommonTypeCount, _
                            validationTokens(), validationTokenCount) = 0 Then
                            canBuildAllParts = 0
                            Exit For
                        End If
                    Next

                    If canBuildAllParts <> 0 Then
                        outputText = ""
                        For tokenPartIndex As Long = 0 To partCount - 1
                            Dim tokenItemDeclaration As String = _
                                Trim(parts(tokenPartIndex))
                            If tokenPartIndex > 0 Then _
                                tokenItemDeclaration = _
                                    tokenBaseType & " " & tokenItemDeclaration
                            Dim itemTokens() As String
                            Dim itemTokenCount As Long
                            If BuildC2fbDeclaratorPartTokens( _
                                sourceTokens(), sourceRanges(), sourceRangeCount, _
                                tokenPartIndex, tokenCommonTypeCount, _
                                itemTokens(), itemTokenCount) = 0 Then _
                                ReportError("body declaration token split changed during lowering")
                            Dim converted As String
                            If TranslateSingleDeclarationStatementWithTokens( _
                                translation, catalog, tokenItemDeclaration, _
                                itemTokens(), itemTokenCount, converted, context, _
                                locals(), localCount, callableNames(), callableCount, _
                                parameterNames(), parameterAliases(), parameterTypes(), _
                                symbolCount) = 0 Then Return 0
                            If tokenPartIndex > 0 Then outputText &= Chr(10)
                            outputText &= converted
                        Next
                        Return -1
                    End If
                End If
            End If
        End If
    End If

    Dim prefix As String = parts(0)
    Dim equalsPosition As Long = InStr(prefix, "=")
    If equalsPosition > 0 Then prefix = Trim(Left(prefix, equalsPosition - 1))
    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(prefix, tokens(), tokenCount)
    If tokenCount < 2 Then Return 0
    Dim nameIndex As Long = tokenCount - 1
    For i As Long = 0 To tokenCount - 1
        If tokens(i) = "[" Then
            nameIndex = i - 1
            Exit For
        End If
    Next
    Dim typeEnd As Long = nameIndex - 1
    While typeEnd >= 0 AndAlso (tokens(typeEnd) = "*" OrElse IsQualifierToken(tokens(typeEnd)) <> 0)
        typeEnd -= 1
    Wend
    If typeEnd < 0 Then Return 0
    Dim baseType As String
    For i As Long = 0 To typeEnd
        If i > 0 Then baseType &= " "
        baseType &= tokens(i)
    Next
    outputText = ""
    For i As Long = 0 To partCount - 1
        Dim declaration As String = parts(i)
        If i > 0 Then declaration = baseType & " " & declaration
        Dim converted As String
        If TranslateSingleDeclarationStatement(translation, catalog, declaration, converted, context, locals(), localCount, _
                                              callableNames(), callableCount, parameterNames(), _
                                              parameterAliases(), parameterTypes(), symbolCount) = 0 Then Return 0
        If i > 0 Then outputText &= Chr(10)
        outputText &= converted
    Next
    Return -1
End Function

/' end of c2fb_body_declarations.bi '/
