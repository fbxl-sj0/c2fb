/'
    Project: C2FB generic header converter

    File: c2fb_generic_declarations.bi

    Purpose:

        Lower plain generic C declarations into external FreeBASIC bindings.

    Responsibilities:

        - Consume shared declaration tokens and split multi-object declarations
          by token range.
        - Emit function-type aliases, external objects, and callbacks.
        - Apply generic type, array-size, and string override policy.
        - Report declarations that do not map safely to supported bindings.

    This file intentionally does NOT contain:

        - C record layout preparation or enum constant output.
        - Generic macro collection, lowering, or formatting.
'/

Private Function GenericDeclaratorWithoutInitializer(ByVal declaration As String) As String
    Dim parenDepth As Long = 0
    Dim bracketDepth As Long = 0
    Dim braceDepth As Long = 0
    Dim quoteByte As Long = 0
    For position As Long = 1 To Len(declaration)
        Dim currentByte As Long = Asc(declaration, position)
        If quoteByte <> 0 Then
            If currentByte = Asc("\\") Then
                position += 1
            ElseIf currentByte = quoteByte Then
                quoteByte = 0
            End If
        ElseIf currentByte = Asc("'") OrElse currentByte = Asc(Chr(34)) Then
            quoteByte = currentByte
        Else
            Select Case currentByte
            Case Asc("("): parenDepth += 1
            Case Asc(")"): parenDepth -= 1
            Case Asc("["): bracketDepth += 1
            Case Asc("]"): bracketDepth -= 1
            Case Asc("{"): braceDepth += 1
            Case Asc("}"): braceDepth -= 1
            Case Asc("=")
                If parenDepth = 0 AndAlso bracketDepth = 0 AndAlso braceDepth = 0 Then _
                    Return Trim(Left(declaration, position - 1))
            End Select
        End If
    Next
    Return Trim(declaration)
End Function

Declare Function GenericHeaderPlainDeclaratorRemoved( _
    ByRef sourceDeclaration As C2fbDeclarationModel, ByVal sourceName As String _
) As Long

Declare Function ParseGenericHeaderDeclaratorTokens( _
    tokens() As String, ByVal firstToken As Long, ByVal lastToken As Long, _
    ByRef commonType As String, ByRef sourceName As String, _
    ByRef bindingOrder As String, ByRef commonTypeTokenCount As Long _
) As Long

Private Function ParseGenericHeaderDeclarator( _
    ByVal declaration As String, ByRef commonType As String, _
    ByRef sourceName As String, ByRef bindingOrder As String _
) As Long
    commonType = ""
    sourceName = ""
    bindingOrder = ""
    declaration = GenericDeclaratorWithoutInitializer( _
        StripCFunctionAttributes(declaration))
    If declaration = "" Then Return 0

    Dim tokens() As String
    Dim tokenCount As Long
    Dim commonTypeTokenCount As Long
    TokenizeCDeclaration(declaration, tokens(), tokenCount)
    Return ParseGenericHeaderDeclaratorTokens( _
        tokens(), 0, tokenCount - 1, commonType, sourceName, bindingOrder, _
        commonTypeTokenCount)
End Function

Private Function ParseGenericHeaderDeclaratorTokens( _
    tokens() As String, ByVal firstToken As Long, ByVal lastToken As Long, _
    ByRef commonType As String, ByRef sourceName As String, _
    ByRef bindingOrder As String, ByRef commonTypeTokenCount As Long _
) As Long
    commonType = ""
    sourceName = ""
    bindingOrder = ""
    commonTypeTokenCount = 0
    If firstToken < 0 OrElse lastToken < firstToken OrElse _
       lastToken > UBound(tokens) Then Return 0
    If tokens(lastToken) = ";" Then lastToken -= 1
    If lastToken < firstToken + 1 Then Return 0

    ' C object initializers do not contribute to the FreeBASIC binding.
    ' Ignore only a top-level assignment so nested expressions remain intact.
    Dim assignmentToken As Long
    If FindC2fbTopLevelAssignmentToken( _
        tokens(), firstToken, lastToken, assignmentToken) = 0 Then Return 0
    If assignmentToken >= firstToken Then lastToken = assignmentToken - 1
    If lastToken < firstToken + 1 OrElse _
       lastToken - firstToken + 1 >= MAX_TYPE_WORDS Then Return 0

    For declaratorStart As Long = firstToken + 1 To lastToken
        If IsCFunctionReturnTypePrefix( _
            tokens(), firstToken, declaratorStart - 1) = 0 Then Continue For
        Dim position As Long = declaratorStart
        Dim candidate As C2fbDeclaratorModel
        If ParseCDeclaratorModel( _
            tokens(), position, lastToken, candidate) = 0 Then Continue For
        If position <> lastToken + 1 OrElse _
           IsIdentifierToken(candidate.name) = 0 OrElse _
           IsCDeclarationSpecifierKeyword(candidate.name) <> 0 Then Continue For
        Dim commonTypeTokens() As String
        ReDim commonTypeTokens(0 To MAX_TYPE_WORDS - 1)
        Dim filteredCommonTypeTokenCount As Long = 0
        For typeToken As Long = firstToken To declaratorStart - 1
            Select Case tokens(typeToken)
            Case "_Thread_local", "__thread", "thread_local"
                Continue For
            End Select
            AddToken(commonTypeTokens(), filteredCommonTypeTokenCount, _
                     tokens(typeToken))
        Next
        commonType = GenericTokensToText( _
            commonTypeTokens(), 0, filteredCommonTypeTokenCount - 1)
        commonTypeTokenCount = declaratorStart - firstToken
        sourceName = candidate.name
        bindingOrder = candidate.bindingOrder
        Return CInt(commonType <> "") * -1
    Next
    Return 0
End Function

Private Function GenericLastIdentifierPosition( _
    ByVal sourceText As String, ByVal identifier As String _
) As Long
    If identifier = "" Then Return 0
    Dim matchPosition As Long = 1
    Dim lastPosition As Long = 0
    While matchPosition <= Len(sourceText)
        matchPosition = InStr(matchPosition, sourceText, identifier)
        If matchPosition = 0 Then Exit While
        Dim beforeIsIdentifier As Long = 0
        Dim afterIsIdentifier As Long = 0
        If matchPosition > 1 Then _
            beforeIsIdentifier = IsIdentifierPart(Asc(sourceText, matchPosition - 1))
        If matchPosition + Len(identifier) <= Len(sourceText) Then _
            afterIsIdentifier = IsIdentifierPart(Asc(sourceText, matchPosition + Len(identifier)))
        If beforeIsIdentifier = 0 AndAlso afterIsIdentifier = 0 Then _
            lastPosition = matchPosition
        matchPosition += Len(identifier)
    Wend
    Return lastPosition
End Function

Private Function ParseGenericArrayBoundsInDeclaration( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceText As String, ByVal identifierEndPosition As Long, _
    ByVal expectedDimensionCount As Long, ByRef arrayBounds As String, _
    ByRef firstDimensionCount As Long, ByRef totalElementCount As Long, _
    ByRef isUnsized As Long, ByVal allowUnsized As Long, _
    ByVal allowZeroLength As Long, _
    ByVal unsizedBoundHint As Long = 0 _
) As Long
    arrayBounds = ""
    firstDimensionCount = 0
    totalElementCount = 1
    isUnsized = 0
    If identifierEndPosition < 1 OrElse _
       identifierEndPosition > Len(sourceText) + 1 OrElse _
       expectedDimensionCount <= 0 OrElse _
       expectedDimensionCount > MAX_GENERIC_ARRAY_RANK Then Return 0

    Dim searchPosition As Long = identifierEndPosition
    For dimensionIndex As Long = 0 To expectedDimensionCount - 1
        Dim openingBracket As Long = InStr(searchPosition, sourceText, "[")
        If openingBracket = 0 Then Return 0
        Dim bracketDepth As Long = 1
        Dim closingBracket As Long = openingBracket + 1
        While closingBracket <= Len(sourceText) AndAlso bracketDepth > 0
            Select Case Mid(sourceText, closingBracket, 1)
            Case "[": bracketDepth += 1
            Case "]": bracketDepth -= 1
            End Select
            closingBracket += 1
        Wend
        If bracketDepth <> 0 Then Return 0
        closingBracket -= 1

        Dim boundExpression As String = Trim(Mid( _
            sourceText, openingBracket + 1, closingBracket - openingBracket - 1))
        Dim dimensionCount As Long = 0
        If boundExpression = "" Then
            If allowUnsized = 0 OrElse dimensionIndex <> 0 Then Return 0
            If unsizedBoundHint > 0 Then
                dimensionCount = unsizedBoundHint
            Else
                If expectedDimensionCount <> 1 Then Return 0
                isUnsized = -1
                totalElementCount = 0
            End If
        ElseIf EvaluateSimpleArrayBound( _
            boundExpression, dimensionCount, MAX_GENERIC_ARRAY_COUNT, _
            allowZeroLength) = 0 AndAlso _
           EvaluateGenericHeaderArrayBound( _
            translation, boundExpression, dimensionCount, _
            MAX_GENERIC_ARRAY_COUNT, allowZeroLength) = 0 Then
            Return 0
        End If
        If isUnsized = 0 Then
            If dimensionIndex = 0 Then firstDimensionCount = dimensionCount
            If dimensionCount = 0 Then
                totalElementCount = 0
            ElseIf totalElementCount > 0 Then
                If totalElementCount > MAX_GENERIC_ARRAY_COUNT \ dimensionCount Then _
                    Return 0
                totalElementCount *= dimensionCount
            End If
            If arrayBounds <> "" Then arrayBounds &= ", "
            arrayBounds &= "0 To " & Trim(Str(dimensionCount - 1))
        End If
        searchPosition = closingBracket + 1
    Next
    If isUnsized <> 0 Then arrayBounds = ""
    Return -1
End Function

Private Function CombineGenericArrayBounds( _
    ByVal firstBounds As String, ByVal secondBounds As String _
) As String
    If firstBounds = "" Then Return secondBounds
    If secondBounds = "" Then Return firstBounds
    Return firstBounds & ", " & secondBounds
End Function

Private Function TranslateGenericModuleStaticAddressInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceText As String, ByVal isCallbackObject As Long, _
    ByRef initialValue As String _
) As Long
    initialValue = ""
    sourceText = GenericMacroStripOuterParentheses(Trim(sourceText))
    Dim tokens() As String
    Dim tokenCount As Long
    If TokenizeCPreprocessorReplacement( _
        sourceText, tokens(), tokenCount) = 0 Then Return 0

    Dim sourceName As String
    Dim isAddressOfObject As Long = 0
    If tokenCount = 2 AndAlso tokens(0) = "&" AndAlso _
       IsIdentifierToken(tokens(1)) <> 0 Then
        sourceName = tokens(1)
        isAddressOfObject = -1
    ElseIf tokenCount = 1 AndAlso isCallbackObject <> 0 AndAlso _
           IsIdentifierToken(tokens(0)) <> 0 Then
        sourceName = tokens(0)
    Else
        Return 0
    End If

    Dim symbolIndex As Long = _
        GenericHeaderSymbolIndexInContext(translation, sourceName)
    If symbolIndex < 0 Then Return 0
    With translation.symbolTable.symbols(symbolIndex)
        Select Case .kind
        Case GENERIC_SYMBOL_FUNCTION, GENERIC_SYMBOL_SUB
            If isCallbackObject = 0 Then Return 0
            initialValue = "ProcPtr(" & .freeBasicName & ")"
        Case GENERIC_SYMBOL_OBJECT
            If isAddressOfObject = 0 Then Return 0
            initialValue = "@" & .freeBasicName
        Case Else
            Return 0
        End Select
    End With
    Return -1
End Function

Private Function ReadGenericStaticStringEscape( _
    ByVal sourceText As String, ByRef position As Long, _
    ByRef byteValue As Long _
) As Long
    position += 1
    If position > Len(sourceText) Then Return 0
    Dim escapedByte As Long = Asc(sourceText, position)
    position += 1
    Select Case escapedByte
    Case Asc("a"): byteValue = 7
    Case Asc("b"): byteValue = 8
    Case Asc("t"): byteValue = 9
    Case Asc("n"): byteValue = 10
    Case Asc("v"): byteValue = 11
    Case Asc("f"): byteValue = 12
    Case Asc("r"): byteValue = 13
    Case Asc(Chr(92)), Asc("'"), Asc(Chr(34)), Asc("?")
        byteValue = escapedByte
    Case Else
        Dim radix As Long = 8
        Dim maximumDigits As Long = 3
        If escapedByte = Asc("x") Then
            radix = 16
            maximumDigits = Len(sourceText)
        ElseIf escapedByte >= Asc("0") AndAlso escapedByte <= Asc("7") Then
            position -= 1
        Else
            Return 0
        End If
        byteValue = 0
        Dim digitCount As Long = 0
        While position <= Len(sourceText) AndAlso digitCount < maximumDigits
            Dim digit As Long = SimpleDigitValue(Asc(sourceText, position))
            If digit < 0 OrElse digit >= radix Then Exit While
            If byteValue > (255 - digit) \ radix Then Return 0
            byteValue = byteValue * radix + digit
            position += 1
            digitCount += 1
        Wend
        If digitCount = 0 Then Return 0
    End Select
    Return -1
End Function

Private Function ReadGenericStaticStringBytes( _
    ByVal sourceText As String, values() As Long, _
    ByRef byteCount As Long _
) As Long
    byteCount = 0
    sourceText = Trim(sourceText)
    If sourceText = "" Then Return 0

    Dim capacity As Long = 16
    ReDim values(0 To capacity - 1)
    Dim position As Long = 1
    SkipCSpaces(sourceText, position)
    Do
        If position > Len(sourceText) OrElse _
           Asc(sourceText, position) <> Asc(Chr(34)) Then Return 0
        position += 1
        Dim hasClosingQuote As Long = 0
        While position <= Len(sourceText)
            Dim byteValue As Long = Asc(sourceText, position)
            If byteValue = Asc(Chr(34)) Then
                position += 1
                hasClosingQuote = -1
                Exit While
            ElseIf byteValue = Asc(Chr(92)) Then
                If ReadGenericStaticStringEscape( _
                    sourceText, position, byteValue) = 0 Then Return 0
            Else
                If byteValue = 0 OrElse byteValue = 10 OrElse byteValue = 13 Then _
                    Return 0
                position += 1
            End If

            If byteCount >= MAX_C_STRING_LITERAL_BYTES Then Return 0
            If byteCount >= capacity Then
                If capacity > MAX_C_STRING_LITERAL_BYTES \ 2 Then
                    capacity = MAX_C_STRING_LITERAL_BYTES
                Else
                    capacity *= 2
                End If
                ReDim Preserve values(0 To capacity - 1)
            End If
            values(byteCount) = byteValue
            byteCount += 1
        Wend
        If hasClosingQuote = 0 Then Return 0
        SkipCSpaces(sourceText, position)
        If position > Len(sourceText) OrElse _
           Asc(sourceText, position) <> Asc(Chr(34)) Then Exit Do
    Loop
    SkipCSpaces(sourceText, position)
    Return CInt(position > Len(sourceText)) * -1
End Function

Private Function TranslateGenericCharacterArrayInitializer( _
    ByVal sourceText As String, ByVal isUnsizedArray As Long, _
    ByVal useStringStorage As Long, ByRef arrayCount As Long, _
    ByRef initialValue As String _
) As Long
    Dim byteValues() As Long
    Dim byteCount As Long
    If ReadGenericStaticStringBytes( _
        sourceText, byteValues(), byteCount) = 0 Then Return 0

    Dim includeTerminator As Long = -1
    If isUnsizedArray <> 0 Then
        arrayCount = byteCount + 1
    ElseIf arrayCount <= 0 Then
        Return 0
    ElseIf byteCount >= arrayCount Then
        ' C permits an exact character-count bound without storing the final NUL.
        If byteCount <> arrayCount Then Return 0
        includeTerminator = 0
    End If

    If useStringStorage <> 0 Then
        initialValue = "!" & Chr(34)
        For byteIndex As Long = 0 To byteCount - 1
            Dim byteValue As Long = byteValues(byteIndex)
            If byteValue >= 32 AndAlso byteValue <= 126 AndAlso _
               byteValue <> Asc(Chr(92)) AndAlso byteValue <> Asc(Chr(34)) Then
                initialValue &= Chr(byteValue)
            Else
                initialValue &= Chr(92) & Right( _
                    "00" & Trim(Str(byteValue)), 3)
            End If
        Next
        initialValue &= Chr(34)
        Return -1
    End If

    initialValue = "{"
    For byteIndex As Long = 0 To byteCount - 1
        If byteIndex > 0 Then initialValue &= ", "
        initialValue &= Trim(Str(byteValues(byteIndex)))
    Next
    If includeTerminator <> 0 Then
        If byteCount > 0 Then initialValue &= ", "
        initialValue &= "0"
    End If
    ' FreeBASIC zero-fills the remaining elements of a static array initializer.
    initialValue &= "}"
    Return -1
End Function

Private Function IsGenericCharacterArrayType( _
    ByRef translation As C2fbTranslationContext, _
    ByVal commonType As String, ByVal freeBasicType As String, _
    ByVal stringOverrideMode As Long _
) As Long
    If Left(freeBasicType, 6) = "Const " Then _
        freeBasicType = Mid(freeBasicType, 7)
    If freeBasicType <> "Byte" AndAlso freeBasicType <> "UByte" Then Return 0

    Dim typeTokens() As String
    Dim typeTokenCount As Long
    TokenizeCDeclaration(commonType, typeTokens(), typeTokenCount)
    If typeTokenCount <= 0 Then Return 0
    Dim parsedType As C2fbCTypeModel
    If ParseCTypeModel(typeTokens(), 0, typeTokenCount - 1, parsedType) = 0 OrElse _
       parsedType.pointerDepth <> 0 Then Return 0
    Select Case parsedType.baseName
    Case "char", "signed char", "unsigned char"
        Return -1
    End Select
    If stringOverrideMode > 0 Then
        Dim aliasIndex As Long = GenericHeaderTypeAliasIndexInHeader( _
            translation.genericC.header, parsedType.baseName, 0)
        If aliasIndex >= 0 Then
            Dim characterKind As Long = _
                translation.genericC.header.headerTypeAliases(aliasIndex).characterKind
            If characterKind = C2FB_C_CHARACTER_CHAR OrElse _
               characterKind = C2FB_C_CHARACTER_WIDE Then Return -1
        End If
    End If
    Return 0
End Function

Declare Function TranslateGenericRecordModuleStaticInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceText As String, ByVal freeBasicType As String, _
    ByRef initialValue As String _
) As Long

Private Function TranslateGenericModuleStaticInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceText As String, ByVal freeBasicType As String, _
    ByVal isArray As Long, ByVal isPointerLike As Long, _
    ByVal isCallbackObject As Long, ByVal isCharacterArray As Long, _
    ByVal isUnsizedArray As Long, ByVal useStringStorage As Long, _
    ByRef arrayCount As Long, ByRef initialValue As String _
) As Long
    sourceText = Trim(sourceText)
    initialValue = ""
    If sourceText = "" Then Return -1

    If sourceText = "{0}" Then
        ' Module-level FreeBASIC storage starts zeroed, matching C's aggregate
        ' zero initializer without relying on C brace syntax.
        Return -1
    End If

    If isArray = 0 AndAlso isPointerLike = 0 AndAlso _
       Left(sourceText, 1) = "{" AndAlso _
       TranslateGenericRecordModuleStaticInitializer( _
           translation, sourceText, freeBasicType, initialValue) <> 0 Then _
        Return -1

    If Left(sourceText, 1) = "{" Then
        Dim closingBrace As Long = 1
        If ScanSimpleExpressionGroup( _
            sourceText, closingBrace, Asc("{"), Asc("}")) = 0 OrElse _
           Trim(Mid(sourceText, closingBrace)) <> "" Then Return 0
        Dim initializerBody As String = Trim(Mid( _
            sourceText, 2, closingBrace - 3))
        If initializerBody = "" Then Return -1
        If Right(initializerBody, 1) = "," Then _
            initializerBody = RTrim(Left( _
                initializerBody, Len(initializerBody) - 1))
        Dim values() As String
        Dim valueCount As Long
        If SplitSimpleCList( _
            initializerBody, values(), valueCount, 0, _
            MAX_GENERIC_STATIC_INITIALIZER_VALUES) = 0 OrElse _
           valueCount <= 0 Then Return 0
        If isArray = 0 AndAlso valueCount <> 1 Then Return 0
        If arrayCount > 0 AndAlso valueCount > arrayCount Then Return 0
        If arrayCount = 0 AndAlso isArray <> 0 Then arrayCount = valueCount

        Dim translatedValues As String
        For valueIndex As Long = 0 To valueCount - 1
            Dim translatedValue As String
            If TranslateGenericModuleStaticInitializer( _
                translation, values(valueIndex), freeBasicType, 0, _
                isPointerLike, isCallbackObject, 0, 0, 0, arrayCount, _
                translatedValue) = 0 Then Return 0
            If valueIndex > 0 Then translatedValues &= ", "
            translatedValues &= translatedValue
        Next
        initialValue = "{" & translatedValues & "}"
        Return -1
    End If

    If isArray <> 0 Then
        If isCharacterArray <> 0 AndAlso Left(sourceText, 1) = Chr(34) Then _
            Return TranslateGenericCharacterArrayInitializer( _
                sourceText, isUnsizedArray, useStringStorage, _
                arrayCount, initialValue)
        Return 0
    End If
    If isPointerLike <> 0 AndAlso isCallbackObject = 0 Then
        Dim pointerStorageType As String = _
            GenericHeaderTypeAliasStorageTypeInHeader( _
                translation.genericC.header, freeBasicType, 0)
        If pointerStorageType = "" Then pointerStorageType = freeBasicType
        If Left(pointerStorageType, 6) = "Const " Then _
            pointerStorageType = Trim(Mid(pointerStorageType, 7))
        If pointerStorageType = "ZString Ptr" Then
            Dim stringPointerValue As String
            If TranslateGenericHeaderStringMacro( _
                sourceText, stringPointerValue) <> 0 Then
                ' C string literals have static storage duration. STRPTR on a
                ' FreeBASIC literal likewise points at compiler-owned storage.
                initialValue = "CPtr(" & freeBasicType & ", " & _
                    stringPointerValue & ")"
                Return -1
            End If
        End If
    End If
    If isPointerLike <> 0 AndAlso _
       GenericNullPointerConstantText(sourceText) <> 0 Then
        initialValue = "CPtr(" & freeBasicType & ", 0)"
        Return -1
    End If
    If TranslateGenericModuleStaticAddressInitializer( _
        translation, sourceText, isCallbackObject, initialValue) <> 0 Then Return -1

    If translation.target.profile = C2FB_TARGET_GENERIC Then
        Dim enumExpression As String
        If TranslateGenericEnumExpression( _
            translation, sourceText, enumExpression) <> 0 Then
            initialValue = enumExpression
            Return -1
        End If
    End If

    If GenericIntegerMacroCandidate(sourceText) <> 0 Then
        If TranslateGenericHeaderIntegerExpression( _
            translation, sourceText, initialValue, 0) <> 0 Then Return -1
    End If
    If TranslateGenericHeaderMacroExpression( _
        translation, sourceText, initialValue) <> 0 Then Return -1
    Return 0
End Function

' C record initializers follow member order. Explicit braces around fixed
' array fields are kept so FreeBASIC receives the same aggregate shape.
Private Function SplitGenericStaticInitializerList( _
    ByVal sourceText As String, values() As String, _
    ByRef valueCount As Long _
) As Long
    sourceText = Trim(sourceText)
    valueCount = 0
    If Left(sourceText, 1) <> "{" Then Return 0

    Dim closingBrace As Long = 1
    If ScanSimpleExpressionGroup( _
        sourceText, closingBrace, Asc("{"), Asc("}")) = 0 OrElse _
       Trim(Mid(sourceText, closingBrace)) <> "" Then Return 0
    Dim bodyText As String = Trim(Mid( _
        sourceText, 2, closingBrace - 3))
    If bodyText = "" Then Return -1
    If Right(bodyText, 1) = "," Then _
        bodyText = RTrim(Left(bodyText, Len(bodyText) - 1))
    If bodyText = "" Then Return -1
    Return SplitSimpleCList(bodyText, values(), valueCount)
End Function

Private Function UnwrapGenericStaticScalarInitializer( _
    ByVal sourceText As String, ByRef scalarText As String, _
    ByVal nestingDepth As Long _
) As Long
    If nestingDepth > MAX_GENERIC_ARRAY_RANK Then Return 0
    sourceText = Trim(sourceText)
    If Left(sourceText, 1) <> "{" Then
        scalarText = sourceText
        Return IIf(sourceText <> "", -1, 0)
    End If

    Dim values() As String
    Dim valueCount As Long
    If SplitGenericStaticInitializerList( _
        sourceText, values(), valueCount) = 0 OrElse valueCount <> 1 Then Return 0
    Return UnwrapGenericStaticScalarInitializer( _
        values(0), scalarText, nestingDepth + 1)
End Function

Private Function GenericRecordBindingForStaticInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal freeBasicType As String, _
    ByRef recordBinding As C2fbRecordBindingModel _
) As Long
    If Left(freeBasicType, 6) = "Const " Then freeBasicType = Mid(freeBasicType, 7)
    Dim storageType As String = GenericHeaderTypeAliasStorageTypeInHeader( _
        translation.genericC.header, freeBasicType, 0)
    If storageType = "" OrElse storageType = "Any" Then Return 0

    For declarationIndex As Long = 0 To translation.sourceUnit.declarationCount - 1
        If declarationIndex > UBound(translation.genericC.header.recordPlans) Then Exit For
        Dim plan As C2fbGenericRecordPlanModel = _
            translation.genericC.header.recordPlans(declarationIndex)
        ' A forward typedef may resolve through its backing UDT name rather
        ' than the generic "Record" storage marker.
        If plan.isSupported <> 0 AndAlso plan.bindingReady <> 0 AndAlso _
           (plan.freeBasicName = freeBasicType OrElse _
            plan.aliasFreeBasicName = freeBasicType OrElse _
            plan.implementationName = storageType) Then
            recordBinding = plan.binding
            Return -1
        End If
        For nestedIndex As Long = 0 To plan.nestedBindingCount - 1
            If plan.nestedBindings(nestedIndex).freeBasicName = freeBasicType OrElse _
               plan.nestedBindings(nestedIndex).freeBasicName = storageType Then
                recordBinding = plan.nestedBindings(nestedIndex)
                Return -1
            End If
        Next
    Next
    Return 0
End Function

Private Function TranslateGenericRecordModuleStaticInitializer( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceText As String, ByVal freeBasicType As String, _
    ByRef initialValue As String _
) As Long
    initialValue = ""
    Dim recordBinding As C2fbRecordBindingModel
    Dim bindingResult As Long = GenericRecordBindingForStaticInitializer( _
        translation, freeBasicType, recordBinding)
    If bindingResult = 0 OrElse recordBinding.itemCount <= 0 Then Return 0

    Dim items() As String
    Dim itemCount As Long
    If SplitGenericStaticInitializerList( _
        sourceText, items(), itemCount) = 0 Then Return 0

    Dim initializerIndex As Long = 0
    Dim translatedFields As String
    For fieldIndex As Long = 0 To recordBinding.itemCount - 1
        With recordBinding.items(fieldIndex)
            If .kind <> C2FB_RECORD_BINDING_ITEM_FIELD OrElse _
               .isBitField <> 0 OrElse _
               .fieldType = "" OrElse _
               GenericHeaderTypeAliasStorageTypeInHeader( _
                   translation.genericC.header, .fieldType, 0) = "Record" Then Return 0
            Dim isCallbackField As Long = CInt( _
                Left(.fieldType, 9) = "Function " OrElse _
                Left(.fieldType, 4) = "Sub ") * -1
            Dim isPointerField As Long = _
                CInt(InStr(.fieldType, " Ptr") <> 0 OrElse _
                     isCallbackField <> 0) * -1
            Dim fieldValueCount As Long = .arrayCount
            If fieldValueCount = 0 Then
                If .arrayBounds <> "" Then Return 0
                fieldValueCount = 1
            ElseIf InStr(.arrayBounds, ",") <> 0 Then
                ' FreeBASIC nested-array initializers need a shape-preserving
                ' translation. Keep this first subset to one-dimensional fields.
                Return 0
            End If
            If fieldValueCount > MAX_FUNCTIONS Then Return 0

            Dim fieldInitialValue As String
            If .arrayCount > 0 Then
                Dim arrayValues() As String
                Dim arrayValueCount As Long = 0
                Dim explicitArrayList As Long = 0
                If initializerIndex < itemCount AndAlso _
                   Left(Trim(items(initializerIndex)), 1) = "{" Then
                    If SplitGenericStaticInitializerList( _
                        items(initializerIndex), arrayValues(), arrayValueCount) = 0 OrElse _
                       arrayValueCount > fieldValueCount Then Return 0
                    initializerIndex += 1
                    explicitArrayList = -1
                End If

                Dim translatedArrayValues As String
                For elementIndex As Long = 0 To fieldValueCount - 1
                    Dim sourceValue As String = "0"
                    If explicitArrayList <> 0 Then
                        If elementIndex < arrayValueCount Then _
                            sourceValue = arrayValues(elementIndex)
                    ElseIf initializerIndex < itemCount Then
                        sourceValue = items(initializerIndex)
                        initializerIndex += 1
                    End If
                    Dim scalarValue As String
                    If UnwrapGenericStaticScalarInitializer( _
                        sourceValue, scalarValue, 0) = 0 Then Return 0
                    Dim translatedValue As String
                    If isCallbackField <> 0 Then
                        If GenericNullPointerConstantText(scalarValue) = 0 Then _
                            Return 0
                        translatedValue = "0"
                    Else
                        Dim scalarArrayCount As Long = 0
                        If TranslateGenericModuleStaticInitializer( _
                            translation, scalarValue, .fieldType, 0, _
                            isPointerField, 0, 0, 0, 0, _
                            scalarArrayCount, translatedValue) = 0 Then _
                            Return 0
                    End If
                    If translatedArrayValues <> "" Then _
                        translatedArrayValues &= ", "
                    translatedArrayValues &= translatedValue
                Next
                fieldInitialValue = "{" & translatedArrayValues & "}"
            Else
                Dim sourceValue As String = "0"
                If initializerIndex < itemCount Then
                    sourceValue = items(initializerIndex)
                    initializerIndex += 1
                End If
                Dim scalarSourceValue As String
                If UnwrapGenericStaticScalarInitializer( _
                    sourceValue, scalarSourceValue, 0) = 0 Then Return 0
                Dim translatedValue As String
                If isCallbackField <> 0 Then
                    If GenericNullPointerConstantText(scalarSourceValue) = 0 Then _
                        Return 0
                    translatedValue = "0"
                Else
                    Dim scalarArrayCount As Long = 0
                    Dim fieldResult As Long = _
                        TranslateGenericModuleStaticInitializer( _
                            translation, scalarSourceValue, .fieldType, 0, _
                            isPointerField, 0, 0, 0, 0, _
                            scalarArrayCount, translatedValue)
                    If fieldResult = 0 Then Return 0
                End If
                fieldInitialValue = translatedValue
            End If

            If translatedFields <> "" Then translatedFields &= ", "
            translatedFields &= fieldInitialValue
            If recordBinding.isUnion <> 0 Then Exit For
        End With
    Next
    If initializerIndex < itemCount Then Return 0
    initialValue = "(" & translatedFields & ")"
    Return -1
End Function

Private Function ResolveGenericCallbackArrayObjectTypeInContext( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, tokens() As String, _
    ByVal tokenCount As Long, ByVal sourceName As String, _
    ByRef callbackResult As C2fbCallbackTypeResultModel _
) As Long
    For nameToken As Long = 0 To tokenCount - 2
        If tokens(nameToken) <> sourceName OrElse _
           tokens(nameToken + 1) <> "[" Then Continue For
        Dim arrayClose As Long = nameToken + 1
        Dim arrayDepth As Long = 0
        For tokenIndex As Long = nameToken + 1 To tokenCount - 1
            If tokens(tokenIndex) = "[" Then
                arrayDepth += 1
            ElseIf tokens(tokenIndex) = "]" Then
                arrayDepth -= 1
                If arrayDepth = 0 Then
                    arrayClose = tokenIndex
                    Exit For
                End If
            End If
        Next
        If arrayDepth <> 0 Then Return 0

        Dim callbackTokens() As String
        ReDim callbackTokens(0 To tokenCount - 1)
        Dim callbackTokenCount As Long = 0
        For tokenIndex As Long = 0 To tokenCount - 1
            If tokenIndex = nameToken + 1 Then
                tokenIndex = arrayClose
                Continue For
            End If
            If tokenIndex = nameToken Then
                callbackTokens(callbackTokenCount) = "c2fb_callback_array_element"
            Else
                callbackTokens(callbackTokenCount) = tokens(tokenIndex)
            End If
            callbackTokenCount += 1
        Next
        ResolveCBodyCallbackTypeInContext( _
            translation, catalog, callbackTokens(), 0, _
            callbackTokenCount - 1, callbackResult, 0, _
            C2FB_TYPE_MAPPING_GENERIC_HEADER)
        Return CInt(callbackResult.status = _
                    C2FB_CALLBACK_TYPE_CONVERTED) * -1
    Next
    Return 0
End Function

Private Function EmitGenericFunctionAliasUsage( _
    ByRef translation As C2fbTranslationContext, _
    ByVal functionType As String, ByVal sourceName As String, _
    ByRef outputFile As C2fbGeneratedFileOutputModel, names() As String, ByRef nameCount As Long, _
    outputNames() As String, ByRef outputNameCount As Long, _
    ByRef conversionSummary As C2fbConversionSummaryModel _
) As Long
    Dim isSub As Long = CInt(Left(functionType, 4) = "Sub ")
    If isSub = 0 AndAlso Left(functionType, 9) <> "Function " Then Return 0
    Dim callingConventionPosition As Long = 0
    Dim callingConvention As String
    Dim cdeclPosition As Long = InStr(functionType, "CDecl(")
    Dim stdcallPosition As Long = InStr(functionType, "StdCall(")
    Dim pascalPosition As Long = InStr(functionType, "Pascal(")
    If cdeclPosition > 0 Then
        callingConventionPosition = cdeclPosition
        callingConvention = "CDecl"
    End If
    If stdcallPosition > 0 AndAlso _
       (callingConventionPosition = 0 OrElse _
        stdcallPosition < callingConventionPosition) Then
        callingConventionPosition = stdcallPosition
        callingConvention = "StdCall"
    End If
    If pascalPosition > 0 AndAlso _
       (callingConventionPosition = 0 OrElse _
        pascalPosition < callingConventionPosition) Then
        callingConventionPosition = pascalPosition
        callingConvention = "Pascal"
    End If
    If callingConventionPosition = 0 Then Return 0
    Dim openParen As Long = callingConventionPosition + Len(callingConvention)
    Dim closePosition As Long = openParen
    If ScanSimpleExpressionGroup( _
        functionType, closePosition, Asc("("), Asc(")")) = 0 Then Return 0
    Dim parameters As String = Trim(Mid( _
        functionType, openParen + 1, closePosition - openParen - 2))
    Dim trailingType As String = Trim(Mid(functionType, closePosition))
    If isSub = 0 Then
        If Left(trailingType, 3) <> "As " OrElse Len(trailingType) <= 3 Then Return 0
        trailingType = Trim(Mid(trailingType, 4))
    ElseIf trailingType <> "" Then
        Return 0
    End If
    parameters = ApplyGenericStringOverrideInGenericC( _
        translation.genericC, sourceName, parameters)
    trailingType = ApplyGenericStringOverrideInGenericC( _
        translation.genericC, sourceName, trailingType)
    If GenericHeaderSourceNameWasEmitted( _
        translation, names(), nameCount, sourceName) <> 0 Then Return -1

    Dim emittedName As String = _
        GenericFreeBasicFunctionNameInContext( _
            translation, sourceName, outputNames(), outputNameCount, _
            C2FB_SYMBOL_RENAME_PROCEDURE)
    If nameCount >= MAX_GENERIC_HEADER_OUTPUT_NAMES OrElse _
       outputNameCount >= MAX_GENERIC_HEADER_OUTPUT_NAMES Then _
        ReportError("generic header exceeds the configured function limit")
    Dim procedureBinding As C2fbProcedureBindingModel
    procedureBinding.freeBasicName = emittedName
    procedureBinding.cName = sourceName
    procedureBinding.returnType = trailingType
    procedureBinding.parameterList = parameters
    procedureBinding.callingConvention = callingConvention
    procedureBinding.isSub = isSub
    WriteGenericProcedureBindingWithGuardsInPolicy( _
        translation.genericC.policy, outputFile, sourceName, procedureBinding)
    RecordGenericHeaderProcedureInContext(translation, sourceName, procedureBinding)
    If nameCount > UBound(names) Then _
        ReDim Preserve names(0 To UBound(names) * 2 + 1)
    names(nameCount) = sourceName
    nameCount += 1
    If outputNameCount > UBound(outputNames) Then _
        ReDim Preserve outputNames(0 To UBound(outputNames) * 2 + 1)
    outputNames(outputNameCount) = emittedName
    outputNameCount += 1
    conversionSummary.convertedCount += 1
    Return -1
End Function

Declare Sub EmitGenericExternalObjectTokensCore( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal declaration As String, ByRef outputFile As C2fbGeneratedFileOutputModel, _
    tokens() As String, ByVal tokenCount As Long, _
    names() As String, ByRef nameCount As Long, _
    outputNames() As String, ByRef outputNameCount As Long, _
    ByRef conversionSummary As C2fbConversionSummaryModel, _
    ByVal linkName As String, ByVal isReferenceObject As Long = 0 _
)

Private Sub EmitGenericExternalObjectTokens( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal declaration As String, ByRef outputFile As C2fbGeneratedFileOutputModel, _
    tokens() As String, ByVal tokenCount As Long, _
    names() As String, ByRef nameCount As Long, _
    outputNames() As String, ByRef outputNameCount As Long, _
    ByRef conversionSummary As C2fbConversionSummaryModel, _
    ByVal isReferenceObject As Long = 0 _
)
    Dim loweredDeclaration As String
    Dim linkName As String
    Dim assemblyNameStatus As Long = ParseCAssemblyNameLabel( _
        declaration, loweredDeclaration, linkName)
    If assemblyNameStatus > 0 Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, _
            "unsupported assembler symbol name", declaration)
        Exit Sub
    ElseIf assemblyNameStatus < 0 Then
        Dim loweredTokens() As String
        Dim loweredTokenCount As Long
        TokenizeCDeclaration( _
            StripCFunctionAttributes(loweredDeclaration), _
            loweredTokens(), loweredTokenCount)
        EmitGenericExternalObjectTokensCore( _
            translation, catalog, loweredDeclaration, outputFile, _
            loweredTokens(), loweredTokenCount, names(), nameCount, _
            outputNames(), outputNameCount, conversionSummary, linkName, _
            isReferenceObject)
    Else
        EmitGenericExternalObjectTokensCore( _
            translation, catalog, declaration, outputFile, tokens(), tokenCount, _
            names(), nameCount, outputNames(), outputNameCount, _
            conversionSummary, "", isReferenceObject)
    End If
End Sub

Private Sub EmitGenericExternalObjectTokensCore( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal declaration As String, ByRef outputFile As C2fbGeneratedFileOutputModel, _
    tokens() As String, ByVal tokenCount As Long, _
    names() As String, ByRef nameCount As Long, _
    outputNames() As String, ByRef outputNameCount As Long, _
    ByRef conversionSummary As C2fbConversionSummaryModel, _
    ByVal linkName As String, ByVal isReferenceObject As Long = 0 _
)
    If isReferenceObject = 0 Then
        Dim referenceIndex As Long = -1
        Dim parenDepth As Long = 0
        Dim bracketDepth As Long = 0
        Dim braceDepth As Long = 0
        For tokenIndex As Long = 0 To tokenCount - 1
            Select Case tokens(tokenIndex)
            Case "(": parenDepth += 1
            Case ")": If parenDepth > 0 Then parenDepth -= 1
            Case "[": bracketDepth += 1
            Case "]": If bracketDepth > 0 Then bracketDepth -= 1
            Case "{": braceDepth += 1
            Case "}": If braceDepth > 0 Then braceDepth -= 1
            Case "=":
                If parenDepth = 0 AndAlso bracketDepth = 0 AndAlso _
                   braceDepth = 0 Then Exit For
            Case "&":
                If parenDepth = 0 AndAlso bracketDepth = 0 AndAlso _
                   braceDepth = 0 Then
                    If referenceIndex >= 0 Then
                        EmitC2fbSkipped(translation, outputFile, _
                            conversionSummary, "unsupported C++ reference object", declaration)
                        Exit Sub
                    End If
                    referenceIndex = tokenIndex
                End If
            End Select
        Next
        If referenceIndex >= 0 Then
            If translation.genericC.header.cppMode = 0 Then
                EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                    "C++ reference object requires C++ preprocessing mode", declaration)
                Exit Sub
            End If
            Dim referenceTokens() As String
            ReDim referenceTokens(0 To tokenCount - 2)
            Dim referenceTokenCount As Long = 0
            For tokenIndex As Long = 0 To tokenCount - 1
                If tokenIndex = referenceIndex Then Continue For
                AddToken(referenceTokens(), referenceTokenCount, tokens(tokenIndex))
            Next
            EmitGenericExternalObjectTokensCore( _
                translation, catalog, declaration, outputFile, _
                referenceTokens(), referenceTokenCount, names(), nameCount, _
                outputNames(), outputNameCount, conversionSummary, linkName, -1)
            Exit Sub
        End If
    End If

    Dim commonType As String
    Dim sourceName As String
    Dim bindingOrder As String
    Dim baseTokenCount As Long
    If ParseGenericHeaderDeclaratorTokens( _
        tokens(), 0, tokenCount - 1, commonType, sourceName, bindingOrder, _
        baseTokenCount) = 0 Then Exit Sub
    Dim isThreadLocal As Long = 0
    For tokenIndex As Long = 0 To tokenCount - 1
        Select Case tokens(tokenIndex)
        Case "_Thread_local", "__thread": isThreadLocal = -1
        Case "thread_local"
            If tokens(tokenIndex) <> sourceName Then isThreadLocal = -1
        End Select
    Next
    Dim hasStaticStorage As Long = 0
    Dim hasExternStorage As Long = 0
    For tokenIndex As Long = 0 To tokenCount - 1
        If tokens(tokenIndex) = "typedef" Then Exit Sub
        If tokens(tokenIndex) = "static" Then hasStaticStorage = -1
        If tokens(tokenIndex) = "extern" Then hasExternStorage = -1
    Next
    If isThreadLocal <> 0 AndAlso _
       translation.genericC.header.cppMode <> 0 AndAlso _
       hasStaticStorage = 0 Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, _
            "external C++ thread-local object needs a C++ linker name", _
            sourceName & ": " & declaration)
        Exit Sub
    End If
    If GenericVariableExcludedInPolicy(translation.genericC.policy, sourceName) <> 0 AndAlso _
       (hasStaticStorage = 0 OrElse _
        translation.genericC.policy.includeStaticObjects = 0) Then Exit Sub
    Dim initializerToken As Long
    If FindC2fbTopLevelAssignmentToken( _
        tokens(), 0, tokenCount - 1, initializerToken) = 0 Then Exit Sub
    Dim sourceInitializerText As String
    If initializerToken >= 0 Then
        Dim initializerOperator As String
        Dim initializerPosition As Long
        If FindGenericMacroAssignmentOperator( _
            declaration, initializerOperator, initializerPosition) <= 0 OrElse _
           initializerOperator <> "=" Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported object initializer", declaration)
            Exit Sub
        End If
        sourceInitializerText = Trim(Mid(declaration, initializerPosition + 1))
        If Right(sourceInitializerText, 1) = ";" Then _
            sourceInitializerText = RTrim(Left( _
                sourceInitializerText, Len(sourceInitializerText) - 1))
        If sourceInitializerText = "" Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "empty object initializer", declaration)
            Exit Sub
        End If
    End If

    Dim hasFunctionOperator As Long = 0
    Dim pointerDepth As Long = 0
    Dim arrayDimensions As Long = 0
    For bindingIndex As Long = 1 To Len(bindingOrder)
        Select Case Mid(bindingOrder, bindingIndex, 1)
        Case "F": hasFunctionOperator = -1
        Case "P": pointerDepth += 1
        Case "A": arrayDimensions += 1
        End Select
    Next
    If Len(bindingOrder) > 0 AndAlso Left(bindingOrder, 1) = "F" Then Exit Sub
    Dim callbackObject As Long = 0
    Dim isCallbackArray As Long = 0
    Dim callbackResult As C2fbCallbackTypeResultModel
    Dim freeBasicType As String
    If hasFunctionOperator <> 0 Then
        Dim callbackLastToken As Long = tokenCount - 1
        If initializerToken >= 0 Then callbackLastToken = initializerToken - 1
        If callbackLastToken >= 0 AndAlso _
           tokens(callbackLastToken) = ";" Then callbackLastToken -= 1
        ResolveCBodyCallbackTypeInContext( _
            translation, catalog, tokens(), 0, callbackLastToken, _
            callbackResult, 0, C2FB_TYPE_MAPPING_GENERIC_HEADER, -1)
        isCallbackArray = CInt( _
            arrayDimensions = 1 AndAlso Left(bindingOrder, 1) = "A" AndAlso _
            Mid(bindingOrder, 2) = "PF") * -1
        If isCallbackArray <> 0 AndAlso _
           (callbackResult.status <> C2FB_CALLBACK_TYPE_CONVERTED OrElse _
            callbackResult.callbackName <> sourceName) Then _
            ResolveGenericCallbackArrayObjectTypeInContext( _
                translation, catalog, tokens(), callbackLastToken + 1, _
                sourceName, callbackResult)
        If callbackResult.status = C2FB_CALLBACK_TYPE_UNSUPPORTED Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported callback object", declaration)
            Exit Sub
        End If
        If callbackResult.status = C2FB_CALLBACK_TYPE_CONVERTED AndAlso _
           (callbackResult.callbackName = sourceName OrElse isCallbackArray <> 0) Then
            freeBasicType = CBodyCallbackDeclarationInContext( _
                translation, callbackResult.freeBasicType)
            If freeBasicType <> "" Then callbackObject = -1
        End If
    End If
    If hasFunctionOperator <> 0 AndAlso callbackObject = 0 Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, "function-pointer object", declaration)
        Exit Sub
    End If
    If GenericHeaderSourceNameWasEmitted( _
        translation, names(), nameCount, sourceName) <> 0 Then Exit Sub

    If callbackObject <> 0 AndAlso callbackResult.pointerDepth > 1 Then
        If callbackResult.pointerDepth > MAX_FREEBASIC_POINTER_LEVELS Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "pointer depth exceeds FreeBASIC's configured limit", declaration)
            Exit Sub
        End If
        Dim generatedPrefix As String = translation.genericC.policy.generatedNamePrefix
        If generatedPrefix = "" Then generatedPrefix = "c2fb_"
        Dim callbackAliasName As String = _
            "_" & generatedPrefix & "callback_" & sourceName
        While C2fbNameListContains( _
            outputNames(), outputNameCount, callbackAliasName) <> 0
            callbackAliasName &= "_"
        Wend
        If outputNameCount >= MAX_GENERIC_HEADER_OUTPUT_NAMES Then _
            ReportError("generic header exceeds the configured output-name limit")
        Dim callbackTypeAlias As C2fbTypeAliasBindingModel
        callbackTypeAlias.freeBasicName = callbackAliasName
        callbackTypeAlias.targetType = freeBasicType
        WriteC2fbTypeAlias(outputFile, callbackTypeAlias)
        If outputNameCount > UBound(outputNames) Then _
            ReDim Preserve outputNames(0 To UBound(outputNames) * 2 + 1)
        outputNames(outputNameCount) = callbackAliasName
        outputNameCount += 1
        freeBasicType = callbackAliasName
        For pointerIndex As Long = 2 To callbackResult.pointerDepth
            freeBasicType &= " Ptr"
        Next
    End If

    If baseTokenCount <= 0 Then Exit Sub
    Dim baseTokens() As String
    ReDim baseTokens(0 To baseTokenCount - 1)
    Dim sourceTypeTokenCount As Long = baseTokenCount
    baseTokenCount = 0
    For tokenIndex As Long = 0 To sourceTypeTokenCount - 1
        Select Case tokens(tokenIndex)
        Case "_Thread_local", "__thread", "thread_local"
            Continue For
        End Select
        AddToken(baseTokens(), baseTokenCount, tokens(tokenIndex))
    Next
    If baseTokenCount = 0 Then Exit Sub
    commonType = GenericTokensToText(baseTokens(), 0, baseTokenCount - 1)

    If callbackObject = 0 AndAlso bindingOrder = "" Then
        Dim functionTypeAliasIndex As Long = -1
        For tokenIndex As Long = 0 To baseTokenCount - 1
            Dim candidateAlias As Long = _
                GenericHeaderTypeAliasIndexInHeader( _
                    translation.genericC.header, baseTokens(tokenIndex), 0)
            If candidateAlias >= 0 AndAlso _
               translation.genericC.header.headerTypeAliases(candidateAlias).isFunction <> 0 Then
                functionTypeAliasIndex = candidateAlias
                Exit For
            End If
        Next
        If functionTypeAliasIndex >= 0 Then
            If CDeclarationHasStdCall(declaration) <> 0 Then
                EmitC2fbSkipped(translation,  _
                    outputFile, conversionSummary, "non-CDecl function type alias use", declaration)
                Exit Sub
            End If
            Dim functionType As String = _
                GenericHeaderFunctionAliasTypeInHeader( _
                    translation.genericC.header, functionTypeAliasIndex)
            If functionType = "" OrElse EmitGenericFunctionAliasUsage( _
                translation, _
                functionType, sourceName, outputFile, names(), nameCount, _
                outputNames(), outputNameCount, conversionSummary) = 0 Then
                EmitC2fbSkipped(translation,  _
                    outputFile, conversionSummary, "unsupported function type alias use", declaration)
            End If
            Exit Sub
        End If
    End If

    Dim arrayCount As Long = 0
    Dim arrayBounds As String
    Dim unsizedArray As Long = 0
    Dim arraySizeHint As Long = 0
    Dim unsizedStringType As String
    Dim arrayTypeAliasIndex As Long = -1
    For tokenIndex As Long = 0 To baseTokenCount - 1
        Dim candidateAlias As Long = GenericHeaderTypeAliasIndexInHeader( _
            translation.genericC.header, baseTokens(tokenIndex), 0)
        If candidateAlias >= 0 AndAlso _
           translation.genericC.header.headerTypeAliases(candidateAlias).isArray <> 0 Then
            arrayTypeAliasIndex = candidateAlias
            Exit For
        End If
    Next
    If isCallbackArray <> 0 Then
        Dim namePosition As Long = InStr(declaration, sourceName)
        Dim openBracket As Long = 0
        If namePosition > 0 Then _
            openBracket = InStr(namePosition + Len(sourceName), declaration, "[")
        Dim closeBracket As Long = 0
        If openBracket > 0 Then _
            closeBracket = InStr(openBracket + 1, declaration, "]")
        If openBracket = 0 OrElse closeBracket = 0 Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported callback array bound", declaration)
            Exit Sub
        End If
        Dim boundExpression As String = Trim(Mid( _
            declaration, openBracket + 1, closeBracket - openBracket - 1))
        If boundExpression = "" Then
            unsizedArray = -1
        ElseIf EvaluateSimpleArrayBound( _
            boundExpression, arrayCount, MAX_GENERIC_ARRAY_COUNT) = 0 AndAlso _
            EvaluateGenericHeaderArrayBound( _
                translation, boundExpression, arrayCount, _
                MAX_GENERIC_ARRAY_COUNT) = 0 Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported callback array bound", declaration)
            Exit Sub
        End If
    End If
    If callbackObject = 0 Then
    If arrayTypeAliasIndex >= 0 Then
        If bindingOrder = "" Then
            freeBasicType = translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).arrayElementType
            arrayCount = translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).arrayCount
            arrayBounds = translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).arrayBounds
            If arrayCount = 0 Then unsizedArray = -1
        ElseIf Left(bindingOrder, 1) = "P" AndAlso pointerDepth > 0 Then
            ' A pointer to an array typedef is kept opaque because its stride
            ' includes the typedef's hidden array dimension.
            freeBasicType = "Any Ptr"
            For pointerIndex As Long = 2 To pointerDepth
                freeBasicType &= " Ptr"
            Next
        ElseIf Left(bindingOrder, 1) = "A" AndAlso _
               arrayDimensions > 0 AndAlso pointerDepth = 0 Then
            Dim identifierPosition As Long = _
                GenericLastIdentifierPosition(declaration, sourceName)
            Dim objectBounds As String
            Dim objectFirstCount As Long
            Dim objectElementCount As Long
            Dim objectUnsized As Long
            If identifierPosition = 0 OrElse _
               ParseGenericArrayBoundsInDeclaration( _
                translation, declaration, identifierPosition + Len(sourceName), _
                arrayDimensions, objectBounds, objectFirstCount, _
                objectElementCount, objectUnsized) = 0 OrElse objectUnsized <> 0 Then
                EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                    "unsupported nested array typedef object", declaration)
                Exit Sub
            End If
            Dim typedefElementCount As Long = _
                translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).arrayCount
            If typedefElementCount <= 0 OrElse _
               objectElementCount > MAX_GENERIC_ARRAY_COUNT \ typedefElementCount Then
                EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                    "oversized nested array typedef object", declaration)
                Exit Sub
            End If
            arrayCount = objectElementCount * typedefElementCount
            arrayBounds = CombineGenericArrayBounds( _
                objectBounds, _
                translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).arrayBounds)
            freeBasicType = _
                translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).arrayElementType
        ElseIf Left(bindingOrder, 1) = "A" AndAlso _
               arrayDimensions = 1 AndAlso pointerDepth > 0 Then
            Dim openingBracket As Long = InStr(declaration, "[")
            Dim closingBracket As Long = InStr(openingBracket + 1, declaration, "]")
            If openingBracket = 0 OrElse closingBracket = 0 OrElse _
               (EvaluateSimpleArrayBound(Trim(Mid( _
                   declaration, openingBracket + 1, _
                   closingBracket - openingBracket - 1)), arrayCount, _
                   MAX_GENERIC_ARRAY_COUNT) = 0 AndAlso _
                EvaluateGenericHeaderArrayBound(translation, Trim(Mid( _
                    declaration, openingBracket + 1, _
                    closingBracket - openingBracket - 1)), arrayCount, _
                    MAX_GENERIC_ARRAY_COUNT) = 0) Then
                EmitC2fbSkipped(translation,  _
                    outputFile, conversionSummary, "unsupported array typedef object", declaration)
                Exit Sub
            End If
            freeBasicType = "Any Ptr"
            For pointerIndex As Long = 2 To pointerDepth
                freeBasicType &= " Ptr"
            Next
        Else
            EmitC2fbSkipped(translation, outputFile, conversionSummary, "nested array typedef object", declaration)
            Exit Sub
        End If
    ElseIf arrayDimensions > 0 AndAlso Left(bindingOrder, 1) = "A" Then
        Dim identifierPosition As Long = _
            GenericLastIdentifierPosition(declaration, sourceName)
        Dim firstArrayDimensionCount As Long
        Dim unsizedArrayBoundHint As Long = 0
        If arrayDimensions > 1 AndAlso hasStaticStorage = 0 AndAlso _
           initializerToken < 0 Then
            unsizedArrayBoundHint = UseGenericArraySizeOverrideInPolicy( _
                translation.genericC.policy, sourceName)
            If unsizedArrayBoundHint > 0 Then _
                arraySizeHint = unsizedArrayBoundHint
        End If
        If identifierPosition = 0 OrElse _
           ParseGenericArrayBoundsInDeclaration( _
            translation, declaration, identifierPosition + Len(sourceName), _
            arrayDimensions, arrayBounds, firstArrayDimensionCount, arrayCount, _
            unsizedArray, -1, 0, unsizedArrayBoundHint) = 0 Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported external array bounds", declaration)
            Exit Sub
        End If
        If unsizedArray <> 0 AndAlso arrayDimensions <> 1 Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported multidimensional unsized array", declaration)
            Exit Sub
        End If
        If pointerDepth > 0 Then
            If baseTokenCount + pointerDepth >= MAX_TYPE_WORDS Then
                EmitC2fbSkipped(translation,  _
                    outputFile, conversionSummary, "oversized external array type", declaration)
                Exit Sub
            End If
            Dim elementTokens() As String
            ReDim elementTokens(0 To MAX_TYPE_WORDS - 1)
            Dim elementTokenCount As Long = 0
            For tokenIndex As Long = 0 To baseTokenCount - 1
                AddToken(elementTokens(), elementTokenCount, baseTokens(tokenIndex))
            Next
            For pointerIndex As Long = 1 To pointerDepth
                AddToken(elementTokens(), elementTokenCount, "*")
            Next
            Dim resolvedArrayElementPointerType As C2fbResolvedTypeModel
            ResolveC2fbTypeModel(translation, catalog, _
                elementTokens(), 0, elementTokenCount - 1, resolvedArrayElementPointerType, _
                C2FB_POINTER_MAPPING_OPAQUE, C2FB_TYPE_MAPPING_GENERIC_HEADER)
            If resolvedArrayElementPointerType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then
                EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                    "unsupported external array type", _
                    C2fbTypeResolutionFailureDescription( _
                        resolvedArrayElementPointerType.status) & ": " & declaration)
                Exit Sub
            End If
            freeBasicType = resolvedArrayElementPointerType.freeBasicType
            If resolvedArrayElementPointerType.isConst <> 0 AndAlso _
               resolvedArrayElementPointerType.pointerCount = 0 Then _
                freeBasicType = "Const " & freeBasicType
        Else
            Dim resolvedArrayElementValueType As C2fbResolvedTypeModel
            ResolveC2fbTypeModel(translation, catalog, _
                baseTokens(), 0, baseTokenCount - 1, resolvedArrayElementValueType, _
                C2FB_POINTER_MAPPING_OPAQUE, C2FB_TYPE_MAPPING_GENERIC_HEADER)
            If resolvedArrayElementValueType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then
                EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                    "unsupported external array type", _
                    C2fbTypeResolutionFailureDescription( _
                        resolvedArrayElementValueType.status) & ": " & declaration)
                Exit Sub
            End If
            freeBasicType = resolvedArrayElementValueType.freeBasicType
            If resolvedArrayElementValueType.isConst <> 0 AndAlso _
               resolvedArrayElementValueType.pointerCount = 0 Then _
                freeBasicType = "Const " & freeBasicType
        End If
    ElseIf pointerDepth > 0 Then
        If arrayDimensions > 0 Then
            ' A pointer to an array is represented opaquely so BASIC indexing
            ' cannot apply the wrong element stride.
            freeBasicType = "Any Ptr"
            For pointerIndex As Long = 2 To pointerDepth
                freeBasicType &= " Ptr"
            Next
        Else
            If pointerDepth + baseTokenCount >= MAX_TYPE_WORDS Then
                EmitC2fbSkipped(translation,  _
                    outputFile, conversionSummary, "oversized external pointer type", declaration)
                Exit Sub
            End If
            Dim pointerTokens() As String
            ReDim pointerTokens(0 To MAX_TYPE_WORDS - 1)
            Dim pointerTokenCount As Long = 0
            For tokenIndex As Long = 0 To baseTokenCount - 1
                AddToken(pointerTokens(), pointerTokenCount, baseTokens(tokenIndex))
            Next
            For pointerIndex As Long = 1 To pointerDepth
                AddToken(pointerTokens(), pointerTokenCount, "*")
            Next
            Dim resolvedPointerType As C2fbResolvedTypeModel
            ResolveC2fbTypeModel(translation, catalog, _
                pointerTokens(), 0, pointerTokenCount - 1, resolvedPointerType, _
                C2FB_POINTER_MAPPING_OPAQUE, C2FB_TYPE_MAPPING_GENERIC_HEADER)
            If resolvedPointerType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then
                EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                    "unsupported external pointer type", _
                    C2fbTypeResolutionFailureDescription( _
                        resolvedPointerType.status) & ": " & declaration)
                Exit Sub
            End If
            freeBasicType = resolvedPointerType.freeBasicType
        End If
    ElseIf arrayDimensions > 0 Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, "unsupported external array", declaration)
        Exit Sub
    ElseIf bindingOrder = "" Then
        Dim resolvedValueType As C2fbResolvedTypeModel
        ' C permits an extern object declaration with an incomplete record
        ' type. The FreeBASIC declaration is useful for address-taking and
        ' ByRef calls; operations that need its layout remain compiler errors.
        Dim allowIncompleteExternValue As Long = CInt( _
            hasExternStorage <> 0 AndAlso hasStaticStorage = 0 AndAlso _
            initializerToken < 0 AndAlso isReferenceObject = 0) * -1
        ResolveC2fbTypeModel(translation, catalog, _
            baseTokens(), 0, baseTokenCount - 1, resolvedValueType, _
            C2FB_POINTER_MAPPING_OPAQUE, C2FB_TYPE_MAPPING_GENERIC_HEADER, _
            C2FB_TYPE_RECOGNITION_ALLOW_FALLBACK, allowIncompleteExternValue)
        If resolvedValueType.status <> C2FB_TYPE_RESOLUTION_RESOLVED Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported external value type", _
                C2fbTypeResolutionFailureDescription( _
                    resolvedValueType.status) & ": " & declaration)
            Exit Sub
        End If
        freeBasicType = resolvedValueType.freeBasicType
        If resolvedValueType.isConst <> 0 AndAlso resolvedValueType.pointerCount = 0 Then _
            freeBasicType = "Const " & freeBasicType
    Else
        EmitC2fbSkipped(translation,  _
            outputFile, conversionSummary, "unsupported external declarator", declaration)
        Exit Sub
    End If
    End If

    If pointerDepth = 0 AndAlso arrayDimensions = 0 AndAlso callbackObject = 0 AndAlso _
       GenericHeaderTypeIsOpaqueEmptyRecordInHeader( _
        translation.genericC.header, freeBasicType) <> 0 Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, _
            "zero-size record object has no FreeBASIC by-value storage", declaration)
        Exit Sub
    End If

    Dim stringOverrideName As String = sourceName
    If GenericStringOverrideModeInPolicy( _
        translation.genericC.policy, stringOverrideName) = 0 AndAlso _
       arrayTypeAliasIndex >= 0 Then _
        stringOverrideName = _
            translation.genericC.header.headerTypeAliases(arrayTypeAliasIndex).sourceName
    Dim stringOverrideMode As Long = _
        GenericStringOverrideModeInPolicy( _
            translation.genericC.policy, stringOverrideName)
    If stringOverrideMode = 0 Then _
        stringOverrideMode = GenericHeaderFbfrogStringDefaultMode( _
            translation, commonType, freeBasicType)
    If stringOverrideMode > 0 AndAlso pointerDepth = 0 AndAlso _
       arrayDimensions = 0 AndAlso callbackObject = 0 Then
        Dim characterScalarType As String = _
            GenericHeaderFbfrogCharacterScalarType( _
                translation, commonType, freeBasicType)
        If characterScalarType <> "" Then freeBasicType = characterScalarType
    End If
    Dim isCharacterArray As Long = CInt( _
        arrayDimensions = 1 AndAlso pointerDepth = 0 AndAlso _
        IsGenericCharacterArrayType( _
            translation, commonType, freeBasicType, stringOverrideMode) <> 0) * -1

    Dim constAggregateStorageType As String
    Dim isConstAggregateAccessor As Long = 0
    If Left(freeBasicType, 6) = "Const " AndAlso _
       pointerDepth = 0 AndAlso arrayDimensions = 0 AndAlso _
       arrayCount = 0 AndAlso unsizedArray = 0 AndAlso callbackObject = 0 AndAlso _
       isReferenceObject = 0 Then
        constAggregateStorageType = Trim(Mid(freeBasicType, 7))
        If constAggregateStorageType = "jmp_buf" OrElse _
           GenericHeaderTypeAliasStorageTypeInHeader( _
               translation.genericC.header, constAggregateStorageType, 0) = "Record" Then
            isConstAggregateAccessor = -1
        Else
            constAggregateStorageType = ""
        End If
    End If

    Dim initialValue As String
    If hasStaticStorage <> 0 AndAlso initializerToken >= 0 Then
        Dim isArrayObject As Long = CInt( _
            arrayBounds <> "" OrElse arrayCount > 0 OrElse _
            unsizedArray <> 0) * -1
        Dim isPointerObject As Long = CInt( _
            pointerDepth > 0 OrElse callbackObject <> 0) * -1
        Dim initializerType As String = freeBasicType
        If isConstAggregateAccessor <> 0 Then _
            initializerType = constAggregateStorageType
        If TranslateGenericModuleStaticInitializer( _
            translation, sourceInitializerText, initializerType, _
            isArrayObject, isPointerObject, callbackObject, _
            isCharacterArray, unsizedArray, _
            CInt(stringOverrideMode > 0) * -1, arrayCount, initialValue) = 0 Then
            EmitC2fbSkipped(translation, outputFile, conversionSummary, _
                "unsupported module-static initializer", declaration)
            Exit Sub
        End If
        If unsizedArray <> 0 AndAlso arrayCount > 0 Then
            unsizedArray = 0
        End If
    End If

    If unsizedArray <> 0 Then
        arraySizeHint = UseGenericArraySizeOverrideInPolicy(translation.genericC.policy, sourceName)
        If arraySizeHint > 0 Then
            arrayCount = arraySizeHint
            unsizedArray = 0
        End If
    End If

    If arrayCount > 0 AndAlso stringOverrideMode > 0 AndAlso _
       (arrayDimensions = 1 OrElse initializerToken < 0) Then
        Dim fixedStringType As String
        Dim outerBounds As String
        Dim outerElementCount As Long
        If GenericHeaderFbfrogStringArrayShape( _
            translation.genericC.header, freeBasicType, arrayBounds, _
            arrayCount, fixedStringType, outerBounds, outerElementCount) <> 0 Then
            freeBasicType = fixedStringType
            arrayBounds = outerBounds
            arrayCount = outerElementCount
            If arrayBounds = "" Then arrayCount = 0
        End If
    End If
    freeBasicType = ApplyGenericStringOverrideModeInHeader( _
        translation.genericC.header, stringOverrideMode, freeBasicType)
    If unsizedArray <> 0 AndAlso _
       (stringOverrideMode > 0 OrElse _
        (translation.genericC.policy.fixUnsizedArrays <> 0 AndAlso _
         stringOverrideMode = 0)) Then _
        unsizedStringType = GenericStringPointerTypeInHeader(translation.genericC.header, freeBasicType)
    If freeBasicType = "" OrElse freeBasicType = "void" Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, "unknown external object type", declaration)
        Exit Sub
    End If
    If isReferenceObject <> 0 AndAlso _
       (arrayCount > 0 OrElse unsizedArray <> 0 OrElse callbackObject <> 0 OrElse _
        arrayDimensions > 0) Then
        EmitC2fbSkipped(translation, outputFile, conversionSummary, _
            "unsupported C++ reference object shape", declaration)
        Exit Sub
    End If

    Dim emittedName As String = _
        GenericFreeBasicFunctionNameInContext(translation, sourceName, outputNames(), outputNameCount)
    Dim storedName As String
    If unsizedArray <> 0 Then
        storedName = GenericFreeBasicFunctionNameInContext(translation,  _
            sourceName & "_storage", outputNames(), outputNameCount)
    ElseIf isConstAggregateAccessor <> 0 Then
        storedName = GenericFreeBasicFunctionNameInContext( _
            translation, sourceName & "_storage", outputNames(), outputNameCount)
        While LCase(storedName) = LCase(emittedName)
            storedName &= "_"
        Wend
    End If
    If nameCount >= MAX_GENERIC_HEADER_OUTPUT_NAMES Then _
        ReportError("generic header exceeds the configured symbol limit")
    Dim addedOutputNames As Long = 1
    If unsizedArray <> 0 OrElse isConstAggregateAccessor <> 0 Then _
        addedOutputNames = 2
    If outputNameCount + addedOutputNames > MAX_GENERIC_HEADER_OUTPUT_NAMES Then _
        ReportError("generic header exceeds the configured output-name limit")

    If arraySizeHint > 0 AndAlso unsizedArray = 0 Then _
        WriteC2fbOutputLine( _
            outputFile, "' c2fb applies --array-size hint: " & sourceName & "=" & _
            Trim(Str(arraySizeHint)))
    Dim objectBinding As C2fbObjectBindingModel
    objectBinding.freeBasicName = emittedName
    objectBinding.cName = sourceName
    objectBinding.linkName = linkName
    objectBinding.freeBasicType = freeBasicType
    objectBinding.arrayCount = arrayCount
    objectBinding.arrayBounds = arrayBounds
    objectBinding.isUnsizedArray = unsizedArray
    objectBinding.isByRef = isReferenceObject
    objectBinding.isModuleStatic = hasStaticStorage
    objectBinding.isThreadLocal = isThreadLocal
    objectBinding.isPrivate = CInt( _
        hasStaticStorage <> 0 AndAlso _
        translation.genericC.policy.privateStaticObjects <> 0) * -1
    objectBinding.isConstAggregateAccessor = isConstAggregateAccessor
    objectBinding.initialValue = initialValue
    If unsizedArray <> 0 Then
        objectBinding.storageName = storedName
        objectBinding.stringAccessorType = unsizedStringType
    ElseIf isConstAggregateAccessor <> 0 Then
        objectBinding.storageName = storedName
    End If
    WriteGenericObjectBindingWithGuardsInPolicy( _
        translation.genericC.policy, outputFile, sourceName, objectBinding)
    RecordGenericHeaderObjectInContext(translation, sourceName, objectBinding)

    If nameCount > UBound(names) Then ReDim Preserve names(0 To UBound(names) * 2 + 1)
    names(nameCount) = sourceName
    nameCount += 1
    If outputNameCount > UBound(outputNames) Then _
        ReDim Preserve outputNames(0 To UBound(outputNames) * 2 + 1)
    outputNames(outputNameCount) = emittedName
    outputNameCount += 1
    If unsizedArray <> 0 OrElse isConstAggregateAccessor <> 0 Then
        If outputNameCount > UBound(outputNames) Then _
            ReDim Preserve outputNames(0 To UBound(outputNames) * 2 + 1)
        outputNames(outputNameCount) = storedName
        outputNameCount += 1
    End If
    If hasStaticStorage <> 0 Then
        conversionSummary.genericHeader.moduleStaticObjectCount += 1
    Else
        conversionSummary.genericHeader.externalObjectCount += 1
    End If
End Sub

Private Sub EmitGenericExternalObject( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal declaration As String, ByRef outputFile As C2fbGeneratedFileOutputModel, _
    names() As String, ByRef nameCount As Long, _
    outputNames() As String, ByRef outputNameCount As Long, _
    ByRef conversionSummary As C2fbConversionSummaryModel _
)
    Dim normalized As String = StripCFunctionAttributes(declaration)
    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(normalized, tokens(), tokenCount)
    EmitGenericExternalObjectTokens( _
        translation, catalog, declaration, outputFile, tokens(), tokenCount, _
        names(), nameCount, outputNames(), outputNameCount, conversionSummary)
End Sub

Private Sub EmitGenericHeaderDeclaration( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef sourceDeclaration As C2fbDeclarationModel, _
    ByRef outputFile As C2fbGeneratedFileOutputModel, _
    names() As String, ByRef nameCount As Long, _
    outputNames() As String, ByRef outputNameCount As Long, _
    ByRef conversionSummary As C2fbConversionSummaryModel _
)
    Dim declaration As String = sourceDeclaration.text
    If sourceDeclaration.removeFromGenericHeader <> 0 Then Exit Sub
    Dim parts() As String
    Dim partCount As Long
    Dim textPartsReady As Long = _
        SplitSimpleCList(declaration, parts(), partCount)
    Dim sourceRanges() As C2fbTokenRangeModel
    Dim sourceRangeCount As Long = 0
    Dim sourceRangesReady As Long = 0
    If sourceDeclaration.tokenCount > 0 AndAlso _
       SplitC2fbTokenList( _
           sourceDeclaration.tokens(), 0, sourceDeclaration.tokenCount - 1, _
           sourceRanges(), sourceRangeCount) <> 0 Then
        sourceRangesReady = -1
    End If

    If textPartsReady <> 0 AndAlso sourceRangesReady <> 0 AndAlso _
        partCount = sourceRangeCount Then
        If sourceRangeCount = 1 Then
            Dim singleCommonType As String
            Dim singleSourceName As String
            Dim singleBindingOrder As String
            If ParseGenericHeaderDeclarator( _
                declaration, singleCommonType, singleSourceName, _
                singleBindingOrder) <> 0 AndAlso _
               GenericHeaderPlainDeclaratorRemoved( _
                   sourceDeclaration, singleSourceName) <> 0 Then Exit Sub
            EmitPrototypeWithTokens(translation, catalog, declaration, _
                sourceDeclaration.tokens(), sourceDeclaration.tokenCount, outputFile, names(), nameCount, _
                outputNames(), outputNameCount, conversionSummary, _
                C2FB_TYPE_MAPPING_GENERIC_HEADER)
            EmitGenericExternalObjectTokens( _
                translation, catalog, declaration, outputFile, _
                sourceDeclaration.tokens(), sourceDeclaration.tokenCount, _
                names(), nameCount, outputNames(), outputNameCount, conversionSummary)
            Exit Sub
        End If

        Dim tokenCommonType As String
        Dim tokenFirstName As String
        Dim tokenFirstBinding As String
        Dim tokenCommonTypeCount As Long
        If ParseGenericHeaderDeclaratorTokens( _
            sourceDeclaration.tokens(), sourceRanges(0).firstToken, _
            sourceRanges(0).lastToken, tokenCommonType, tokenFirstName, _
            tokenFirstBinding, tokenCommonTypeCount) <> 0 Then
            Dim canBuildAllParts As Long = -1
            For validatePartIndex As Long = 0 To partCount - 1
                Dim validationTokens() As String
                Dim validationTokenCount As Long
                If BuildC2fbDeclaratorPartTokens( _
                    sourceDeclaration.tokens(), sourceRanges(), sourceRangeCount, _
                    validatePartIndex, tokenCommonTypeCount, validationTokens(), _
                    validationTokenCount) = 0 Then
                    canBuildAllParts = 0
                    Exit For
                End If
            Next

            If canBuildAllParts <> 0 Then
                Dim tokenPartOrder() As Long
                ApplyGenericMoveAboveToDeclarationParts( _
                    translation, parts(), partCount, tokenCommonType, _
                    tokenPartOrder())
                For tokenPartPosition As Long = 0 To partCount - 1
                    Dim tokenPartIndex As Long = tokenPartOrder(tokenPartPosition)
                    Dim tokenItemDeclaration As String = Trim(parts(tokenPartIndex))
                    If tokenPartIndex > 0 Then _
                        tokenItemDeclaration = _
                            tokenCommonType & " " & tokenItemDeclaration
                    Dim itemTokens() As String
                    Dim itemTokenCount As Long
                    If BuildC2fbDeclaratorPartTokens( _
                        sourceDeclaration.tokens(), sourceRanges(), sourceRangeCount, _
                        tokenPartIndex, tokenCommonTypeCount, itemTokens(), _
                        itemTokenCount) = 0 Then _
                        ReportError("generic declaration token split changed during lowering")
                    Dim itemCommonType As String
                    Dim itemSourceName As String
                    Dim itemBindingOrder As String
                    If ParseGenericHeaderDeclarator( _
                        tokenItemDeclaration, itemCommonType, itemSourceName, _
                        itemBindingOrder) <> 0 AndAlso _
                       GenericHeaderPlainDeclaratorRemoved( _
                           sourceDeclaration, itemSourceName) <> 0 Then Continue For
                    EmitPrototypeWithTokens(translation, catalog, tokenItemDeclaration, _
                        itemTokens(), itemTokenCount, _
                        outputFile, names(), nameCount, outputNames(), _
                        outputNameCount, conversionSummary, _
                        C2FB_TYPE_MAPPING_GENERIC_HEADER)
                    EmitGenericExternalObjectTokens( _
                        translation, catalog, tokenItemDeclaration, outputFile, _
                        itemTokens(), itemTokenCount, names(), nameCount, _
                        outputNames(), outputNameCount, conversionSummary)
                Next
                Exit Sub
            End If
        End If
    End If

    If textPartsReady = 0 OrElse partCount <= 1 Then
        Dim singleCommonType As String
        Dim singleSourceName As String
        Dim singleBindingOrder As String
        If ParseGenericHeaderDeclarator( _
            declaration, singleCommonType, singleSourceName, _
            singleBindingOrder) <> 0 AndAlso _
           GenericHeaderPlainDeclaratorRemoved( _
               sourceDeclaration, singleSourceName) <> 0 Then Exit Sub
        EmitPrototype(translation, catalog, declaration, outputFile, names(), nameCount, _
            outputNames(), outputNameCount, conversionSummary, _
            C2FB_TYPE_MAPPING_GENERIC_HEADER)
        EmitGenericExternalObject(translation, catalog, declaration, outputFile, _
            names(), nameCount, outputNames(), outputNameCount, conversionSummary)
        Exit Sub
    End If

    Dim commonType As String
    Dim firstName As String
    Dim firstBinding As String
    If ParseGenericHeaderDeclarator( _
        parts(0), commonType, firstName, firstBinding) = 0 Then
        EmitPrototype(translation, catalog, declaration, outputFile, names(), nameCount, _
            outputNames(), outputNameCount, conversionSummary, _
            C2FB_TYPE_MAPPING_GENERIC_HEADER)
        Exit Sub
    End If

    Dim partOrder() As Long
    ApplyGenericMoveAboveToDeclarationParts( _
        translation, parts(), partCount, commonType, partOrder())
    For partPosition As Long = 0 To partCount - 1
        Dim partIndex As Long = partOrder(partPosition)
        Dim itemDeclaration As String = Trim(parts(partIndex))
        If partIndex > 0 Then itemDeclaration = commonType & " " & itemDeclaration
        Dim itemCommonType As String
        Dim itemSourceName As String
        Dim itemBindingOrder As String
        If ParseGenericHeaderDeclarator( _
            itemDeclaration, itemCommonType, itemSourceName, _
            itemBindingOrder) <> 0 AndAlso _
           GenericHeaderPlainDeclaratorRemoved( _
               sourceDeclaration, itemSourceName) <> 0 Then Continue For
        EmitPrototype(translation, catalog, itemDeclaration, outputFile, names(), nameCount, _
            outputNames(), outputNameCount, conversionSummary, _
            C2FB_TYPE_MAPPING_GENERIC_HEADER)
        EmitGenericExternalObject(translation, catalog, itemDeclaration, outputFile, names(), nameCount, _
            outputNames(), outputNameCount, conversionSummary)
    Next
End Sub

/' ------------------------------------------------------------------------- '/
/' Generic header output passes                                             '/
/' ------------------------------------------------------------------------- '/

/' end of c2fb_generic_declarations.bi '/
