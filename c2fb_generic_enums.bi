/'
    Project: C2FB generic C enum support

    File: c2fb_generic_enums.bi

    Purpose:

        Retain supported C enum values for generic body conversion and lower
        enum constants into generic FreeBASIC bindings.

    Responsibilities:

        - Assign stable FreeBASIC names to enum members.
        - Translate explicit values and implicit increments.
        - Retain known numeric values for enum-based fixed array bounds.
        - Index known integer values for generic C body expressions.
        - Emit enum constants through the common output backend.

    This file intentionally does NOT contain:

        - General macro expansion or record layout validation.
        - C preprocessing or SQLite-specific constant policy.
'/

Private Function GenericEnumMemberIndexInContext( _
    ByRef translation As C2fbTranslationContext, ByVal sourceName As String _
) As Long
    For i As Long = 0 To translation.genericC.header.enumMemberCount - 1
        If translation.genericC.header.enumMembers(i).sourceName = sourceName Then Return i
    Next
    Return -1
End Function

Declare Function GenericArrayBoundNormalizeType( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceType As String, ByVal depth As Long = 0 _
) As String

Declare Function GenericHeaderEnumMemberRemoved( _
    ByRef sourceDeclaration As C2fbDeclarationModel, ByVal sourceName As String _
) As Long

Declare Function TranslateGenericEnumExpressionWithBodyParser( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal sourceExpression As String, ByRef freeBasicExpression As String _
) As Long

Function TranslateGenericEnumExpressionWithBodyParser( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal sourceExpression As String, ByRef freeBasicExpression As String _
) As Long
    Dim callableNames(0 To 0) As String
    Dim parameterNames(0 To 0) As String
    Dim parameterAliases(0 To 0) As String
    Dim parameterTypes(0 To 0) As String
    Dim hasBooleanOperator As Long
    Dim hasFunctionCall As Long

    ' Enum values must remain integer constant expressions. The body parser
    ' handles C constructs such as _Generic before the header fallback runs.
    Return TranslateSimpleExpression( _
        translation, catalog, sourceExpression, freeBasicExpression, _
        hasBooleanOperator, hasFunctionCall, callableNames(), 0, _
        parameterNames(), parameterAliases(), parameterTypes(), 0, "", -1, -1)
End Function

Private Function GenericEnumMemberFreeBasicReference( _
    ByRef translation As C2fbTranslationContext, ByVal enumIndex As Long, _
    ByRef freeBasicReference As String _
) As Long
    freeBasicReference = ""
    If enumIndex < 0 OrElse _
       enumIndex >= translation.genericC.header.enumMemberCount Then Return 0
    With translation.genericC.header.enumMembers(enumIndex)
        If .isEmitted <> 0 Then
            freeBasicReference = .freeBasicName
            Return CInt(freeBasicReference <> "")
        End If
        If .integerValueKnown = 0 Then Return 0
        freeBasicReference = Trim(Str(.integerValue))
        Return -1
    End With
End Function

Private Function ReplaceGenericEnumNamesWithIntegerValues( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef numericExpression As String _
) As Long
    numericExpression = ""
    Dim position As Long = 1
    While position <= Len(sourceExpression)
        Dim currentByte As Long = Asc(sourceExpression, position)
        If currentByte >= Asc("0") AndAlso currentByte <= Asc("9") Then
            Dim tokenStart As Long = position
            position += 1
            While position <= Len(sourceExpression) AndAlso _
                  IsIdentifierPart(Asc(sourceExpression, position)) <> 0
                position += 1
            Wend
            numericExpression &= Mid( _
                sourceExpression, tokenStart, position - tokenStart)
        ElseIf IsIdentifierStart(currentByte) <> 0 Then
            Dim tokenStart As Long = position
            position += 1
            While position <= Len(sourceExpression) AndAlso _
                  IsIdentifierPart(Asc(sourceExpression, position)) <> 0
                position += 1
            Wend
            Dim sourceName As String = Mid( _
                sourceExpression, tokenStart, position - tokenStart)
            Dim enumIndex As Long = _
                GenericEnumMemberIndexInContext(translation, sourceName)
            If enumIndex < 0 OrElse _
               translation.genericC.header.enumMembers(enumIndex).integerValueKnown = 0 Then _
                Return 0
            numericExpression &= Trim(Str( _
                translation.genericC.header.enumMembers(enumIndex).integerValue))
        Else
            numericExpression &= Chr(currentByte)
            position += 1
        End If
    Wend
    Return -1
End Function

Private Function GenericArrayBoundTypeSize( _
    ByVal sourceType As String, ByRef byteSize As Long _
) As Long
    Dim typeTokens() As String
    Dim typeTokenCount As Long
    TokenizeCDeclaration(sourceType, typeTokens(), typeTokenCount)
    If typeTokenCount <= 0 Then Return 0
    Dim typeName As String
    Dim pointerDepth As Long = 0
    For tokenIndex As Long = 0 To typeTokenCount - 1
        If IsQualifierToken(typeTokens(tokenIndex)) <> 0 Then Continue For
        If typeTokens(tokenIndex) = "*" Then
            pointerDepth += 1
            Continue For
        End If
        If typeName <> "" Then typeName &= " "
        typeName &= LCase(typeTokens(tokenIndex))
    Next
    If pointerDepth > 0 Then
        byteSize = CLng(SizeOf(Any Ptr))
        For pointerIndex As Long = 1 To pointerDepth - 1
            byteSize *= CLng(SizeOf(Any Ptr))
        Next
        Return -1
    End If
    Select Case typeName
    Case "char", "signed char", "unsigned char", "_bool": byteSize = 1
    Case "short", "short int", "signed short", "signed short int", _
         "unsigned short", "unsigned short int": byteSize = 2
    Case "int", "signed", "signed int", "unsigned", "unsigned int": _
        byteSize = CLng(SizeOf(Long))
    Case "long", "long int", "signed long", "signed long int", _
         "unsigned long", "unsigned long int":
#if defined(__FB_64BIT__) AndAlso Not defined(__FB_WIN32__)
        byteSize = CLng(SizeOf(LongInt))
#else
        byteSize = CLng(SizeOf(Long))
#endif
    Case "long long", "long long int", "signed long long", _
         "signed long long int", "unsigned long long", _
         "unsigned long long int": byteSize = CLng(SizeOf(LongInt))
    Case "float": byteSize = CLng(SizeOf(Single))
    Case "double": byteSize = CLng(SizeOf(Double))
    Case "_float16", "__bf16": byteSize = 2
    Case Else
        Return 0
    End Select
    Return -1
End Function

Private Function GenericArrayBoundTypeSizeInContext( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceType As String, ByRef byteSize As Long _
) As Long
    sourceType = GenericArrayBoundNormalizeType(translation, sourceType)
    If sourceType = "" Then Return 0
    Return GenericArrayBoundTypeSize(sourceType, byteSize)
End Function

Private Function GenericArrayBoundStringSize( _
    ByVal sourceText As String, ByRef position As Long, _
    ByRef byteSize As Long _
) As Long
    Dim characterCount As Long = 0
    Dim literalCount As Long = 0
    Do
        SkipCSpaces(sourceText, position)
        If position > Len(sourceText) OrElse _
           Asc(sourceText, position) <> Asc(Chr(34)) Then Exit Do
        position += 1
        literalCount += 1
        Dim closed As Long = 0
        While position <= Len(sourceText)
            Dim currentByte As Long = Asc(sourceText, position)
            If currentByte = Asc(Chr(34)) Then
                position += 1
                closed = -1
                Exit While
            ElseIf currentByte = Asc(Chr(92)) Then
                position += 1
                If position > Len(sourceText) Then Return 0
                currentByte = Asc(sourceText, position)
                If currentByte = Asc("x") OrElse currentByte = Asc("X") Then
                    position += 1
                    Dim digitCount As Long = 0
                    While position <= Len(sourceText) AndAlso _
                          IsHexDigit(Asc(sourceText, position)) <> 0
                        position += 1
                        digitCount += 1
                    Wend
                    If digitCount = 0 Then Return 0
                ElseIf currentByte >= Asc("0") AndAlso _
                       currentByte <= Asc("7") Then
                    Dim digitCount As Long = 0
                    While position <= Len(sourceText) AndAlso _
                          Asc(sourceText, position) >= Asc("0") AndAlso _
                          Asc(sourceText, position) <= Asc("7") AndAlso _
                          digitCount < 3
                        position += 1
                        digitCount += 1
                    Wend
                ElseIf currentByte = Asc("u") OrElse _
                       currentByte = Asc("U") Then
                    ' Universal characters depend on the execution encoding.
                    Return 0
                Else
                    position += 1
                End If
                characterCount += 1
            Else
                If currentByte = 10 OrElse currentByte = 13 Then Return 0
                position += 1
                characterCount += 1
            End If
        Wend
        If closed = 0 Then Return 0
    Loop
    If literalCount = 0 Then Return 0
    byteSize = characterCount + 1
    Return -1
End Function

Private Function NormalizeGenericArrayBoundSizeof( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef normalizedExpression As String _
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
            Dim byteSize As Long
            If operandPosition <= Len(sourceExpression) AndAlso _
               Asc(sourceExpression, operandPosition) = Asc(Chr(34)) Then
                If GenericArrayBoundStringSize( _
                    sourceExpression, operandPosition, byteSize) = 0 Then Return 0
            ElseIf operandPosition <= Len(sourceExpression) AndAlso _
                   Asc(sourceExpression, operandPosition) = Asc("(") Then
                Dim groupEnd As Long = operandPosition
                If ScanSimpleExpressionGroup( _
                    sourceExpression, groupEnd, Asc("("), Asc(")")) = 0 Then Return 0
                Dim typeName As String = Trim(Mid( _
                    sourceExpression, operandPosition + 1, _
                    groupEnd - operandPosition - 2))
                If GenericArrayBoundTypeSizeInContext( _
                    translation, typeName, byteSize) = 0 Then Return 0
                operandPosition = groupEnd
            Else
                Return 0
            End If
            normalizedExpression &= Trim(Str(byteSize))
            position = operandPosition
        Else
            normalizedExpression &= Mid(sourceExpression, position, 1)
            position += 1
        End If
    Wend
    Return -1
End Function

Private Function GenericArrayBoundTypedefSourceType( _
    ByRef translation As C2fbTranslationContext, ByVal sourceType As String _
) As String
    Dim sourceTypeText As String
    For declarationIndex As Long = 0 To _
        translation.sourceUnit.declarationCount - 1
        With translation.sourceUnit.declarations(declarationIndex)
            If .isTypedef = 0 OrElse .sourceName <> sourceType OrElse _
               .tokenCount < 3 Then Continue For
            Dim aliasPosition As Long = -1
            For tokenIndex As Long = 1 To .tokenCount - 1
                If .tokens(tokenIndex) = sourceType Then _
                    aliasPosition = tokenIndex
            Next
            If aliasPosition <= 1 Then Continue For
            For tokenIndex As Long = 1 To aliasPosition - 1
                If sourceTypeText <> "" Then sourceTypeText &= " "
                sourceTypeText &= .tokens(tokenIndex)
            Next
            Return sourceTypeText
        End With
    Next
    Return ""
End Function

Private Function GenericArrayBoundNormalizeType( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceType As String, ByVal depth As Long = 0 _
) As String
    ' Header typedefs can reach a C primitive through several ABI aliases.
    ' Keep the lookup bounded before evaluating a _Generic association.
    If depth >= 32 Then Return ""
    sourceType = LCase(Trim(sourceType))
    If sourceType = "" Then Return ""
    ' Body aliases retain FreeBASIC storage names, so resolve those names back
    ' to the C integer rank before comparing them with association types.
    For aliasIndex As Long = 0 To translation.body.aliasCount - 1
        With translation.body.aliases(aliasIndex)
            If .name <> sourceType Then Continue For
            If .ambiguous <> 0 OrElse .baseType = "" OrElse _
               .baseType = sourceType Then Return ""
            Return GenericArrayBoundNormalizeType( _
                translation, .baseType, depth + 1)
        End With
    Next

    Dim typedefSourceType As String = _
        GenericArrayBoundTypedefSourceType(translation, sourceType)
    If typedefSourceType <> "" AndAlso typedefSourceType <> sourceType Then
        Return GenericArrayBoundNormalizeType( _
            translation, typedefSourceType, depth + 1)
    End If

    ' Header typedef aliases may be known before body aliases are indexed.
    ' Follow their generated type layers back to the C primitive rank.
    For aliasIndex As Long = 0 To _
        translation.genericC.header.headerTypeAliasCount - 1
        With translation.genericC.header.headerTypeAliases(aliasIndex)
            If LCase(.sourceName) <> sourceType AndAlso _
               LCase(.freeBasicName) <> sourceType Then Continue For
            Dim aliasBaseType As String = .baseType
            If aliasBaseType = "" Then aliasBaseType = .freeBasicName
            If LCase(aliasBaseType) = sourceType Then Return ""
            Return GenericArrayBoundNormalizeType( _
                translation, aliasBaseType, depth + 1)
        End With
    Next

    Select Case sourceType
    ' These aliases name the standard C integer ranks in generated modules.
    Case "c2fb_c_int", "c2fb_c_uint"
        Return IIf(sourceType = "c2fb_c_int", "int", "unsigned int")
    Case "c2fb_c_long", "clong"
        Return "long"
    Case "c2fb_c_ulong", "culong"
        Return "unsigned long"
    End Select

    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(sourceType, tokens(), tokenCount)
    If tokenCount <= 0 Then Return sourceType
    Dim hasOtherIntegerTypeToken As Long = 0
    For tokenIndex As Long = 0 To tokenCount - 1
        Select Case tokens(tokenIndex)
        Case "const", "volatile", "restrict", "__const", "__volatile", _
             "__restrict", "signed", "*", "[", "]"
            Continue For
        Case "int"
            Continue For
        Case Else
            If IsIdentifierToken(tokens(tokenIndex)) <> 0 Then _
                hasOtherIntegerTypeToken = -1
        End Select
    Next

    Dim normalizedType As String
    For tokenIndex As Long = 0 To tokenCount - 1
        Select Case tokens(tokenIndex)
        Case "const", "volatile", "restrict", "__const", "__volatile", _
             "__restrict", "signed"
            Continue For
        Case "int"
            If hasOtherIntegerTypeToken <> 0 Then Continue For
        End Select
        If normalizedType <> "" Then normalizedType &= " "
        normalizedType &= tokens(tokenIndex)
    Next
    Return normalizedType
End Function

Private Function GenericArrayBoundTypeSelectionKey( _
    ByRef translation As C2fbTranslationContext, ByVal sourceType As String _
) As String
    Return GenericArrayBoundNormalizeType(translation, sourceType)
End Function

Private Function GenericArrayBoundTypeFromControl( _
    ByRef translation As C2fbTranslationContext, _
    ByVal controlExpression As String, ByRef controlType As String _
) As Long
    controlType = ""
    Dim typeOperator As Long = InStr(controlExpression, "__typeof__")
    If typeOperator = 0 Then Return 0
    Dim groupStart As Long = typeOperator + Len("__typeof__")
    SkipCSpaces(controlExpression, groupStart)
    If groupStart > Len(controlExpression) OrElse _
       Asc(controlExpression, groupStart) <> Asc("(") Then Return 0
    Dim groupEnd As Long = groupStart
    If ScanSimpleExpressionGroup( _
        controlExpression, groupEnd, Asc("("), Asc(")")) = 0 Then Return 0
    controlType = GenericArrayBoundTypeSelectionKey(translation, Trim(Mid( _
        controlExpression, groupStart + 1, groupEnd - groupStart - 2)))
    Return CInt(controlType <> "")
End Function

Private Function GenericArrayBoundAssociationColon( _
    ByVal association As String _
) As Long
    Dim parenthesisDepth As Long = 0
    Dim bracketDepth As Long = 0
    Dim braceDepth As Long = 0
    Dim quoteByte As Long = 0
    Dim escaped As Long = 0
    For position As Long = 1 To Len(association)
        Dim currentByte As Long = Asc(association, position)
        If quoteByte <> 0 Then
            If escaped <> 0 Then
                escaped = 0
            ElseIf currentByte = Asc(Chr(92)) Then
                escaped = -1
            ElseIf currentByte = quoteByte Then
                quoteByte = 0
            End If
            Continue For
        End If
        If currentByte = Asc(Chr(34)) OrElse currentByte = Asc("'") Then
            quoteByte = currentByte
        ElseIf currentByte = Asc("(") Then
            parenthesisDepth += 1
        ElseIf currentByte = Asc(")") Then
            parenthesisDepth -= 1
        ElseIf currentByte = Asc("[") Then
            bracketDepth += 1
        ElseIf currentByte = Asc("]") Then
            bracketDepth -= 1
        ElseIf currentByte = Asc("{") Then
            braceDepth += 1
        ElseIf currentByte = Asc("}") Then
            braceDepth -= 1
        ElseIf currentByte = Asc(":") AndAlso parenthesisDepth = 0 AndAlso _
               bracketDepth = 0 AndAlso braceDepth = 0 Then
            Return position
        End If
    Next
    Return 0
End Function

Private Function NormalizeGenericArrayBoundSelections( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef normalizedExpression As String _
) As Long
    normalizedExpression = sourceExpression
    For selectionIndex As Long = 0 To 15
        Dim selectionPosition As Long = InStr(normalizedExpression, "_Generic")
        If selectionPosition = 0 Then Return -1
        Dim groupStart As Long = selectionPosition + Len("_Generic")
        SkipCSpaces(normalizedExpression, groupStart)
        If groupStart > Len(normalizedExpression) OrElse _
           Asc(normalizedExpression, groupStart) <> Asc("(") Then Return 0
        Dim groupEnd As Long = groupStart
        If ScanSimpleExpressionGroup( _
            normalizedExpression, groupEnd, Asc("("), Asc(")")) = 0 Then Return 0
        Dim selectionText As String = Trim(Mid( _
            normalizedExpression, groupStart + 1, groupEnd - groupStart - 2))
        Dim parts() As String
        Dim partCount As Long
        If SplitSimpleCList(selectionText, parts(), partCount) = 0 OrElse _
           partCount < 2 Then Return 0
        Dim controlType As String
        If GenericArrayBoundTypeFromControl( _
            translation, parts(0), controlType) = 0 Then Return 0
        Dim selectedExpression As String
        Dim defaultExpression As String
        For partIndex As Long = 1 To partCount - 1
            Dim colonPosition As Long = _
                GenericArrayBoundAssociationColon(parts(partIndex))
            If colonPosition <= 0 Then Return 0
            Dim associationType As String = GenericArrayBoundTypeSelectionKey( _
                translation, Trim(Left( _
                    parts(partIndex), colonPosition - 1)))
            Dim associationValue As String = Trim(Mid( _
                parts(partIndex), colonPosition + 1))
            If associationType = controlType Then
                selectedExpression = associationValue
                Exit For
            ElseIf associationType = "default" Then
                defaultExpression = associationValue
            End If
        Next
        If selectedExpression = "" Then selectedExpression = defaultExpression
        If selectedExpression = "" Then Return 0
        Dim replacementExpression As String = selectedExpression
        Dim literalParser As SimpleArrayBoundParser
        literalParser.sourceText = selectedExpression
        literalParser.position = 1
        Dim literalValue As LongInt
        Dim isPrimaryExpression As Long = CInt( _
            IsIdentifierToken(selectedExpression) <> 0)
        If ReadSimpleArrayBoundLiteral( _
            literalParser, literalValue) <> 0 Then
            SkipCSpaces(literalParser.sourceText, literalParser.position)
            If literalParser.position > Len(literalParser.sourceText) Then _
                isPrimaryExpression = -1
        End If
        If isPrimaryExpression = 0 Then _
            replacementExpression = "(" & selectedExpression & ")"
        normalizedExpression = Left(normalizedExpression, selectionPosition - 1) & _
            replacementExpression & Mid(normalizedExpression, groupEnd)
    Next
    If InStr(normalizedExpression, "_Generic") <> 0 Then Return 0
    Return -1
End Function

Private Function EvaluateGenericEnumIntegerValue( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef integerValue As Long _
) As Long
    Dim normalizedExpression As String = sourceExpression
    If InStr(normalizedExpression, "sizeof") <> 0 Then
        Dim sizeofExpression As String
        If NormalizeGenericArrayBoundSizeof( _
            translation, normalizedExpression, sizeofExpression) = 0 Then Return 0
        normalizedExpression = sizeofExpression
    End If
    If InStr(normalizedExpression, "_Generic") <> 0 Then
        Dim genericExpression As String
        If NormalizeGenericArrayBoundSelections( _
            translation, normalizedExpression, genericExpression) = 0 Then Return 0
        normalizedExpression = genericExpression
    End If

    Dim numericExpression As String
    If ReplaceGenericEnumNamesWithIntegerValues( _
        translation, normalizedExpression, numericExpression) = 0 Then Return 0
    Dim parser As SimpleArrayBoundParser
    parser.sourceText = numericExpression
    parser.position = 1
    Dim value As LongInt
    ' C enumerator values use the same conditional-expression grammar as array
    ' bounds, including shifts, bitwise operators, and the ?: operator.
    If ReadSimpleArrayBoundConditional(parser, value, 0) = 0 Then Return 0
    SkipCSpaces(parser.sourceText, parser.position)
    If parser.position <= Len(parser.sourceText) OrElse _
       value < -MAX_GENERIC_ARRAY_BOUND_VALUE OrElse _
       value > MAX_GENERIC_ARRAY_BOUND_VALUE Then Return 0
    integerValue = CLng(value)
    Return -1
End Function

Private Function EvaluateGenericBodyArrayBound( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef arrayCount As Long, _
    ByVal maximumCount As Long = MAX_SIMPLE_ARRAY_COUNT _
) As Long
    Dim integerValue As Long
    If InStr(sourceExpression, "sizeof") <> 0 Then
        Dim normalizedSizeof As String
        If NormalizeGenericArrayBoundSizeof( _
            translation, sourceExpression, normalizedSizeof) = 0 Then Return 0
        sourceExpression = normalizedSizeof
    End If
    If InStr(sourceExpression, "_Generic") <> 0 Then
        Dim normalizedGeneric As String
        If NormalizeGenericArrayBoundSelections( _
            translation, sourceExpression, normalizedGeneric) = 0 Then Return 0
        sourceExpression = normalizedGeneric
    End If
    If maximumCount < 1 OrElse maximumCount > MAX_GENERIC_ARRAY_COUNT OrElse _
       EvaluateGenericEnumIntegerValue( _
           translation, sourceExpression, integerValue) = 0 OrElse _
       integerValue < 1 OrElse integerValue > maximumCount Then Return 0
    arrayCount = integerValue
    Return -1
End Function

Private Sub IndexGenericBodyEnumConstants( _
    ByRef translation As C2fbTranslationContext, _
    ByVal resetExisting As Long = -1 _
)
    If resetExisting <> 0 Then
        ReDim translation.genericC.header.enumMembers(0 To 255)
        translation.genericC.header.enumMemberCount = 0
    End If

    For declarationIndex As Long = 0 To translation.sourceUnit.declarationCount - 1
        With translation.sourceUnit.declarations(declarationIndex)
            If .entryKind <> C2FB_DECLARATION_RECORD OrElse _
               .recordKind <> C2FB_RECORD_ENUM Then Continue For
            Dim enumTypeName As String = .recordAlias
            Dim enumTagName As String = .recordTag
            Dim enumParts() As String
            Dim enumPartCount As Long
            If SplitSimpleCList( _
                .recordBody, enumParts(), enumPartCount, -1) = 0 Then Continue For

            Dim previousValue As Long = 0
            Dim previousValueKnown As Long = 0
            Dim memberCount As Long = 0
            For partIndex As Long = 0 To enumPartCount - 1
                Dim partText As String = Trim(enumParts(partIndex))
                Dim enumTokens() As String
                Dim enumTokenCount As Long
                TokenizeCDeclaration(partText, enumTokens(), enumTokenCount)
                If enumTokenCount = 0 OrElse _
                   IsIdentifierToken(enumTokens(0)) = 0 Then
                    previousValueKnown = 0
                    memberCount += 1
                    Continue For
                End If

                Dim sourceName As String = enumTokens(0)
                Dim duplicateIndex As Long = _
                    GenericEnumMemberIndexInContext(translation, sourceName)
                If duplicateIndex >= 0 Then
                    previousValue = translation.genericC.header.enumMembers( _
                        duplicateIndex).integerValue
                    previousValueKnown = translation.genericC.header.enumMembers( _
                        duplicateIndex).integerValueKnown
                    memberCount += 1
                    Continue For
                End If

                Dim integerValue As Long = 0
                Dim integerValueKnown As Long = 0
                Dim equalsPosition As Long = InStr(partText, "=")
                If equalsPosition = 0 Then
                    If memberCount = 0 Then
                        integerValueKnown = -1
                    ElseIf previousValueKnown <> 0 AndAlso _
                           previousValue < MAX_GENERIC_ARRAY_COUNT Then
                        integerValue = previousValue + 1
                        integerValueKnown = -1
                    End If
                ElseIf EvaluateGenericEnumIntegerValue( _
                    translation, Trim(Mid(partText, equalsPosition + 1)), _
                    integerValue) <> 0 Then
                    integerValueKnown = -1
                End If

                If translation.genericC.header.enumMemberCount >= _
                   MAX_GENERIC_HEADER_OUTPUT_NAMES Then _
                    ReportError("generic body exceeds the configured enum constant limit")
                If translation.genericC.header.enumMemberCount > _
                   UBound(translation.genericC.header.enumMembers) Then _
                    ReDim Preserve translation.genericC.header.enumMembers( _
                        0 To UBound(translation.genericC.header.enumMembers) * 2 + 1)
                With translation.genericC.header.enumMembers( _
                    translation.genericC.header.enumMemberCount)
                    .sourceName = sourceName
                    .freeBasicName = ""
                    .enumTypeName = enumTypeName
                    .enumTagName = enumTagName
                    .integerValue = integerValue
                    .integerValueKnown = integerValueKnown
                End With
                translation.genericC.header.enumMemberCount += 1
                previousValue = integerValue
                previousValueKnown = integerValueKnown
                memberCount += 1
            Next
        End With
    Next
End Sub

Private Sub IndexGenericBodyEnumConstantExpressions( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog _
)
    For declarationIndex As Long = 0 To translation.sourceUnit.declarationCount - 1
        With translation.sourceUnit.declarations(declarationIndex)
            If .entryKind <> C2FB_DECLARATION_RECORD OrElse _
               .recordKind <> C2FB_RECORD_ENUM Then Continue For
            Dim enumParts() As String
            Dim enumPartCount As Long
            If SplitSimpleCList( _
                .recordBody, enumParts(), enumPartCount, -1) = 0 Then Continue For
            For partIndex As Long = 0 To enumPartCount - 1
                Dim partText As String = Trim(enumParts(partIndex))
                Dim equalsPosition As Long = InStr(partText, "=")
                If equalsPosition <= 0 Then Continue For
                Dim memberTokens() As String
                Dim memberTokenCount As Long
                TokenizeCDeclaration( _
                    Trim(Left(partText, equalsPosition - 1)), _
                    memberTokens(), memberTokenCount)
                If memberTokenCount <> 1 OrElse _
                   IsIdentifierToken(memberTokens(0)) = 0 Then Continue For
                Dim enumIndex As Long = GenericEnumMemberIndexInContext( _
                    translation, memberTokens(0))
                If enumIndex < 0 OrElse _
                   translation.genericC.header.enumMembers(enumIndex).integerValueKnown <> 0 OrElse _
                   translation.genericC.header.enumMembers(enumIndex).isEmitted <> 0 Then _
                    Continue For

                Dim valueExpression As String = Trim( _
                    Mid(partText, equalsPosition + 1))
                ' The bound evaluator does not know every valid C cast. Keep
                ' any enum expression the body parser can emit as a constant.
                Dim freeBasicExpression As String
                If TranslateGenericEnumExpressionWithBodyParser( _
                    translation, catalog, valueExpression, _
                    freeBasicExpression) <> 0 Then
                    With translation.genericC.header.enumMembers(enumIndex)
                        .freeBasicName = freeBasicExpression
                        .isEmitted = -1
                    End With
                End If
            Next
        End With
    Next
End Sub

Declare Function ReplaceGenericHeaderSizeofExpressions( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, _
    ByRef normalizedExpression As String _
) As Long

Private Function EvaluateGenericHeaderArrayBound( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef arrayCount As Long, _
    ByVal maximumCount As Long = MAX_GENERIC_ARRAY_COUNT, _
    ByVal allowZero As Long = 0 _
) As Long
    Dim sizeofExpression As String
    If ReplaceGenericHeaderSizeofExpressions( _
        translation, sourceExpression, sizeofExpression) = 0 Then Return 0
    Dim numericExpression As String
    If ReplaceGenericEnumNamesWithIntegerValues( _
        translation, sizeofExpression, numericExpression) = 0 Then Return 0
    Return EvaluateSimpleArrayBound( _
        numericExpression, arrayCount, maximumCount, allowZero)
End Function

Private Function TranslateGenericEnumExpression( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef freeBasicExpression As String _
) As Long
    Dim normalizedExpression As String
    If RewriteGenericMacroCharacterConstants( _
        sourceExpression, normalizedExpression) = 0 Then Return 0
    sourceExpression = normalizedExpression
    Dim rewritten As String
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
            Dim sourceName As String = Mid(sourceExpression, tokenStart, position - tokenStart)
            Dim enumIndex As Long = _
                GenericEnumMemberIndexInContext(translation, sourceName)
            If enumIndex < 0 Then Return 0
            Dim memberReference As String
            If GenericEnumMemberFreeBasicReference( _
                translation, enumIndex, memberReference) = 0 Then Return 0
            rewritten &= memberReference
        ElseIf currentByte >= Asc("0") AndAlso currentByte <= Asc("9") Then
            Dim tokenStart As Long = position
            position += 1
            While position <= Len(sourceExpression)
                Dim digitByte As Long = Asc(sourceExpression, position)
                If IsIdentifierPart(digitByte) = 0 Then Exit While
                position += 1
            Wend
            rewritten &= Mid(sourceExpression, tokenStart, position - tokenStart)
        Else
            rewritten &= Chr(currentByte)
            position += 1
        End If
    Wend
    Return TranslateGenericHeaderIntegerExpression( _
        translation, rewritten, freeBasicExpression, 0)
End Function

Private Function TranslateGenericEnumExpressionWithCatalog( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByVal sourceExpression As String, ByRef freeBasicExpression As String _
) As Long
    Dim integerValue As Long
    If EvaluateGenericEnumIntegerValue( _
        translation, sourceExpression, integerValue) <> 0 Then
        freeBasicExpression = Trim(Str(integerValue))
        Return -1
    End If
    If TranslateGenericEnumExpressionWithBodyParser( _
        translation, catalog, sourceExpression, freeBasicExpression) <> 0 Then _
        Return -1
    Return TranslateGenericEnumExpression( _
        translation, sourceExpression, freeBasicExpression)
End Function

Private Sub WriteGenericHeaderEnumConstants( _
    ByRef translation As C2fbTranslationContext, _
    ByRef catalog As C2fbProjectCatalog, _
    ByRef outputFile As C2fbGeneratedFileOutputModel, outputNames() As String, _
    ByRef outputNameCount As Long, ByRef conversionSummary As C2fbConversionSummaryModel _
)
    For entryIndex As Long = 0 To translation.sourceUnit.declarationCount - 1
        With translation.sourceUnit.declarations(entryIndex)
            If .entryKind <> C2FB_DECLARATION_RECORD OrElse _
               .recordKind <> C2FB_RECORD_ENUM Then Continue For
            If .removeFromGenericHeader <> 0 Then Continue For
            If GenericSourceIncludedInPolicy( _
                translation.genericC.policy, .sourceSpan.startLocation.path) = 0 Then _
                Continue For
            If (.recordTag <> "" AndAlso _
                GenericTypeExcludedInPolicy(translation.genericC.policy, .recordTag) <> 0) OrElse _
               (.recordAlias <> "" AndAlso _
                GenericTypeExcludedInPolicy(translation.genericC.policy, .recordAlias) <> 0) Then
                EmitC2fbSkippedAtLocation(translation, outputFile, conversionSummary, _
                    "excluded enum type", _
                    IIf(.recordTag <> "", .recordTag, .recordAlias), _
                    .sourceSpan.startLocation)
                Continue For
            End If
            If .unsupported <> 0 Then
                EmitC2fbSkippedAtLocation( _
                    translation, outputFile, conversionSummary, _
                    "unsupported enum declaration", _
                    IIf(.recordTag <> "", .recordTag, .recordAlias), _
                    .sourceSpan.startLocation)
                Continue For
            End If
            Dim enumParts() As String
            Dim enumPartCount As Long
            If SplitSimpleCList( _
                .recordBody, enumParts(), enumPartCount, -1) = 0 Then
                EmitC2fbSkippedAtLocation( _
                    translation, outputFile, conversionSummary, "enum constants", _
                    IIf(.recordTag <> "", .recordTag, .recordAlias), _
                    .sourceSpan.startLocation)
                Continue For
            End If

            Dim previousFreeBasicName As String
            Dim previousSourceName As String
            Dim previousFreeBasicExpression As String
            Dim previousValue As Long = 0
            Dim previousValueKnown As Long = 0
            Dim previousWasEmitted As Long = 0
            Dim failedExpression As Long = 0
            For partIndex As Long = 0 To enumPartCount - 1
                Dim partText As String = Trim(enumParts(partIndex))
                If partText = "" Then Continue For
                Dim enumTokens() As String
                Dim enumTokenCount As Long
                TokenizeCDeclaration(partText, enumTokens(), enumTokenCount)
                If enumTokenCount = 0 OrElse IsIdentifierToken(enumTokens(0)) = 0 Then
                    failedExpression = -1
                    Exit For
                End If
                Dim sourceName As String = enumTokens(0)
                Dim isRemoved As Long = GenericHeaderEnumMemberRemoved( _
                    translation.sourceUnit.declarations(entryIndex), sourceName)
                Dim equalsPosition As Long = InStr(partText, "=")
                Dim freeBasicExpression As String
                Dim integerValue As Long = 0
                Dim integerValueKnown As Long = 0
                If equalsPosition = 0 Then
                    If previousSourceName = "" Then
                        freeBasicExpression = "0"
                        integerValueKnown = -1
                    ElseIf previousWasEmitted <> 0 Then
                        freeBasicExpression = previousFreeBasicName & " + 1"
                        If previousValueKnown <> 0 AndAlso _
                           previousValue < MAX_GENERIC_ARRAY_COUNT Then
                            integerValue = previousValue + 1
                            integerValueKnown = -1
                        End If
                    ElseIf previousValueKnown <> 0 AndAlso _
                           previousValue < MAX_GENERIC_ARRAY_COUNT Then
                        integerValue = previousValue + 1
                        integerValueKnown = -1
                        freeBasicExpression = Trim(Str(integerValue))
                    Else
                        freeBasicExpression = previousFreeBasicExpression & " + 1"
                    End If
                Else
                    If InStr(equalsPosition + 1, partText, "=") <> 0 OrElse _
                       TranslateGenericEnumExpressionWithCatalog( _
                           translation, catalog, _
                           Trim(Mid(partText, equalsPosition + 1)), _
                           freeBasicExpression) = 0 Then
                        failedExpression = -1
                        Exit For
                    End If
                    If EvaluateGenericEnumIntegerValue( _
                        translation, Trim(Mid(partText, equalsPosition + 1)), _
                        integerValue) <> 0 Then integerValueKnown = -1
                End If

                If GenericEnumMemberIndexInContext(translation, sourceName) >= 0 Then
                    EmitC2fbSkippedAtLocation(translation,  _
                        outputFile, conversionSummary, "duplicate enum constant", sourceName, _
                        .sourceSpan.startLocation, 0)
                    failedExpression = -1
                    Exit For
                End If
                Dim freeBasicName As String
                If isRemoved = 0 Then
                    freeBasicName = GenericFreeBasicFunctionNameInContext( _
                        translation, sourceName, outputNames(), outputNameCount)
                    If outputNameCount > UBound(outputNames) Then _
                        ReDim Preserve outputNames(0 To UBound(outputNames) * 2 + 1)
                    outputNames(outputNameCount) = freeBasicName
                    outputNameCount += 1
                End If
                If translation.genericC.header.enumMemberCount >= _
                   MAX_GENERIC_HEADER_OUTPUT_NAMES Then _
                    ReportError("generic header exceeds the configured enum constant limit")
                If translation.genericC.header.enumMemberCount > UBound(translation.genericC.header.enumMembers) Then _
                    ReDim Preserve translation.genericC.header.enumMembers(0 To UBound(translation.genericC.header.enumMembers) * 2 + 1)
                With translation.genericC.header.enumMembers( _
                    translation.genericC.header.enumMemberCount)
                    .sourceName = sourceName
                    .freeBasicName = freeBasicName
                    .integerValue = integerValue
                    .integerValueKnown = integerValueKnown
                    .isEmitted = CInt(isRemoved = 0)
                End With
                translation.genericC.header.enumMemberCount += 1
                If isRemoved = 0 Then
                    Dim constantBinding As C2fbConstantBindingModel
                    constantBinding.freeBasicName = freeBasicName
                    constantBinding.targetType = "Long"
                    constantBinding.valueExpression = freeBasicExpression
                    WriteGenericConstantBindingWithGuardsInPolicy( _
                        translation.genericC.policy, outputFile, sourceName, constantBinding)
                End If
                previousFreeBasicName = freeBasicName
                previousSourceName = sourceName
                previousFreeBasicExpression = freeBasicExpression
                previousValue = integerValue
                previousValueKnown = integerValueKnown
                previousWasEmitted = CInt(isRemoved = 0)
            Next
            If failedExpression <> 0 Then
                EmitC2fbSkippedAtLocation(translation,  _
                    outputFile, conversionSummary, _
                    "remaining enum constants after an unsupported value", _
                    IIf(.recordTag <> "", .recordTag, .recordAlias), _
                    .sourceSpan.startLocation)
            End If
        End With
    Next
    If translation.genericC.header.enumMemberCount > 0 Then _
        WriteC2fbOutputLine(outputFile, "")
End Sub

/' end of c2fb_generic_enums.bi '/
