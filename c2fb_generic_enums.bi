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

Declare Function GenericHeaderEnumMemberRemoved( _
    ByRef sourceDeclaration As C2fbDeclarationModel, ByVal sourceName As String _
) As Long

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

Private Function EvaluateGenericEnumIntegerValue( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef integerValue As Long _
) As Long
    Dim numericExpression As String
    If ReplaceGenericEnumNamesWithIntegerValues( _
        translation, sourceExpression, numericExpression) = 0 Then Return 0
    Dim parser As SimpleArrayBoundParser
    parser.sourceText = numericExpression
    parser.position = 1
    Dim value As LongInt
    ' C enumerator values may use shifts without parentheses, unlike the
    ' simpler additive array-bound expression handled by the inner parser.
    If ReadSimpleArrayBoundShift(parser, value, 0) = 0 Then Return 0
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

Private Sub WriteGenericHeaderEnumConstants( _
    ByRef translation As C2fbTranslationContext, _
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
                       TranslateGenericEnumExpression( _
                           translation, Trim(Mid(partText, equalsPosition + 1)), _
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
