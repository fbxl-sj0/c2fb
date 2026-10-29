/'
    Project: C2FB generic C type-size expressions
    ---------------------------------------------

    File: c2fb_generic_sizeof.bi

    Purpose:

        Resolve sizeof(type) terms used in generic C integer constant
        expressions, using the active C target ABI.

    Responsibilities:

        - Read scalar and pointer widths from predefined C ABI macros.
        - Resolve typedef storage through the generic-header type model.
        - Resolve sizeof terms for modeled scalar and simple aggregate types.
        - Replace sizeof terms in array bounds only when their size is known.
        - Keep integer casts only when they preserve the resolved size value.

    This file intentionally does NOT contain:

        - Complete C aggregate layout-size calculation.
        - Runtime sizeof expression lowering for function bodies.
'/

Private Function GenericHeaderPreprocessorSizeValue( _
    ByRef translation As C2fbTranslationContext, ByVal macroName As String _
) As Long
    For macroIndex As Long = 0 To _
        translation.genericC.header.headerMacroCount - 1
        With translation.genericC.header.headerMacros(macroIndex)
            If .active = 0 OrElse .definition.sourceName <> macroName Then _
                Continue For
            Dim byteCount As Long
            If EvaluateSimpleArrayBound( _
                Trim(.definition.rawValue), byteCount, 16) <> 0 Then _
                Return byteCount
            Return 0
        End With
    Next
    Return 0
End Function

Private Function GenericHeaderFreeBasicScalarSize( _
    ByRef translation As C2fbTranslationContext, _
    ByVal freeBasicType As String _
) As Long
    If Left(freeBasicType, 6) = "Const " Then _
        freeBasicType = Mid(freeBasicType, 7)
    Dim storedType As String = GenericHeaderTypeAliasStorageTypeInHeader( _
        translation.genericC.header, freeBasicType, 0)
    If storedType <> "" Then freeBasicType = storedType
    If Left(freeBasicType, 6) = "Const " Then _
        freeBasicType = Mid(freeBasicType, 7)

    Dim lowerTypeName As String = LCase(freeBasicType)
    Dim fixedStringPrefix As String
    Dim stringElementSize As Long = 0
    If Left(lowerTypeName, Len("zstring * ")) = "zstring * " Then
        fixedStringPrefix = "zstring * "
        stringElementSize = 1
    ElseIf Left(lowerTypeName, Len("wstring * ")) = "wstring * " Then
        fixedStringPrefix = "wstring * "
        stringElementSize = GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_WCHAR_T__")
    End If
    If fixedStringPrefix <> "" Then
        Dim characterCount As Long
        If stringElementSize <= 0 OrElse EvaluateSimpleArrayBound( _
            Trim(Mid(lowerTypeName, Len(fixedStringPrefix) + 1)), _
            characterCount, MAX_GENERIC_ARRAY_COUNT) = 0 OrElse _
           characterCount <= 0 OrElse _
           characterCount > MAX_GENERIC_ARRAY_COUNT \ stringElementSize Then _
            Return 0
        Return characterCount * stringElementSize
    End If

    Select Case LCase(freeBasicType)
    Case "byte", "ubyte", "boolean": Return 1
    Case "short", "ushort": Return 2
    Case "long", "ulong", "c2fb_c_int", "c2fb_c_uint": Return 4
    Case "longint", "ulongint", "c2fb_int64", "c2fb_uint64": Return 8
    Case "integer", "uinteger", "c2fb_size_t"
        Return GenericHeaderPreprocessorSizeValue(translation, "__SIZEOF_POINTER__")
    Case "clong", "culong", "c2fb_c_long", "c2fb_c_ulong"
        Dim longSize As Long = _
            GenericHeaderPreprocessorSizeValue(translation, "__SIZEOF_LONG__")
        If longSize > 0 Then Return longSize
        If translation.genericC.policy.cLong32 <> 0 Then Return 4
        Return 0
    Case "single": Return 4
    Case "double": Return 8
    Case "__bf16", "_float16": Return 2
    Case "zstring": Return 1
    Case "wstring"
        Return GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_WCHAR_T__")
    Case "clongdouble"
        Return GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_LONG_DOUBLE__")
    End Select
    Return 0
End Function

Private Function GenericHeaderCLongDoubleAlignmentBytes( _
    ByRef translation As C2fbTranslationContext _
) As Long
    Dim longDoubleSize As Long = GenericHeaderPreprocessorSizeValue( _
        translation, "__SIZEOF_LONG_DOUBLE__")
    Dim pointerSize As Long = GenericHeaderPreprocessorSizeValue( _
        translation, "__SIZEOF_POINTER__")

    Select Case longDoubleSize
    Case 16
        ' The x86-64 C ABI aligns its 16-byte long double to 16 bytes.
        If pointerSize = 8 AndAlso _
           GenericHeaderPreprocessorSizeValue(translation, "__x86_64__") <> 0 Then _
            Return 16
    Case 8
        ' On 64-bit targets where long double is double-sized, its alignment
        ' matches the native 64-bit integer carrier used by FreeBASIC.
        If pointerSize = 8 Then Return 8
    Case 12
        ' The x86 32-bit ABI stores the extended value in 12 bytes and aligns
        ' it to the four-byte word boundary.
        If pointerSize = 4 AndAlso _
           GenericHeaderPreprocessorSizeValue(translation, "__i386__") <> 0 Then _
            Return 4
    End Select
    Return 0
End Function

Private Function GenericHeaderFreeBasicScalarAlignment( _
    ByRef translation As C2fbTranslationContext, _
    ByVal freeBasicType As String, ByVal scalarSize As Long _
) As Long
    If Left(freeBasicType, 6) = "Const " Then _
        freeBasicType = Mid(freeBasicType, 7)
    Dim storedType As String = GenericHeaderTypeAliasStorageTypeInHeader( _
        translation.genericC.header, freeBasicType, 0)
    If storedType <> "" Then freeBasicType = storedType

    Select Case LCase(freeBasicType)
    Case "byte", "ubyte", "boolean", "zstring", "wstring"
        Return 1
    Case "short", "ushort"
        Return 2
    Case "long", "ulong", "single"
        Return 4
    Case "clong", "culong", "c2fb_c_long", "c2fb_c_ulong"
        Return GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_LONG__")
    Case "integer", "uinteger", "c2fb_size_t"
        Return GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_POINTER__")
    Case "longint", "ulongint", "double", "c2fb_int64", "c2fb_uint64"
        Dim pointerSize As Long = GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_POINTER__")
        If pointerSize >= 8 Then Return 8
        Return 0
    Case "clongdouble"
        Return GenericHeaderCLongDoubleAlignmentBytes(translation)
    End Select
    If scalarSize > 0 AndAlso scalarSize <= 4 Then Return scalarSize
    Return 0
End Function

Private Function GenericHeaderSimpleRecordSizeInBytes( _
    ByRef translation As C2fbTranslationContext, ByVal freeBasicType As String _
) As Long
    ' Generic body translation has no header record-plan array. It can still
    ' resolve scalar sizeof operands, but must leave aggregate sizes unknown.
    If translation.genericC.header.recordPlanCount <= 0 Then Return 0
    Dim storageType As String = GenericHeaderTypeAliasStorageTypeInHeader( _
        translation.genericC.header, freeBasicType, 0)
    If storageType <> "" AndAlso LCase(storageType) <> "record" Then _
        freeBasicType = storageType

    For declarationIndex As Long = 0 To _
        translation.sourceUnit.declarationCount - 1
        If declarationIndex >= _
           translation.genericC.header.recordPlanCount Then Continue For
        With translation.sourceUnit.declarations(declarationIndex)
            If .entryKind <> C2FB_DECLARATION_RECORD OrElse _
               (.recordKind <> C2FB_RECORD_STRUCT AndAlso _
                .recordKind <> C2FB_RECORD_UNION) Then Continue For
            If translation.genericC.header.recordPlans( _
                declarationIndex).isPrepared = 0 OrElse _
               translation.genericC.header.recordPlans( _
                declarationIndex).isSupported = 0 OrElse _
               translation.genericC.header.recordPlans( _
                declarationIndex).bindingReady = 0 OrElse _
               translation.genericC.header.recordPlans( _
                declarationIndex).binding.isPacked <> 0 OrElse _
               translation.genericC.header.recordPlans( _
                declarationIndex).binding.packValue <> 0 OrElse _
               translation.genericC.header.recordPlans( _
                declarationIndex).binding.alignmentValue <> 0 OrElse _
               translation.genericC.header.recordPlans( _
                declarationIndex).hasNonByteFlexibleTail <> 0 Then Continue For
            If translation.genericC.header.recordPlans( _
                declarationIndex).freeBasicName <> freeBasicType AndAlso _
               translation.genericC.header.recordPlans( _
                declarationIndex).aliasFreeBasicName <> freeBasicType AndAlso _
               translation.genericC.header.recordPlans( _
                declarationIndex).implementationName <> freeBasicType Then _
                Continue For
            Dim offset As Long = 0
            Dim unionSize As Long = 0
            Dim maximumAlignment As Long = 1
            For itemIndex As Long = 0 To _
                translation.genericC.header.recordPlans( _
                    declarationIndex).binding.itemCount - 1
                With translation.genericC.header.recordPlans( _
                    declarationIndex).binding.items(itemIndex)
                    If .kind <> C2FB_RECORD_BINDING_ITEM_FIELD OrElse _
                       .isBitField <> 0 OrElse .zeroArrayAccessorName <> "" OrElse _
                       .arrayCount < 0 OrElse .packValue <> 0 Then Return 0
                    Dim fieldType As String = .fieldType
                    Dim fieldPointerDepth As Long = 0
                    While Right(LCase(fieldType), 4) = " ptr"
                        fieldType = Trim(Left(fieldType, Len(fieldType) - 4))
                        fieldPointerDepth += 1
                    Wend
                    Dim fieldSize As Long
                    Dim fieldAlignment As Long
                    If fieldPointerDepth > 0 Then
                        fieldSize = GenericHeaderPreprocessorSizeValue( _
                            translation, "__SIZEOF_POINTER__")
                        fieldAlignment = fieldSize
                    Else
                        fieldSize = GenericHeaderFreeBasicScalarSize( _
                            translation, fieldType)
                        fieldAlignment = GenericHeaderFreeBasicScalarAlignment( _
                            translation, fieldType, fieldSize)
                    End If
                    If fieldSize <= 0 OrElse fieldAlignment <= 0 Then Return 0
                    If .arrayCount > 0 Then
                        If fieldSize > MAX_GENERIC_ARRAY_BOUND_VALUE \ .arrayCount Then _
                            Return 0
                        fieldSize *= .arrayCount
                    End If
                    If fieldAlignment > maximumAlignment Then _
                        maximumAlignment = fieldAlignment

                    If translation.genericC.header.recordPlans( _
                        declarationIndex).binding.isUnion <> 0 Then
                        If fieldSize > unionSize Then unionSize = fieldSize
                    Else
                        Dim remainder As Long = offset Mod fieldAlignment
                        If remainder <> 0 Then
                            Dim padding As Long = fieldAlignment - remainder
                            If offset > MAX_GENERIC_ARRAY_BOUND_VALUE - padding Then _
                                Return 0
                            offset += padding
                        End If
                        If fieldSize > MAX_GENERIC_ARRAY_BOUND_VALUE - offset Then _
                            Return 0
                        offset += fieldSize
                    End If
                End With
            Next

            If translation.genericC.header.recordPlans( _
                declarationIndex).binding.isUnion <> 0 Then offset = unionSize
            Dim remainder As Long = offset Mod maximumAlignment
            If remainder <> 0 Then
                Dim padding As Long = maximumAlignment - remainder
                If offset > MAX_GENERIC_ARRAY_BOUND_VALUE - padding Then Return 0
                offset += padding
            End If
            Return offset
        End With
    Next
    Return 0
End Function

Private Function GenericHeaderSizeofTypeInBytes( _
    ByRef translation As C2fbTranslationContext, ByVal sourceType As String _
) As Long
    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(sourceType, tokens(), tokenCount)
    If tokenCount = 0 OrElse tokenCount >= MAX_TYPE_WORDS Then Return 0
    Dim parsedType As C2fbCTypeModel
    If ParseCTypeModel(tokens(), 0, tokenCount - 1, parsedType) = 0 OrElse _
       parsedType.hasUnsupportedSyntax <> 0 Then Return 0

    Dim aliasIndex As Long = GenericHeaderTypeAliasIndexInHeader( _
        translation.genericC.header, parsedType.baseName, 0)
    If parsedType.pointerDepth = 0 AndAlso aliasIndex >= 0 Then
        With translation.genericC.header.headerTypeAliases(aliasIndex)
            If .isArray <> 0 Then
                If .arrayCount <= 0 Then Return 0
                Dim elementSize As Long = GenericHeaderFreeBasicScalarSize( _
                    translation, .arrayElementType)
                If elementSize <= 0 OrElse _
                   elementSize > MAX_GENERIC_ARRAY_BOUND_VALUE \ .arrayCount Then _
                    Return 0
                Return elementSize * .arrayCount
            End If
        End With
    End If

    Dim pointerCount As Long = 0
    Dim isConst As Long = 0
    Dim freeBasicType As String = MapGenericHeaderCType( _
        translation, parsedType, pointerCount, isConst, _
        C2FB_POINTER_MAPPING_PRESERVE)
    If freeBasicType = "" Then Return 0
    Dim freeBasicPointerDepth As Long = _
        GenericFreeBasicTypePointerDepthInHeader( _
            translation.genericC.header, freeBasicType, 0)
    If pointerCount > 0 OrElse freeBasicPointerDepth > 0 OrElse _
       InStr(freeBasicType, " Ptr") <> 0 Then _
        Return GenericHeaderPreprocessorSizeValue( _
            translation, "__SIZEOF_POINTER__")
    If parsedType.isStruct <> 0 OrElse parsedType.isUnion <> 0 Then _
        Return GenericHeaderSimpleRecordSizeInBytes( _
            translation, freeBasicType)
    Dim scalarSize As Long = GenericHeaderFreeBasicScalarSize( _
        translation, freeBasicType)
    If scalarSize > 0 Then Return scalarSize
    Return GenericHeaderSimpleRecordSizeInBytes(translation, freeBasicType)
End Function

Private Function GenericHeaderIntegerTypeCanHoldSizeof( _
    ByRef translation As C2fbTranslationContext, _
    ByVal castType As String, ByVal sizeValue As Long _
) As Long
    Dim tokens() As String
    Dim tokenCount As Long
    TokenizeCDeclaration(castType, tokens(), tokenCount)
    If tokenCount = 0 OrElse tokenCount >= MAX_TYPE_WORDS Then Return 0
    Dim parsedType As C2fbCTypeModel
    If ParseCTypeModel(tokens(), 0, tokenCount - 1, parsedType) = 0 OrElse _
       parsedType.hasUnsupportedSyntax <> 0 OrElse _
       parsedType.isStruct <> 0 OrElse parsedType.isUnion <> 0 OrElse _
       parsedType.isEnum <> 0 OrElse parsedType.pointerDepth <> 0 Then Return 0
    If parsedType.baseName = "bool" OrElse parsedType.baseName = "_Bool" Then Return 0

    Dim pointerCount As Long = 0
    Dim isConst As Long = 0
    Dim freeBasicType As String = MapGenericHeaderCType( _
        translation, parsedType, pointerCount, isConst, _
        C2FB_POINTER_MAPPING_PRESERVE)
    If freeBasicType = "" OrElse pointerCount <> 0 OrElse _
       InStr(freeBasicType, " Ptr") <> 0 Then Return 0
    Dim storageType As String = GenericHeaderTypeAliasStorageTypeInHeader( _
        translation.genericC.header, freeBasicType, 0)
    If storageType <> "" Then freeBasicType = storageType

    Dim isUnsigned As Long = 0
    Select Case LCase(freeBasicType)
    Case "ubyte", "ushort", "ulong", "ulongint", "uinteger", "culong", _
         "c2fb_c_uint", "c2fb_c_ulong", "c2fb_uint64", "c2fb_size_t"
        isUnsigned = -1
    Case "byte", "short", "long", "longint", "integer", "clong", _
         "c2fb_c_int", "c2fb_c_long", "c2fb_int64"
    Case Else
        Return 0
    End Select

    Dim typeSize As Long = GenericHeaderFreeBasicScalarSize( _
        translation, freeBasicType)
    If typeSize <= 0 Then Return 0
    If typeSize >= 4 Then Return -1
    Dim maxValue As LongInt = 1
    Dim significantBits As Long = typeSize * 8 - CInt(isUnsigned = 0)
    For bitIndex As Long = 1 To significantBits
        maxValue *= 2
    Next
    maxValue -= 1
    Return CInt(sizeValue >= 0 AndAlso sizeValue <= maxValue) * -1
End Function

Private Function GenericHeaderReadSizeofOperand( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, ByRef position As Long, _
    ByRef sizeValue As Long, ByRef nextPosition As Long _
) As Long
    sizeValue = 0
    nextPosition = position
    If position < 1 OrElse position + 5 > Len(sourceExpression) OrElse _
       LCase(Mid(sourceExpression, position, 6)) <> "sizeof" Then Return 0
    If position > 1 AndAlso _
       IsIdentifierPart(Asc(sourceExpression, position - 1)) <> 0 Then Return 0
    If position + 6 <= Len(sourceExpression) AndAlso _
       IsIdentifierPart(Asc(sourceExpression, position + 6)) <> 0 Then Return 0
    position += 6
    SkipCSpaces(sourceExpression, position)
    If position > Len(sourceExpression) OrElse _
       Asc(sourceExpression, position) <> Asc("(") Then Return 0
    Dim groupEnd As Long = position
    If ScanSimpleExpressionGroup( _
        sourceExpression, groupEnd, Asc("("), Asc(")")) = 0 Then Return 0
    Dim sourceType As String = Trim(Mid( _
        sourceExpression, position + 1, groupEnd - position - 2))
    sizeValue = GenericHeaderSizeofTypeInBytes(translation, sourceType)
    If sizeValue <= 0 Then Return 0
    nextPosition = groupEnd
    Return -1
End Function

Private Function ReplaceGenericHeaderSizeofExpressions( _
    ByRef translation As C2fbTranslationContext, _
    ByVal sourceExpression As String, _
    ByRef normalizedExpression As String _
) As Long
    normalizedExpression = ""
    Dim position As Long = 1
    While position <= Len(sourceExpression)
        If Asc(sourceExpression, position) = Asc("(") Then
            Dim castGroupEnd As Long = position
            If ScanSimpleExpressionGroup( _
                sourceExpression, castGroupEnd, Asc("("), Asc(")")) <> 0 Then
                Dim castType As String = Trim(Mid( _
                    sourceExpression, position + 1, _
                    castGroupEnd - position - 2))
                Dim sizeofPosition As Long = castGroupEnd
                SkipCSpaces(sourceExpression, sizeofPosition)
                Dim sizeValue As Long
                Dim afterSizeof As Long
                If GenericHeaderReadSizeofOperand( _
                    translation, sourceExpression, sizeofPosition, _
                    sizeValue, afterSizeof) <> 0 AndAlso _
                   GenericHeaderIntegerTypeCanHoldSizeof( _
                       translation, castType, sizeValue) <> 0 Then
                    normalizedExpression &= Trim(Str(sizeValue))
                    position = afterSizeof
                    Continue While
                End If
            End If
        End If

        Dim sizeValue As Long
        Dim afterSizeof As Long
        Dim sizeofPosition As Long = position
        If GenericHeaderReadSizeofOperand( _
            translation, sourceExpression, sizeofPosition, _
            sizeValue, afterSizeof) <> 0 Then
            normalizedExpression &= Trim(Str(sizeValue))
            position = afterSizeof
        Else
            normalizedExpression &= Mid(sourceExpression, position, 1)
            position += 1
        End If
    Wend
    Return -1
End Function


/' end of c2fb_generic_sizeof.bi '/
