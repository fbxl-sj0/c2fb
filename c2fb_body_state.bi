/'
    Project: SQLite FreeBASIC port tools

    File: c2fb_body_state.bi

    Purpose:

        Keep lexical scopes and control-flow targets local to one C procedure.

    Responsibilities:

        - Retain local variable names and types for one C procedure.
        - Give each automatic variable a distinct FreeBASIC name.
        - Retain loop and switch break targets and loop continue targets.
        - Match C labels using C's case-sensitive names.
        - Collect automatic declarations before executable statements.
        - Keep expression temporaries and const storage names procedure-local.
        - Track alloca blocks through each procedure's return paths.

    This file intentionally does NOT contain:

        - Global conversion state, expression parsing, or runtime code.
'/

' Bound parser nesting and per-procedure labels independently of source size.
Private Const MAX_BODY_NESTING As Long = 128
Private Const MAX_BODY_LABELS As Long = 512

Private Type SimpleLocalVariable
    name As String
    typeName As String
    isConst As Long
End Type

Private Type SimpleControlFrame
    breakLabel As String
    continueLabel As String
    hasBreak As Long
End Type

Private Type SimpleBodyLabel
    sourceName As String
    fbName As String
    isDefined As Long
    isUsed As Long
End Type

Private Type SimpleBodyLocalRecordTag
    sourceName As String
    freeBasicType As String
End Type

Private Type SimpleBodyContext
    functionName As String
    failureCode As String
    failureMessage As String
    declarations As String
    localSerial As Long
    temporarySerial As Long
    allocaHeadName As String
    allocaNextName As String
    readOnlyAliases As String
    flowSerial As Long
    scopeDepth As Long
    scopeSymbols(0 To MAX_BODY_NESTING - 1) As Long
    scopeLocals(0 To MAX_BODY_NESTING - 1) As Long
    scopeRecordTags(0 To MAX_BODY_NESTING - 1) As Long
    localRecordTags(0 To MAX_SIMPLE_VARIABLES - 1) As SimpleBodyLocalRecordTag
    localRecordTagCount As Long
    frameCount As Long
    frames(0 To MAX_BODY_NESTING - 1) As SimpleControlFrame
    labelCount As Long
    branchJump As Long
    labels(0 To MAX_BODY_LABELS - 1) As SimpleBodyLabel
End Type

Private Function SimpleBodyLocalRecordType( _
    ByRef context As SimpleBodyContext, ByVal sourceName As String _
) As String
    For tagIndex As Long = context.localRecordTagCount - 1 To 0 Step -1
        If context.localRecordTags(tagIndex).sourceName = sourceName Then _
            Return context.localRecordTags(tagIndex).freeBasicType
    Next
    Return ""
End Function

Private Function RegisterSimpleBodyLocalRecordTag( _
    ByRef context As SimpleBodyContext, _
    ByVal sourceName As String, ByVal freeBasicType As String _
) As Long
    If sourceName = "" OrElse freeBasicType = "" Then Return 0
    Dim scopeStart As Long = 0
    If context.scopeDepth > 0 Then _
        scopeStart = context.scopeRecordTags(context.scopeDepth - 1)
    For tagIndex As Long = scopeStart To context.localRecordTagCount - 1
        If context.localRecordTags(tagIndex).sourceName = sourceName Then Return 0
    Next
    If context.localRecordTagCount >= MAX_SIMPLE_VARIABLES Then Return 0
    context.localRecordTags(context.localRecordTagCount).sourceName = sourceName
    context.localRecordTags(context.localRecordTagCount).freeBasicType = freeBasicType
    context.localRecordTagCount += 1
    Return -1
End Function

Private Sub ResetSimpleBodyFailure(ByRef context As SimpleBodyContext)
    context.failureCode = ""
    context.failureMessage = ""
End Sub

Private Sub RecordSimpleBodyFailure( _
    ByRef context As SimpleBodyContext, _
    ByVal code As String, ByVal message As String _
)
    If context.failureMessage <> "" Then Exit Sub
    context.failureCode = code
    context.failureMessage = message
End Sub

Private Function NewSimpleFlowLabel(ByRef context As SimpleBodyContext) As String
    Dim result As String = "c2fb_flow_" & Trim(Str(context.flowSerial))
    context.flowSerial += 1
    Return result
End Function

Private Function PushSimpleBodyScope( _
    ByRef context As SimpleBodyContext, ByVal symbolCount As Long, ByVal localCount As Long _
) As Long
    If context.scopeDepth >= MAX_BODY_NESTING Then Return 0
    context.scopeSymbols(context.scopeDepth) = symbolCount
    context.scopeLocals(context.scopeDepth) = localCount
    context.scopeRecordTags(context.scopeDepth) = context.localRecordTagCount
    context.scopeDepth += 1
    Return -1
End Function

Private Sub PopSimpleBodyScope( _
    ByRef context As SimpleBodyContext, ByRef symbolCount As Long, ByRef localCount As Long _
)
    context.scopeDepth -= 1
    symbolCount = context.scopeSymbols(context.scopeDepth)
    localCount = context.scopeLocals(context.scopeDepth)
    context.localRecordTagCount = _
        context.scopeRecordTags(context.scopeDepth)
End Sub

Private Function PushSimpleControlFrame( _
    ByRef context As SimpleBodyContext, ByVal breakLabel As String, ByVal continueLabel As String _
) As Long
    If context.frameCount >= MAX_BODY_NESTING Then Return 0
    With context.frames(context.frameCount)
        .breakLabel = breakLabel
        .continueLabel = continueLabel
        .hasBreak = 0
    End With
    context.frameCount += 1
    Return -1
End Function

Private Function SimpleBreakTarget(ByRef context As SimpleBodyContext) As String
    If context.frameCount = 0 Then Return ""
    context.frames(context.frameCount - 1).hasBreak = -1
    Return context.frames(context.frameCount - 1).breakLabel
End Function

Private Function SimpleContinueTarget(ByRef context As SimpleBodyContext) As String
    ' A switch captures break but does not capture continue. Continue selects
    ' the nearest loop, including the increment step of a converted C for.
    For i As Long = context.frameCount - 1 To 0 Step -1
        If context.frames(i).continueLabel <> "" Then Return context.frames(i).continueLabel
    Next
    Return ""
End Function

Private Function SimpleSourceLabel( _
    ByRef context As SimpleBodyContext, ByVal sourceName As String, ByVal isDefinition As Long _
) As String
    Dim index As Long = -1
    For i As Long = 0 To context.labelCount - 1
        If context.labels(i).sourceName = sourceName Then
            index = i
            Exit For
        End If
    Next
    If index < 0 Then
        If context.labelCount >= MAX_BODY_LABELS Then Return ""
        index = context.labelCount
        context.labelCount += 1
        context.labels(index).sourceName = sourceName
        context.labels(index).fbName = "c2fb_label_" & Trim(Str(index))
    End If
    If isDefinition <> 0 Then
        If context.labels(index).isDefined <> 0 Then Return ""
        context.labels(index).isDefined = -1
    Else
        context.labels(index).isUsed = -1
        For i As Long = 0 To context.frameCount - 1
            context.frames(i).hasBreak = -1
        Next
    End If
    Return context.labels(index).fbName
End Function

Private Function IndentSimpleBodyText(ByVal sourceText As String, ByVal indentText As String) As String
    Dim result As String = indentText
    For i As Long = 1 To Len(sourceText)
        result &= Mid(sourceText, i, 1)
        If Asc(sourceText, i) = 10 AndAlso i < Len(sourceText) Then result &= indentText
    Next
    Return result
End Function

Private Function SimpleBodyContainsFunctionCall( _
    ByVal sourceText As String, ByVal procedureName As String _
) As Long
    Dim position As Long = 1
    While position <= Len(sourceText)
        Dim currentByte As Long = Asc(sourceText, position)
        If currentByte = Asc(Chr(34)) OrElse currentByte = Asc("'") Then
            Dim quoteByte As Long = currentByte
            position += 1
            While position <= Len(sourceText)
                currentByte = Asc(sourceText, position)
                If currentByte = Asc(Chr(92)) Then
                    position += 2
                ElseIf currentByte = quoteByte Then
                    position += 1
                    Exit While
                Else
                    position += 1
                End If
            Wend
        ElseIf currentByte = Asc("/") AndAlso position < Len(sourceText) Then
            If Mid(sourceText, position + 1, 1) = "/" Then
                position += 2
                While position <= Len(sourceText) AndAlso _
                      Asc(sourceText, position) <> 10
                    position += 1
                Wend
            ElseIf Mid(sourceText, position + 1, 1) = "*" Then
                position += 2
                While position < Len(sourceText)
                    If Mid(sourceText, position, 2) = "*/" Then
                        position += 2
                        Exit While
                    End If
                    position += 1
                Wend
            Else
                position += 1
            End If
        ElseIf IsIdentifierStart(currentByte) <> 0 Then
            Dim identifierStart As Long = position
            position += 1
            While position <= Len(sourceText) AndAlso _
                  IsIdentifierPart(Asc(sourceText, position)) <> 0
                position += 1
            Wend
            If position - identifierStart = Len(procedureName) AndAlso _
               Mid(sourceText, identifierStart, Len(procedureName)) = procedureName Then
                Dim callPosition As Long = position
                SkipCSpaces(sourceText, callPosition)
                If callPosition <= Len(sourceText) AndAlso _
                   Mid(sourceText, callPosition, 1) = "(" Then Return -1
            End If
        Else
            position += 1
        End If
    Wend
    Return 0
End Function

Private Function SimpleBodyContainsBuiltinAlloca(ByVal sourceText As String) As Long
    Return SimpleBodyContainsFunctionCall(sourceText, "__builtin_alloca")
End Function

Private Function SimpleBodyContainsNonlocalExit(ByVal sourceText As String) As Long
    Dim names(0 To 3) As String
    names(0) = "longjmp"
    names(1) = "_longjmp"
    names(2) = "siglongjmp"
    names(3) = "__longjmp_chk"
    For i As Long = 0 To UBound(names)
        If SimpleBodyContainsFunctionCall(sourceText, names(i)) <> 0 Then Return -1
    Next
    Return 0
End Function

Private Function SimpleAllocaCleanupText( _
    ByRef context As SimpleBodyContext, ByVal indentText As String = "" _
) As String
    If context.allocaHeadName = "" OrElse context.allocaNextName = "" Then Return ""
    Dim result As String = indentText & "While " & context.allocaHeadName & " <> 0" & Chr(10)
    result &= indentText & "    " & context.allocaNextName & " = Cptr(Any Ptr Ptr, " & _
        context.allocaHeadName & ")[0]" & Chr(10)
    result &= indentText & "    Deallocate(" & context.allocaHeadName & ")" & Chr(10)
    result &= indentText & "    " & context.allocaHeadName & " = " & _
        context.allocaNextName & Chr(10)
    result &= indentText & "Wend"
    Return result
End Function

/' end of c2fb_body_state.bi '/
