Attribute VB_Name = "modWheel"
Option Explicit

' VB6 nu are MouseWheel in PictureBox.
' Interceptam WM_MOUSEWHEEL pentru cele doua poze si fereastra.
' ATENTIE IN IDE: inchideti formularul normal inainte de Stop/End.

Private Const WM_MOUSEWHEEL As Long = &H20A
Private Const GWL_WNDPROC As Long = -4
Private Const WHEEL_DELTA As Long = 120

Private Type POINTAPI
    X As Long
    Y As Long
End Type

Private Type RECTAPI
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" _
    (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Private Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
    (ByVal lpPrevWndFunc As Long, ByVal hWnd As Long, ByVal uMsg As Long, _
     ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Private Declare Function ScreenToClient Lib "user32" (ByVal hWnd As Long, lpPoint As POINTAPI) As Long
Private Declare Function GetWindowRect Lib "user32" (ByVal hWnd As Long, lpRect As RECTAPI) As Long
Private Declare Function GetFocus Lib "user32" () As Long
Private Declare Function GetTickCount Lib "kernel32" () As Long

Private gFormHwnd As Long, gSourceHwnd As Long, gCanvasHwnd As Long
Private gFormOld As Long, gSourceOld As Long, gCanvasOld As Long
Private gWheelCarry As Long, gLastArea As Long

Public Function GetFocusHwnd() As Long
    GetFocusHwnd = GetFocus()
End Function

Public Function ClockTick() As Long
    ClockTick = GetTickCount()
End Function

Public Sub StartWheelHook(ByVal hForm As Long, ByVal hSource As Long, ByVal hCanvas As Long)
    StopWheelHook
    gFormHwnd = hForm
    gSourceHwnd = hSource
    gCanvasHwnd = hCanvas
    gSourceOld = SetWindowLong(gSourceHwnd, GWL_WNDPROC, AddressOf WheelWndProc)
    gCanvasOld = SetWindowLong(gCanvasHwnd, GWL_WNDPROC, AddressOf WheelWndProc)
    gFormOld = SetWindowLong(gFormHwnd, GWL_WNDPROC, AddressOf WheelWndProc)
End Sub

Public Sub StopWheelHook()
    Dim status As Long
    If gFormHwnd <> 0 And gFormOld <> 0 Then
        status = SetWindowLong(gFormHwnd, GWL_WNDPROC, gFormOld)
    End If
    If gCanvasHwnd <> 0 And gCanvasOld <> 0 Then
        status = SetWindowLong(gCanvasHwnd, GWL_WNDPROC, gCanvasOld)
    End If
    If gSourceHwnd <> 0 And gSourceOld <> 0 Then
        status = SetWindowLong(gSourceHwnd, GWL_WNDPROC, gSourceOld)
    End If
    gFormHwnd = 0: gSourceHwnd = 0: gCanvasHwnd = 0
    gFormOld = 0: gSourceOld = 0: gCanvasOld = 0
    gWheelCarry = 0: gLastArea = 0
End Sub

Private Function HitWindow(ByVal hWnd As Long, ByRef pt As POINTAPI) As Boolean
    Dim r As RECTAPI
    If hWnd = 0 Then Exit Function
    If GetWindowRect(hWnd, r) = 0 Then Exit Function
    HitWindow = (pt.X >= r.Left And pt.X < r.Right And _
                 pt.Y >= r.Top And pt.Y < r.Bottom)
End Function

Public Function WheelWndProc(ByVal hWnd As Long, ByVal uMsg As Long, _
                             ByVal wParam As Long, ByVal lParam As Long) As Long
    Dim oldProc As Long, pt As POINTAPI
    Dim area As Long, targetHwnd As Long
    Dim delta As Long, steps As Long
    If hWnd = gSourceHwnd Then oldProc = gSourceOld
    If hWnd = gCanvasHwnd Then oldProc = gCanvasOld
    If hWnd = gFormHwnd Then oldProc = gFormOld

    On Error GoTo forwardMessage
    If uMsg = WM_MOUSEWHEEL Then
        If GetCursorPos(pt) <> 0 Then
            If HitWindow(gSourceHwnd, pt) Then
                area = 1: targetHwnd = gSourceHwnd
            ElseIf HitWindow(gCanvasHwnd, pt) Then
                area = 2: targetHwnd = gCanvasHwnd
            End If
        End If

        If area <> 0 Then
            ' HIWORD(wParam), interpretat ca signed 16-bit.
            delta = (wParam And &HFFFF0000) \ &H10000
            If area <> gLastArea Then gWheelCarry = 0
            gLastArea = area
            gWheelCarry = gWheelCarry + delta
            steps = Fix(CDbl(gWheelCarry) / WHEEL_DELTA)
            gWheelCarry = gWheelCarry - steps * WHEEL_DELTA
            If steps <> 0 Then
                Call ScreenToClient(targetHwnd, pt)
                frmMain.HandleMouseWheel area, steps, pt.X, pt.Y
            End If
            WheelWndProc = 0
            Exit Function
        End If
    End If

forwardMessage:
    If oldProc <> 0 Then
        WheelWndProc = CallWindowProc(oldProc, hWnd, uMsg, wParam, lParam)
    Else
        WheelWndProc = 0
    End If
End Function
