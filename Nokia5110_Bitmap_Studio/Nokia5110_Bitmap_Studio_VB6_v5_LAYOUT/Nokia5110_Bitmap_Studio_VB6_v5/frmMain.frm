VERSION 5.00
Begin VB.Form frmMain
   Caption         =   "Nokia 5110 Bitmap Studio 5.0  |  84 x 48"
   ClientHeight    =   10720
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   16845
   LinkTopic       =   "Form1"
   ScaleHeight     =   11115
   ScaleWidth      =   16845
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picSource
      AutoRedraw      =   -1  'True
      BackColor       =   &H00303030&
      BorderStyle     =   1
      Height         =   5220
      Left           =   240
      ScaleMode       =   3  'Pixel
      TabIndex        =   0
      Top            =   1350
      Width          =   6900   End
   Begin VB.PictureBox picCanvas
      AutoRedraw      =   -1  'True
      BackColor       =   &H00D2DFCB&
      BorderStyle     =   1
      Height         =   5220
      Left           =   7380
      ScaleMode       =   3  'Pixel
      TabIndex        =   1
      Top            =   1350
      Width          =   8940   End
   Begin VB.Label lblSourceTitle
      Caption         =   "01   FOTOGRAFIE ORIGINALA"
      Height         =   285
      Left           =   240
      Top            =   940
      Width          =   6900   End
   Begin VB.Label lblCanvasTitle
      Caption         =   "02   BITMAP 84 x 48  /  EDITARE LIVE"
      Height         =   285
      Left           =   7380
      Top            =   940
      Width          =   8940   End
   Begin VB.Label lblZoomLabel
      Caption         =   "ROTiTA: zoom  |  CLIC: fixeaza cadrul"
      Height         =   285
      Left           =   240
      Top            =   6745
      Width          =   4700   End
   Begin VB.Label lblZoom
      Caption         =   "100%  | CADRU MOBIL"
      Height         =   285
      Left           =   5130
      Top            =   6745
      Width          =   1970   End
   Begin VB.CheckBox chkDither
      Caption         =   "Dithering Floyd-Steinberg (actualizare LIVE)"
      Height         =   345
      Left           =   7380
      TabIndex        =   4
      Top            =   6680
      Value           =   1
      Width          =   7800   End
   Begin VB.Label lblThreshold
      Caption         =   "PRAG 128 - rotita peste rezultatul din dreapta"
      Height         =   285
      Left           =   7380
      Top            =   7070
      Width          =   8000   End
   Begin VB.CommandButton cmdOpen
      Caption         =   "Deschide imagine"
      Height         =   540
      Left           =   240
      TabIndex        =   6
      Top            =   7840
      Width          =   3900   End
   Begin VB.CommandButton cmdInvert
      Caption         =   "Inverseaza"
      Height         =   540
      Left           =   4300
      TabIndex        =   7
      Top            =   7840
      Width          =   3900   End
   Begin VB.CommandButton cmdClear
      Caption         =   "Goleste bitmapul"
      Height         =   540
      Left           =   8360
      TabIndex        =   8
      Top            =   7840
      Width          =   3900   End
   Begin VB.CommandButton cmdResetEdits
      Caption         =   "Sterge retusurile"
      Height         =   540
      Left           =   12420
      TabIndex        =   9
      Top            =   7840
      Width          =   3900   End
   Begin VB.CommandButton cmdHex
      Caption         =   "Export HEX  |  504 octeti"
      Height         =   600
      Left           =   240
      TabIndex        =   10
      Top            =   8870
      Width          =   8000   End
   Begin VB.CommandButton cmdVerilog
      Caption         =   "Export Verilog  |  bitmap integrat"
      Height         =   600
      Left           =   8435
      TabIndex        =   11
      Top            =   8870
      Width          =   7885   End
   Begin VB.Label lblHowto
      Caption         =   "STANGA: click fixeaza cadrul. DREAPTA: editare pixeli. Rotita: zoom / prag."
      Height         =   390
      Left           =   260
      Top            =   9680
      Width          =   16100   End
   Begin VB.Label lblStatus
      Caption         =   "Gata. Deschide o imagine sau deseneaza pe bitmap."
      Height         =   430
      Left           =   260
      Top            =   10100
      Width          =   16100   End
   Begin VB.Label lblBrand
      Caption         =   "NOKIA 5110  /  BITMAP STUDIO"
      Height          =   355
      Left            =   240
      Top             =   140
      Width           =   10600
      ForeColor       =   &H003C3630&
      BackStyle       =   0  'Transparent
      BeginProperty Font
         Name            =   "Segoe UI"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Label lblBrandSub
      Caption         =   "Crop interactiv  /  Monocrom  /  Desen cu mouse-ul  /  Export FPGA"
      Height          =   300
      Left            =   240
      Top             =   560
      Width           =   12600
      ForeColor       =   &H00777777&
      BackStyle       =   0  'Transparent
   End
   Begin VB.Label lblSourceTips
      Caption         =   "DREAPTA + tragere: deseneaza  |  SHIFT + dreapta: sterge"
      Height          =   285
      Left            =   240
      Top             =   7060
      Width           =   6900
      ForeColor       =   &H00777777&
      BackStyle       =   0  'Transparent
   End
   Begin VB.Label lblEditHeading
      Caption         =   "EDITARE"
      Height          =   270
      Left            =   240
      Top             =   7490
      Width           =   4600
      ForeColor       =   &H00555555&
      BackStyle       =   0  'Transparent
      BeginProperty Font
         Name            =   "Segoe UI"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Label lblExportHeading
      Caption         =   "EXPORT PENTRU FPGA"
      Height          =   270
      Left            =   240
      Top             =   8510
      Width           =   4600
      ForeColor       =   &H00555555&
      BackStyle       =   0  'Transparent
      BeginProperty Font
         Name            =   "Segoe UI"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Line linSeparator1
      BorderColor     =   &H00DDDDDD&
      X1              =   240
      X2              =   16320
      Y1              =   7390
      Y2              =   7390
   End
   Begin VB.Line linSeparator2
      BorderColor     =   &H00DDDDDD&
      X1              =   240
      X2              =   16320
      Y1              =   8440
      Y2              =   8440
   End
   Begin VB.Line linSeparator3
      BorderColor     =   &H00DDDDDD&
      X1              =   240
      X2              =   16320
      Y1              =   9550
      Y2              =   9550
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

' Nokia 5110 Bitmap Studio v4 - click freeze crop; left-source paint; wheel zoom / threshold.
' Fara OCX-uri; JPG, PNG, BMP prin GDI+ din Windows.
Private Const W As Long = 84
Private Const H As Long = 48
Private Const ZOOM As Long = 7
Private Const OFN_EXPLORER As Long = &H80000
Private Const OFN_FILEMUSTEXIST As Long = &H1000
Private Const OFN_PATHMUSTEXIST As Long = &H800
Private Const OFN_OVERWRITEPROMPT As Long = &H2

Private Type OPENFILENAME
    lStructSize As Long
    hwndOwner As Long
    hInstance As Long
    lpstrFilter As String
    lpstrCustomFilter As String
    nMaxCustFilter As Long
    nFilterIndex As Long
    lpstrFile As String
    nMaxFile As Long
    lpstrFileTitle As String
    nMaxFileTitle As Long
    lpstrInitialDir As String
    lpstrTitle As String
    Flags As Long
    nFileOffset As Integer
    nFileExtension As Integer
    lpstrDefExt As String
    lCustData As Long
    lpfnHook As Long
    lpTemplateName As String
End Type

Private Type GDIPLUS_STARTUP_INPUT
    GdiplusVersion As Long
    DebugEventCallback As Long
    SuppressBackgroundThread As Long
    SuppressExternalCodecs As Long
End Type

Private Type SourceMark
    x As Double
    y As Double
End Type

Private Declare Function GetOpenFileName Lib "comdlg32.dll" Alias "GetOpenFileNameA" (ofn As OPENFILENAME) As Long
Private Declare Function GetSaveFileName Lib "comdlg32.dll" Alias "GetSaveFileNameA" (ofn As OPENFILENAME) As Long
Private Declare Function GdiplusStartup Lib "gdiplus.dll" (token As Long, si As GDIPLUS_STARTUP_INPUT, ByVal output As Long) As Long
Private Declare Sub GdiplusShutdown Lib "gdiplus.dll" (ByVal token As Long)
Private Declare Function GdipCreateBitmapFromFile Lib "gdiplus.dll" (ByVal filename As Long, bitmap As Long) As Long
Private Declare Function GdipGetImageWidth Lib "gdiplus.dll" (ByVal bitmap As Long, imageWidth As Long) As Long
Private Declare Function GdipGetImageHeight Lib "gdiplus.dll" (ByVal bitmap As Long, imageHeight As Long) As Long
Private Declare Function GdipBitmapGetPixel Lib "gdiplus.dll" (ByVal bitmap As Long, ByVal x As Long, ByVal y As Long, color As Long) As Long
Private Declare Function GdipDisposeImage Lib "gdiplus.dll" (ByVal bitmap As Long) As Long
Private Declare Function GdipCreateFromHDC Lib "gdiplus.dll" (ByVal hdc As Long, graphics As Long) As Long
Private Declare Function GdipDrawImageRectI Lib "gdiplus.dll" (ByVal graphics As Long, ByVal image As Long, ByVal x As Long, ByVal y As Long, ByVal width As Long, ByVal height As Long) As Long
Private Declare Function GdipDeleteGraphics Lib "gdiplus.dll" (ByVal graphics As Long) As Long

Private pixels(0 To 83, 0 To 47) As Byte
Private edits(0 To 83, 0 To 47) As Boolean
Private editValues(0 To 83, 0 To 47) As Byte
Private gray(0 To 83, 0 To 47) As Double
Private gdipToken As Long
Private currentFile As String
Private sourceBitmap As Long
Private imageW As Long, imageH As Long
Private frameX As Double, frameY As Double
Private frameW As Double, frameH As Double
Private previewScale As Double
Private previewLeft As Double, previewTop As Double
Private lastX As Long, lastY As Long
Private drawing As Boolean
Private paintValue As Byte
Private invertResult As Boolean
Private zoomValue As Long
Private thresholdValue As Long
Private lastPreviewUpdate As Long
Private frameLocked As Boolean
Private sourceDrawing As Boolean
Private sourceErasing As Boolean
Private lastSourcePX As Long, lastSourcePY As Long
Private sourceMarks() As SourceMark
Private sourceMarkCount As Long


Private Sub Form_Load()
    Dim si As GDIPLUS_STARTUP_INPUT
    si.GdiplusVersion = 1
    If GdiplusStartup(gdipToken, si, 0) <> 0 Then
        MsgBox "GDI+ nu poate fi initializat pe acest sistem.", vbCritical
    End If
    zoomValue = 100
    thresholdValue = 128
    frameLocked = False
    ReDim sourceMarks(0 To 255)
    RefreshControlLabels
    lblHowto.Caption = "STANGA: muta cadrul cu mouse-ul; clic = fixeaza/elibereaza; rotita = zoom; dreapta + tragere = deseneaza." & vbCrLf & _
                       "DREAPTA: clic stanga = deseneaza; clic dreapta = sterge; rotita = prag de conversie."
    picSource.Cls
    RedrawAll
    StartWheelHook Me.hWnd, picSource.hWnd, picCanvas.hWnd
End Sub

Private Sub Form_Unload(Cancel As Integer)
    StopWheelHook
    If sourceBitmap <> 0 Then GdipDisposeImage sourceBitmap
    If gdipToken <> 0 Then GdiplusShutdown gdipToken
End Sub

Private Function ChoosePath(ByVal saveMode As Boolean, ByVal kind As String) As String
    Dim ofn As OPENFILENAME, result As Long, p As Long
    ofn.lStructSize = Len(ofn)
    ofn.hwndOwner = Me.hWnd
    ofn.lpstrFile = String$(512, vbNullChar)
    ofn.nMaxFile = 512
    ofn.Flags = OFN_EXPLORER Or OFN_PATHMUSTEXIST
    If saveMode Then
        ofn.Flags = ofn.Flags Or OFN_OVERWRITEPROMPT
        If kind = "hex" Then
            ofn.lpstrFilter = "HEX text (*.hex)" & vbNullChar & "*.hex" & vbNullChar & vbNullChar
            ofn.lpstrDefExt = "hex"
            ofn.lpstrTitle = "Export 504 octeti HEX"
        Else
            ofn.lpstrFilter = "Verilog (*.v)" & vbNullChar & "*.v" & vbNullChar & vbNullChar
            ofn.lpstrDefExt = "v"
            ofn.lpstrTitle = "Export Verilog autonom"
        End If
        result = GetSaveFileName(ofn)
    Else
        ofn.Flags = ofn.Flags Or OFN_FILEMUSTEXIST
        ofn.lpstrFilter = "Imagini (*.png;*.jpg;*.jpeg;*.bmp;*.gif;*.tif)" & vbNullChar & _
            "*.png;*.jpg;*.jpeg;*.bmp;*.gif;*.tif" & vbNullChar & "Toate (*.*)" & vbNullChar & "*.*" & vbNullChar & vbNullChar
        ofn.lpstrTitle = "Alege fotografia"
        result = GetOpenFileName(ofn)
    End If
    If result = 0 Then Exit Function
    p = InStr(1, ofn.lpstrFile, vbNullChar)
    If p > 1 Then ChoosePath = Left$(ofn.lpstrFile, p - 1)
End Function

Private Sub cmdOpen_Click()
    Dim path As String
    path = ChoosePath(False, "")
    If Len(path) = 0 Then Exit Sub
    LoadSource path
End Sub

Private Sub LoadSource(ByVal path As String)
    Dim bmp As Long, iw As Long, ih As Long
    On Error GoTo failed
    If gdipToken = 0 Then Err.Raise 5, , "GDI+ indisponibil."
    If GdipCreateBitmapFromFile(StrPtr(path), bmp) <> 0 Then Err.Raise 5, , "Imagine incompatibila."
    If GdipGetImageWidth(bmp, iw) <> 0 Then Err.Raise 5
    If GdipGetImageHeight(bmp, ih) <> 0 Then Err.Raise 5
    If iw <= 0 Or ih <= 0 Then Err.Raise 5
    If sourceBitmap <> 0 Then GdipDisposeImage sourceBitmap
    sourceBitmap = bmp
    imageW = iw: imageH = ih
    currentFile = path
    invertResult = False
    ClearEdits
    sourceMarkCount = 0
    frameLocked = False
    sourceDrawing = False
    ResetFrame
    DrawSource
    ConvertCurrent
    lblStatus.Caption = "Fotografie: " & path & " | Click stanga fixeaza cadrul. Dreapta+drag deseneaza pe fotografie."
    Exit Sub
failed:
    If bmp <> 0 Then GdipDisposeImage bmp
    MsgBox "Nu am putut incarca imaginea: " & Err.Description, vbExclamation
End Sub

' Cadru de decupaj cu aspect fix 84:48; zoom 100%-600%.
Private Sub ResetFrame()
    If imageW <= 0 Or imageH <= 0 Then Exit Sub
    frameX = imageW / 2#
    frameY = imageH / 2#
    zoomValue = 100
    UpdateFrame
End Sub

Private Sub UpdateFrame()
    Dim baseW As Double, baseH As Double, k As Double
    If sourceBitmap = 0 Then Exit Sub
    baseW = imageW
    baseH = baseW * H / W
    If baseH > imageH Then
        baseH = imageH
        baseW = baseH * W / H
    End If
    k = zoomValue / 100#
    frameW = baseW / k
    frameH = baseH / k
    If frameX < frameW / 2# Then frameX = frameW / 2#
    If frameX > imageW - frameW / 2# Then frameX = imageW - frameW / 2#
    If frameY < frameH / 2# Then frameY = frameH / 2#
    If frameY > imageH - frameH / 2# Then frameY = imageH - frameH / 2#
    RefreshControlLabels
End Sub

Private Sub RefreshControlLabels()
    If frameLocked Then
        lblZoom.Caption = CStr(zoomValue) & "%  | CADRU FIX"
    Else
        lblZoom.Caption = CStr(zoomValue) & "%  | CADRU MOBIL"
    End If
    lblThreshold.Caption = "PRAG " & CStr(thresholdValue) & " - rotita pe imagine"
End Sub

' Fixeaza centrul decupajului pe cursorul aflat deasupra fotografiei.
' Returneaza False daca mouse-ul este in marginile goale ale PictureBox-ului.
Private Function PlaceFrameAt(ByVal px As Double, ByVal py As Double) As Boolean
    Dim cx As Double, cy As Double
    If sourceBitmap = 0 Or previewScale <= 0 Then Exit Function
    cx = (px - previewLeft) / previewScale
    cy = (py - previewTop) / previewScale
    If cx < 0 Or cy < 0 Or cx >= imageW Or cy >= imageH Then Exit Function
    frameX = cx
    frameY = cy
    UpdateFrame
    PlaceFrameAt = True
End Function


' Primul click stanga fixeaza cadrul. Al doilea il elibereaza.
' Desenare pe original: dreapta+tragere; Shift+dreapta sterge trasaturile.
Private Sub picSource_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Dim nowTick As Long
    If sourceBitmap = 0 Then Exit Sub
    If GetFocusHwnd() <> picSource.hWnd Then picSource.SetFocus
    If sourceDrawing Then
        If (Button And vbRightButton) <> 0 Then
            PaintSourceStroke CLng(X), CLng(Y)
        Else
            sourceDrawing = False
        End If
        Exit Sub
    End If
    If frameLocked Then Exit Sub
    nowTick = ClockTick()
    If (nowTick - lastPreviewUpdate) < 40 And (nowTick - lastPreviewUpdate) >= 0 Then Exit Sub
    If PlaceFrameAt(X, Y) Then
        lastPreviewUpdate = nowTick
        DrawSource
        ConvertCurrent
    End If
End Sub

Private Sub picSource_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Dim sx As Double, sy As Double
    If sourceBitmap = 0 Then Exit Sub
    If Not SourceCoords(X, Y, sx, sy) Then Exit Sub
    If Button = vbLeftButton Then
        If Not frameLocked Then
            PlaceFrameAt X, Y
            frameLocked = True
        Else
            frameLocked = False
            PlaceFrameAt X, Y
        End If
        RefreshControlLabels
        DrawSource
        ConvertCurrent
    ElseIf Button = vbRightButton Then
        sourceDrawing = True
        sourceErasing = ((Shift And vbShiftMask) <> 0)
        lastSourcePX = CLng(X)
        lastSourcePY = CLng(Y)
        PaintSourceAt lastSourcePX, lastSourcePY
        DrawSource
        ConvertCurrent
    End If
End Sub

Private Sub picSource_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    sourceDrawing = False
End Sub

Private Function SourceCoords(ByVal px As Double, ByVal py As Double, _
                              ByRef ix As Double, ByRef iy As Double) As Boolean
    If sourceBitmap = 0 Or previewScale <= 0 Then Exit Function
    ix = (px - previewLeft) / previewScale
    iy = (py - previewTop) / previewScale
    SourceCoords = (ix >= 0 And iy >= 0 And ix < imageW And iy < imageH)
End Function

Private Sub PaintSourceAt(ByVal px As Long, ByVal py As Long)
    Dim ix As Double, iy As Double, j As Long, writeAt As Long
    Dim r As Double
    If Not SourceCoords(px, py, ix, iy) Then Exit Sub
    If sourceErasing Then
        r = 5# / previewScale
        writeAt = 0
        For j = 0 To sourceMarkCount - 1
            If (sourceMarks(j).x - ix) ^ 2 + (sourceMarks(j).y - iy) ^ 2 > r * r Then
                sourceMarks(writeAt) = sourceMarks(j)
                writeAt = writeAt + 1
            End If
        Next j
        sourceMarkCount = writeAt
    Else
        If sourceMarkCount > 0 Then
            If Abs(sourceMarks(sourceMarkCount - 1).x - ix) < 0.3 / previewScale And _
               Abs(sourceMarks(sourceMarkCount - 1).y - iy) < 0.3 / previewScale Then Exit Sub
        End If
        If sourceMarkCount > UBound(sourceMarks) Then
            If sourceMarkCount >= 60000 Then Exit Sub
            ReDim Preserve sourceMarks(0 To (UBound(sourceMarks) + 1) * 2 - 1)
        End If
        sourceMarks(sourceMarkCount).x = ix
        sourceMarks(sourceMarkCount).y = iy
        sourceMarkCount = sourceMarkCount + 1
    End If
End Sub

Private Sub PaintSourceStroke(ByVal px As Long, ByVal py As Long)
    Dim dx As Long, dy As Long, sx As Long, sy As Long
    Dim e As Long, e2 As Long, xx As Long, yy As Long
    xx = lastSourcePX: yy = lastSourcePY
    dx = Abs(px - xx): dy = -Abs(py - yy)
    If xx < px Then sx = 1 Else sx = -1
    If yy < py Then sy = 1 Else sy = -1
    e = dx + dy
    Do
        PaintSourceAt xx, yy
        If xx = px And yy = py Then Exit Do
        e2 = 2 * e
        If e2 >= dy Then e = e + dy: xx = xx + sx
        If e2 <= dx Then e = e + dx: yy = yy + sy
    Loop
    lastSourcePX = px: lastSourcePY = py
    DrawSource
    ConvertCurrent
End Sub

' Apelata din modWheel.bas (WM_MOUSEWHEEL).
' area = 1 pentru fotografia originala; area = 2 pentru bitmap.
Public Sub HandleMouseWheel(ByVal area As Long, ByVal wheelSteps As Long, _
                            ByVal px As Long, ByVal py As Long)
    Dim nextValue As Long
    If wheelSteps = 0 Then Exit Sub

    If area = 1 Then
        If sourceBitmap = 0 Then Exit Sub
        ' In sus: crop mai mic = zoom mai mare.
        nextValue = zoomValue + wheelSteps * 20
        If nextValue < 100 Then nextValue = 100
        If nextValue > 600 Then nextValue = 600
        zoomValue = nextValue
        If Not frameLocked Then PlaceFrameAt px, py
        UpdateFrame
        DrawSource
        ConvertCurrent
    ElseIf area = 2 Then
        ' In sus: mai multi pixeli devin negri.
        nextValue = thresholdValue + wheelSteps * 8
        If nextValue < 16 Then nextValue = 16
        If nextValue > 240 Then nextValue = 240
        thresholdValue = nextValue
        RefreshControlLabels
        If sourceBitmap <> 0 Then ConvertCurrent
    End If
End Sub

Private Sub DrawSource()
    Dim gr As Long, s As Double, dw As Long, dh As Long
    Dim l As Long, t As Long, r As Long, b As Long
    Dim j As Long, px As Long, py As Long
    If sourceBitmap = 0 Then Exit Sub
    picSource.Cls
    s = (picSource.ScaleWidth - 8#) / imageW
    If (picSource.ScaleHeight - 8#) / imageH < s Then s = (picSource.ScaleHeight - 8#) / imageH
    previewScale = s
    previewLeft = (picSource.ScaleWidth - imageW * s) / 2#
    previewTop = (picSource.ScaleHeight - imageH * s) / 2#
    dw = CLng(imageW * s): dh = CLng(imageH * s)
    If GdipCreateFromHDC(picSource.hDC, gr) = 0 Then
        GdipDrawImageRectI gr, sourceBitmap, CLng(previewLeft), CLng(previewTop), dw, dh
        GdipDeleteGraphics gr
    End If
    l = CLng(previewLeft + (frameX - frameW / 2#) * previewScale)
    t = CLng(previewTop + (frameY - frameH / 2#) * previewScale)
    r = CLng(previewLeft + (frameX + frameW / 2#) * previewScale)
    b = CLng(previewTop + (frameY + frameH / 2#) * previewScale)
    ' Desenul de pe fotografia din stanga este legat de coordonatele originale.
    For j = 0 To sourceMarkCount - 1
        px = CLng(previewLeft + sourceMarks(j).x * previewScale)
        py = CLng(previewTop + sourceMarks(j).y * previewScale)
        picSource.Line (px - 1, py - 1)-(px + 1, py + 1), vbBlack, BF
    Next j
    picSource.Line (l - 1, t - 1)-(r + 1, b + 1), vbBlack, B
    If frameLocked Then
        picSource.Line (l, t)-(r, b), vbGreen, B
        picSource.Line (l + 1, t + 1)-(r - 1, b - 1), vbGreen, B
    Else
        picSource.Line (l, t)-(r, b), vbYellow, B
        picSource.Line (l + 1, t + 1)-(r - 1, b - 1), vbYellow, B
    End If
    picSource.Refresh
End Sub

Private Sub ConvertCurrent()
    Dim x As Long, y As Long, sx As Long, sy As Long
    Dim j As Long, outX As Long, outY As Long
    Dim fx As Double, fy As Double
    Dim errValue(0 To 83, 0 To 47) As Double
    Dim v As Double, errorPixel As Double, quantized As Double
    If sourceBitmap = 0 Then Exit Sub
    For y = 0 To H - 1
        For x = 0 To W - 1
            fx = frameX - frameW / 2# + (x + 0.5) * frameW / W
            fy = frameY - frameH / 2# + (y + 0.5) * frameH / H
            sx = CLng(Fix(fx)): sy = CLng(Fix(fy))
            If sx < 0 Then sx = 0
            If sy < 0 Then sy = 0
            If sx >= imageW Then sx = imageW - 1
            If sy >= imageH Then sy = imageH - 1
            gray(x, y) = SampleGray(sourceBitmap, sx, sy)
        Next x
    Next y
    If chkDither.Value = vbChecked Then
        For y = 0 To H - 1
            For x = 0 To W - 1
                v = gray(x, y) + errValue(x, y)
                If v < thresholdValue Then
                    pixels(x, y) = 1
                    quantized = 0
                Else
                    pixels(x, y) = 0
                    quantized = 255
                End If
                errorPixel = v - quantized
                If x < W - 1 Then errValue(x + 1, y) = errValue(x + 1, y) + errorPixel * 7 / 16
                If y < H - 1 Then
                    If x > 0 Then errValue(x - 1, y + 1) = errValue(x - 1, y + 1) + errorPixel * 3 / 16
                    errValue(x, y + 1) = errValue(x, y + 1) + errorPixel * 5 / 16
                    If x < W - 1 Then errValue(x + 1, y + 1) = errValue(x + 1, y + 1) + errorPixel / 16
                End If
            Next x
        Next y
    Else
        For y = 0 To H - 1
            For x = 0 To W - 1
                If gray(x, y) < thresholdValue Then pixels(x, y) = 1 Else pixels(x, y) = 0
            Next x
        Next y
    End If
    For y = 0 To H - 1
        For x = 0 To W - 1
            If invertResult Then pixels(x, y) = 1 - pixels(x, y)
        Next x
    Next y

    ' Suprapunem trasaturile desenate pe fotografia originala
    ' dupa contrast/dithering si inainte de retusurile de pe bitmap.
    For j = 0 To sourceMarkCount - 1
        fx = (sourceMarks(j).x - frameX + frameW / 2#) * W / frameW
        fy = (sourceMarks(j).y - frameY + frameH / 2#) * H / frameH
        If fx >= 0 And fy >= 0 And fx < W And fy < H Then
            outX = Fix(fx): outY = Fix(fy)
            pixels(outX, outY) = 1
        End If
    Next j
    For y = 0 To H - 1
        For x = 0 To W - 1
            If edits(x, y) Then pixels(x, y) = editValues(x, y)
        Next x
    Next y
    RedrawAll
End Sub

Private Function SampleGray(ByVal bitmap As Long, ByVal x As Long, ByVal y As Long) As Double
    Dim c As Long, r As Long, g As Long, b As Long, a As Long
    If GdipBitmapGetPixel(bitmap, x, y, c) <> 0 Then
        SampleGray = 255
        Exit Function
    End If
    r = (c And &HFF0000) \ &H10000
    g = (c And &HFF00&) \ &H100&
    b = (c And &HFF&)
    a = (c And &H7F000000) \ &H1000000
    If c < 0 Then a = a + 128
    SampleGray = (299# * r + 587# * g + 114# * b) / 1000#
    If a < 255 Then SampleGray = (a * SampleGray + (255 - a) * 255) / 255
End Function

Private Sub RedrawAll()
    Dim x As Long, y As Long
    picCanvas.Cls
    For y = 0 To H - 1
        For x = 0 To W - 1
            If pixels(x, y) <> 0 Then DrawPixel x, y
        Next x
    Next y
    picCanvas.Refresh
End Sub

Private Sub DrawPixel(ByVal x As Long, ByVal y As Long)
    Dim color As Long
    If pixels(x, y) <> 0 Then color = vbBlack Else color = picCanvas.BackColor
    picCanvas.Line (x * ZOOM, y * ZOOM)-((x + 1) * ZOOM - 1, (y + 1) * ZOOM - 1), color, BF
End Sub

Private Sub PaintAt(ByVal x As Long, ByVal y As Long)
    If x < 0 Or x >= W Or y < 0 Or y >= H Then Exit Sub
    pixels(x, y) = paintValue
    edits(x, y) = True
    editValues(x, y) = paintValue
    DrawPixel x, y
End Sub

Private Sub PaintStroke(ByVal x As Long, ByVal y As Long)
    Dim dx As Long, dy As Long, sx As Long, sy As Long
    Dim e As Long, e2 As Long, px As Long, py As Long
    px = lastX: py = lastY
    dx = Abs(x - px): dy = -Abs(y - py)
    sx = IIf(px < x, 1, -1): sy = IIf(py < y, 1, -1)
    e = dx + dy
    Do
        PaintAt px, py
        If px = x And py = y Then Exit Do
        e2 = 2 * e
        If e2 >= dy Then e = e + dy: px = px + sx
        If e2 <= dx Then e = e + dx: py = py + sy
    Loop
    lastX = x: lastY = y
    picCanvas.Refresh
End Sub

Private Sub picCanvas_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button <> vbLeftButton And Button <> vbRightButton Then Exit Sub
    drawing = True
    If Button = vbLeftButton Then paintValue = 1 Else paintValue = 0
    lastX = Fix(X / ZOOM): lastY = Fix(Y / ZOOM)
    PaintAt lastX, lastY
    picCanvas.Refresh
End Sub

Private Sub picCanvas_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If GetFocusHwnd() <> picCanvas.hWnd Then picCanvas.SetFocus
    If Not drawing Or Button = 0 Then Exit Sub
    PaintStroke Fix(X / ZOOM), Fix(Y / ZOOM)
End Sub

Private Sub picCanvas_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    drawing = False
End Sub

Private Sub cmdClear_Click()
    Dim x As Long, y As Long
    For y = 0 To H - 1
        For x = 0 To W - 1
            pixels(x, y) = 0
            edits(x, y) = True
            editValues(x, y) = 0
        Next x
    Next y
    RedrawAll
    lblStatus.Caption = "Ecran gol. Poti desena direct cu mouse-ul. Sterge retusurile pentru a restaura poza."
End Sub

Private Sub ClearEdits()
    Dim x As Long, y As Long
    For y = 0 To H - 1
        For x = 0 To W - 1
            edits(x, y) = False
        Next x
    Next y
End Sub

Private Sub cmdResetEdits_Click()
    ClearEdits
    sourceMarkCount = 0
    If sourceBitmap <> 0 Then
        DrawSource
        ConvertCurrent
    Else
        cmdClear_Click
    End If
End Sub

Private Sub cmdInvert_Click()
    Dim x As Long, y As Long
    invertResult = Not invertResult
    If sourceBitmap <> 0 Then
        ConvertCurrent
    Else
        For y = 0 To H - 1
            For x = 0 To W - 1
                pixels(x, y) = 1 - pixels(x, y)
            Next x
        Next y
        RedrawAll
    End If
End Sub

Private Sub chkDither_Click()
    If sourceBitmap <> 0 Then ConvertCurrent
End Sub

Private Function BitmapByte(ByVal n As Long) As Byte
    Dim bank As Long, xx As Long, bit As Long, value As Long
    bank = n \ W
    xx = n Mod W
    For bit = 0 To 7
        If pixels(xx, bank * 8 + bit) <> 0 Then value = value Or (2 ^ bit)
    Next bit
    BitmapByte = CByte(value)
End Function

Private Function Hex2(ByVal n As Long) As String
    Hex2 = Right$("0" & Hex$(n), 2)
End Function

Private Sub cmdHex_Click()
    Dim fname As String, f As Integer, n As Long
    fname = ChoosePath(True, "hex")
    If Len(fname) = 0 Then Exit Sub
    On Error GoTo failed
    f = FreeFile
    Open fname For Output As #f
    For n = 0 To 503
        Print #f, Hex2(BitmapByte(n))
    Next n
    Close #f
    lblStatus.Caption = "Export HEX: " & fname & " | exact 504 octeti, PCD8544 page order."
    Exit Sub
failed:
    On Error Resume Next
    Close #f
    MsgBox "Eroare export HEX: " & Err.Description, vbExclamation
End Sub

Private Sub cmdVerilog_Click()
    Dim fname As String
    fname = ChoosePath(True, "v")
    If Len(fname) = 0 Then Exit Sub
    WriteStandaloneSource fname
End Sub

Private Sub WriteStandaloneSource(ByVal fname As String)
    Dim f As Integer, n As Long, line As String
    On Error GoTo failed
    f = FreeFile
    Open fname For Output As #f
    Print #f, "// Generated by Nokia 5110 Bitmap Studio (VB6)."
    Print #f, "// 84x48 monochrome bitmap embedded: no external .hex required."
    Print #f, "module LCD5110 ("
    Print #f, "  input wire clk,"
    Print #f, "  output reg lcd_rst=0, lcd_ce=1, lcd_dc=0, lcd_din=0, lcd_clk=0"
    Print #f, ");"
    Print #f, "  reg [7:0] img [0:503];"
    Print #f, "  initial begin"
    For n = 0 To 503
        If n Mod 8 = 0 Then line = "    "
        line = line & "img[" & CStr(n) & "]=8'h" & Hex2(BitmapByte(n)) & "; "
        If n Mod 8 = 7 Or n = 503 Then Print #f, line
    Next n
    Print #f, "  end"
    Print #f, "  function [7:0] cmd;"
    Print #f, "    input [2:0] n;"
    Print #f, "    begin case(n)"
    Print #f, "      0:cmd=8'h21; 1:cmd=8'hB8; 2:cmd=8'h04; 3:cmd=8'h14;"
    Print #f, "      4:cmd=8'h20; 5:cmd=8'h0C; 6:cmd=8'h80; 7:cmd=8'h40;"
    Print #f, "      default:cmd=8'h00;"
    Print #f, "    endcase end"
    Print #f, "  endfunction"
    Print #f, "  reg [21:0] startup=0;"
    Print #f, "  reg [6:0] divcount=0;"
    Print #f, "  reg [8:0] index=0;"
    Print #f, "  reg [7:0] shifter=0;"
    Print #f, "  reg [2:0] bit_no=7;"
    Print #f, "  reg [1:0] state=0;"
    Print #f, "  reg done=0;"
    Print #f, "  wire [7:0] data_byte=(index<9'd8)?cmd(index[2:0]):img[index-9'd8];"
    Print #f, "  always @(posedge clk) begin"
    Print #f, "    if(startup<22'd2500000) begin"
    Print #f, "      startup<=startup+1'b1;"
    Print #f, "      if(startup==22'd1000000) lcd_rst<=1'b1;"
    Print #f, "    end else if(!done) begin"
    Print #f, "      if(divcount==7'd99) begin"
    Print #f, "        divcount<=0;"
    Print #f, "        case(state)"
    Print #f, "          0: begin"
    Print #f, "            lcd_ce<=0; lcd_dc<=(index>=9'd8); lcd_clk<=0;"
    Print #f, "            lcd_din<=data_byte[7]; shifter<=data_byte;"
    Print #f, "            bit_no<=7; state<=1;"
    Print #f, "          end"
    Print #f, "          1: begin lcd_clk<=1; state<=2; end"
    Print #f, "          2: begin"
    Print #f, "            lcd_clk<=0;"
    Print #f, "            if(bit_no==0) state<=3;"
    Print #f, "            else begin"
    Print #f, "              bit_no<=bit_no-1'b1; lcd_din<=shifter[6];"
    Print #f, "              shifter<={shifter[6:0],1'b0}; state<=1;"
    Print #f, "            end"
    Print #f, "          end"
    Print #f, "          3: begin"
    Print #f, "            lcd_ce<=1;"
    Print #f, "            if(index==9'd511) done<=1;"
    Print #f, "            else index<=index+1'b1;"
    Print #f, "            state<=0;"
    Print #f, "          end"
    Print #f, "        endcase"
    Print #f, "      end else divcount<=divcount+1'b1;"
    Print #f, "    end"
    Print #f, "  end"
    Print #f, "endmodule"
    Close #f
    lblStatus.Caption = "Sursa generata: " & fname & " | 504 octeti integrati in Verilog; NU necesita .hex."
    MsgBox "Sursa Verilog generata cu imaginea curenta incorporata!" & vbCrLf & _
        "Adauga doar fisierul .v in Quartus. Nu mai este necesar .hex.", vbInformation
    Exit Sub
failed:
    On Error Resume Next
    Close #f
    MsgBox "Eroare export Verilog: " & Err.Description, vbExclamation
End Sub
