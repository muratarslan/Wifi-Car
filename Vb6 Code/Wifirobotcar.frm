VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Begin VB.Form Form1 
   BackColor       =   &H00404040&
   Caption         =   "Wifi Car v1.0"
   ClientHeight    =   10350
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   20250
   Enabled         =   0   'False
   FillColor       =   &H00FFFFFF&
   Icon            =   "Wifirobotcar.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   25915.49
   ScaleMode       =   0  'User
   ScaleWidth      =   20250
   WhatsThisHelp   =   -1  'True
   Begin MSWinsockLib.Winsock Winsock1 
      Left            =   720
      Top             =   8160
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.CommandButton Command6 
      Caption         =   "Ekran Görüntüsü"
      Height          =   495
      Left            =   4440
      TabIndex        =   12
      Top             =   7320
      Width           =   1215
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Kaydet"
      Height          =   495
      Left            =   1800
      TabIndex        =   11
      Top             =   7320
      Width           =   1215
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Durdur"
      Height          =   495
      Left            =   3120
      TabIndex        =   10
      Top             =   7320
      Width           =   1215
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Baðlan"
      Height          =   495
      Left            =   480
      TabIndex        =   9
      Top             =   7320
      Width           =   1215
   End
   Begin VB.Frame Video 
      BackColor       =   &H80000015&
      Caption         =   "Video"
      Height          =   7575
      Left            =   360
      TabIndex        =   8
      Top             =   360
      Width           =   19575
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Camera (K)"
      Height          =   495
      Left            =   360
      TabIndex        =   7
      Top             =   9720
      Width           =   1335
   End
   Begin VB.Timer call_timer 
      Left            =   12120
      Top             =   9600
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Connect (T)"
      Height          =   495
      Left            =   360
      TabIndex        =   5
      Top             =   9000
      Width           =   1335
   End
   Begin VB.TextBox lbladdress 
      Alignment       =   2  'Center
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   162
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   360
      TabIndex        =   4
      Text            =   "192.168.1.1"
      Top             =   8040
      Width           =   1335
   End
   Begin VB.CommandButton cmd_right 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Wifirobotcar.frx":F172
      Height          =   855
      Left            =   11160
      MaskColor       =   &H00808080&
      Picture         =   "Wifirobotcar.frx":11164
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   9240
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.CommandButton cmd_left 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Wifirobotcar.frx":13156
      Height          =   855
      Left            =   9240
      MaskColor       =   &H00808080&
      Picture         =   "Wifirobotcar.frx":15148
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   9240
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.CommandButton cmd_up 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Wifirobotcar.frx":1713A
      Height          =   855
      Left            =   10200
      MaskColor       =   &H00808080&
      Picture         =   "Wifirobotcar.frx":1912C
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   8280
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.CommandButton cmd_down 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Wifirobotcar.frx":1B11E
      Height          =   855
      Left            =   10200
      MaskColor       =   &H00808080&
      Picture         =   "Wifirobotcar.frx":1D110
      Style           =   1  'Graphical
      TabIndex        =   0
      Top             =   9240
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.Label Label5 
      Caption         =   "Label5"
      Height          =   255
      Left            =   360
      TabIndex        =   6
      Top             =   8520
      Width           =   1335
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public direction As String
Public ttime As Integer
Public address As String
Public upstatus, downstatus, leftstatus, rightstatus, hornstatus As Integer
Public output As Integer

Private Sub call_timer_Timer()
    Call motion("manual", upstatus + downstatus + rightstatus + leftstatus + hornstatus)
End Sub

Private Sub Command1_Click()
  If Winsock1.State = sckClosed Then
    Winsock1.RemoteHost = "192.168.1.1"
    Winsock1.RemotePort = "1500"
    Label5.Caption = "Connected"
    Winsock1.Connect
  Else
    Winsock1.Close
    Label5.Caption = "Not Connected"
  End If
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = vbKeyEscape Then End
    If KeyCode = vbKeyT Then Call Command1_Click
    If KeyCode = vbKeyUp Or KeyCode = vbKeyW Then: cmd_up.BackColor = &HFF0000: cmd_up.Picture = cmd_up.DownPicture: upstatus = 8
    If KeyCode = vbKeyDown Or KeyCode = vbKeyS Then cmd_down.BackColor = &HFF0000: cmd_down.Picture = cmd_down.DownPicture: downstatus = 32
    If KeyCode = vbKeyLeft Or KeyCode = vbKeyA Then cmd_left.BackColor = &HFF0000: cmd_left.Picture = cmd_left.DownPicture: rightstatus = 4
    If KeyCode = vbKeyRight Or KeyCode = vbKeyD Then cmd_right.BackColor = &HFF0000: cmd_right.Picture = cmd_right.DownPicture: leftstatus = 2
    If KeyCode = vbKeyH Then hornstatus = 32
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    If KeyCode = vbKeyUp Or KeyCode = vbKeyW Then cmd_up.BackColor = &HFFFFFF: cmd_up.Picture = cmd_up.DisabledPicture: upstatus = 0
    If KeyCode = vbKeyDown Or KeyCode = vbKeyS Then cmd_down.BackColor = &HFFFFFF: cmd_down.Picture = cmd_down.DisabledPicture: downstatus = 0
    If KeyCode = vbKeyLeft Or KeyCode = vbKeyA Then cmd_left.BackColor = &HFFFFFF: cmd_left.Picture = cmd_left.DisabledPicture: rightstatus = 0
    If KeyCode = vbKeyRight Or KeyCode = vbKeyD Then cmd_right.BackColor = &HFFFFFF: cmd_right.Picture = cmd_right.DisabledPicture: leftstatus = 0
    If KeyCode = vbKeyH Then hornstatus = 0
End Sub

Private Sub Form_Load()
    Dim line As String
    cmd_up.DisabledPicture = cmd_up.Picture
    cmd_down.DisabledPicture = cmd_down.Picture
    cmd_left.DisabledPicture = cmd_left.Picture
    cmd_right.DisabledPicture = cmd_right.Picture
    lbladdress.Text = "192.168.1.1"
    call_timer.Interval = 5
End Sub

Private Sub sock_Close()
    Winsock1.Close
End Sub
  
Private Sub sock_Error(ByVal Number As Integer, Description As String, ByVal Scode As Long, ByVal Source As String, ByVal HelpFile As String, ByVal HelpContext As Long, CancelDisplay As Boolean)
    MsgBox "Socket Error " & Number & ": " & Description
    Winsock1.Close
End Sub

Sub motion(direction As String, ttime As Integer)
    address = Form1.address
    Select Case direction
    Case 1 To 10
    Case "manual"
    output = ttime
    Case "FF"
        output = 1
    Case "BB"
        output = 2
    Case "RR"
        output = 8
    Case "LL"
        output = 4
    Case "FR"
        output = 9
    Case "FL"
        output = 5
    Case "BR"
        output = 10
    Case "BL"
        output = 6
    Case "SS"
        output = 0
    End Select
    If Winsock1.State = sckConnected Then
     If output = &HC Or output = &H1C Then output = output - 8
     If output = &H3 Or output = &H13 Then output = output - 2
     Winsock1.SendData Chr(output + 1) & Chr(0)
      Label5.Caption = Chr(output + 1) & Chr(0)
    Else
        Label5.Caption = "Disconnected"
    End If
End Sub
