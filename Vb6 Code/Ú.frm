VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00404040&
   Caption         =   "Wifi Araba v1.0"
   ClientHeight    =   10350
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   20250
   Enabled         =   0   'False
   FillColor       =   &H00FFFFFF&
   Icon            =   "Ú.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   25915.49
   ScaleMode       =   0  'User
   ScaleWidth      =   20250
   WhatsThisHelp   =   -1  'True
   Begin VB.CommandButton Command6 
      Caption         =   "Ekran Görüntüsü"
      Height          =   495
      Left            =   4440
      TabIndex        =   18
      Top             =   7320
      Width           =   1215
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Kaydet"
      Height          =   495
      Left            =   1800
      TabIndex        =   17
      Top             =   7320
      Width           =   1215
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Durdur"
      Height          =   495
      Left            =   3120
      TabIndex        =   16
      Top             =   7320
      Width           =   1215
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Baðlan"
      Height          =   495
      Left            =   480
      TabIndex        =   15
      Top             =   7320
      Width           =   1215
   End
   Begin VB.Frame Video 
      BackColor       =   &H80000015&
      Caption         =   "Video"
      Height          =   7575
      Left            =   360
      TabIndex        =   14
      Top             =   360
      Width           =   19575
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Kemara"
      Height          =   495
      Left            =   12840
      TabIndex        =   12
      Top             =   9600
      Width           =   1215
   End
   Begin VB.Timer call_timer 
      Left            =   12120
      Top             =   9600
   End
   Begin VB.CommandButton Command1 
      Caption         =   "CON"
      Height          =   495
      Left            =   12960
      TabIndex        =   7
      Top             =   8880
      Width           =   855
   End
   Begin VB.PictureBox sock 
      Height          =   480
      Left            =   12120
      ScaleHeight     =   420
      ScaleWidth      =   1140
      TabIndex        =   19
      Top             =   10080
      Width           =   1200
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
      Left            =   5160
      TabIndex        =   5
      Text            =   "192.168.1.1"
      Top             =   9840
      Width           =   1335
   End
   Begin VB.CommandButton cmd_right 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Ú.frx":F172
      Height          =   855
      Left            =   11160
      MaskColor       =   &H00808080&
      Picture         =   "Ú.frx":11164
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   9240
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.CommandButton cmd_left 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Ú.frx":13156
      Height          =   855
      Left            =   9240
      MaskColor       =   &H00808080&
      Picture         =   "Ú.frx":15148
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   9240
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.CommandButton cmd_up 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Ú.frx":1713A
      Height          =   855
      Left            =   10200
      MaskColor       =   &H00808080&
      Picture         =   "Ú.frx":1912C
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   8280
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.CommandButton cmd_down 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      DownPicture     =   "Ú.frx":1B11E
      Height          =   855
      Left            =   10200
      MaskColor       =   &H00808080&
      Picture         =   "Ú.frx":1D110
      Style           =   1  'Graphical
      TabIndex        =   0
      Top             =   9240
      UseMaskColor    =   -1  'True
      Width           =   855
   End
   Begin VB.Label Label4 
      BackColor       =   &H80000010&
      Caption         =   "T: connect"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   162
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   240
      Left            =   5280
      TabIndex        =   13
      Top             =   9000
      Width           =   1095
   End
   Begin VB.Label Label2 
      BackColor       =   &H80000010&
      Caption         =   "see config.txt"
      ForeColor       =   &H8000000E&
      Height          =   240
      Left            =   14400
      TabIndex        =   11
      Top             =   9960
      Width           =   1095
   End
   Begin VB.Label Label7 
      BackColor       =   &H80000010&
      Caption         =   "T: connect"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   162
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   240
      Left            =   14280
      TabIndex        =   10
      Top             =   9360
      Width           =   1095
   End
   Begin VB.Label Label6 
      BackColor       =   &H80000010&
      Caption         =   "H: horn"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   162
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   240
      Left            =   14280
      TabIndex        =   9
      Top             =   8880
      Width           =   1095
   End
   Begin VB.Label Label5 
      Caption         =   "Label5"
      Height          =   735
      Left            =   7320
      TabIndex        =   8
      Top             =   8880
      Width           =   975
   End
   Begin VB.Label Label3 
      BackColor       =   &H80000010&
      Caption         =   "Esc = Exit"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   162
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   360
      Left            =   7200
      TabIndex        =   6
      Top             =   9720
      Width           =   1095
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000010&
      Caption         =   "Address:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   162
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000E&
      Height          =   360
      Left            =   5280
      TabIndex        =   4
      Top             =   9360
      Width           =   1095
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



Private Sub Command1_Click()
  If sock.State = sckClosed Then ' if the socket is closed
    sock.RemoteHost = lbladdress.Text ' set server adress
    sock.Protocol = sckTCPProtocol
    sock.RemotePort = "1500" ' set server port
    Label5.Caption = "Connected"
    sock.Connect ' start connection attempt
  Else ' if the socket is open
    sock.Close ' close it
    Label5.Caption = "Not Connected"
  End If
End Sub


Private Sub Command2_Click()
Shell ("C:\Program Files\DVD Maker\DVDMaker.exe")
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    'Detects if control keys are pressed
    If KeyCode = vbKeyEscape Then End 'Exits
    If KeyCode = vbKeyT Then Call Command1_Click
    If KeyCode = vbKeyK Then Call Command2_Click
    If KeyCode = vbKeyUp Or KeyCode = vbKeyW Then: cmd_up.BackColor = &HFF0000: cmd_up.Picture = cmd_up.DownPicture: upstatus = 1
    'If up arrow is pressed or W then change pictures (make it blue) and change upstatus.
    If KeyCode = vbKeyDown Or KeyCode = vbKeyS Then cmd_down.BackColor = &HFF0000: cmd_down.Picture = cmd_down.DownPicture: downstatus = 2
    'If down arrow is pressed or S then change pictures (make it blue) and change downstatus.
    If KeyCode = vbKeyLeft Or KeyCode = vbKeyA Then cmd_left.BackColor = &HFF0000: cmd_left.Picture = cmd_left.DownPicture: rightstatus = 4
    'If left arrow is pressed or A then change pictures (make it blue) and change rightstatus.
    If KeyCode = vbKeyRight Or KeyCode = vbKeyD Then cmd_right.BackColor = &HFF0000: cmd_right.Picture = cmd_right.DownPicture: leftstatus = 8
    'If right arrow is pressed or D then change pictures (make it blue) and change leftstatus.
    If KeyCode = vbKeyH Then hornstatus = 32
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    'Stops output to that direction when key is lifted.
    'Changes pictures back to unactivated (none-blue).
    If KeyCode = vbKeyUp Or KeyCode = vbKeyW Then cmd_up.BackColor = &HFFFFFF: cmd_up.Picture = cmd_up.DisabledPicture: upstatus = 0
    If KeyCode = vbKeyDown Or KeyCode = vbKeyS Then cmd_down.BackColor = &HFFFFFF: cmd_down.Picture = cmd_down.DisabledPicture: downstatus = 0
    If KeyCode = vbKeyLeft Or KeyCode = vbKeyA Then cmd_left.BackColor = &HFFFFFF: cmd_left.Picture = cmd_left.DisabledPicture: rightstatus = 0
    If KeyCode = vbKeyRight Or KeyCode = vbKeyD Then cmd_right.BackColor = &HFFFFFF: cmd_right.Picture = cmd_right.DisabledPicture: leftstatus = 0
    If KeyCode = vbKeyH Then hornstatus = 0
End Sub

Private Sub Form_Load()
    Dim line As String
    'Stores {Arrow with white} pic in .DisabledPicture for each button
    cmd_up.DisabledPicture = cmd_up.Picture
    cmd_down.DisabledPicture = cmd_down.Picture
    cmd_left.DisabledPicture = cmd_left.Picture
    cmd_right.DisabledPicture = cmd_right.Picture
    'Following Opens Config.txt and collect Parallel Port Address and Refresh Rate
   ' Open CurDir & "\config.txt" For Input As #1
    'Line Input #1, line
    'Line Input #1, line
    'address = line 'Sets parallel port address
    lbladdress.Text = "192.168.1.1"
    call_timer.Interval = 5
    
End Sub
Private Sub sock_Close()
    sock.Close ' has to be
End Sub
  
Private Sub sock_Error(ByVal Number As Integer, Description As String, ByVal Scode As Long, ByVal Source As String, ByVal HelpFile As String, ByVal HelpContext As Long, CancelDisplay As Boolean)
    MsgBox "Socket Error " & Number & ": " & Description
    sock.Close ' close the erraneous connection
End Sub


