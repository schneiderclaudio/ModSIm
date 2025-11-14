VERSION 5.00
Begin VB.Form OutFormat 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify the format for the output file"
   ClientHeight    =   5895
   ClientLeft      =   645
   ClientTop       =   1815
   ClientWidth     =   10170
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "Outformat.frx":0000
   LinkMode        =   1  'Source
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5895
   ScaleWidth      =   10170
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for metal content"
      Height          =   975
      Left            =   8160
      TabIndex        =   79
      Top             =   1440
      Width           =   1935
      Begin VB.OptionButton optMetalUnits 
         BackColor       =   &H00C0C0C0&
         Caption         =   "g/t"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   81
         Tag             =   "MetalUnits"
         Top             =   600
         Width           =   735
      End
      Begin VB.OptionButton optMetalUnits 
         BackColor       =   &H00C0C0C0&
         Caption         =   "%"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   80
         Tag             =   "MetalUnits"
         Top             =   240
         Value           =   -1  'True
         Width           =   735
      End
   End
   Begin VB.CheckBox ChkAccumulate 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accumulate data from selected streams"
      Enabled         =   0   'False
      Height          =   255
      Left            =   120
      TabIndex        =   76
      Tag             =   "Accumulate"
      Top             =   3000
      Width           =   4095
   End
   Begin VB.ListBox LstSelected 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Enabled         =   0   'False
      Height          =   1500
      Index           =   1
      Left            =   3840
      TabIndex        =   75
      ToolTipText     =   "Double click to remove"
      Top             =   3720
      Width           =   615
   End
   Begin VB.CheckBox ChkCoalFlag 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Use format for coal data"
      Height          =   255
      Left            =   120
      TabIndex        =   74
      Tag             =   "CoalFlag"
      Top             =   5280
      Width           =   2175
   End
   Begin VB.CheckBox ChkLatestData 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Display latest output data"
      Height          =   255
      Left            =   3000
      TabIndex        =   72
      Top             =   5280
      Width           =   2535
   End
   Begin VB.TextBox TxtMetalName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   285
      Index           =   3
      Left            =   7800
      TabIndex        =   43
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtMetalName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   285
      Index           =   2
      Left            =   7080
      TabIndex        =   35
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtMetalName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   285
      Index           =   1
      Left            =   6360
      TabIndex        =   27
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtMetalName 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   285
      Index           =   0
      Left            =   5640
      TabIndex        =   19
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   7800
      TabIndex        =   50
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   7800
      TabIndex        =   49
      Top             =   4560
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   7800
      TabIndex        =   48
      Top             =   4320
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   7800
      TabIndex        =   47
      Top             =   4080
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   7800
      TabIndex        =   46
      Top             =   3840
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   7800
      TabIndex        =   45
      Top             =   3600
      Width           =   615
   End
   Begin VB.TextBox TxtMetal4 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   7800
      TabIndex        =   44
      Top             =   3360
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   7080
      TabIndex        =   42
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   7080
      TabIndex        =   41
      Top             =   4560
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   7080
      TabIndex        =   40
      Top             =   4320
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   7080
      TabIndex        =   39
      Top             =   4080
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   7080
      TabIndex        =   38
      Top             =   3840
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   7080
      TabIndex        =   37
      Top             =   3600
      Width           =   615
   End
   Begin VB.TextBox TxtMetal3 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   7080
      TabIndex        =   36
      Top             =   3360
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   6360
      TabIndex        =   34
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   6360
      TabIndex        =   32
      Top             =   4320
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   6360
      TabIndex        =   33
      Top             =   4560
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   6360
      TabIndex        =   31
      Top             =   4080
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   6360
      TabIndex        =   30
      Top             =   3840
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   6360
      TabIndex        =   29
      Top             =   3600
      Width           =   615
   End
   Begin VB.TextBox TxtMetal2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   6360
      TabIndex        =   28
      Top             =   3360
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   5640
      TabIndex        =   26
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   5640
      TabIndex        =   25
      Top             =   4560
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   5640
      TabIndex        =   24
      Top             =   4320
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   5640
      TabIndex        =   23
      Top             =   4080
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   5640
      TabIndex        =   22
      Top             =   3840
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   5640
      TabIndex        =   21
      Top             =   3600
      Width           =   615
   End
   Begin VB.TextBox TxtMetal1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   5640
      TabIndex        =   20
      Top             =   3360
      Width           =   615
   End
   Begin VB.ListBox LstSelected 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Enabled         =   0   'False
      Height          =   1500
      Index           =   0
      Left            =   3000
      TabIndex        =   18
      ToolTipText     =   "Double click to remove astream"
      Top             =   3720
      Width           =   615
   End
   Begin VB.ListBox LstStreamList 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Enabled         =   0   'False
      Height          =   1500
      Left            =   120
      TabIndex        =   17
      ToolTipText     =   "Double click to select"
      Top             =   3720
      Width           =   2775
   End
   Begin VB.CheckBox ChkSizeDistributions 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Display size distributions of selected streams"
      Height          =   255
      Left            =   120
      TabIndex        =   16
      Tag             =   "Size"
      Top             =   2640
      Width           =   4335
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for water flowrate"
      Height          =   975
      Left            =   4800
      TabIndex        =   59
      Top             =   1440
      Width           =   3255
      Begin VB.OptionButton OptWaterFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "liters/min"
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   73
         Tag             =   "WaterFl"
         Top             =   600
         Width           =   1215
      End
      Begin VB.OptionButton OptWaterFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "gal/min"
         Height          =   255
         Index           =   4
         Left            =   1320
         TabIndex        =   15
         Tag             =   "WaterFl"
         Top             =   600
         Width           =   975
      End
      Begin VB.OptionButton OptWaterFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cub m/hr"
         Height          =   255
         Index           =   2
         Left            =   2160
         TabIndex        =   14
         Tag             =   "WaterFl"
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton OptWaterFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "tonnes/hr"
         Height          =   255
         Index           =   1
         Left            =   960
         TabIndex        =   13
         Tag             =   "WaterFl"
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton OptWaterFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kg/s"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   12
         Tag             =   "WaterFl"
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for solid flowrate"
      Height          =   975
      Left            =   120
      TabIndex        =   58
      Top             =   1440
      Width           =   4575
      Begin VB.OptionButton OptSolFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "short tons/day"
         Height          =   255
         Index           =   5
         Left            =   3000
         TabIndex        =   11
         Tag             =   "SolFl"
         Top             =   600
         Width           =   1455
      End
      Begin VB.OptionButton OptSolFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "short tons/hr"
         Height          =   255
         Index           =   4
         Left            =   1560
         TabIndex        =   10
         Tag             =   "SolFl"
         Top             =   600
         Width           =   1335
      End
      Begin VB.OptionButton OptSolFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mtonnes/year"
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   9
         Tag             =   "SolFl"
         Top             =   600
         Width           =   1335
      End
      Begin VB.OptionButton OptSolFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "ktonnes/month"
         Height          =   255
         Index           =   2
         Left            =   2760
         TabIndex        =   8
         Tag             =   "SolFl"
         Top             =   240
         Width           =   1455
      End
      Begin VB.OptionButton OptSolFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "tonnes/hr"
         Height          =   255
         Index           =   1
         Left            =   1200
         TabIndex        =   7
         Tag             =   "SolFl"
         Top             =   240
         Width           =   1215
      End
      Begin VB.OptionButton OptSolFl 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kg/s"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   6
         Tag             =   "SolFl"
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select quatities to display"
      Height          =   975
      Left            =   120
      TabIndex        =   57
      Top             =   360
      Width           =   6975
      Begin VB.CheckBox ChkQuantity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Recovery and grade of individual metals"
         Height          =   255
         Index           =   5
         Left            =   3480
         TabIndex        =   5
         Tag             =   "Quantity"
         Top             =   600
         Width           =   3255
      End
      Begin VB.CheckBox ChkQuantity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Recovery and grade of individual minerals"
         Height          =   255
         Index           =   4
         Left            =   3480
         TabIndex        =   2
         Tag             =   "Quantity"
         Top             =   240
         Width           =   3375
      End
      Begin VB.CheckBox ChkQuantity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Yield of total solids"
         Height          =   255
         Index           =   3
         Left            =   1680
         TabIndex        =   4
         Tag             =   "Quantity"
         Top             =   600
         Width           =   1695
      End
      Begin VB.CheckBox ChkQuantity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "% solids"
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   3
         Tag             =   "Quantity"
         Top             =   600
         Width           =   975
      End
      Begin VB.CheckBox ChkQuantity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Water flowrate"
         Height          =   255
         Index           =   1
         Left            =   1680
         TabIndex        =   1
         Tag             =   "Quantity"
         Top             =   240
         Width           =   1575
      End
      Begin VB.CheckBox ChkQuantity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Solids flowrate"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   0
         Tag             =   "Quantity"
         Top             =   240
         Width           =   1455
      End
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   375
      Left            =   7200
      TabIndex        =   54
      Top             =   5160
      Width           =   1215
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   375
      Left            =   5880
      TabIndex        =   55
      Top             =   5160
      Width           =   1095
   End
   Begin VB.Frame Frame8 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   8880
      TabIndex        =   56
      Top             =   240
      Width           =   1215
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   53
         Tag             =   "Default"
         Top             =   720
         Value           =   -1  'True
         Width           =   975
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   52
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   51
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.Label LblAccumulate 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accumulate"
      Enabled         =   0   'False
      Height          =   255
      Left            =   3720
      TabIndex        =   78
      Top             =   3480
      Width           =   855
   End
   Begin VB.Label LblSize 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size"
      Enabled         =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   77
      Top             =   3480
      Width           =   615
   End
   Begin VB.Label Instruction 
      BackColor       =   &H00C0FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   71
      Top             =   120
      Visible         =   0   'False
      Width           =   8415
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   6
      Left            =   4680
      TabIndex        =   70
      Top             =   4800
      Width           =   900
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   5
      Left            =   4680
      TabIndex        =   69
      Top             =   4560
      Width           =   900
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   4
      Left            =   4680
      TabIndex        =   68
      Top             =   4320
      Width           =   900
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   3
      Left            =   4680
      TabIndex        =   67
      Top             =   4080
      Width           =   900
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   2
      Left            =   4680
      TabIndex        =   66
      Top             =   3840
      Width           =   900
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   1
      Left            =   4680
      TabIndex        =   65
      Top             =   3600
      Width           =   900
   End
   Begin VB.Label LblMineralName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   225
      Index           =   0
      Left            =   4680
      TabIndex        =   64
      Top             =   3360
      Width           =   900
   End
   Begin VB.Label LblMetal 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Metals or elements"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5640
      TabIndex        =   63
      Top             =   2520
      Width           =   1935
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Minerals"
      Height          =   255
      Left            =   4680
      TabIndex        =   62
      Top             =   3120
      Width           =   900
   End
   Begin VB.Label LblSelected 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Selected streams"
      Enabled         =   0   'False
      Height          =   255
      Left            =   3000
      TabIndex        =   61
      Top             =   3240
      Width           =   1575
   End
   Begin VB.Label LblStreamList 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Double click to select streams"
      Enabled         =   0   'False
      Height          =   255
      Left            =   120
      TabIndex        =   60
      Top             =   3480
      Width           =   2655
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "OutFormat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private loadingForm As Boolean
Private CurrentFormat As OutFileFormat
Private NEWDATA As OutFileFormat
Private CumOut As Long

Private CHANGE_FLAG As Integer
Private FormFile As Integer
Private Flags As String * 9

  Private JOBNAMEVAR As String
  Private Nstream As Integer
  Private Stream As Integer
  Private StreamName As String
  Private STREAM_ID As Integer
  Private StreamType As String
  Private From_unit As Integer
  Private To_unit As Integer
  Private NumberMetals As Integer
  Private MetalName As String * 8
  Private NumberMinerals As Integer
  Private MinMetal As Single

Private Sub ChkAccumulate_Click()
'********************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "Accumulate" Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      If ChkAccumulate.Value = vbChecked Then
        LblAccumulate.Enabled = True
        LblSelected.Enabled = True
        LblStreamList.Enabled = True
        LstStreamList.Enabled = True
        LstSelected(1).Enabled = True
      ElseIf ChkSizeDistributions.Value = vbUnchecked Then
        LblAccumulate.Enabled = False
        LblSelected.Enabled = False
        LblStreamList.Enabled = False
        LstStreamList.Enabled = False
        LstSelected(1).Enabled = False
      Else
        LblAccumulate.Enabled = False
        LstSelected(1).Enabled = False
      End If
    End If
  End If
End Sub


Private Sub ChkAccumulate_GotFocus()
'***********************************
  Instruction.Caption = "Check to select streams to accumulate output data for consecutive simulations"
  Instruction.Visible = True
End Sub

Private Sub ChkAccumulate_LostFocus()
'************************************
  Instruction.Visible = False
End Sub

Private Sub ChkCoalFlag_Click()
'*************************
If Not loadingForm Then
  If Screen.ActiveControl.Tag = "CoalFlag" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End If
End Sub

Private Sub ChkCoalFlag_LostFocus()
'**********************************
If CHANGE_FLAG Then
  Call MakeNew(NEWDATA)
End If
End Sub



Private Sub ChkQuantity_Click(Index As Integer)
'**********************************************
  If Not loadingForm Then
    If TypeOf Screen.ActiveControl Is CheckBox Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
  If Index = 1 Then
    If ChkQuantity(1).Value = 1 Then
      Frame3.Enabled = True
    Else
      Frame3.Enabled = False
    End If
  End If
End Sub


Private Sub ChkQuantity_LostFocus(Index As Integer)
'**************************************************
If CHANGE_FLAG Then
   Call MakeNew(NEWDATA)
End If
End Sub


Private Sub ChkSizeDistributions_Click()
'***************************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "Size" Then
      If OptNew.Value = 0 Then OptNew.Value = -1
        If ChkSizeDistributions.Value = vbChecked Then
          LblSize.Enabled = True
          LblSelected.Enabled = True
          LblStreamList.Enabled = True
          LstStreamList.Enabled = True
          LstSelected(0).Enabled = True
        ElseIf ChkAccumulate.Value = vbUnchecked Then
          LblSize.Enabled = False
          LblSelected.Enabled = False
          LblStreamList.Enabled = False
          LstStreamList.Enabled = False
          LstSelected(0).Enabled = False
        Else
          LblSize.Enabled = False
          LstSelected(0).Enabled = False
        End If
    End If
  End If
End Sub

Private Sub ChkSizeDistributions_GotFocus()
'******************************************
  Instruction.Caption = "Check if you want particle size distributions displayed in the output"
  Instruction.Visible = True
End Sub


Private Sub ChkSizeDistributions_LostFocus()
'*******************************************
 If CHANGE_FLAG Then
   Call MakeNew(NEWDATA)
End If
Instruction.Visible = False
End Sub

Private Sub CmdAccept_Click()
'*****************************
  'Write the formats to FORMAT.OUT for future use.
  Dim I As Integer, M As Integer, CumOut As Long
  Dim ICODE As Integer, resp As Integer
  
  On Error GoTo ErrHandler
  
  FormFile = FreeFile
  Open RTrim(JobPath) + "FORMAT.OUT" For Output As #FormFile
  Mid(Flags, 1, 1) = CStr(NEWDATA.SolidUnits)
  If NEWDATA.Quantity(2) = vbChecked Then
    Mid(Flags, 2, 1) = "T"
  Else
    Mid(Flags, 2, 1) = "F"
  End If
  Mid(Flags, 3, 1) = CStr(NEWDATA.WaterUnits)
  For I = 4 To 7
    If NEWDATA.Quantity(I - 1) = vbChecked Then
      Mid(Flags, I, 1) = "T"
    Else
      Mid(Flags, I, 1) = "F"
    End If
  Next I
  Mid(Flags, 8, 1) = CStr(NEWDATA.MetalUnits)
  If ChkCoalFlag.Value = 1 Then
    Mid(Flags, 9, 1) = "T"
  Else
    Mid(Flags, 9, 1) = "F"
  End If
  
  Print #FormFile, Flags
  
  'Output information on metals and elements
  NumberMetals = 0
  For I = 1 To 4
    If TxtMetalName(I - 1).text <> "" Then
      NumberMetals = NumberMetals + 1
    End If
  Next I
  Print #FormFile, NumberMetals
  If NumberMetals > 0 Then
    For I = 1 To NumberMetals
      MetalName = Trim(TxtMetalName(I - 1).text) & " "
      Print #FormFile, MetalName;
    Next I
    Print #FormFile,
    Print #FormFile, Sysdata.NoMin.text
    For M = 1 To Val(Sysdata.NoMin.text)
      For I = 1 To NumberMetals
        If I = 1 Then Print #FormFile, TxtMetal1(M - 1).text & " ";
        If I = 2 Then Print #FormFile, TxtMetal2(M - 1).text & " ";
        If I = 3 Then Print #FormFile, TxtMetal3(M - 1).text & " ";
        If I = 4 Then Print #FormFile, TxtMetal4(M - 1).text & " ";
      Next I
      Print #FormFile,
    Next M
  End If
  
  'Ouput information on required size distributions
  If Val(LstSelected(0).ListCount) > 0 Then
    Print #FormFile, "T"
    Print #FormFile, LstSelected(0).ListCount
    For I = 0 To Val(LstSelected(0).ListCount) - 1
      Print #FormFile, Format(LstSelected(0).List(I), " 000");
    Next I
    Print #FormFile,
    ICODE = 0
    Print #FormFile, ICODE
  Else
    Print #FormFile, "F"
  End If

  'Ouput information on streams to accumulate
  If Val(LstSelected(1).ListCount) > 0 And ProfessionalVersion Then
    Print #FormFile, "T"
    Print #FormFile, LstSelected(1).ListCount
    For I = 0 To Val(LstSelected(1).ListCount) - 1
      Print #FormFile, Format(LstSelected(1).List(I), " 000");
    Next I
    Print #FormFile,
  Else
    Print #FormFile, "F"
  End If

  Close (FormFile)
  JobOutputFormat = True
  CumOut = 0
  resp = SIMOP(JobPath, CumOut)
  If ChkLatestData.Value = vbChecked Then
    If resp = -1 Then
      EchoFileName = "SIM.OUT"
      If SetUpViewFile = 0 Then Exit Sub
      ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
      ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
      ViewPage(ViewPageNumber).Show 1
    ElseIf resp = 2 Then
      MsgBox "No output data are available", 16, "ERROR"
    ElseIf resp = 3 Then
      MsgBox "Insufficient data from the simulator", 16, "ERROR"
    Else
      MsgBox "Error in setting up output data", 16, "ERROR"
    End If
  End If
  ChangedOutputFormat = True
  SavedJob = False
  Unload Me
  Exit Sub
  
ErrHandler:
  MsgBox "Error when writing output format data", 16, "ERROR"
  Close (FormFile)
  Exit Sub
End Sub

Private Sub CmdCancel_Click()
'*********************
  Unload OutFormat
  MODSIM.Show
End Sub

Private Sub Form_Load()
'**********************
  Dim I As Integer, M As Integer, N As Integer
  Dim ReturnFile As String
  Dim ICODE As Integer

  On Error GoTo ErrHandler
  loadingForm = True
  CHANGE_FLAG = 0
  
  If ProfessionalVersion Then
    ChkAccumulate.Enabled = True
  Else
    ChkAccumulate.Enabled = False
    LblAccumulate.Enabled = False
    LstSelected(1).Enabled = False
  End If
  OutFormat.Caption = "Specify the format for the output file for job " + MODSIM.JOBNAME
  If JobOutputFormat Then
    'Read the file containing output formats
    FormFile = FreeFile
    Open RTrim(JobPath) + "FORMAT.OUT" For Input As #FormFile
    Input #FormFile, Flags
    ChkQuantity(0).Value = 1
    I = Val(Mid(Flags, 1, 1))
    If I < 1 Then I = 1
    OptSolFl(I - 1).Value = -1
    If Mid(Flags, 2, 1) = "T" Then
       ChkQuantity(1).Value = 1
    Else
       ChkQuantity(1).Value = 0
    End If
    I = Val(Mid(Flags, 3, 1))
    If I < 1 Then I = 1
    OptWaterFl(I - 1).Value = -1
    For I = 3 To 6
      If Mid(Flags, I + 1, 1) = "T" Then
         ChkQuantity(I - 1).Value = 1
      Else
         ChkQuantity(I - 1).Value = 0
      End If
    Next I
    If Mid(Flags, 8, 1) = "T" Then
      'This is an old file
      I = 1
      ChkCoalFlag.Value = 1
    ElseIf Mid(Flags, 8, 1) = "F" Then
      'This is an old file
      I = 1
      ChkCoalFlag.Value = 0
    Else
      I = Val(Mid(Flags, 8, 1))
      Me.optMetalUnits(I - 1) = True
      If Mid(Flags, 9, 1) = "T" Then
        ChkCoalFlag.Value = 1
      Else
        ChkCoalFlag.Value = 0
      End If
    End If
  
    'Read data on metals or elements
    Input #FormFile, NumberMetals
    If NumberMetals > 0 Then
      Line Input #FormFile, WSTRING
      For I = 1 To NumberMetals
        MetalName = Mid(WSTRING, 8 * (I - 1) + 1, 8)
        TxtMetalName(I - 1).text = MetalName
      Next I
      Input #FormFile, NumberMinerals
      For M = 1 To NumberMinerals
        For I = 1 To NumberMetals
          Input #FormFile, MinMetal
          If I = 1 Then TxtMetal1(M - 1).text = CStr(MinMetal)
          If I = 2 Then TxtMetal2(M - 1).text = CStr(MinMetal)
          If I = 3 Then TxtMetal3(M - 1).text = CStr(MinMetal)
          If I = 4 Then TxtMetal4(M - 1).text = CStr(MinMetal)
        Next I
      Next M
    End If
  
    'Get data on size distributions
    Input #FormFile, Flags
    If RTrim(Flags) = "T" Then
      ChkSizeDistributions.Value = vbChecked
      Input #FormFile, Nstream
      For I = 1 To Nstream
        Input #FormFile, Stream
        LstSelected(0).AddItem CStr(Stream)
      Next I
      LblSelected.Enabled = True
      LblSize.Enabled = True
      LblStreamList.Enabled = True
      LstStreamList.Enabled = True
      LstSelected(0).Enabled = True
      Input #FormFile, ICODE
    End If
  
    'Get data on accumulated streams
    Input #FormFile, Flags
    If Trim(Flags) = "T" Then
      ChkAccumulate.Value = vbChecked
      Input #FormFile, Nstream
      For I = 1 To Nstream
        Input #FormFile, Stream
        LstSelected(1).AddItem CStr(Stream)
      Next I
      LblSelected.Enabled = True
      LblAccumulate.Enabled = True
      LblStreamList.Enabled = True
      LstStreamList.Enabled = True
      LstSelected(1).Enabled = True
    End If
    Close #FormFile
  Else
    LoadDefault
    OptCurrent.Enabled = False
  End If
  
  'Get the list of feed streams from the Sysdata form
  For N = 1 To Sysdata.StreamList.ListCount
    LstStreamList.AddItem Sysdata.StreamList.List(N - 1)
  Next N
  For N = 1 To Sysdata.OStreamList.ListCount
    LstStreamList.AddItem Sysdata.OStreamList.List(N - 1)
  Next
  
  'Get the mineral names from Form SysData
  For N = 1 To Val(Sysdata.NoMin.text)
    LblMineralName(N - 1).Caption = Sysdata.MinName(N - 1).text
  Next N
  For N = Val(Sysdata.NoMin.text) + 1 To 7
    TxtMetal1(N - 1).Visible = 0
    TxtMetal2(N - 1).Visible = 0
    TxtMetal3(N - 1).Visible = 0
    TxtMetal4(N - 1).Visible = 0
  Next N
  Call MakeNew(CurrentFormat)
  Call MakeNew(NEWDATA)
  OptNew.Value = -1
  
  Unload Sysdata
  loadingForm = False
  Exit Sub

ErrHandler:
  loadingForm = False
  Close FormFile
  Exit Sub
End Sub

Private Sub LoadDefault()
'************************
  ChkQuantity(0).Value = 1
  ChkQuantity(1).Value = 0
  ChkQuantity(2).Value = 0
  ChkQuantity(3).Value = 1
  ChkQuantity(4).Value = 0
  ChkQuantity(5).Value = 0
  OptSolFl(0).Value = -1
  OptWaterFl(0).Value = -1
  optMetalUnits(0).Value = True
  ChkSizeDistributions.Value = 0
  ChkAccumulate.Value = 0
  LstSelected(0).Clear
  LstSelected(1).Clear
End Sub

Private Sub LoadNew(NDATA As OutFileFormat)
'******************************************
  Dim I As Integer, M As Integer
  
  On Error GoTo ErrHandler
  
  For I = 1 To 6
    ChkQuantity(I - 1).Value = NDATA.Quantity(I)
  Next I
  OptSolFl(NDATA.SolidUnits - 1).Value = -1
  OptWaterFl(NDATA.WaterUnits - 1).Value = -1
  optMetalUnits(NDATA.MetalUnits - 1).Value = True
  LstSelected(0).Clear
  LstSelected(1).Clear
  If NDATA.SizeDistributions Then
    LblStreamList.Enabled = True
    LblSelected.Enabled = True
    LstStreamList.Enabled = True
    LstSelected(0).Enabled = True
    ChkSizeDistributions.Value = 1
    For I = 1 To NDATA.Nstream
      LstSelected(0).AddItem CStr(NDATA.Stream(I))
    Next I
  Else
    LstSelected(0).Enabled = False
    ChkSizeDistributions.Value = 0
  End If
  
  If NDATA.Accumulation Then
    LblStreamList.Enabled = True
    LblSelected.Enabled = True
    LstStreamList.Enabled = True
    LstSelected(1).Enabled = True
    ChkAccumulate.Value = 1
    For I = 1 To NDATA.NAccStream
      LstSelected(1).AddItem CStr(NDATA.AccStream(I))
    Next I
  Else
    LstSelected(1).Enabled = False
    ChkAccumulate.Value = 0
  End If
  If ChkSizeDistributions.Value = 0 And ChkAccumulate.Value = 0 Then
    LblSelected.Enabled = False
    LblStreamList.Enabled = False
    LstStreamList.Enabled = False
  End If
  For I = 1 To NDATA.NumberMetals
    TxtMetalName(I - 1).text = NDATA.MetalName(I)
    For M = 1 To Val(Sysdata.NoMin.text)
      If I = 1 Then
        TxtMetal1(M - 1).text = CStr(NDATA.MinMetal(M, I))
      ElseIf I = 2 Then
        TxtMetal2(M - 1).text = CStr(NDATA.MinMetal(M, I))
      ElseIf I = 3 Then
        TxtMetal3(M - 1).text = CStr(NDATA.MinMetal(M, I))
      ElseIf I = 4 Then
        TxtMetal4(M - 1).text = CStr(NDATA.MinMetal(M, I))
      End If
    Next M
  Next I
   For I = 1 To NDATA.NumberMetals
    TxtMetalName(I - 1).text = NDATA.MetalName(I)
  Next I
  'Must unload Sysdata now otherwise it remains loaded and will not initialize properly when next needed.
  Unload Sysdata
  Exit Sub
  
ErrHandler:
  Unload Sysdata
  Exit Sub
End Sub

Private Sub MakeNew(NDATA As OutFileFormat)
'******************************************
  Dim I As Integer, M As Integer
  CHANGE_FLAG = 0
  For I = 1 To 6
    NDATA.Quantity(I) = ChkQuantity(I - 1).Value
  Next I
  For I = 1 To 6
    If OptSolFl(I - 1) = -1 Then NDATA.SolidUnits = I
  Next I
  For I = 1 To 5
    If OptWaterFl(I - 1) = -1 Then NDATA.WaterUnits = I
  Next I
  For I = 1 To 2
    If optMetalUnits(I - 1) = True Then NDATA.MetalUnits = I
  Next I
  

  NDATA.Nstream = LstSelected(0).ListCount
  If NDATA.Nstream > 0 Then
    NDATA.SizeDistributions = True
    If NDATA.Nstream > 30 Then
      MsgBox "No more than 30 streams may be selected", 16, "ERROR"
      Exit Sub
    End If
    For I = 1 To NDATA.Nstream
      NDATA.Stream(I) = Val(LstSelected(0).List(I - 1))
    Next I
  Else
    NDATA.SizeDistributions = False
  End If

  NDATA.NAccStream = LstSelected(1).ListCount
  If NDATA.NAccStream > 0 Then
    NDATA.Accumulation = True
    If NDATA.NAccStream > 30 Then
      MsgBox "No more than 30 streams may be selected", 16, "ERROR"
      Exit Sub
    End If
    For I = 1 To NDATA.NAccStream
      NDATA.AccStream(I) = Val(LstSelected(1).List(I - 1))
    Next I
  Else
    NDATA.Accumulation = False
  End If

  NDATA.NumberMetals = 0
  For I = 1 To 4
    If TxtMetalName(I - 1).text <> "" Then
      NDATA.NumberMetals = NDATA.NumberMetals + 1
      NDATA.MetalName(I) = TxtMetalName(I - 1).text
      For M = 1 To Val(Sysdata.NoMin.text)
        If I = 1 Then
          NDATA.MinMetal(M, I) = Val(TxtMetal1(M - 1))
        ElseIf I = 2 Then
          NDATA.MinMetal(M, I) = Val(TxtMetal2(M - 1))
        ElseIf I = 3 Then
          NDATA.MinMetal(M, I) = Val(TxtMetal3(M - 1))
        ElseIf I = 4 Then
          NDATA.MinMetal(M, I) = Val(TxtMetal4(M - 1))
        End If
      Next M
    End If
  Next I
End Sub

Private Sub LstSelected_DblClick(Index As Integer)
'*************************************************
  If OptNew.Value = 0 Then OptNew.Value = -1
  CHANGE_FLAG = -1
  LstSelected(Index).RemoveItem LstSelected(Index).ListIndex
End Sub


Private Sub LstSelected_GotFocus(Index As Integer)
'*************************************************
    Instruction.Caption = "Double click to remove stream"
End Sub

Private Sub LstSelected_LostFocus(Index As Integer)
'**************************************************
  Instruction.Caption = ""
  Instruction.Visible = False
  If CHANGE_FLAG Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub LstStreamList_DblClick()
'***********************************
  If OptNew.Value = 0 Then OptNew.Value = -1
  CHANGE_FLAG = -1
  If ChkSizeDistributions.Value = vbChecked Then
    LstSelected(0).AddItem CStr(Val(LstStreamList.text))
  End If
  
  If ChkAccumulate.Value = vbChecked Then
    LstSelected(1).AddItem CStr(Val(LstStreamList.text))
  End If
  
End Sub


Private Sub LstStreamList_GotFocus()
'***********************************
If Not LstStreamList.Enabled Then
  Instruction.Caption = "Enable using 'Size distribution' or 'Accumulate' check box"
End If
End Sub

Private Sub LstStreamList_LostFocus()
  Instruction.Caption = ""
  Instruction.Visible = False
  If CHANGE_FLAG Then
    Call MakeNew(NEWDATA)
  End If
End Sub


Private Sub MnuAccept_Click()
'****************************
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
'****************************
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form.
PrintForm
End Sub

Private Sub OptCurrent_Click()
'******************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "Current" Then
      Call LoadNew(CurrentFormat)
    End If
  End If
End Sub

Private Sub OptDefault_Click()
'******************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "Default" Then
      Call LoadDefault
    End If
  End If
End Sub

Private Sub optMetalUnits_Click(Index As Integer)
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "MetalUnits" Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub

Private Sub optMetalUnits_LostFocus(Index As Integer)
  If CHANGE_FLAG Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub OptNew_Click()
'**************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "New" Then
      Call LoadNew(NEWDATA)
    End If
  End If
End Sub

Private Sub OPtSolFl_Click(Index As Integer)
'*******************************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "SolFl" Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub


Private Sub OPtSolFl_LostFocus(Index As Integer)
  If CHANGE_FLAG Then
    Call MakeNew(NEWDATA)
  End If
End Sub


Private Sub OPtWaterFl_Click(Index As Integer)
'*********************************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "WaterFl" Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub




Private Sub OPtWaterFl_LostFocus(Index As Integer)
'*************************************************
  If CHANGE_FLAG Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub TxtMetal1_Change(Index As Integer)
'*********************************************
  If Not loadingForm Then
    If TypeOf Screen.ActiveControl Is TextBox Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub

Private Sub TxtMetal1_GotFocus(Index As Integer)
'***********************************************
  Instruction.Caption = "Specify mass fraction of metal or element in the mineral"
  Instruction.Visible = -1
End Sub

Private Sub TxtMetal1_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If KeyAscii = 13 Then
    KeyAscii = 0
    If Val(TxtMetal1(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal1(Index).SetFocus
      Exit Sub
    End If
    If Index < (Sysdata.NoMin) - 1 Then
      TxtMetal1(Index + 1).SetFocus
    End If
  End If
End Sub

Private Sub TxtMetal1_LostFocus(Index As Integer)
'*************************************************
  Instruction.Visible = 0
  If Index > 0 Then
    If Val(TxtMetal1(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal1(Index).SetFocus
      Exit Sub
    End If
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub TxtMetal2_Change(Index As Integer)
'*********************************************
  If Not loadingForm Then
    If TypeOf Screen.ActiveControl Is TextBox Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub

Private Sub TxtMetal2_GotFocus(Index As Integer)
'***********************************************
  Instruction.Caption = "Specify mass fraction of metal or element in the mineral"
  Instruction.Visible = -1
End Sub

Private Sub TxtMetal2_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If KeyAscii = 13 Then
    KeyAscii = 0
    If Val(TxtMetal2(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal2(Index).SetFocus
      Exit Sub
    End If
    If Index < (Sysdata.NoMin) - 1 Then
      TxtMetal2(Index + 1).SetFocus
    End If
  End If
End Sub

Private Sub TxtMetal2_LostFocus(Index As Integer)
'*************************************************
  Instruction.Visible = 0
  If Index > 0 Then
    If Val(TxtMetal2(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal2(Index).SetFocus
      Exit Sub
    End If
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub TxtMetal3_Change(Index As Integer)
'*********************************************
  If Not loadingForm Then
    If TypeOf Screen.ActiveControl Is TextBox Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub

Private Sub TxtMetal3_GotFocus(Index As Integer)
'***********************************************
  Instruction.Caption = "Specify mass fraction of metal or element in the mineral"
  Instruction.Visible = -1
End Sub

Private Sub TxtMetal3_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If KeyAscii = 13 Then
    KeyAscii = 0
    If Val(TxtMetal3(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal3(Index).SetFocus
      Exit Sub
    End If
    If Index < (Sysdata.NoMin) - 1 Then
      TxtMetal3(Index + 1).SetFocus
    End If
  End If
End Sub

Private Sub TxtMetal3_LostFocus(Index As Integer)
'*************************************************
  Instruction.Visible = 0
  If Index > 0 Then
    If Val(TxtMetal3(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal3(Index).SetFocus
      Exit Sub
    End If
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub TxtMetal4_Change(Index As Integer)
'*********************************************
  If Not loadingForm Then
    If TypeOf Screen.ActiveControl Is TextBox Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
    End If
  End If
End Sub

Private Sub TxtMetal4_GotFocus(Index As Integer)
'***********************************************
  Instruction.Caption = "Specify mass fraction of metal or element in the mineral"
  Instruction.Visible = -1
End Sub

Private Sub TxtMetal4_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If KeyAscii = 13 Then
    KeyAscii = 0
    If Val(TxtMetal4(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal4(Index).SetFocus
      Exit Sub
    End If
    If Index < (Sysdata.NoMin) - 1 Then
      TxtMetal4(Index + 1).SetFocus
    End If
  End If
End Sub

Private Sub TxtMetal4_LostFocus(Index As Integer)
'*************************************************
  Instruction.Visible = 0
  If Index > 0 Then
    If Val(TxtMetal4(Index).text) > 1# Then
      MsgBox "Fractional value is required here.", 0, "ERROR"
      TxtMetal4(Index).SetFocus
      Exit Sub
    End If
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub TxtMetalName_GotFocus(Index As Integer)
'**************************************************
  Instruction.Caption = "Specify the name of the metal or element"
  Instruction.Visible = True
  If CHANGE_FLAG Then
    Call MakeNew(NEWDATA)
  End If
End Sub


Private Sub TxtMetalName_LostFocus(Index As Integer)
'***************************************************
  Instruction.Visible = False
End Sub
