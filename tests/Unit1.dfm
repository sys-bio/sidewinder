object Form1: TForm1
  Left = 240
  Top = 266
  Width = 640
  Height = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  OnCreate = WebFormCreate
  object SBMLrxns: TWebLabel
    Left = 32
    Top = 47
    Width = 50
    Height = 14
    Caption = 'SBMLrxns'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object WebLabel1: TWebLabel
    Left = 232
    Top = 47
    Width = 89
    Height = 14
    Caption = 'Reactants (stoich)'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object Products: TWebLabel
    Left = 368
    Top = 47
    Width = 83
    Height = 14
    Caption = 'Products (stoich)'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object WebLabel2: TWebLabel
    Left = 80
    Top = 118
    Width = 60
    Height = 14
    Caption = 'Kinetic Law:'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object speciesListLabel: TWebLabel
    Left = 33
    Top = 147
    Width = 107
    Height = 14
    Caption = 'Species list (inital amt)'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object CompLabel: TWebLabel
    Left = 176
    Top = 147
    Width = 100
    Height = 14
    Caption = 'Compartments (size)'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object ParamLabel: TWebLabel
    Left = 344
    Top = 147
    Width = 96
    Height = 14
    Caption = 'Parameters (init val)'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object WebLabel3: TWebLabel
    Left = 328
    Top = 254
    Width = 87
    Height = 14
    Caption = 'Simulation results:'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object rtLenLabel: TWebLabel
    Left = 120
    Top = 415
    Width = 108
    Height = 14
    Caption = 'Run time length: (sec):'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object steSizeWLabel: TWebLabel
    Left = 146
    Top = 443
    Width = 85
    Height = 14
    Caption = 'Step size (msec):'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object ParamWLabel1: TWebLabel
    Left = 32
    Top = 336
    Width = 39
    Height = 14
    Caption = 'Param 1'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object AssignRuleLabel: TWebLabel
    Left = 40
    Top = 576
    Width = 87
    Height = 14
    Caption = 'Assignment rules:'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object SBMLmodelWMemo: TWebMemo
    Left = 33
    Top = 463
    Width = 254
    Height = 80
    AutoSize = False
    HeightPercent = 100.000000000000000000
    Lines.Strings = (
      'SBMLmodelWMemo')
    SelLength = 0
    SelStart = 0
    WidthPercent = 100.000000000000000000
  end
  object SBMLFilePicker: TWebFilePicker
    Left = 33
    Top = 300
    Width = 121
    Height = 19
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    ChildOrder = 3
    OnChange = SBMLFilePickerChange
    OnGetFileAsText = SBMLFilePickerGetFileAsText
  end
  object ReactionID: TWebComboBox
    Left = 32
    Top = 67
    Width = 145
    Height = 22
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnChange = ReactionIDChange
    ItemIndex = -1
  end
  object SBMLReactants: TWebListBox
    Left = 232
    Top = 68
    Width = 121
    Height = 41
    HeightPercent = 100.000000000000000000
    ItemHeight = 14
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object SBMLProducts: TWebListBox
    Left = 368
    Top = 67
    Width = 121
    Height = 41
    HeightPercent = 100.000000000000000000
    ItemHeight = 14
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object ModelNotes: TWebMemo
    Left = 32
    Top = 8
    Width = 330
    Height = 33
    AutoSize = False
    HeightPercent = 100.000000000000000000
    Lines.Strings = (
      'ModelNotes')
    SelLength = 0
    SelStart = 0
    WidthPercent = 100.000000000000000000
  end
  object kinLaw: TWebMemo
    Left = 146
    Top = 115
    Width = 375
    Height = 26
    AutoSize = False
    HeightPercent = 100.000000000000000000
    Lines.Strings = (
      'kinLaw')
    SelLength = 0
    SelStart = 0
    WidthPercent = 100.000000000000000000
  end
  object speciesListBox: TWebListBox
    Left = 33
    Top = 167
    Width = 121
    Height = 50
    HeightPercent = 100.000000000000000000
    ItemHeight = 14
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object CompListBox: TWebListBox
    Left = 176
    Top = 167
    Width = 113
    Height = 50
    HeightPercent = 100.000000000000000000
    ItemHeight = 14
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object ParamListBox: TWebListBox
    Left = 328
    Top = 167
    Width = 145
    Height = 50
    HeightPercent = 100.000000000000000000
    ItemHeight = 14
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object SimResultsWMemo: TWebMemo
    Left = 255
    Top = 274
    Width = 377
    Height = 85
    AutoSize = False
    HeightPercent = 100.000000000000000000
    Lines.Strings = (
      'SimResultsWMemo')
    SelLength = 0
    SelStart = 0
    WidthPercent = 100.000000000000000000
  end
  object RtLength: TWebEdit
    Left = 232
    Top = 412
    Width = 44
    Height = 22
    ChildOrder = 21
    HeightPercent = 100.000000000000000000
    Text = '100'
    WidthPercent = 100.000000000000000000
    OnChange = RtLengthChange
  end
  object stepsizeWEdit: TWebEdit
    Left = 232
    Top = 440
    Width = 44
    Height = 22
    ChildOrder = 22
    HeightPercent = 100.000000000000000000
    Text = '100'
    WidthPercent = 100.000000000000000000
    OnChange = stepsizeWEditChange
  end
  object ODEsolverWRadioGroup: TWebRadioGroup
    Left = 183
    Top = 274
    Width = 66
    Height = 109
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    Caption = 'ODE solver'
    ChildOrder = 24
    Columns = 1
    ItemIndex = 1
    Items.Strings = (
      'Euler '
      'RK4'
      'LSODA')
    Role = ''
    OnChange = ODEsolverWRadioGroupChange
  end
  object onlineWebB: TWebButton
    Left = 328
    Top = 223
    Width = 73
    Height = 25
    Caption = 'Online'
    ChildOrder = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
    OnClick = onlineWebBClick
  end
  object offlineWebB: TWebButton
    Left = 425
    Top = 223
    Width = 96
    Height = 25
    Caption = 'Offline'
    ChildOrder = 26
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = offlineWebBClick
  end
  object ParamWTrackBar1: TWebTrackBar
    Left = 27
    Top = 352
    Width = 150
    Height = 20
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    ChildOrder = 26
    Max = 10
    Min = 0
    Position = 0
    Role = ''
  end
  object PlotWebPB: TWebPaintBox
    Left = 293
    Top = 365
    Width = 324
    Height = 139
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    ChildOrder = 28
    OnPaint = PlotWebPBPaint
  end
  object AssignRulesLB: TWebListBox
    Left = 33
    Top = 592
    Width = 254
    Height = 97
    HeightPercent = 100.000000000000000000
    ItemHeight = 14
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object WebTimer1: TWebTimer
    Enabled = False
    Interval = 100
    OnTimer = WebTimer1Timer
    Left = 520
    Top = 184
  end
end
