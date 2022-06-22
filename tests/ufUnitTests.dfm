object fUnitTests: TfUnitTests
  Width = 720
  Height = 480
  OnCreate = WebFormCreate
  object lblPickTests: TWebLabel
    Left = 16
    Top = 58
    Width = 96
    Height = 16
    Caption = 'Pick tests to run:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
  end
  object lblResults: TWebLabel
    Left = 280
    Top = 39
    Width = 39
    Height = 13
    Caption = 'Results:'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object btnRunall: TWebButton
    Left = 16
    Top = 8
    Width = 96
    Height = 25
    Caption = 'Run all tests'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnRunallClick
  end
  object lbTestGroups: TWebListBox
    Left = 16
    Top = 80
    Width = 209
    Height = 129
    HeightPercent = 100.000000000000000000
    ItemHeight = 13
    ItemIndex = -1
    MultiSelect = True
    WidthPercent = 100.000000000000000000
  end
  object btnRunSpecifiedTests: TWebButton
    Left = 104
    Top = 224
    Width = 121
    Height = 25
    Caption = 'Run Specified tests'
    ChildOrder = 2
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnRunSpecifiedTestsClick
  end
  object lbTestResults: TWebListBox
    Left = 280
    Top = 58
    Width = 409
    Height = 169
    HeightPercent = 100.000000000000000000
    ItemHeight = 13
    ItemIndex = -1
    WidthPercent = 100.000000000000000000
  end
  object btnSaveFile: TWebButton
    Left = 440
    Top = 8
    Width = 113
    Height = 33
    Caption = 'Save Results to File'
    ChildOrder = 6
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnSaveFileClick
  end
end
