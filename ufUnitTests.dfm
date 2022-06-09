object fUnitTests: TfUnitTests
  Width = 640
  Height = 480
  OnCreate = WebFormCreate
  object lblPickTests: TWebLabel
    Left = 112
    Top = 162
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
  object btnRunall: TWebButton
    Left = 112
    Top = 80
    Width = 96
    Height = 25
    Caption = 'Run all tests'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object lbTestGroups: TWebListBox
    Left = 112
    Top = 184
    Width = 209
    Height = 129
    HeightPercent = 100.000000000000000000
    ItemHeight = 13
    ItemIndex = -1
    MultiSelect = True
    WidthPercent = 100.000000000000000000
    OnClick = lbTestGroupsClick
  end
  object btnRunSpecifiedTests: TWebButton
    Left = 200
    Top = 336
    Width = 121
    Height = 25
    Caption = 'Run Specified tests'
    ChildOrder = 2
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnRunSpecifiedTestsClick
  end
end
