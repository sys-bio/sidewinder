object FYAxisMinMaxEdit: TFYAxisMinMaxEdit
  Width = 271
  Height = 217
  CSSLibrary = cssBootstrap
  ElementClassName = ' bg-dark border border-dark'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  OnClose = WebFormClose
  OnCreate = WebFormCreate
  object lblYMax: TWebLabel
    Left = 56
    Top = 75
    Width = 51
    Height = 19
    Caption = 'Y max:'
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
  end
  object lblYMin: TWebLabel
    Left = 56
    Top = 131
    Width = 48
    Height = 19
    Alignment = taCenter
    Caption = 'Y min:'
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
  end
  object lblYMaxMin: TWebLabel
    Left = 16
    Top = 24
    Width = 143
    Height = 19
    Caption = 'Set Y axis max/min:'
    ElementClassName = 'label label-primary '
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
  end
  object editYMax: TWebEdit
    Left = 118
    Top = 72
    Width = 81
    Height = 22
    ElementClassName = 'form-control'
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    ParentFont = False
    Text = '10'
    WidthPercent = 100.000000000000000000
    OnChange = editYMaxChange
  end
  object editYMin: TWebEdit
    Left = 118
    Top = 128
    Width = 81
    Height = 22
    ChildOrder = 1
    ElementClassName = 'form-control'
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    ParentFont = False
    Text = '0'
    WidthPercent = 100.000000000000000000
    OnChange = editYMinChange
    OnExit = editYMinExit
  end
end
