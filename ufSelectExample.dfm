object formExamples: TformExamples
  Width = 271
  Height = 246
  CSSLibrary = cssBootstrap
  ElementClassName = ' bg-dark border border-dark'
  ElementFont = efCSS
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  object rgSelectExample: TWebRadioGroup
    Left = 8
    Top = 21
    Width = 249
    Height = 204
    ElementClassName = 'custom-control custom-radio'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    Caption = 'Select Model to view:'
    Columns = 1
    ElementButtonClassName = 'custom-control-input'
    ElementGroupClassName = 'modal-content'
    ElementLabelClassName = 'custom-control-label'
    ElementLegendClassName = 'h6'
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    ParentFont = False
    Role = ''
    OnChange = rgSelectExampleChange
  end
  object btnClose: TWebButton
    Left = 208
    Top = 0
    Width = 49
    Height = 25
    Caption = 'OK'
    ChildOrder = 1
    ElementClassName = 'btn btn-primary btn-sm'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnCloseClick
  end
end
