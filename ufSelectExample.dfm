object formExamples: TformExamples
  Width = 271
  Height = 209
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
    Top = 8
    Width = 249
    Height = 185
    ElementClassName = 'form-radio-input'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    Caption = 'Select Model to view:'
    Columns = 1
    ElementGroupClassName = 'modal-content'
    ElementLegendClassName = 'h6'
    ElementFont = efCSS
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = -1
    ParentFont = False
    Role = ''
    OnChange = rgSelectExampleChange
  end
end
