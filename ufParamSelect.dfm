object ParamSliderSForm: TParamSliderSForm
  Width = 234
  Height = 212
  ElementClassName = 'card'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  OnCreate = WebFormCreate
  OnShow = WebFormShow
  object OkButton1: TWebButton
    Left = 152
    Top = 3
    Width = 58
    Height = 22
    Caption = 'OK'
    ChildOrder = 1
    ElementClassName = 'btn btn-primary btn-sm'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = OkButton1Click
  end
  object paramRG: TWebRadioGroup
    Left = 16
    Top = 31
    Width = 137
    Height = 120
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    Caption = ''
    ChildOrder = 2
    Columns = 1
    ItemIndex = 0
    Role = ''
    OnChange = paramRGChange
  end
end
