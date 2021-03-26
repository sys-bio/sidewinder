object ParamSliderSForm: TParamSliderSForm
  Width = 200
  Height = 212
  OnCreate = WebFormCreate
  OnShow = WebFormShow
  object OkButton1: TWebButton
    Left = 112
    Top = 0
    Width = 64
    Height = 24
    Caption = 'OK'
    ChildOrder = 1
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = OkButton1Click
  end
  object paramRG: TWebRadioGroup
    Left = 16
    Top = 31
    Width = 160
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
