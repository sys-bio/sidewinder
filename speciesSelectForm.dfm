object SpeciesSWForm: TSpeciesSWForm
  Width = 204
  Height = 230
  CSSLibrary = cssBootstrap
  OnCreate = plotFormCreate
  OnShow = WebFormShow
  object okButton1: TWebButton
    Left = 8
    Top = 180
    Width = 70
    Height = 22
    Caption = 'OK'
    ChildOrder = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HeightPercent = 100.000000000000000000
    ParentFont = False
    WidthPercent = 100.000000000000000000
    OnClick = okButton1Click
  end
  object SpPlotCG: TWebCheckGroup
    Left = 8
    Top = 30
    Width = 129
    Height = 91
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    Caption = ''
    ChildOrder = 2
    Columns = 1
    Role = ''
    OnCheckClick = SpPlotCGCheckClick
  end
end
