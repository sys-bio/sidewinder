object VarSelectForm: TVarSelectForm
  Top = 20
  Width = 200
  Height = 145
  CSSLibrary = cssBootstrap
  ElementClassName = ' bg-dark border border-dark'
  ShowClose = False
  OnCreate = plotFormCreate
  object okButton1: TWebButton
    Left = 145
    Top = 0
    Width = 40
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ChildOrder = 1
    ElementClassName = 'btn btn-primary btn-sm'
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
    Top = 31
    Width = 140
    Height = 91
    Caption = ''
    ChildOrder = 2
    Columns = 1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Role = ''
    OnCheckClick = SpPlotCGCheckClick
  end
end
