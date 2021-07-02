object SpeciesSWForm: TSpeciesSWForm
  Top = 20
  Width = 204
  Height = 145
  CSSLibrary = cssBootstrap
  ElementClassName = ' bg-dark border border-dark'
  OnCreate = plotFormCreate
  OnShow = WebFormShow
  object okButton1: TWebButton
    Left = 128
    Top = 24
    Width = 52
    Height = 24
    Anchors = [akTop]
    Caption = 'OK'
    ChildOrder = 2
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
    Top = 30
    Width = 114
    Height = 91
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
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
