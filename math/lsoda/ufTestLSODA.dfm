object Form1: TForm1
  Width = 640
  Height = 480
  OnCreate = WebFormCreate
  object StartLSODATestButton: TWebButton
    Left = 56
    Top = 48
    Width = 129
    Height = 33
    Caption = 'Start LSODA Unit Test'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = StartLSODATestButtonClick
  end
end
