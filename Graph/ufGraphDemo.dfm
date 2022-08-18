object Form1: TForm1
  Width = 891
  Height = 626
  OnCreate = WebFormCreate
  object WebLabel1: TWebLabel
    Left = 24
    Top = 152
    Width = 53
    Height = 13
    Caption = 'WebLabel1'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object btnStartPlot: TWebButton
    Left = 8
    Top = 200
    Width = 96
    Height = 25
    Caption = 'Start'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnStartPlotClick
  end
  object pnlPlot: TWebPanel
    Left = 128
    Top = 40
    Width = 721
    Height = 578
    BorderColor = clNone
    ChildOrder = 2
  end
  object btnPause: TWebButton
    Left = 8
    Top = 312
    Width = 96
    Height = 25
    Caption = 'Pause'
    ChildOrder = 3
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = btnPauseClick
  end
  object pnlYmaxScale: TWebPanel
    Left = 8
    Top = 368
    Width = 112
    Height = 94
    ChildOrder = 8
    object lblYMax: TWebLabel
      Left = 16
      Top = 8
      Width = 29
      Height = 13
      Caption = 'Y max'
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
    end
    object editYMax: TWebEdit
      Left = 3
      Top = 41
      Width = 88
      Height = 22
      HeightPercent = 100.000000000000000000
      Text = '10'
      WidthPercent = 100.000000000000000000
      OnExit = editYMaxExit
    end
    object rbtAutoYMax: TWebRadioButton
      Left = 3
      Top = 69
      Width = 113
      Height = 22
      Caption = 'Autoscale Y max'
      Checked = False
      ChildOrder = 5
      Color = clNone
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = rbtAutoYMaxClick
    end
  end
  object pnlYminScale: TWebPanel
    Left = 8
    Top = 496
    Width = 114
    Height = 94
    ChildOrder = 7
    object lblYMin: TWebLabel
      Left = 8
      Top = 16
      Width = 25
      Height = 13
      Caption = 'Y min'
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
    end
    object editYMin: TWebEdit
      Left = 3
      Top = 41
      Width = 93
      Height = 22
      HeightPercent = 100.000000000000000000
      Text = '0'
      WidthPercent = 100.000000000000000000
      OnExit = editYMinExit
    end
    object rbtAutoYMin: TWebRadioButton
      Left = 3
      Top = 69
      Width = 113
      Height = 22
      Caption = 'Autoscale Y min'
      Checked = False
      ChildOrder = 6
      Color = clNone
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = rbtAutoYMinClick
    end
  end
  object WebTimer1: TWebTimer
    Interval = 100
    OnTimer = WebTimer1Timer
    Left = 568
    Top = 432
  end
end
