unit ufGraphDemo;
// Demo/Test GUI for TWebScrollingChart

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.ExtCtrls, uWebScrollingChart, Math;

type
  TForm1 = class(TWebForm)
    btnStartPlot: TWebButton;
    WebLabel1: TWebLabel;
    pnlPlot: TWebPanel; // Holds chart
    WebTimer1: TWebTimer;
    btnPause: TWebButton;
    rbtAutoYMax: TWebRadioButton;
    rbtAutoYMin: TWebRadioButton;
    editYMax: TWebEdit;
    editYMin: TWebEdit;
    pnlYmaxScale: TWebPanel;
    pnlYminScale: TWebPanel;
    lblYMax: TWebLabel;
    lblYMin: TWebLabel;
    pnlBase: TWebPanel;
    pnlRight: TWebPanel;
    WebSplitter1: TWebSplitter;
    procedure btnStartPlotClick(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure InitComps;
    procedure WebTimer1Timer(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
   // procedure editYMaxChange(Sender: TObject);
    procedure rbtAutoYMaxClick(Sender: TObject);
    procedure rbtAutoYMinClick(Sender: TObject);
    procedure editYMinExit(Sender: TObject);
    procedure editYMaxExit(Sender: TObject);
    procedure WebSplitter1Move(Sender: TObject);
  //  procedure rbtAutoYMaxClick(Sender: TObject);
  //  procedure rbtAutoYMinClick(Sender: TObject);

  private
    procedure initPlot();

  public
    chart: TWebScrollingChart;
    time: double;
    bmp: TBitMap;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnPauseClick(Sender: TObject);
begin
  if self.btnPause.Caption = 'Pause' then
    begin
       self.btnPause.Caption := 'Resume';
       WebTimer1.Enabled := false;
    end
  else
    begin
       self.btnPause.Caption := 'Pause';
       WebTimer1.Enabled := true;
    end;
end;

procedure TForm1.btnStartPlotClick(Sender: TObject);
var
  status: Boolean;
begin
  if self.chart <> nil then
    self.chart.Destroy;
  self.initPlot;
  status := WebTimer1.Enabled;
  WebTimer1.Enabled := true;
  time := 0;
  chart.restart;
  self.btnStartPlot.Caption := 'Restart';
  self.btnPause.Caption := 'Pause';
  WebTimer1.Enabled := status;
end;

{procedure TForm1.editYMaxChange(Sender: TObject);
var newVal: double;
begin
  try
      newVal := strtofloat(self.editYMax.Text);
    except
      on Exception : EConvertError do
      begin
      ShowMessage(Exception.Message);
      self.editYMax.Text := floattostr(10);
      newVal := 10;
      end;
  end;

  if self.chart <> nil then
    begin
      self.chart.YAxisMax := newVal;
    end;
end; }

procedure TForm1.editYMaxExit(Sender: TObject);
var newVal: double;
begin
  try
      newVal := strtofloat(self.editYMax.Text);
    except
      on Exception : EConvertError do
      begin
      ShowMessage(Exception.Message);
      self.editYMax.Text := floattostr(10);
      newVal := 10;
      end;
  end;

  if self.chart <> nil then
    begin
      self.chart.YAxisMax := newVal;
    end;
end;

{procedure TForm1.editYMinChange(Sender: TObject);
var newVal: integer;
begin
  try
      newVal := strtoint(self.editYMin.Text);
    except
      on Exception : EConvertError do
      begin
      ShowMessage(Exception.Message);
      self.editYMin.Text := floattostr(0);
      newVal := 0;
      end;
  end;

   if self.chart <> nil then
    begin
      self.chart.YAxisMax := newVal;
    end;

end;  }

procedure TForm1.editYMinExit(Sender: TObject);
var newVal: double;
begin
  try
    newVal := strtofloat(self.editYMin.Text);
    except
      on Exception : EConvertError do
      begin
      ShowMessage(Exception.Message);
      self.editYMin.Text := floattostr(0);
      newVal := 0;
      end;
  end;

  if self.chart <> nil then
    begin
      self.chart.YAxisMax := newVal;
    end;

end;

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  self.time := 0;
  self.WebTimer1.Enabled := false;
end;

 procedure TForm1.WebSplitter1Move(Sender: TObject);
begin
  self.chart.Width := self.pnlPlot.Width;
end;

procedure TForm1.initPlot();
 begin
   self.chart := TWebScrollingChart.Create(self.pnlPlot);
   self.chart.Parent := self.pnlPlot;

   self.chart.autoScaleUp := self.rbtAutoYMax.Checked;
   self.chart.autoScaleDown := self.rbtAutoYMin.Checked;
  // if not self.rbtAutoYMax.Checked then
   self.chart.YAxisMax := strtofloat(self.editYMax.Text);
  // else self.chart.YAxisMax := 10000;
  // if not self.rbtAutoYMin.Checked then
   self.chart.YAxisMin := strtofloat(self.editYMin.Text);
  // else self.chart.YAxisMin := -1000;

   self.chart.BackgroundColor := clGray; //clNavy;
   self.chart.PlotPanelBackgroundColor := clBlack; // currently does not show.
   self.chart.LegendBackgroundColor := clSilver;
   self.chart.GridColor := clBlack;
   self.chart.AddSerie;
   self.chart.AddSerie;
   self.chart.AddSerie;
   self.chart.AddSerie;

   self.chart.series[0].color := clRed;
   self.chart.series[1].color := clGreen;
   self.chart.series[2].color := clWhite;
   self.chart.series[3].color := clYellow;

   InitComps; // Not currently needed.
   time := 0;
   self.WebTimer1.Enabled := true;
   self.chart.ExternalTimer := self.WebTimer1;
 end;


procedure TForm1.rbtAutoYMaxClick(Sender: TObject);
begin
  if self.rbtAutoYMax.Checked then
    begin
    self.editYMax.Enabled := false;
    end
  else
    begin
    //self.rbtAutoYMax.Checked := false;
    self.editYMax.Enabled := true;
    end;
end;

procedure TForm1.rbtAutoYMinClick(Sender: TObject);
begin
  if self.rbtAutoYMin.Checked then
    begin
    self.editYMin.Enabled := false;
    end
  else
    begin
    self.editYMin.Enabled := true;
    end;
end;

function func1(t: double): double;
var
  A, s, c, Tetha: Double;
begin
  A := 1.5;
  Tetha := (2*PI*60*t)/360;
  SinCos(Tetha, s, c);
  Result := A*s;
end;
function func2(t: double): double;
var
  A, s, c, Tetha: Double;
begin
  A := 1.8;
  Tetha := (2*PI*120*t)/360;
  SinCos(Tetha, s, c);
  Result := A*s;
end;
function sen(x: double): double;
var
  c: double;
begin
  SinCos(x, Result, c);
end;
function func3(t: double): double;
var
  A0, A1, A2, A3: Double;
  alpha, f: double;
begin
  f := 60;
  A0 := 3;
  A1 := A0/2;
  A2 := A0/3;
  A3 := A0/4;
  alpha := PI/4;
  Result := A0*sen(2*PI*f*t/360 + alpha) +
            A1*sen(2*PI*2*f*t/360 + alpha) +
            A2*sen(2*PI*3*f*t/360 + alpha) +
            A3*sen(2*PI*4*f*t/360 + alpha);
end;
function func4(t: double): double;
var
  A, s, c, Tetha: Double;
begin
  A := 8;
  Tetha := (2*PI*120*t)/360;
  SinCos(Tetha, s, c);
  Result := A*c + 20; // offset to test ymax/ymin
end;


procedure TForm1.WebTimer1Timer(Sender: TObject);
var
  y: double;
begin
  y := func1(time);
 // self.chart.series[0].add(time, y);
  self.chart.updateSerie(0, time, y);
  y := func2(time);
 // self.chart.series[1].add(time, y);
  self.chart.updateSerie(1, time, y);
  y := func3(time);
 // self.chart.series[2].add(time, y);
  self.chart.updateSerie(2, time, y);
  y := func4(time);
 // self.chart.series[3].add(time, y);
  self.chart.updateSerie(3, time, y);
  self.chart.plot;

  time := time + chart.DeltaX;
end;

procedure TForm1.InitComps;
//var
 // i: Integer;
begin
 {  ColorBox1.Selected := chart.BackgroundColor;
   RadioGroup1.ItemIndex := getIndex(chart.title.align);
   Edit1.Text := chart.ChartTitle;
   ColorBox2.Selected := chart.ChartTitleColor;
   ColorBox3.Selected := chart.LegendFontColor;
   ColorBox4.Selected := chart.LegendBackgroundColor;
   ComboBox1.ItemIndex := chart.LegendReference;
   SpinEdit1.Value := round(chart.LegendPosX);
   SpinEdit2.Value := round(chart.LegendPosY);
   CheckBox1.Checked := chart.LegendVisible;
   ColorBox5.Selected := chart.AxisColor;
   SpinEdit3.Value := chart.AxisStrokeWidth;
   Edit2.Text := chart.XAxisCaption;
   Edit3.Text := chart.YAxisCaption;
   CheckBox2.Checked := chart.autoScaleUp;
   CheckBox3.Checked := chart.autoScaleDown;
   ColorBox7.Selected := chart.PlotPanelBackgroundColor;
   ColorBox8.Selected := chart.GridColor;
   SpinEdit5.value := chart.GridStrokeWidth;
   CheckListBox1.Items.Clear;
   for I := 0 to length(chart.series) - 1 do
     begin
       CheckListBox1.Items.Add(chart.series[I].name);
       CheckListBox1.Checked[I] := chart.series[I].visible;
     end;
   GroupBox6.Caption := '';
   selectNone;
   Label16.Caption := Format('%s: , %s:', [chart.XAxisCaption, chart.YAxisCaption]);}
end;


{procedure TForm1.rbtAutoYMaxClick(Sender: TObject);
begin
  if self.chart.autoScaleUp = false then
    self.chart.autoScaleUp := true
  else
    begin
    self.rbtAutoYMax.Checked := false;
    self.chart.autoScaleUp := false;
    end;
end;  }


{procedure TForm1.rbtAutoYMinClick(Sender: TObject);
begin
  if self.chart.autoScaleDown = false then
    self.chart.autoScaleDown := true
  else
    begin
    self.rbtAutoYMin.Checked := false;
    self.chart.autoScaleDown := false;
    end;
end;
           }
end.