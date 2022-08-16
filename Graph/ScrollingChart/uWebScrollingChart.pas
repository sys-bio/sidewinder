unit uWebScrollingChart;
interface
uses SysUtils, Classes, WebLib.Controls, {VCL.Controls,} WebLib.ExtCtrls, {Vcl.ExtCtrls,}
     WebLib.Dialogs,{ Vcl.Dialogs,} System.Types, {System.UIConsts,} System.UITypes,
     System.Contnrs, System.Generics.Defaults, System.Generics.Collections, JS, Web,
     Graphics, WebLib.Graphics, uWebDataSource, WebLib.Forms, {VCL.Forms,} uWebStage,
     uWebGlobalData, Vcl.Imaging.pngimage, Vcl.StdCtrls, uScrollingTypes;
type
 // TVCLScrollingChart = class (TGraphicControl)
  TWebScrollingChart = class (TWebGraphicControl)
    private
   //   FParent: TWinControl;
      FParent: TWebControl;
     // timer: TTimer;
      timer: TWebTimer;
      time: Double;
      obj: TObject;
      Sh: TShiftState;
      FMouseMoveEvent: TMouseMoveEvent;
      // methods to published properties
      function GetAxisColor: TColor;
      procedure SetAxisColor(Val: TColor);
      function GetBackgroundColor: TColor;
      procedure SetBackgroundColor(val: TColor);
      function GetGridColor: TColor;
      procedure SetGridColor(Val: TColor);
      function GetPanelBackgroundColor: TColor;
      procedure SetPanelBackgroundColor(val: TColor);
      function GetStroke: Byte;
      procedure SetStroke(val: Byte);
      function GetGridStrokeWidth: Byte;
      procedure SetGridStrokeWidth(Val: Byte);
      function GetYAxisTicks: Integer;
      procedure SetYAxisTicks(val: Integer);
      function GetXAxisTicks: Integer;
      procedure SetXAxisTicks(val: Integer);
      function GetReferenceLegend: TPivot;
      procedure SetReferenceLegend(val: TPivot);
      function GetChartTitle: String;
      procedure SetChartTitle(val: String);
      function GetXAxisCaption: String;
      procedure SetXAxisCaption(val: String);
      function GetYAxisCaption: String;
      procedure SetYAxisCaption(val: String);
      function GetChartTitleColor: TColor;
      procedure SetChartTitleColor(val: TColor);
      procedure SetDeltaX(val: Single);
      function GetDeltaX: Single;
      function GetLegendFontColor: TColor;
      procedure SetLegendFontColor(val: TColor);
      function GetLegendBackgroundColor: TColor;
      procedure SetLegendBackgroundColor(val: TColor);
      function GetLegendPosition: TValuesLegend;
      procedure SetLegendPosition(val: TValuesLegend);
      function GetLegendPosX: Single;
      procedure SetLegendPosX(val: Single);
      function GetLegendPosY: Single;
      procedure SetLegendPosY(val: Single);
      function GetLegendVisible: Boolean;
      procedure SetLegendVisible(val: Boolean);
      function GetYAxisMin: Double;
      procedure SetYAxisMin(val: Double);
      function GetYAxisMax: Double;
      procedure SetYAxisMax(val: Double);

      function GetXAxisMax: Double;
      procedure SetXAxisMax(val: Double);
      //General
      procedure SetFMouseMoveEvent(val: TMouseMoveEvent);
      procedure SetDefaultValues;
      function IndexOf(aname: String): Integer;
      procedure Init;
      procedure Finalize;
      procedure OnComplete(X, Y: single);
      procedure FreeSeries;
      procedure InitStage;
      procedure Clear(FCanvas: TCanvas; cl: tcolor);
      procedure getForm(AOwner: TComponent);
      //Timer
      procedure SetDefaultTimer;
 //     procedure SetTimer(Value: TTimer);
 //     function GetTimer: TTimer;
      procedure SetTimer(Value: TWebTimer);
      function GetTimer: TWebTimer;
      function GetInterval: Cardinal;
      procedure SetInterval(Value: Cardinal);
      procedure SetOnTimer(Value: TNotifyEvent);
      function GetOnTimer: TNotifyEvent;
      procedure SetEnabledTimer(Value: Boolean);
      function GetEnabledTimer: Boolean;
     // procedure SetAParent(val: TWinControl);
      procedure SetAParent(val: TWebControl);
 //     procedure ConvertAndSaveToPNG(BmpSrc: TBitmap; sFilename: String); DO not use for now.
      procedure Rebuild;
   strict protected
      procedure ChartOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure ChartOnMouseLeave(Sender: TObject);

    protected
      procedure Paint; override;
      procedure Resize; override;
    public
      globaldata: TGlobalData;
      xAxis, yAxis: TInfoAxis;
      plane: TPlaneXY;
      legend: TLegend;
      title: TTitle;
      grid: TGridInfo;
      series: TSeries;
      stage: TStage;
      autoScaleUp, autoScaleDown: Boolean;
      xMinIni: Double;
      constructor Create (AOwner: TComponent); override;
    //  constructor Create (AOwner: TComponent; chartWidth: integer; chartHeight: integer); override;
      destructor  Destroy; override;
      function AddSerie(functionTime: TFunctionTime; aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie; overload;
      function AddSerie(aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie; overload;
      function AddSerieByName(aname: String; functionTime: TFunctionTime = nil): TDataSerie;
      procedure DeleteSerie(index: Integer);
      procedure DeleteSeries;
      procedure updateSeries(timeList: TList<double>; yList: TList<double>);
      procedure updateSerie(index: Integer; t: double; y: double);
      procedure Pause;
      procedure Resume;
      procedure Restart;
      procedure RestartSeries;
      procedure Run(t: double);
      procedure SaveToFile(FileName: String);
      procedure SaveToPNG(FileName: String);
      procedure SetXAxisRange(xMin, xMax: double);
      procedure SetYAxisRange(yMin, yMax: double);
      procedure plot;


      property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
      property Enabled: Boolean read GetEnabledTimer write SetEnabledTimer default false;
      property Interval: Cardinal read GetInterval write SetInterval default 100;
      property ReferenceLegend: TPivot read GetReferenceLegend write SetReferenceLegend;
 //     property Parent: TWinControl read FParent write SetAParent;
      property Parent: TWebControl read FParent write SetAParent;

    published
      property AxisColor: TColor read GetAxisColor write SetAxisColor;
      property AxisStrokeWidth: Byte read GetStroke write SetStroke;
      property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
      property ChartTitle: String read GetChartTitle write SetChartTitle;
      property ChartTitleColor: TColor read GetChartTitleColor write SetChartTitleColor;
      property DeltaX: Single read GetDeltaX write SetDeltaX;
   //   property ExternalTimer: TTimer read GetTimer write SetTimer default nil;
      property ExternalTimer: TWebTimer read GetTimer write SetTimer default nil;
      property GridColor: TColor read GetGridColor write SetGridColor;
      property GridStrokeWidth: Byte read GetGridStrokeWidth write SetGridStrokeWidth;
      property LegendFontColor: TColor read GetLegendFontColor write SetLegendFontColor;
      property LegendBackgroundColor: TColor read GetLegendBackgroundColor write SetLegendBackgroundColor;
      property LegendReference: TValuesLegend read GetLegendPosition write SetLegendPosition;
      property LegendPosX: Single read GetLegendPosX write SetLegendPosX;
      property LegendPosY: Single read GetLegendPosY write SetLegendPosY;
      property LegendVisible: Boolean read GetLegendVisible write SetLegendVisible;
      property OnMouseWorldMove: TMouseMoveEvent read FMouseMoveEvent write SetFMouseMoveEvent;
      property PlotPanelBackgroundColor: TColor read GetPanelBackgroundColor write SetPanelBackgroundColor;
      property XAxisCaption: String read GetXAxisCaption write SetXAxisCaption;
      property YAxisCaption: String read GetYAxisCaption write SetYAxisCaption;
      property XAxisTicks: Integer read GetXAxisTicks write SetXAxisTicks;
      property YAxisTicks: Integer read GetYAxisTicks write SetYAxisTicks;

      property YAxisMin: Double read GetYAxisMin write SetYAxisMin;
      property YAxisMax: Double read GetYAxisMax write SetYAxisMax;
      property XAxisMax: Double read GetXAxisMax write SetXAxisMax;
      property Align;
end;

//procedure Register;
implementation
uses Math;
{
procedure Register;
begin
  RegisterComponents('UofW', [TVCLScrollingChart]);
end;
  }
//procedure TVCLScrollingChart.SetAParent(val: TWinControl);
procedure TWebScrollingChart.SetAParent(val: TWebControl);
var
  Panel: TWebPanel; //TPanel;
  GB: TWebGroupBox; // TGroupBox;
begin
   if val <> FParent then
     begin
       if val is TWebPanel then
         begin
           Panel := val as TWebPanel;
        //   Panel.ParentBackground := false;
         end
   //    else if val is TGroupBox then
       else if val is TWebGroupBox then
         begin
           GB := val as TWebGroupBox;
        //   GB.ParentBackground := false;
         end;
       FParent := val;
     //  FParent.DoubleBuffered := true;
     //  FParent.ParentDoubleBuffered := true;
       SetParent(val);    // ????
     end;
end;

function TWebScrollingChart.GetLegendVisible: Boolean;
begin
  Result := legend.visible;
end;
procedure TWebScrollingChart.SetLegendVisible(val: Boolean);
begin
  if val <> legend.visible then
    begin
      legend.visible := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetLegendFontColor: TColor;
begin
  Result := legend.FontColor;
end;
procedure TWebScrollingChart.SetLegendFontColor(val: TColor);
begin
   if val <> legend.FontColor then
     begin
       legend.FontColor := val;
       Paint;
     end;
end;
function TWebScrollingChart.GetLegendBackgroundColor: TColor;
begin
   Result := legend.backgroundColor;
end;
procedure TWebScrollingChart.SetLegendBackgroundColor(val: TColor);
begin
   if val <> legend.backgroundColor then
     begin
       legend.backgroundColor := val;
       Paint;
     end;
end;
function TWebScrollingChart.GetDeltaX: Single;
begin
  Result := plane.DeltaX;
end;
procedure TWebScrollingChart.SetDeltaX(val: Single);
begin
   if val <> plane.DeltaX then
     begin
       plane.DeltaX := val;
       Rebuild;
     end;
end;
function TWebScrollingChart.GetReferenceLegend: TPivot;
begin
  Result := legend.pivot;
end;
function TWebScrollingChart.GetXAxisCaption: String;
begin
  Result := xAxis.caption;
end;
procedure TWebScrollingChart.SetXAxisCaption(val: String);
begin
  if val <> xAxis.caption then
    begin
      xAxis.caption := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetYAxisCaption: String;
begin
  Result := yAxis.caption;
end;
procedure TWebScrollingChart.SetYAxisCaption(val: String);
begin
  if val <> yAxis.caption then
    begin
      yAxis.caption := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetChartTitleColor: TColor;
begin
  Result := title.fontColor;
end;
procedure TWebScrollingChart.SetChartTitleColor(val: TColor);
begin
  if val <> title.fontColor then
    begin
      title.fontColor := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetChartTitle: String;
begin
  Result := title.text;
end;
procedure TWebScrollingChart.SetChartTitle(val: String);
begin
  if val <> title.text then
    begin
      title.text := val;
      Paint;
    end;
end;
procedure TWebScrollingChart.SetReferenceLegend(val: TPivot);
var
  k: Integer;
begin
  k := legend.getPos(val);
  if k <> legend.Position then
    begin
      legend.Position := k;
      Paint;
    end;
end;
function TWebScrollingChart.GetLegendPosX: Single;
begin
  Result := legend.x;
end;
procedure TWebScrollingChart.SetLegendPosX(val: Single);
begin
  if val <> legend.x then
    begin
      legend.x := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetLegendPosY: Single;
begin
   Result := legend.y;
end;
procedure TWebScrollingChart.SetLegendPosY(val: Single);
begin
  if val <> legend.y then
    begin
      legend.y := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetLegendPosition: TValuesLegend;
begin
  Result := legend.Position;
end;
procedure TWebScrollingChart.SetLegendPosition(val: TValuesLegend);
begin
  if val <> legend.Position then
    begin
      legend.Position := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetXAxisMax: Double;
begin
   Result := plane.x + plane.width;
end;
procedure TWebScrollingChart.SetXAxisMax(val: Double);
begin
  if XAxisMax <> val then
    begin
       setXAxisRange(plane.x, val);
       Rebuild;
    end;
end;
function TWebScrollingChart.GetYAxisMax: Double;
begin
  Result := plane.y + plane.height;
end;
procedure TWebScrollingChart.SetYAxisMax(val: Double);
begin
  if YAxisMax <> val then
    begin
      setYAxisRange(plane.y, val);
      Rebuild;
    end;
end;

function TWebScrollingChart.GetYAxisMin: Double;
begin
  Result := plane.y;
end;
procedure TWebScrollingChart.SetYAxisMin(val: Double);
begin
  if val <> plane.y then
    begin
      setYAxisRange(val, plane.y + plane.height);
      Rebuild;
    end;
end;
function TWebScrollingChart.GetYAxisTicks: Integer;
begin
  Result := plane.YAxisTicks;
end;
procedure TWebScrollingChart.SetYAxisTicks(val: Integer);
begin
  if val <> plane.YAxisTicks then
    begin
      plane.YAxisTicks := val;
      Rebuild;
    end;
end;
function TWebScrollingChart.GetXAxisTicks: Integer;
begin
  Result := plane.XAxisTicks;
end;
procedure TWebScrollingChart.SetXAxisTicks(val: Integer);
begin
  if val <> plane.XAxisTicks then
    begin
      plane.XAxisTicks := val;
      Rebuild;
    end;
end;
function TWebScrollingChart.GetBackgroundColor: TColor;
begin
  Result := globaldata.BackgroundColor;
end;
procedure TWebScrollingChart.SetBackgroundColor(val: TColor);
begin
  if val <> globaldata.BackgroundColor then
    begin
      globaldata.BackgroundColor := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetPanelBackgroundColor: TColor;
begin
  Result := globaldata.plotPanelBackgroundColor;
end;
procedure TWebScrollingChart.SetPanelBackgroundColor(val: TColor);
begin
  if val <> globaldata.plotPanelBackgroundColor then
    begin
      globaldata.plotPanelBackgroundColor := val;
      Paint;
     // self.Invalidate; // Not necessary ?
    end;
end;
procedure TWebScrollingChart.SetFMouseMoveEvent(val: TMouseMoveEvent);
begin
  FMouseMoveEvent := val;
  if Assigned(FMouseMoveEvent) then
    begin
      OnMouseMove := chartOnMouseMove;
      OnMouseLeave := chartOnMouseLeave;
    end
  else
    begin
      OnMouseMove := nil;
      OnMouseLeave := nil;
      if stage <> nil then stage.rightBox.graph.FOnComplete := nil;
    end;
end;
procedure TWebScrollingChart.chartOnMouseLeave(Sender: TObject);
begin
  if stage <> nil then
    stage.rightBox.graph.FOnComplete := nil;
end;
procedure TWebScrollingChart.OnComplete(X, Y: single);
begin
  FMouseMoveEvent(Obj, Sh, X, Y);
  // If you don't want the OnMouseWorldMove Event to detect
  // relative movement between the mouse and the plane.
  // Uncomment this code
  {
  if stage <> nil then
    stage.rightBox.graph.FOnComplete := nil;
    }
end;
procedure TWebScrollingChart.chartOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPointF;
begin
  if stage = nil then Exit;
  if (Assigned(FMouseMoveEvent)) and (stage.rightBox.graph.isInside(X, Y)) then
     begin
       if timer.Enabled then
         begin
           if not Assigned(stage.rightBox.graph.FOnComplete) then
             stage.rightBox.graph.FOnComplete:= OnComplete;
           stage.rightBox.graph.scanXY(X, Y);
           Obj := Sender;
           Sh := Shift;
         end
       else
         begin
           stage.rightBox.graph.FOnComplete := nil;
           P := stage.rightBox.graph.worldCoordinates(X, Y);
           FMouseMoveEvent(Sender, Shift, P.X, P.Y);
         end;
     end
   else
     if Assigned(stage.rightBox.graph.FOnComplete) then
       stage.rightBox.graph.FOnComplete := nil;
end;
           // Do not use for now
{procedure TWebScrollingChart.ConvertAndSaveToPNG(BmpSrc: TBitmap; sFilename: String);
var
 // PNGDest: TPngImage;
 PNGDest: TPngImage;
begin
  PNGDest := TPngImage.Create;
  try
    PNGDest.Assign(BmpSrc);
    PNGDest.SaveToFile(sFilename);
  finally
    PNGDest.Free;
  end;
end;}
procedure TWebScrollingChart.saveToPNG(FileName: String);
var
  currentCanvas: TCanvas;
  status: Boolean;
  Bmp: TBitmap;
begin
  status := timer.Enabled;
  timer.Enabled := false;
  currentCanvas := stage.data.canvas;
  Bmp := TBitmap.Create;
  Bmp.Width := Width;
  Bmp.Height := Height;
  stage.data.canvas := Bmp.Canvas;
  Clear(Bmp.Canvas, BackgroundColor);
  stage.Draw;
  stage.data.canvas := currentCanvas;
  paint;
 // ConvertAndSaveToPNG(Bmp, Filename); // DO not use for now
  Bmp.Free;
  timer.Enabled := status;
end;
procedure TWebScrollingChart.saveToFile(FileName: String);
var
  i, j: Integer;
 // col: PDataCol;
 // row: PData;
  col: TDataCol;
  row: TData;
  L, aux: TstringList;
begin
  aux := TStringList.Create;
  L := TStringList.Create;
  L.Add('"Time",');
  for i := 0 to length(series) - 1 do
    begin
      L[L.Count - 1] := L[L.Count - 1] + '"' + series[i].name + '"';
      if i <> length(series) - 1 then L[L.Count - 1] := L[L.Count - 1] + ',';
    end;
  //for i :=0 to length(globaldata.dataSource.cols) - 1 do
  for i :=0 to globaldata.dataSource.cols.Count - 1 do
    begin
      col := globaldata.dataSource.cols[i];
      for j := 0 to length(series) - 1 do aux.Values[series[j].name] := '""';
    //  L.Add(plane.niceNumX(col^.x) + ',');
    //  for j := 0 to length(col^.rows) - 1 do
      L.Add(plane.niceNumX(col.x) + ',');
      for j := 0 to col.rows.Count - 1 do
        begin
         // row := col^.rows[j];
         // aux.Values[row^.serie.name] := plane.niceNumY(row^.y);
          row := col.rows[j];
          aux.Values[row.serie.name] := plane.niceNumY(row.y);
        end;
      for j := 0 to length(series) - 1 do
        begin
          L[L.Count - 1] := L[L.Count - 1] + aux.Values[series[j].name];
          if j <> length(series) - 1 then L[L.Count - 1] := L[L.Count - 1] + ',';
        end;
    end;
  aux.Free;
  L.SaveToFile(FileName);
  L.Free;
end;
function TWebScrollingChart.GetStroke: Byte;
begin
  Result := yAxis.lineWidth;
end;
procedure TWebScrollingChart.SetStroke(val: Byte);
begin
  if val <> xAxis.lineWidth then
    begin
      xAxis.lineWidth := val;
      yAxis.lineWidth := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetAxisColor: TColor;
begin
  Result := xAxis.color;
end;
procedure TWebScrollingChart.SetAxisColor(Val: TColor);
begin
  if val <> xAxis.color then
    begin
      xAxis.color := val;
      yAxis.color := val;
      Paint;
    end;
end;
function TWebScrollingChart.GetGridStrokeWidth: Byte;
begin
  Result := grid.lineWidth;
end;
procedure TWebScrollingChart.SetGridStrokeWidth(Val: Byte);
begin
  if val <> grid.lineWidth then
    begin
      grid.lineWidth := Val;
      Paint;
    end;
end;
function TWebScrollingChart.GetGridColor: TColor;
begin
  Result := grid.color;
end;
procedure TWebScrollingChart.SetGridColor(Val: TColor);
begin
  if val <> grid.color then
    begin
      grid.color := val;
      Paint;
    end;
end;
procedure TWebScrollingChart.SetDefaultTimer; // Not needed, external class passes it in
begin
  if timer = nil then
    begin
 //     timer := TTimer.Create(self);
      timer.Enabled := false;
      timer.Interval := 100;
    end;
end;
//procedure TWebScrollingChart.SetTimer(Value: TTimer);
procedure TWebScrollingChart.SetTimer(Value: TWebTimer);
begin
  timer := Value;
end;
procedure TWebScrollingChart.restart;
begin
  if stage = nil then exit;
  restartSeries;
  globaldata.dataSource.reset;
  plane.x := xMinIni;
  stage.rightBox.bottomBox.axisX.reset;
  stage.rightBox.graph.gridX.reset;
end;
//function TWebScrollingChart.GetTimer: TTimer;
function TWebScrollingChart.GetTimer: TWebTimer;
begin
  Result := timer;
end;
procedure TWebScrollingChart.SetOnTimer(Value: TNotifyEvent);
begin
//  SetDefaultTimer;
  timer.OnTimer := Value;
end;
function TWebScrollingChart.GetOnTimer: TNotifyEvent;
begin
  Result := timer.OnTimer;
end;
procedure TWebScrollingChart.SetEnabledTimer(Value: Boolean);
begin
 // SetDefaultTimer;
  timer.Enabled := Value;
end;
function TWebScrollingChart.GetEnabledTimer: Boolean;
begin
  Result := timer.Enabled;
end;
procedure TWebScrollingChart.SetInterval(Value: Cardinal);
begin
//  SetDefaultTimer;
  timer.Interval := Value;
end;
function TWebScrollingChart.GetInterval: Cardinal;
begin
  Result := timer.Interval;
end;
procedure TWebScrollingChart.pause;
begin
  timer.Enabled := false;
end;
procedure TWebScrollingChart.resume;
begin
  timer.Enabled := true;
end;
function TWebScrollingChart.IndexOf(aname: String): Integer;
var
  j: Integer;
begin
  for j := 0 to length(series) - 1 do
     if series[j].name.ToLower = aname.ToLower then
       begin
         Result := j;
         Exit;
       end;
  Result := -1;
end;
function TWebScrollingChart.addSerieByName(aname: String; functionTime: TFunctionTime = nil): TDataSerie;
begin
   Result:= TDataSerie.Create;
   Result.name := aname;
   Result.functionTime := functionTime;
   Result.dataSource := globalData.dataSource;
   series := series + [Result];
end;

function TWebScrollingChart.addSerie(aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie;
var
  temp: String;
begin
  if aname = TConst.DEFAULT_NAME_SERIES then
    temp := Format(aname, [length(series) + 1])
  else
    temp := aname;
  if IndexOf(temp) = -1 then
      Result := addSerieByName(temp)
  else
    begin
      ShowMessage(TConst.ERROR_NAME_SERIE);
      Result := nil;
    end;
end;

function TWebScrollingChart.addSerie(functionTime: TFunctionTime; aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie;
var
  temp: String;
begin
  if aname = TConst.DEFAULT_NAME_SERIES then
    temp := Format(aname, [length(series) + 1])
  else
    temp := aname;
  if IndexOf(temp) = -1 then
      Result := addSerieByName(temp, functionTime)
  else
    begin
      ShowMessage(TConst.ERROR_NAME_SERIE);
      Result := nil;
    end;
end;
procedure TWebScrollingChart.deleteSerie(index: Integer);
begin
  if (index >= 0) and (index <= length(series) - 1) then
    begin
      globalData.dataSource.deleteSerie(series[index]);
      series[index].Destroy;
      Delete(series, index, 1);
      Paint;
    end;
end;
procedure TWebScrollingChart.deleteSeries;
var
  j: Integer;
begin
  globalData.dataSource.deleteAllSeries;
  for j := length(series) - 1 downto 0 do
    begin
      series[j].Destroy;
      Delete(series, j, 1);
    end;
  Paint;
end;

procedure TWebScrollingChart.updateSeries(timeList: TList<double>; yList: TList<double>);
var i: integer;
begin
  for I := 0 to Length(self.series) -1 do
  begin
  if self.GetYAxisMax < yList[i] then yList[i] := self.GetYAxisMax;
  if self.GetYAxisMin > yList[i] then yList[i] := self.GetYAxisMin;
  if (i < timeList.count) and (i < yList.Count) then
    self.series[i].add(timeList[i], yList[i]);
  end;
end;

procedure TWebScrollingChart.updateSerie(index: Integer; t: double; y: double);
begin
console.log(' Y axisMin: ', self.GetYAxisMin);
  if not self.autoScaleUp then
    if self.GetYAxisMax < y then y := self.GetYAxisMax;
  if not self.autoScaleDown then
    if self.GetYAxisMin > y then y := self.GetYAxisMin;
  if length(self.series) > index then
    self.series[index].add(t, y);
end;

procedure TWebScrollingChart.restartSeries;
var
  j: Integer;
begin
  globalData.dataSource.deleteAllSeries;
  for j := length(series) - 1 downto 0 do series[j].init;
end;
procedure TWebScrollingChart.run(t: double);
var
  j: Integer;
  y: double;
begin
  globaldata.dataSource.addX(t);
  for j := 0 to length(series) - 1 do
    if Assigned(series[j].functionTime) then
      begin
        y := series[j].functionTime(t);
        series[j].addY(y);
      end;
  plot;
end;
procedure TWebScrollingChart.plot;
begin
 // repaint;
  self.Invalidate;  // Needed
  stage.checkLimits;
  time := time + timer.Interval/1000;
end;
procedure TWebScrollingChart.setDefaultValues;
begin
  setXAxisRange(TConst.DEFAULT_X_MIN, TConst.DEFAULT_X_MAX);
  setYAxisRange(TConst.DEFAULT_Y_MIN, TConst.DEFAULT_Y_MAX);
  //self.autoScaleUp := false; Not needed?
  //self.autoScaleDown := false;
  time := 0;
end;
procedure TWebScrollingChart.setXAxisRange(xMin, xMax: double);
begin
  xMinIni := xMin;
  time := xMin;
  plane.setXAxisRange(xMin, xMax);
end;
procedure TWebScrollingChart.setYAxisRange(yMin, yMax: double);
begin
  plane.initialValueYMin := yMin;
  plane.initialValueYMax := yMax;
  plane.setYAxisRange(yMin, yMax);
end;
procedure TWebScrollingChart.Resize;
begin
  inherited;
  if globalData = nil then Exit;
  if stage <> nil then
    begin
      globalData.chartWidth := Width;
      globalData.chartHeight := Height;
     // console.log('TWebScrollingChart.Resize: globalData width, height: ',globalData.chartWidth,', ', globalData.chartHeight);
      stage.width := Width - 2*TConst.MARGIN_X;
      stage.height := Height - 2*TConst.MARGIN_Y;
      stage.x := TConst.MARGIN_X;
      stage.y := TConst.MARGIN_Y;
      stage.resize;
     // self.Invalidate; // necessary?
      //showMessage('Resize');
    end;
end;
     // Not needed ?
procedure TWebScrollingChart.getForm(AOwner: TComponent);
var
  //F: TWincontrol;
  F: TWebControl;
begin
 // if (AOwner is TWincontrol) then
  if (AOwner is TWebControl) then
    begin
 //     F := AOwner as TWincontrol;
     // F := AOwner as TWebControl;
     // F.DoubleBuffered := true;
     // F.ParentDoubleBuffered := true;
    end
end;

constructor TWebScrollingChart.Create(AOwner: TComponent);
var newPanel: TWebPanel;
begin
  inherited;
 // getForm(AOwner);
  if AOwner.ClassType = TWebPanel then
    begin
    newPanel := AOwner as TWebPanel;
    self.Width := newPanel.width;
    self.height := newPanel.height;
    end
  else
    begin
    self.width := 320;
    self.height := 240;
    end;
  Init;
end;
procedure TWebScrollingChart.Finalize;
begin
     //timer.Enabled := false;
  //OnPaint := nil;
  //OnResize := nil;
  if globalData <> nil then globalData.Destroy;
  if stage <> nil then stage.Destroy;
  FreeSeries;
  if legend <> nil then legend.Destroy;
   //timer.destroy;
end;
procedure TWebScrollingChart.Init;
begin
  //Enabled := false;
  if globalData = nil then
     begin
       globalData := TGlobalData.Create;
       title := globalData.title;
       globalData.dataSource.Redraw := Paint;
       plane := globalData.plane;
       xAxis := plane.xAxis;
       yAxis := plane.yAxis;
       grid := plane.grid;
       setDefaultValues;
       series:= [];
       legend := TLegend.Create;
       timer := nil;
   end;
end;
procedure TWebScrollingChart.initStage;
begin
  if stage = nil then
    begin
     // SetDefaultTimer;
      stage := TStage.Create(Width - 2*TConst.MARGIN_X, Height - 2*TConst.MARGIN_Y, self);
      stage.x := TConst.MARGIN_X;
      stage.y := TConst.MARGIN_Y;
    end;
end;
procedure TWebScrollingChart.Rebuild;
begin
  stage.Destroy;
  stage := Nil;
  Paint;
end;

procedure TWebScrollingChart.Paint;
begin
 // Clear(Canvas, backgroundColor);  //No
  if globalData = nil then Exit;
  globalData.chartWidth := Width;    // Plot area background
  globalData.chartHeight := Height;
  globalData.canvas := Canvas;
  InitStage;
  Clear(Canvas, backgroundColor);   // Paints whole panel backgroundColor
  //self.Invalidate;  // necessary ?
  stage.Draw;
end;
procedure TWebScrollingChart.Clear(FCanvas: TCanvas; cl: tcolor);
begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := cl;
//  FCanvas.FillRect(TRect.Create(0, 0, width, height));
  FCanvas.FillRect(createCanvasRectF(0, 0, width, height));
end;
procedure TWebScrollingChart.FreeSeries;
var
  j: Integer;
begin
  for j := 0 to length(series) - 1 do series[j].Destroy;
  series := nil;
end;
destructor  TWebScrollingChart.Destroy;
begin
   Finalize;
   inherited;
end;
initialization
end.
