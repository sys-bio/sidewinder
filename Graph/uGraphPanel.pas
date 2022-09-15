unit uGraphPanel;

interface
uses SysUtils, Classes, Graphics, Controls, System.Types, System.Generics.Collections,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms, JS, Web,
     uWebScrollingChart, uWebGlobalData, uSidewinderTypes;

const SERIESCOLORS: array[0..9] of integer = ( clRed, clBlue, clGreen,clBlack,
                                   clAqua, clGray,clPurple,clOlive, clLime,clSkyBlue);

type

TGraphPanel = class (TWebPanel)
private
  chart: TWebScrollingChart;
  seriesStrList: TList<string>; // series label list, if '' then do not plot
 //pnlChart: TWebPanel;
public
  constructor create(newParent: TWebPanel; graphPosition: integer; yMax: double);
  procedure initializePlot(newVarStrList: TList<string>; newYMax: double; newYMin: double;
  autoUp: boolean; autoDown: boolean; newDelta: double; newBkgrndColor: TColor);
  procedure setAutoScaleUp(autoScale: boolean); // true: autoscale
  procedure setAutoScaleDown(autoScale: boolean); // true: autoscale
  procedure addChartSerie(varStr: string; maxYVal: double); // need max Y if autoScale off
  procedure setTimer(newTimer: TWebTimer); // Not sure this is necessary
  procedure setSeriesColors();
  procedure setSerieColor(index: integer; newColor: TColor);
  procedure deleteChartSerie(index: integer);
  procedure deleteChartSeries();
  procedure restartChart(newInterval: double);
  procedure setChartDelta(newDelta: double);
  procedure setChartWidth(newWidth: integer);
  procedure setChartTimeInterval(newInterval: double);
  function  getChartTimeInterval(): double;
  procedure setYMax(newYMax: double);
  procedure adjustPlotHeight(numPlots: integer; newHeight: integer);// adjust height based on numbre of plots

  procedure getVals( newTime: Double; newVals: TVarNameValList);// Get new values (species amt) from simulation run
end;

implementation

constructor TGraphPanel.create(newParent: TWebPanel; graphPosition: integer; yMax: double);
begin
  //self.pnlChart := TWebPanel.Create(owner);
  inherited create(newParent);
  self.SetParent(newParent);
  self.tag := graphPosition;
  self.Width := newParent.Width;
  self.Anchors := [akLeft,akRight,akTop];
  self.Height := 200;//round(panelH/3);
  self.Left := 10; //10 gap between the network canvas and plot
  self.Top := 4 + self.Height*(graphPosition-1);

  self.chart := TWebScrollingChart.Create(self);
  self.chart.Parent := self;
  if yMax > 0 then self.chart.YAxisMax := yMax; // need a default ?


end;

procedure TGraphPanel.initializePlot( newVarStrList: TList<string>; newYMax: double;
          newYMin: double; autoUp: boolean; autoDown: boolean; newDelta: double;
          newBkgrndColor: TColor);
var i: integer;
begin
  self.seriesStrList := newVarStrList;
  self.chart.autoScaleUp := autoUp;
  self.chart.autoScaleDown := autoDown;
  self.chart.YAxisMax := newYMax;
  self.chart.YAxisMin := newYMin;
  self.setChartDelta(newDelta);
  self.setChartTimeInterval(newDelta);
  self.chart.SetXAxisMax(TConst.DEFAULT_X_POINTS_DISPLAYED *newDelta); // deltaX same as interval
  if newBkgrndColor < 1 then self.chart.BackgroundColor := clNavy // clNavy
  else self.chart.BackgroundColor := newBkgrndColor;
  self.chart.LegendBackgroundColor := clSilver;
  self.Color := clBlack;
  for i := 0 to newVarStrList.count -1 do
    begin
    if newVarStrList[i] <> '' then
      self.chart.AddSerie(newVarStrList[i]);
      self.setSerieColor(i,0);
    end;
end;

procedure TGraphPanel.setYMax(newYMax: double);
begin
  if self.chart.YAxisMin < newYMax then
    self.chart.YAxisMax := newYMax;
end;

procedure TGraphPanel.setChartWidth(newWidth: integer);
begin
  if newWidth <= self.width then self.chart.width := newWidth;
end;

procedure TGraphPanel.setSeriesColors();
var i: integer;
begin
  for i := 0 to length(self.chart.series)-1 do
    begin
    self.setSerieColor(i,0);
    end;
end;

procedure TGraphPanel.setSerieColor(index: Integer; newColor: TColor);
begin
  if index < length(self.chart.series) then
    begin
    if newColor > 1 then self.chart.series[index].color := newColor
    else
      begin
      if index < length(SERIESCOLORS)then self.chart.series[index].color := SERIESCOLORS[index]
      else self.chart.series[index].color := SERIESCOLORS[index mod length(SERIESCOLORS)];
      end;
    end;
end;

procedure TGraphPanel.setTimer(newTimer: TWebTimer);  // should not be necessary
begin
  self.chart.ExternalTimer := newTimer;
end;


procedure TGraphPanel.setChartTimeInterval(newInterval: double); // seconds
begin
  self.chart.SetInterval(round(newInterval*1000)); // convert to msec (integer)
end;

function  TGraphPanel.getChartTimeInterval(): double;
begin
  Result := self.chart.GetInterval / 1000;
end;


procedure TGraphPanel.getVals(newTime: Double; newVals: TVarNameValList); // callback
var i, j: integer;
begin
//  console.log('updating plot results....');
  for j := 0 to length(self.chart.series) -1 do
    begin
    if j < newVals.getNumPairs then
      self.chart.updateSerie(j, newTime, newVals.getNameValById(self.chart.series[j].name).Val );

    end;
  self.chart.plot;
  //self.chart.Invalidate;   // ??
end;

procedure TGraphPanel.setChartDelta(newDelta: double); // default is 0.1 (tenth of sec )
begin
if newDelta >0 then self.chart.DeltaX := newDelta  // integrator stepsize
else console.log('TGraphPanel.setChartDelta value is not greater than zero');
end;

procedure TGraphPanel.adjustPlotHeight(numPlots: integer; newHeight: integer);
begin
 // height := round(self.Height/numPlots);

  self.Height:= newHeight;
  self.Top:= 5 + newHeight*(self.tag -1);
  self.invalidate;

end;

procedure TGraphPanel.addChartSerie(varStr: string; maxYVal: double);
begin
  if maxYVal > self.chart.YAxisMax then self.chart.YAxisMax := maxYVal;

  self.chart.AddSerieByName(varStr);
end;

procedure TGraphPanel.deleteChartSerie(index: integer);
begin
  self.deleteChartSerie(index);
end;

procedure TGraphPanel.deleteChartSeries();
begin
  self.chart.DeleteSeries;
end;

procedure TGraphPanel.restartChart(newInterval: double);
begin
  self.setChartDelta(newInterval); // ? chat delta versus chart time interval?
  self.setChartTimeInterval(newInterval);
  self.chart.SetXAxisMax(TConst.DEFAULT_X_POINTS_DISPLAYED * newInterval);
  self.chart.Restart;
end;

procedure TGraphPanel.setAutoScaleUp(autoScale: boolean);
begin
  self.chart.autoScaleUp := autoScale;
end;

procedure TGraphPanel.setAutoScaleDown(autoScale: boolean);
begin
  self.chart.autoScaleDown := autoScale;
end;

end.
