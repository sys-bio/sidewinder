unit uGraphPanel;

interface
uses SysUtils, Classes, Graphics, Controls, System.Types, System.Generics.Collections,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms, JS, Web,
     uWebScrollingChart, uSidewinderTypes;

const SERIESCOLORS: array[0..9] of integer = ( clRed, clBlue, clGreen,clBlack,
                                   clAqua, clGray,clPurple,clOlive, clLime,clSkyBlue);

type

TGraphPanel = class (TWebPanel)
private
  chart: TWebScrollingChart;
  seriesStrList: TList<string>; // series label list, if '' then do not plot
 //pnlChart: TWebPanel;
public
  constructor create(newOwner: TWebPanel; graphPosition: integer);
  procedure initializePlot(newVarStrList: TList<string>; newYMax: double; newYMin: double;
  autoUp: boolean; autoDown: boolean; newDelta: double; newBkgrndColor: TColor);
  procedure setAutoScaleUp(autoScale: boolean); // true: autoscale
  procedure addChartSerie(varStr: string; maxYVal: double); // need max Y if autoScale off
  procedure setTimer(newTimer: TWebTimer); // Not sure this is necessary
  procedure setSeriesColors();
  procedure setSerieColor(index: integer; newColor: TColor);
  procedure deleteChartSerie(index: integer);
  procedure deleteChartSeries();
  procedure setChartDelta(newDelta: double);
  procedure setChartWidth();
  procedure adjustPlotHeight(numPlots: integer; newHeight: integer);// adjust height based on numbre of plots

  procedure getVals( newTime: Double; newVals: TVarNameValList);// Get new values (species amt) from simulation run
end;

implementation

constructor TGraphPanel.create(newOwner: TWebPanel; graphPosition: integer);
begin
  //self.pnlChart := TWebPanel.Create(owner);
  inherited create(newOwner);
  self.SetParent(newOwner);
  self.tag := graphPosition;
  self.chart := TWebScrollingChart.Create(self);
  self.chart.parent := self;

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
  if newBkgrndColor < 1 then self.chart.BackgroundColor := clGray // clNavy
  else self.chart.BackgroundColor := newBkgrndColor;
  self.chart.LegendBackgroundColor := clSilver;
  self.Color := clBlack;
  for i := 0 to newVarStrList.count -1 do
    begin
    if newVarStrList[i] <> '' then
      self.chart.AddSerie(newVarStrList[i]);
    end;
end;

procedure TGraphPanel.setChartWidth();
begin
  self.chart.width := self.Width;
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

procedure TGraphPanel.setTimer(newTimer: TWebTimer);
begin
  self.chart.ExternalTimer := newTimer;
end;

procedure TGraphPanel.getVals(newTime: Double; newVals: TVarNameValList); // callback
var i, j: integer;
begin
  for j := 0 to length(self.chart.series) -1 do
    begin
    if j < newVals.getNumPairs then
      self.chart.updateSerie(j, newTime, newVals.getNameValById(self.chart.series[j].name).Val );

    end;
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

procedure TGraphPanel.deleteChartSerie(index: integer);
begin
  self.deleteChartSerie(index);
end;

procedure TGraphPanel.deleteChartSeries();
begin
  self.chart.DeleteSeries;
end;

end.
