unit uGraphPanel;

interface
uses SysUtils, Classes, Graphics, Controls, System.Types, System.Generics.Collections,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms, JS, Web,
     uWebScrollingChart, uWebGlobalData, ufYAxisMinMaxEdit,  uSidewinderTypes;

const SERIESCOLORS: array[0..10] of integer = ( clRed, clBlue, clGreen,clBlack,
                     clAqua, clGray,clPurple,clOlive, clLime,clSkyBlue, clYellow);
      EDIT_TYPE_DELETEPLOT = 0;
      EDIT_TYPE_SPECIES = 1;
      DEFAULT_Y_MAX = 10;

type
TEditGraphEvent = procedure(position: integer; editType: integer) of object;

TGraphPanel = class (TWebPanel)
private
  chart: TWebScrollingChart;
  seriesStrList: TList<string>; // series label list, if '' then do not plot
  lbEditGraph: TWebListBox;
  yMaximum: double;
  yMinimum: double;
  yLabel: string;
  xLabel: string;
  autoUp: boolean;  // Auto scale y axis up
  autoDown: boolean;
  timeDelta: double;
  chartBackGroundColor: TColor;
  fEditGraphEvent: TEditGraphEvent;

  procedure graphEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  procedure editGraphListBoxClick(Sender: TObject);

public
  userDeleteGraph: boolean; // true, user can delete graph (OnEditGraphEvent method required)
  userChangeVarSeries: boolean; // true, user can change var of series (OnEditGraphEvent method required)

  constructor create(newParent: TWebPanel; graphPosition: integer; yMax: double);
  procedure initializePlot(newVarStrList: TList<string>; newYMax: double; newYMin: double;
  newAutoUp: boolean; newAutoDown: boolean; newDelta: double; newBkgrndColor: TColor);
  procedure setAutoScaleUp(autoScale: boolean); // true: autoscale
  procedure setAutoScaleDown(autoScale: boolean); // true: autoscale
  procedure addChartSerie(varStr: string; maxYVal: double); // need max Y if autoScale off
  procedure setTimer(newTimer: TWebTimer); // Not sure this is necessary
  procedure setPanelColor( val: TColor ); // background color for TGraphPanel
  procedure setPanelHeight( newHeight: integer ); // Set height for panel that contains chart
  procedure setPanelTop( val: integer ); // set Top  relative to top of parent panel
  procedure setSeriesColors();
  procedure setSerieColor(index: integer; newColor: TColor);
  procedure deleteChartSerie(index: integer);
  procedure deleteChartSeries();
  procedure deleteChart(); // delete TWebScrollingChart
  procedure createChart();
  procedure setupChart(); // Setup chart based on existing values from and instance of TGraphPanel
  procedure restartChart(newInterval: double); // Needed ??
  procedure setChartDelta(newDelta: double);
  procedure setChartWidth(newWidth: integer);
  procedure setChartTimeInterval(newInterval: double);
  function  getChartTimeInterval(): double;
  procedure setYMax(newYMax: double);
  function  getYMax(): double;
  procedure adjustPanelHeight(newHeight: integer); // adjust height and top, uses self.tag as well
  procedure getVals( newTime: Double; newVals: TVarNameValList);// Get new values (species amt) from simulation run
  procedure notifyGraphEvent(plot_id: integer; eventType: integer);
  property OnEditGraphEvent: TEditGraphEvent read fEditGraphEvent write fEditGraphEvent;
end;

implementation

constructor TGraphPanel.create(newParent: TWebPanel; graphPosition: integer; yMax: double);
begin
  inherited create(newParent);
  self.SetParent(newParent);
  self.OnMouseDown := graphEditMouseDown;
  if graphPosition > -1 then self.tag := graphPosition
  else self.tag := 0;
  self.Width := newParent.Width;
  self.Anchors := [akLeft,akRight,akTop];
  self.Height := round(newParent.height/2); // Default
  self.Left := 10; // gap between panel to left and plot
  self.Top := 4 + self.Height*(graphPosition -1);
  self.Color := clwhite; // default
  self.chartBackGroundColor := -1;
  self.userDeleteGraph := false;
  self.userChangeVarSeries := false;
  self.yMinimum := 0;
  if yMax > 0 then self.yMaximum := yMax
  else self.yMaximum := DEFAULT_Y_MAX;
  self.createChart();
end;

 procedure TGraphPanel.createChart();
 begin
   try
     self.chart := TWebScrollingChart.Create(self);
     self.chart.Height := self.Height;
     self.chart.OnMouseClickEvent := self.graphEditMouseDown;
     self.chart.Parent := self;
     self.chart.YAxisMax := self.yMaximum;
   except
    on E: Exception do
      notifyUser(E.message);
  end;
 end;

procedure TGraphPanel.initializePlot( newVarStrList: TList<string>; newYMax: double;
          newYMin: double; newAutoUp: boolean; newAutoDown: boolean; newDelta: double;
          newBkgrndColor: TColor);
var i: integer;
begin
  self.seriesStrList := newVarStrList;
  if newYMax > 0 then self.yMaximum := newYmax; // Assume max value > zero
  self.yMinimum := newYMin;
  self.autoUp := newAutoUp;
  self.autoDown := newAutoDown;
  self.timeDelta := newDelta;
  self.chartBackGroundColor := newBkgrndColor;
  //self.Color := clBlack;
  self.setupChart();
end;

procedure TGraphPanel.setupChart;
var i: integer;
begin
  self.yLabel := 'Conc';  // Default
  self.xLabel := 'Time (sec)'; // Default
  self.chart.autoScaleUp := self.autoUp;
  self.chart.autoScaleDown := self.autoDown;
  self.chart.YAxisMax := self.yMaximum;
  self.chart.YAxisMin := self.yMinimum;
  self.chart.SetChartTitle(''); // Do not use, if so then need to adjust plot grid height
  self.chart.setYAxisCaption(''); // Add to bottom, xaxis label. Cannot rotate label in HTML ?
  self.chart.SetXAxisCaption( self.yLabel + ' vs. '+ self.xLabel );
  self.setChartDelta(self.timeDelta);
  self.setChartTimeInterval(self.timeDelta);
  self.chart.SetXAxisMax(TConst.DEFAULT_X_POINTS_DISPLAYED *self.timeDelta); // deltaX same as interval
  if self.chartBackGroundColor < 1 then self.chart.BackgroundColor := clNavy
  else self.chart.BackgroundColor := self.chartBackGroundColor;
  self.chart.LegendBackgroundColor := clSilver;
  self.chart.LegendPosX := 0;
  self.chart.LegendPosY := 15;

  for i := 0 to self.seriesStrList.count -1 do
    begin
    if self.seriesStrList[i] <> '' then
      self.chart.AddSerie(self.seriesStrList[i]);
    end;
  self.setSeriesColors;

end;

procedure TGraphPanel.setYMax(newYMax: double);
begin
  if self.chart.YAxisMin < newYMax then
    self.chart.YAxisMax := newYMax;
end;

function  TGraphPanel.getYMax(): double;
begin
  Result := self.yMaximum;
end;

procedure TGraphPanel.setPanelHeight( newHeight: integer ); // Set height for panel that contains chart
begin
  if newHeight >0 then
    begin
    self.Height := newHeight;
    self.chart.Height := self.Height;
    end;
  self.Invalidate;
end;

procedure TGraphPanel.adjustPanelHeight( newHeight: integer );// adjusts based on tag value
begin
  self.Height:= newHeight;
  self.Top:= 5 + newHeight*(self.tag -1);
  self.chart.Height := newHeight;
  self.invalidate;

end;

procedure TGraphPanel.setChartWidth(newWidth: integer);
begin
  if newWidth <= self.width then self.chart.width := newWidth;
end;

procedure TGraphPanel.setPanelTop( val: integer ); // set Top  relative to top of parent panel
begin
  if val > -1 then self.Top := val
  else self.Top := 4;
end;

procedure TGraphPanel.setPanelColor( val: TColor); // background color for TGraphPanel
begin
  if val >0 then self.color := val;
  self.Invalidate;
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
var j: integer;
begin
  if index < length(self.chart.series) then
    begin
    if newColor > 1 then self.chart.series[index].color := newColor
    else
      begin
      for j := 0 to self.seriesStrList.Count -1 do  // Want all charts to use same color for var
        begin
         if self.chart.series[index].name = self.seriesStrList[j] then
           begin
           if index < length(SERIESCOLORS)then self.chart.series[index].color := SERIESCOLORS[j]
           else self.chart.series[index].color := SERIESCOLORS[j mod length(SERIESCOLORS)];
           end;
        end;
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

procedure TGraphPanel.deleteChart();
begin
  if self.chart <> nil then
    begin
    self.deleteChartSeries;
    self.chart.Destroy;
    end;

end;

procedure TGraphPanel.restartChart(newInterval: double); // Needed ? Issue resetting xAis labels
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

procedure TGraphPanel.graphEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
var editList: TStringList;
begin
  self.lbEditGraph := TWebListBox.Create(self);
  self.lbEditGraph.parent := self;
  self.lbEditGraph.tag := self.tag;
  self.lbEditGraph.ElementClassName := 'list-group form-control-sm bg-dark text-white';
  self.lbEditGraph.OnClick := editGraphListBoxClick;
  editList := TStringList.create();
  editList.Add('Toggle legend.');
  editList.Add('Toggle autoscale.');
  editList.Add('Set Y max/min.');
  if self.userChangeVarSeries then
    editList.Add('Change plot species.');
  if self.userDeleteGraph then
    editList.Add('Delete plot.');
  editList.Add('Cancel');
  self.lbEditGraph.Items := editList;
  self.lbEditGraph.Top := 10;
  self.lbEditGraph.left := 10;
  self.lbEditGraph.Height := 120;
  self.lbEditGraph.Width := 140;
  self.lbEditGraph.bringToFront;
  self.lbEditGraph.visible := true;
end;

procedure TGraphPanel.editGraphListBoxClick(Sender: TObject);
begin
  case self.lbEditGraph.ItemIndex of
    0: begin                                 // toggle legend
       if self.chart.getLegendVisible then
         self.chart.SetLegendVisible(false)
       else self.chart.SetLegendVisible(true);
       self.lbEditGraph.Destroy;
       end;
    1: if self.chart.autoScaleUp then        // toggle autoscale
        begin
        self.chart.autoScaleUp := false;
        self.chart.autoScaleDown := false;
        self.lbEditGraph.Destroy;
        end
      else
        begin
        self.chart.autoScaleUp := true;
        self.chart.autoScaleDown := true;
        self.lbEditGraph.Destroy;
        end;
    2: begin                            // Set Y max/min
       self.chart.userUpdateMinMax;
       self.lbEditGraph.Destroy;
       end;
    3: begin                         // Done external to TGraphPanel
       if self.userChangeVarSeries then
         self.notifyGraphEvent(self.tag, EDIT_TYPE_SPECIES)
       else if self.userDeleteGraph then
         self.notifyGraphEvent(self.tag, EDIT_TYPE_DELETEPLOT);
       end;
    4: if self.userDeleteGraph then  // Done external to TGraphPanel
         self.notifyGraphEvent(self.tag, EDIT_TYPE_DELETEPLOT);
    else self.lbEditGraph.Destroy;
  end;
 
end;


procedure TGraphPanel.notifyGraphEvent(plot_id: integer; eventType: integer);
begin
   if Assigned(fEditGraphEvent) then
    fEditGraphEvent(plot_id, eventType);
end;

end.
