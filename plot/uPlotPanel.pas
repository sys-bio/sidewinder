unit uPlotPanel;
// Hold all of the contents of the plot panel
interface
uses SysUtils, Classes, Graphics, Controls, System.Types, System.Generics.Collections,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms, JS, Web, uSidewinderTypes,
     uGraphP;
const DEFAULT_NUMB_PLOTS = 3; PLOT_WIDTH_OVERLAP = 30; LEGEND_WIDTH = 60;
      YSCALEWIDTH = 30; // Gap between the left edge and y axis ( for number labels)
      XSCALE_RATIO = 0.15; // Ratio of x axis labels to height of plot.
type
 TPlotMouseEvent = procedure(plotPosition: integer) of object; // mouse clicked while over plot

TPlotPanel = class
private
  parentWP: TWebPanel;
  plotLegendPB: TWebPaintBox; // parent is plotWPanel
  plotLegendBMap: TBitMap;
  plotSpNames: array of String; // names of species to plot
  plotInitVals: array of double; // Init values of plot species.
  xScaleHeight: integer; // Space reserved for the x axis labeling of plot
  maxYValue: double;  // largest y val for plot
  yMax: integer;  // largest y 'pixel' val for plot.
  plotPosition: integer;
  stepSize: double;
  pixelStep: integer; // pixel equiv of time (integration) step
  EUpdatePlot: TPlotMouseEvent;  // notify listener when plot needs to be edited

public
  plotWPanel: TWebPanel; // Panel that contains the plotPB and LegendPB
  plotPB: TWebPaintBox;  // parent is plotWPanel
  plotGraph: TPlotGraph;
  constructor create(parentContainer: TWebPanel; newPlotPosition: integer;
              maxY: double; spNames: array of string; initVals: array of double);
  destructor  Destroy; override;
  function getPlotPosition(): integer;
  procedure setMaxYValue( newVal: double);
  function getMaxYValue(): double;
  procedure setPlotPBWidth();  // Set plotPB amd plotGraph.bitmap width
  procedure setPlotWidth({newWidth: integer});
  procedure plotPBOnPaint(sender: TObject);
  procedure plotPBOnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  procedure plotLegendPBOnPaint(sender: TObject);
  //procedure plotLegendPBOnMouseDown(sender: TObject);  not needed yet.
  function getXScaleHeight(): integer;
  procedure setXScaleHeight( fractionHeight: double);
  procedure setPlotLegend(plotSpeciesList: TSpeciesList);
  function getPlotLegendPB(): TWebPaintBox;
  function getParentContainer(): TWebPanel;
  procedure processPlotLegendPB(pSpCount: integer);
  procedure processPlotLegendBM( pSList: TSpeciesList);
  procedure editPlot();   // Notify listener that user clicked plot
  procedure configPbPlot( {totalPlots: integer});
  procedure adjustPlotHeight(totalPlots: integer; newHeight: integer);
  procedure initializePlot(runTime: double; newStepSize: double; plotSpeciesList: TSpeciesList);
  procedure processOneScan( t_new: Double; y_new: array of Double;
                        plotSpecies: TSpeciesList; currentGeneration: Integer );
  property OnPlotUpdate: TPlotMouseEvent read EUpdatePlot write EUpdatePlot;
end;


implementation
  constructor TPlotPanel.create(parentContainer: TWebPanel; newPlotPosition: integer;
          maxY: double; spNames: array of string; initVals: array of double);
  begin
    self.maxYValue := maxY;
    self.stepSize := 0;
    self.plotPosition := newPlotPosition;
    self.parentWP := parentContainer;
    self.plotGraph := TPlotGraph.create;
    self.plotGraph.setY_valsMax(maxY);
    self.plotSpNames := spNames;   // List of all possible species
    self.plotInitVals := initVals; // Species init vals to scale plot
    self.plotWPanel := TWebPanel.create(parentContainer);
    self.plotWPanel.Parent := parentContainer;
    self.plotWPanel.Anchors := [akLeft,akRight,akTop];
    self.plotWPanel.Tag := plotPosition;
    self.plotWPanel.Width := self.parentWP.Width;
    self.plotWPanel.Height := 200;//round(panelH/3);
    self.plotWPanel.Left := 10; //10 gap between the network canvas and plot
    self.plotWPanel.Top := 4 + self.plotWPanel.Height*(self.plotPosition-1); // change to variable value based on number of existing plots.
    self.plotPB := TWebPaintBox.create(self.plotWPanel);
    self.plotPB.parent := self.plotWPanel;
  end;

  destructor  TPlotPanel.Destroy;
  begin
    self.plotLegendBMap.free;
    self.plotLegendPB.free;
    self.plotGraph.free;
    self.plotPB.free;
    self.plotWPanel.free;

  end;
  procedure TPlotPanel.initializePlot(runTime: double; newStepSize: double; plotSpeciesList: TSpeciesList );
  var newYMax : integer;
  begin
  //console.log( 'TPlotPanel.initializePlot');

   // self.plotPB.Anchors := [akLeft,akRight,akTop];
    self.stepSize := newStepSize;
    self.plotPB.Tag := self.plotPosition;
    self.plotPB.Height := self.plotWPanel.Height - 10;
    self.plotLegendPB := TWebPaintBox.create(plotWPanel);
    self.plotLegendPB.parent := self.plotWPanel;
    self.setPlotWidth();
    if self.pixelStep < 0 then
      self.pixelStep := 0;
    //self.xscaleHeight := round(0.15 * self.plotPB.Height);// make x-axis %15 of total height
    self.setXScaleHeight(XSCALE_RATIO); // make x-axis %15 of total height
    self.plotPB.OnPaint := plotPBOnPaint;
    self.plotPB.OnMouseDown := self.plotPBOnMouseDown;
    if self.plotGraph.getY_valsMax >0 then newYMax := round(self.plotGraph.getY_valsMax)
      else newYMax := DEFAULTSPECIESPLOTHT;
    self.yMax := newYMax;
    self.plotGraph.resetGraph(self.plotGraph.bitmap.canvas);
    self.setPlotLegend(plotSpeciesList);

    self.plotGraph.initGraph(0, 200, 0, newYMax, 0, self.plotPB.width, 0, self.plotPB.height,
        self.xscaleHeight,  YSCALEWIDTH, {runTime,} stepSize);

    // Display the plot:
    self.processOneScan(0, self.plotInitVals, plotSpeciesList, 0 );
    self.plotWPanel.visible := true;
    self.plotPB.visible := true;
   // Max viewable steps is PlotWebPB.width (1 pixel per step).
    self.pixelStep := 0;  // Default number.
    if runTime / stepSize < self.plotPB.width then
      self.pixelStep := round(self.plotPB.width * stepSize / runTime)
    else
      self.pixelStep := 1;
  end;

  procedure TPlotPanel.plotPBOnPaint(sender: TObject);
  var
  plot_i: Integer;
  begin
    plot_i := (Sender as TWebPaintBox).tag;
    if plot_i = self.plotPB.tag then           // necessary?
    begin
      console.log('TPlotPanel.plotPBOnPaint, tag: ', plot_i);
      if self.plotGraph <> nil then
        if self.plotGraph.bitmap <> nil then
          self.plotPB.canvas.draw(0, 0, self.plotGraph.bitmap);

    end;
  end;

  procedure TPlotPanel.plotLegendPBOnPaint(sender: TObject);
  var  plot_i: Integer;
  begin
    plot_i := (Sender as TWebPaintBox).tag;
    if plot_i = self.plotLegendPB.tag then
    begin
      console.log('TPlotPanel.plotLegendPBOnPaint, tag: ', plot_i);
      self.plotLegendPB.canvas.Draw(0, 0, self.plotLegendBMap);
    end;
  end;


  procedure TPlotPanel.plotPBOnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    i: Integer; // grab plot which received event
  begin
    if (Button = mbRight) or (Button = mbLeft) then // Both for now.
      begin
        if Sender is TWebPaintBox then
          begin
            i := TWebPaintBox(Sender).tag;
            // assume only plot paintboxes in right panel.
            if self.plotPB.tag = i then    // needed ?
               self.EditPlot;   // Notify listener
          end;
      end;
  end;

  procedure TPlotPanel.editPlot();  // needed ???
  var plotXposition, plotYposition: Integer;
      plotIndex: Integer;
      editList: TStringList;
  begin
   // notify listener that plot is to be edited.
    if Assigned(EUpdatePlot) then
     EUpdatePlot(self.plotPosition);
  end;


   procedure TPlotPanel.setPlotLegend(plotSpeciesList: TSpeciesList);
   begin
     self.processPlotLegendBM( plotSpeciesList );
     self.processPlotLegendPB(plotSpeciesList.count);
     self.plotLegendPB.Invalidate;
   end;

  procedure TPlotPanel.processPlotLegendPB(pSpCount: integer);
  begin
    try
      // Check if plotLegendBM created:
      if not Assigned(self.plotLegendBMap)then
      begin
        console.log(' plot legend bitmap not created yet');
        self.plotLegendBMap := TBitMap.create;
      end;

      self.plotLegendPB.Tag := self.plotPosition;
      self.plotLegendPB.OnPaint := self.plotLegendPBOnPaint;
      self.plotLegendPB.parent := plotWPanel;
      self.plotLegendPB.top := 10;
     // self.plotLegendPB.height := 10 * pSpCount; // each row 10 'pixels' tall
     self.plotLegendPB.height := self.plotPB.height;
      self.plotLegendPB.left := self.plotPB.width +5;
      if self.plotWPanel.Width > LEGEND_WIDTH then
        self.plotLegendPB.width := LEGEND_WIDTH
      else self.plotLegendPB.Width := round(self.plotWPanel.Width *0.1); // 10%
    except
     on E: Exception do
      notifyUser(E.message);
    end;

  end;

  procedure TPlotPanel.processPlotLegendBM({names: array of string;} pSList: TSpeciesList);

  var i, j, species, plotSp: integer;
    spName: string;
  begin
    j := 0;
    species := 0; plotSp := 0;
    species := pSList.Count; // max #, some will not be plotted.
    for i := 0 to species -1 do
      if pSList[i] <> '' then inc(plotSp);

    self.plotLegendBMap := TBitMap.create;
    self.plotLegendBMap.width := self.plotLegendPB.width;
    self.plotLegendBMap.height := self.plotLegendPB.height;
    self.plotLegendBMap.canvas.font.name := 'Courier New';
    self.plotLegendBMap.canvas.font.size := 8;

    for i := 0 to species -1 do
      begin
      spName := '';
      if pSList[i] <> '' then
        begin
          spName := pSList[i];
          if i <10 then
            self.plotLegendBMap.canvas.pen.color := uGraphP.COLORS[i]
          else self.plotLegendBMap.canvas.pen.color := uGraphP.COLORS[i mod length(uGraphP.COLORS)];
          self.plotLegendBMap.canvas.moveto( 2, j*10+3 );
          self.plotLegendBMap.canvas.lineto( 10, j*10+3);
          self.plotLegendBMap.canvas.TextOut( 12, j*10, spName);
          inc(j);
        end;
      end;

  end;


  procedure TPlotPanel.processOneScan( t_new: Double; y_new: array of Double;
               plotSpecies: TSpeciesList; currentGeneration: Integer );
  var i: integer;
    plot_y: array of boolean;
  begin
    SetLength(plot_y, length(y_new));
    for i := 0 to length(y_new) - 1 do
      begin
        if plotSpecies[i] = '' then
          plot_y[i] := false
        else
          plot_y[i] := true;
      end;
      // Dynamically draw plots:
    self.plotGraph.addPoint(currentGeneration, y_new,  plot_y, true, t_new);
    self.plotPB.Canvas.draw (0, 0, self.plotGraph.bitmap);

  end;


  procedure TPlotPanel.configPbPlot();
begin
   self.plotWPanel.Left := 10; //10 gap between the network canvas and plot
   self.plotWPanel.Top := 3 + self.plotWPanel.Height*(self.plotPosition-1); // change to variable value based on number of existing plots.
   self.plotWPanel.visible := true;  // ?? onpaint cause canvas error
   self.plotPB.Left := 5; // gap between the plotPanel canvas and plot
   self.plotPB.Top := 5; // change to variable value based on number of existing plots.
   self.plotPB.visible := true;

end;

  procedure TPlotPanel.adjustPlotHeight(totalPlots: integer; newHeight: integer);
  begin
    if totalPlots > DEFAULT_NUMB_PLOTS then
    begin
      self.plotWPanel.Height:= newHeight;
      self.plotWPanel.Top:= 3 + self.plotWPanel.Height*(self.plotPosition-1);
      self.plotPB.Height := self.plotWPanel.Height - 10;
      self.setXScaleHeight(XSCALE_RATIO);
      self.plotLegendPB.Height := self.plotPB.Height;
      self.plotGraph.initGraph(0, 200, 0, self.yMax, 0, self.plotPB.width, 0, self.plotPB.height,
        self.xscaleHeight,  YSCALEWIDTH, self.stepSize);
      self.plotWPanel.invalidate;
      self.plotPB.invalidate;  // Redraw as height and top have changed....
      self.plotLegendPB.Invalidate
    end;

  end;

  function TPlotPanel.getPlotLegendPB(): TWebPaintBox;
  begin
    Result := self.plotLegendPB;
  end;

  function TPlotPanel.getPlotPosition(): integer;
  begin
    Result := self.plotPosition;
  end;

  function TPlotPanel.getParentContainer(): TWebPanel;
  begin
    Result := self.parentWP;
  end;

  procedure TPlotPanel.setMaxYValue( newVal: double);
  begin
    // TODO
  end;

  function TPlotPanel.getMaxYValue(): double;
  begin
    // TODO
  end;

  procedure TPlotPanel.setPlotPBWidth();
  begin
    if self.plotWPanel.width > LEGEND_WIDTH then
    begin
      self.plotPB.Width := self.plotWPanel.width - LEGEND_WIDTH;
    end
    else
      begin
        self.plotPB.Width := self.plotWPanel.width - self.plotLegendPB.Width;
      end;
    if Assigned(self.plotGraph) then
      self.plotGraph.bitmap.width := self.plotPB.width;
  end;

  // Adjust plot PB and legend PB based on new plotWPanel.Width
  procedure TPlotPanel.setPlotWidth();
  begin

    if self.plotWPanel.width > LEGEND_WIDTH then
    begin
      self.plotLegendPB.width := LEGEND_WIDTH;
      self.plotPB.Width := self.plotWPanel.width - LEGEND_WIDTH;
    end
    else
      begin
        self.plotLegendPB.Width := round(self.plotWPanel.Width *0.1); // 10%
        self.plotPB.Width := self.plotWPanel.width - self.plotLegendPB.Width;
      end;

  end;

  function TPlotPanel.getXScaleHeight(): integer;
  begin

  end;

  procedure TPlotPanel.setXScaleHeight(fractionHeight: double);
  begin
    self.xScaleHeight := round(fractionHeight * self.plotPB.Height);// make x-axis %15 of total height
  end;




end.
