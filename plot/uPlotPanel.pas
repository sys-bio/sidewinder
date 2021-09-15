unit uPlotPanel;
// Hold all of the contents of the plot panel
interface
uses SysUtils, Classes, Graphics, Controls, System.Types, System.Generics.Collections,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms, JS, Web, uSidewinderTypes,
     ufSpeciesSelect, uGraphP;
//const DEFAULT_NUMB_PLOTS = 3; PLOT_WIDTH_OVERLAP = 30;
type
 TPlotMouseEvent = procedure(plotPosition: integer) of object; // mouse clicked while over plot

TPlotPanel = class
private
  parentWP: TWebPanel;
  plotLegendPB: TWebPaintBox;
  plotLegendBMap: TBitMap;
  plotSpNames: array of String; // names of species to plot
  plotInitVals: array of double; // Init values of plot species.
  xScaleHeight: integer; // Space reserved for the x axis labeling of plot
  maxYValue: double;  // largest y val for plot
  plotPosition: integer;
  //maxYValueInt: integer; // largest height for plotPB not needed.
  pixelStep: integer; // pixel equiv of time (integration) step
  plotWidth: integer;
  EUpdatePlot: TPlotMouseEvent;  // notify listener when plot needs to be edited

public
  plotWPanel: TWebPanel; // Panel that contains the plotPB and LegendPB
  plotPB: TWebPaintBox;
  plotGraph: TPlotGraph;
  constructor create(parentContainer: TWebPanel; newPlotPosition: integer;
              maxY: double; spNames: array of string; initVals: array of double);
  destructor  Destroy; override;
  function getPlotPosition(): integer;
  procedure setMaxYValue( newVal: double);
  function getMaxYValue(): double;
  procedure setPlotWidth(width: integer);
  procedure plotPBOnPaint(sender: TObject);
  procedure plotPBOnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  procedure plotLegendPBOnPaint(sender: TObject);
  //procedure plotLegendPBOnMouseDown(sender: TObject);  not needed yet.
  function getXScaleHeight(): integer;
  procedure setXScaleHeight( newHeight: integer);
  procedure setPlotLegend(plotSpeciesList: TSpeciesList);
  procedure processPlotLegendPB(pSpCount: integer);
  procedure processPlotLegendBM( pSList: TSpeciesList);
  procedure editPlot();
  procedure initializePlot(runTime: double; stepSize: double; plotSpeciesList: TSpeciesList);
  procedure processOneScan( t_new: Double; y_new: array of Double;
                        plotSpecies: TSpeciesList; currentGeneration: Integer );
  property OnPlotUpdate: TPlotMouseEvent read EUpdatePlot write EUpdatePlot;
end;


implementation
  constructor TPlotPanel.create(parentContainer: TWebPanel; newPlotPosition: integer;
          maxY: double; spNames: array of string; initVals: array of double);
  begin            //ufMain.addPlot()
    self.maxYValue := maxY;
    self.plotPosition := newPlotPosition;
    self.parentWP := parentContainer;
    self.plotGraph := TPlotGraph.create;
    self.plotGraph.setY_valsMax(maxY);
  // Put plot in a TWebPanel:
    self.plotSpNames := spNames;
    self.plotInitVals := initVals;
    self.plotWPanel := TWebPanel.create(parentContainer);
    self.plotWPanel.Parent := parentContainer;
    self.plotWPanel.Tag := plotPosition;
    self.plotPB := TWebPaintBox.create(self.plotWPanel);
    self.plotPB.parent := self.plotWPanel;
    self.plotPB.OnPaint := plotPBOnPaint;
    self.plotPB.OnMouseDown := self.plotPBOnMouseDown;
    self.plotPB.Tag := plotPosition;

  end;

  destructor  TPlotPanel.Destroy;
  begin
    self.plotLegendBMap.free;
    self.plotLegendPB.free;
    self.plotGraph.free;
    self.plotPB.free;
    self.plotWPanel.free;

  end;
  procedure TPlotPanel.initializePlot(runTime: double; stepSize: double; plotSpeciesList: TSpeciesList );
  var yScaleWidth, newYMax : integer;
  begin
    if self.pixelStep < 0 then
      self.pixelStep := 0;
    self.xscaleHeight := round(0.15 * self.plotPB.Height);// make x-axis %15 of total height
    yScaleWidth := 30; // Gap between the left edge and y axis ( for number labels)

    if self.plotGraph.getY_valsMax >0 then newYMax := round(self.plotGraph.getY_valsMax)
      else newYMax := DEFAULTSPECIESPLOTHT;
    self.plotGraph.resetGraph(self.plotGraph.bitmap.canvas);
    self.plotGraph.initGraph(0, 200, 0, newYMax, 0, self.plotPB.width, 0, self.plotPB.height,
        self.xscaleHeight, yscaleWidth, runTime, stepSize);
    self.setPlotLegend(plotSpeciesList);

  // Display the plot:
    self.processOneScan(0, self.plotInitVals, plotSpeciesList, 0 );

   // Max viewable steps is PlotWebPB.width (1 pixel per step).
    self.pixelStep := 0;  // Default number.
    if runTime / stepSize < self.plotPB.width then
      self.pixelStep := round(self.plotPB.width * stepSize / runTime)
    else
      self.pixelStep := 1;
    self.plotLegendPB.Invalidate;
  end;

  procedure TPlotPanel.plotPBOnPaint(sender: TObject);
  var
  plot_i: Integer;
  begin
    plot_i := (Sender as TWebPaintBox).tag;
    if plot_i = self.plotPB.tag then           // necessary?
    begin
      console.log('TPlotPanel.plotPBOnPaint, tag: ', plot_i);
       self.plotPB.canvas.draw(0, 0, self.plotGraph.bitmap);
     //  if self.plotLegendPB <> nil then   ... no effect
     //    if self.plotLegendBMap <> nil then
     //      self.plotLegendPBOnPaint(sender);
    end;
  end;

  procedure TPlotPanel.plotLegendPBOnPaint(sender: TObject);
  var  plot_i: Integer;
  begin
    plot_i := (Sender as TWebPaintBox).tag;
    console.log('TPlotPanel.plotLegendPBOnPaint, tag: ', plot_i);
    self.plotLegendPB.canvas.Draw(0, 0, self.plotLegendBMap);
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


  //procedure TPlotPanel.plotLegendPBOnMouseDown(sender: TObject);

  procedure TPlotPanel.editPlot();  // needed ???
  var plotXposition, plotYposition: Integer;
      plotIndex: Integer;
      editList: TStringList;
  begin
   // notify listenres that plot is to be edited.
    if Assigned(EUpdatePlot) then
     EUpdatePlot(self.plotPosition);
  end;



   procedure TPlotPanel.setPlotLegend(plotSpeciesList: TSpeciesList);
   begin
     self.processPlotLegendPB(plotSpeciesList.count);
     self.processPlotLegendBM( plotSpeciesList );
   end;

  procedure TPlotPanel.processPlotLegendPB(pSpCount: integer);
  var species: integer;
  begin
    species := 0;
    species := pSpCount; // max #, some will not be plotted.
    self.plotLegendPB := TWebPaintBox.create(self.parentWP);
    self.plotLegendBMap := TBitMap.create;
    self.plotLegendPB.Tag := self.plotPosition;
    self.plotLegendPB.OnPaint := self.plotLegendPBOnPaint;
    self.plotLegendPB.parent := self.parentWP;
    self.plotLegendPB.top := 10;
    self.plotLegendPB.height := 10 * species; // each row 10 'pixels' tall
    self.plotLegendPB.left := self.plotPB.width +30;

  end;

  procedure TPlotPanel.processPlotLegendBM({names: array of string;} pSList: TSpeciesList);

  var i, j, species, plotSp: integer;
    spName: string;
  begin
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
          self.plotLegendBMap.canvas.moveto( 2, j*10+2 );
          self.plotLegendBMap.canvas.lineto( 10, j*10+2);
          self.plotLegendBMap.canvas.TextOut( 11, j*10, spName);
          inc(j);
        end;
      end;

  end;


  procedure TPlotPanel.processOneScan( t_new: Double; y_new: array of Double;
              { plotPB: TWebPaintBox;} plotSpecies: TSpeciesList;
               currentGeneration: Integer );
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


 { procedure TPlotPanel.setPlotWP();
  begin
    // TODO
  end;
  function TPlotPanel.getPlotWP(): TWebPanel;
  begin
    // TODO
  end;      }

  //procedure TPlotPanel.setPlotPB();
  //begin
    // TODO
  //end;



 { procedure TPlotPanel.setPlotGraph();
  begin
    // TODO
  end;     }

  {function TPlotPanel.getPlotGraph(): TPlotGraph;
  begin
    // TODO
  end;  }

  function TPlotPanel.getPlotPosition(): integer;
  begin
    Result := self.plotPosition;
  end;

  procedure TPlotPanel.setMaxYValue( newVal: double);
  begin
    // TODO
  end;

  function TPlotPanel.getMaxYValue(): double;
  begin
    // TODO
  end;

  procedure TPlotPanel.setPlotWidth(width: integer);
  begin
    // TODO
  end;

  function TPlotPanel.getXScaleHeight(): integer;
  begin
    // TODO
  end;

  procedure TPlotPanel.setXScaleHeight(newHeight: integer);
  begin
    // TODO
  end;




end.
