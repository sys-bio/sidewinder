unit plotLayout;

interface
uses System.SysUtils, System.Classes, JS, Web, Vcl.Controls, WEBLib.ExtCtrls,WEBLib.Dialogs ;

const defaultNumbPlots = 3;
var panelW, panelH, currPlot: integer;

procedure configPbPlot(plotNumber : integer; plotPanelWidth, plotPanelHeight: integer; newPBplotAr: array of TWebPaintBox);
procedure adjustPlotHeights(totalPlots: integer; pbPlotAr: array of TWebPaintBox);

implementation

procedure configPbPlot(plotNumber : integer; plotPanelWidth, plotPanelHeight: integer; newPBplotAr: array of TWebPaintBox);
var i: integer;
begin
   panelW := plotPanelWidth;
   panelH := plotPanelHeight;
   currPlot := plotNumber;

   newPBplotAr[plotNumber-1].Tag := plotNumber;  // keep track of its order in plot list.
   if plotNumber > defaultNumbPlots then
      adjustPlotHeights(plotNumber, newPBplotAr)
   else
      newPBplotAr[plotNumber-1].Height := 200;//round(panelH/3);

   newPBplotAr[plotNumber-1].Width := panelW - 20; // This must be a bg, without it the slider panel overlaps plot
   newPBplotAr[plotNumber-1].Left := 10; // gap between the network canvas and plot
   newPBplotAr[plotNumber-1].Top := 3 + newPBPlotAr[plotNumber-1].Height*(plotNumber-1); // change to variable value based on number of existing plots.
   newPBplotAr[plotNumber-1].visible := true;
   newPBplotAr[plotNumber-1].invalidate;
end;

 // adjust Plot Heights and abs vertical coord of existing plots to accomodate new plot.
 // --> there is a problem here, once the plots start to reduce in side the scaling of the y axis I think goes wrong.
procedure adjustPlotHeights(totalPlots: integer; pbPlotAr: array of TWebPaintBox);
var i, newHeight: integer;
begin
  newHeight := round(panelH/totalPlots);
  for i := 0 to totalPlots-1 do
      begin
      pbPlotAr[i].Height:= newHeight;
      pbPlotAr[i].Top:= 5 + pbPlotAr[i].Height*(i);
      pbPlotAr[i].invalidate;
      end;
end;

end.
