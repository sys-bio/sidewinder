unit plotLayout;

interface
uses System.SysUtils, System.Classes, JS, Web, Vcl.Controls, WEBLib.ExtCtrls,WEBLib.Dialogs ;

const defaultNumbPlots = 3;
var panelW, panelH, currPlot: integer;

procedure configPbPlot(plotNumb: integer; plotPanelWidth, plotPanelHeight: integer;
  newPBplotAr: array of TWebPaintBox);
procedure adjustPlotHeights(totalPlots: integer; pbPlotAr: array of TWebPaintBox);

implementation

procedure configPbPlot(plotNumb: integer; plotPanelWidth, plotPanelHeight: integer; newPBplotAr: array of TWebPaintBox);
 var i: integer;
begin

   panelW:= plotPanelWidth;
   panelH:= plotPanelHeight;
   currPlot:= plotNumb;
   newPBplotAr[plotNumb-1].Tag:= plotNumb;  // keep track of its order in plot list.
   if plotNumb > defaultNumbPlots then
     adjustPlotHeights(plotNumb, newPBplotAr)
   else newPBplotAr[plotNumb-1].Height:= round(panelH/3);
   newPBplotAr[plotNumb-1].Width:= panelW -20;
   newPBplotAr[plotNumb-1].Left:= 10;
   newPBplotAr[plotNumb-1].Top:= 5+ newPBPlotAr[plotNumb-1].Height*(plotNumb-1); // change to variable value based on number of existing plots.
   newPBplotAr[plotNumb-1].visible:= true;
   newPBplotAr[plotNumb-1].invalidate;

end;

 // adjust Plot Heights and abs vertical coord of existing plots to accomadate new plot.
procedure adjustPlotHeights(totalPlots: integer; pbPlotAr: array of TWebPaintBox);
var i, newHeight: integer;
begin
newHeight:= round(panelH/totalPlots);
for i := 0 to totalPlots-1 do
  begin
    pbPlotAr[i].Height:= newHeight;
    pbPlotAr[i].Top:= 5+ pbPlotAr[i].Height*(i);
    pbPlotAr[i].invalidate;
  end;

end;




end.
