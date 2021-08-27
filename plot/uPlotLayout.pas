unit uPlotLayout;

interface
uses System.SysUtils, System.Classes, System.Generics.Collections,
JS, Web, Vcl.Controls, WEBLib.ExtCtrls, WEBLib.Dialogs ;

const defaultNumbPlots = 3; PLOT_WIDTH_OVERLAP = 20;
var panelW, panelH, currPlot: integer;

procedure configPbPlot(plotNumber, totalPlots : integer; plotPanelWidth, plotPanelHeight: integer; newPBplotList: TList<TWebPaintBox>);
procedure adjustPlotHeights(totalPlots: integer; pbPlotList: TList<TWebPaintBox>);

implementation

procedure configPbPlot(plotNumber, totalPlots : integer; plotPanelWidth, plotPanelHeight: integer; newPBplotList: TList<TWebPaintBox>);
begin
   panelW := plotPanelWidth;
   panelH := plotPanelHeight;
   currPlot := plotNumber;

   if plotNumber > defaultNumbPlots then
      adjustPlotHeights(totalPlots, newPBplotList)
   else
      newPBplotList.Items[totalPlots-1].Height := 200;//round(panelH/3);

   newPBplotList[totalPlots-1].Width := panelW - PLOT_WIDTH_OVERLAP; // This must be a bg, without it the slider panel overlaps plot
   newPBplotList[totalPlots-1].Left := 10; // gap between the network canvas and plot
   newPBplotList[totalPlots-1].Top := 3 + newPBplotList.Items[totalPlots-1].Height*(plotNumber-1); // change to variable value based on number of existing plots.
   newPBplotList[totalPlots-1].visible := true;
   newPBplotList[totalPlots-1].invalidate;
end;

 // adjust Plot Heights and abs vertical coord of existing plots to accomodate new plot.
 // --> there is a problem here, once the plots start to reduce in side the scaling of the y axis I think goes wrong.
procedure adjustPlotHeights(totalPlots: integer; pbPlotList: TList<TWebPaintBox>);
var i, newHeight: integer;
begin
  newHeight := round(panelH/totalPlots);
  for i := 0 to totalPlots-1 do
      begin
      pbPlotList[i].Height:= newHeight;
      pbPlotList[i].Top:= 5 + pbPlotList.Items[i].Height*(i);
      pbPlotList[i].invalidate;
      end;
end;

end.
