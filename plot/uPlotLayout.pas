unit uPlotLayout;
  // Set/adjust width and height of plots
interface
uses System.SysUtils, System.Classes, System.Generics.Collections,
JS, Web, Vcl.Controls, WEBLib.ExtCtrls, WEBLib.Dialogs ;

const DEFAULT_NUMB_PLOTS = 3; PLOT_WIDTH_OVERLAP = 30;
var panelW, panelH, paintBoxW, paintBoxH, currPlot: integer;

procedure configPbPlot(plotNumber, totalPlots : integer; plotPanelWidth, plotPanelHeight: integer;
         newPBplotList: TList<TWebPaintBox>; plotPanelList: TList<TWebPanel>);
procedure adjustPlotHeights(totalPlots: integer; pbPlotList: TList<TWebPaintBox>; plotPanelList: TList<TWebPanel>);

implementation

procedure configPbPlot(plotNumber, totalPlots : integer; plotPanelWidth, plotPanelHeight: integer;
         newPBplotList: TList<TWebPaintBox>; plotPanelList: TList<TWebPanel>);
begin
   panelW := plotPanelWidth;
   panelH := plotPanelHeight;
   currPlot := plotNumber;

   if plotNumber > DEFAULT_NUMB_PLOTS then
      adjustPlotHeights(totalPlots, newPBplotList, plotPanelList)
   else
     begin
      plotPanelList[totalPlots-1].Height := 200;//round(panelH/3);
      newPBplotList[totalPlots-1].Height := plotPanelList.Items[totalPlots-1].Height - 50;
     end;

   plotPanelList[totalPlots-1].Width := panelW - PLOT_WIDTH_OVERLAP; // This must be a bg, without it the slider panel overlaps plot
   plotPanelList[totalPlots-1].Left := 10; // gap between the network canvas and plot
   plotPanelList[totalPlots-1].Top := 3 + plotPanelList[totalPlots-1].Height*(plotNumber-1); // change to variable value based on number of existing plots.
   plotPanelList[totalPlots-1].visible := true;
   plotPanelList[totalPlots-1].invalidate;

   newPBplotList[totalPlots-1].Width := plotPanelList[totalPlots-1].Width - 80; // This must be a bg, without it the slider panel overlaps plot
   newPBplotList[totalPlots-1].Left := 5; // gap between the plotPanel canvas and plot
   newPBplotList[totalPlots-1].Top := 5; // change to variable value based on number of existing plots.
   newPBplotList[totalPlots-1].visible := true;
   newPBplotList[totalPlots-1].invalidate;
end;

 // adjust Plot Heights and abs vertical coord of existing plots to accomodate new plot.
 // --> there is a problem here, once the plots start to reduce in side the scaling of the y axis I think goes wrong.
procedure adjustPlotHeights(totalPlots: integer; pbPlotList: TList<TWebPaintBox>; plotPanelList: TList<TWebPanel>);
var i, newHeight: integer;
begin
  newHeight := round(panelH/totalPlots);
  for i := 0 to totalPlots-1 do
      begin
      plotPanelList[i].Height:= newHeight;
      plotPanelList[i].Top:= 5 + pbPlotList.Items[i].Height*(i);
      plotPanelList[i].invalidate;
      pbPlotList[i].invalidate;  // Redraw as height and top have changed....
      end;
end;

end.
