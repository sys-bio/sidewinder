unit uPlotLayout;
  // Set/adjust width and height of plots
interface
uses System.SysUtils, System.Classes, System.Generics.Collections,
JS, Web, Vcl.Controls, WEBLib.ExtCtrls, WEBLib.Dialogs, uPlotPanel ;

const DEFAULT_NUMB_PLOTS = 3; PLOT_WIDTH_OVERLAP = 30;
var panelW, panelH,{ paintBoxW, paintBoxH,} currPlot: integer;

procedure configPbPlot(plotNumber, totalPlots, plotPanelWidth : integer; plotPanelHeight: integer;
          plotPanelList: TList<TPlotPanel>);

procedure adjustPlotHeights(totalPlots: integer; plotPanelList: TList<TPlotPanel>);


implementation

procedure configPbPlot(plotNumber, totalPlots, plotPanelWidth : integer; plotPanelHeight: integer;
          plotPanelList: TList<TPlotPanel>);
begin
   panelW := plotPanelWidth;
   panelH := plotPanelHeight;

   if plotPanelList.Count > DEFAULT_NUMB_PLOTS then
      adjustPlotHeights(totalPlots, plotPanelList)
   else
     begin
      plotPanelList[totalPlots-1].plotWPanel.Height := 200;//round(panelH/3);
      plotPanelList[totalPlots-1].plotPB.Height := plotPanelList[totalPlots-1].plotWPanel.Height - 50;
     end;
                                                   // ISSUE: plotLegend has not created yet ..,..
   //plotPanelList[totalPlots-1].plotWPanel.Width := panelW - PLOT_WIDTH_OVERLAP; // This must be a bg, without it the slider panel overlaps plot
   plotPanelList[totalPlots-1].plotWPanel.Width := plotPanelList[totalPlots-1].getParentContainer.width - plotPanelList[totalPlots-1].getPlotLegendPB().Width;
   plotPanelList[totalPlots-1].plotWPanel.Left := 15; //10 gap between the network canvas and plot
   plotPanelList[totalPlots-1].plotWPanel.Top := 3 + plotPanelList[totalPlots-1].plotWPanel.Height*(plotNumber-1); // change to variable value based on number of existing plots.
   plotPanelList[totalPlots-1].plotWPanel.visible := true;
   plotPanelList[totalPlots-1].plotWPanel.invalidate;

   plotPanelList[totalPlots-1].plotPB.Width := plotPanelList[totalPlots-1].plotWPanel.Width {- 80}; // This must be a bg, without it the slider panel overlaps plot
   plotPanelList[totalPlots-1].plotPB.Left := 5; // gap between the plotPanel canvas and plot
   plotPanelList[totalPlots-1].plotPB.Top := 5; // change to variable value based on number of existing plots.
   plotPanelList[totalPlots-1].plotPB.visible := true;
   plotPanelList[totalPlots-1].plotPB.invalidate;

end;

 // adjust Plot Heights and abs vertical coord of existing plots to accomodate new plot.
 // --> there is a problem here, once the plots start to reduce in side the scaling of the y axis I think goes wrong.
procedure adjustPlotHeights(totalPlots: integer; plotPanelList: TList<TPlotPanel>);
var i, newHeight: integer;
begin
  newHeight := round(panelH/totalPlots);
  for i := 0 to totalPlots-1 do
      begin
      plotPanelList[i].plotWPanel.Height:= newHeight;
      plotPanelList[i].plotWPanel.Top:= 5 + plotPanelList[i].plotPB.Height*(i);
      plotPanelList[i].plotWPanel.invalidate;
      plotPanelList[i].plotPB.invalidate;  // Redraw as height and top have changed....
      end;
end;

end.
