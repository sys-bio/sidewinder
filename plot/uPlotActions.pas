unit uPlotActions;
 // May want to fold this in with uPlotLayout
interface

uses System.Generics.Collections, WEBLib.ExtCtrls, uGraphP, uModel, uSidewinderTypes;
procedure processScan(t_new: Double; y_new: array of Double; plots: TList<TPlotGraph>;
               plotPBs: TList<TWebPaintBox>; plotSpecies: TList<TSpeciesList>;
               currentGeneration: Integer);
procedure processScanOnePlot( t_new: Double; y_new: array of Double; plot: TPlotGraph;
               plotPB: TWebPaintBox; plotSpecies: TSpeciesList;
               currentGeneration: Integer );

implementation

procedure processScan(t_new: Double; y_new: array of Double; plots: TList<TPlotGraph>;
               plotPBs: TList<TWebPaintBox>; plotSpecies: TList<TSpeciesList>;
               currentGeneration: Integer);
var  j: Integer;  // Plot number
begin
  for j := 0 to plots.Count - 1 do // cycle through all of the plots
    begin
      processScanOnePlot( t_new, y_new, plots[j], plotPBs[j], plotSpecies[j], currentGeneration );
    end;
end;


procedure processScanOnePlot( t_new: Double; y_new: array of Double; plot: TPlotGraph;
               plotPB: TWebPaintBox; plotSpecies: TSpeciesList;
               currentGeneration: Integer );
var i: integer;
    plot_y: array of boolean;
begin
  SetLength(plot_y, length(y_new));
  for i := 0 to length(y_new) - 1 do
        begin
          if plotSpecies.Items[i] = '' then
            plot_y[i] := false
          else
            plot_y[i] := true;
        end;
      // Dynamically draw plots:
  plot.addPoint(currentGeneration, y_new,  plot_y, true, t_new);
  plotPB.Canvas.draw (0, 0, plot.bitmap);


end;

end.
