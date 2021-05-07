unit uPlotActions;
 // May want to fold this in with uPlotLayout
interface

uses uGraphP,WEBLib.ExtCtrls, uModel,  uSidewinderTypes;
procedure processScan(t_new: Double; y_new: array of Double; plots: array of TPlotGraph;
               plotPBs: array of TWebPaintBox; plotSpecies: array of TSpeciesAr;
               currentGeneration: Integer);

implementation

procedure processScan(t_new: Double; y_new: array of Double; plots: array of TPlotGraph;
               plotPBs: array of TWebPaintBox; plotSpecies: array of TSpeciesAr;
               currentGeneration: Integer);
var
  i: Integer;
  plot_y: array of Boolean;
  j: Integer;  // Plot number
begin
  SetLength(plot_y, length(y_new));
  for j := 0 to length(plotSpecies) - 1 do // cycle through all of the plots
    begin
      for i := 0 to length(y_new) - 1 do
        begin
          if plotSpecies[j][i] = '' then
            plot_y[i] := false
          else
            plot_y[i] := true;
        end;
      // Dynamically draw plots:
      plots[j].addPoint(currentGeneration, y_new,  plot_y, true, t_new);
      plotPBs[j].Canvas.draw (0, 0, plots[j].bitmap);
    end;
end;

end.
