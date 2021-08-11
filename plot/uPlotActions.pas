unit uPlotActions;
 // May want to fold this in with uPlotLayout
interface

uses System.Generics.Collections, WEBLib.ExtCtrls, uGraphP, uModel, uSidewinderTypes;
procedure processScan(t_new: Double; y_new: array of Double; plots: TList<TPlotGraph>;
               plotPBs: TList<TWebPaintBox>; plotSpecies: TList<TSpeciesList>;
               currentGeneration: Integer);

implementation

procedure processScan(t_new: Double; y_new: array of Double; plots: TList<TPlotGraph>;
               plotPBs: TList<TWebPaintBox>; plotSpecies: TList<TSpeciesList>;
               currentGeneration: Integer);
var
  i: Integer;
  plot_y: array of Boolean;
  j: Integer;  // Plot number
begin
  SetLength(plot_y, length(y_new));
  for j := 0 to plotSpecies.Count - 1 do // cycle through all of the plots
    begin
      for i := 0 to length(y_new) - 1 do
        begin
          if plotSpecies.Items[j].Items[i] = '' then
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
