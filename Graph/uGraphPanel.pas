unit uGraphPanel;

interface
uses SysUtils, Classes, Graphics, Controls, System.Types, System.Generics.Collections,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms, JS, Web,
     uWebScrollingChart;

type

TGraphPanel = class
private
  chart: TWebScrollingChart;
  pnlChart: TWebPanel;
public
  constructor create(owner: TComponent);


end;

implementation

constructor TGraphPanel.create(owner: TComponent);
begin
  // TODO
  self.pnlChart := TWebPanel.Create(owner);
  self.chart := TWebScrollingChart.Create(self.pnlChart);
  self.chart.parent := self.pnlChart;



end;

end.
