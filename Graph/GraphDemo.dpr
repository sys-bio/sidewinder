program GraphDemo;

uses
  Vcl.Forms,
  WEBLib.Forms,
  ufGraphDemo in 'ufGraphDemo.pas' {Form1: TWebForm} {*.html},
  uWebDataSource in 'ScrollingChart\uWebDataSource.pas',
  uWebComps in 'ScrollingChart\uWebComps.pas',
  uWebContainer in 'ScrollingChart\uWebContainer.pas',
  uWebGlobalData in 'ScrollingChart\uWebGlobalData.pas',
  uWebScrollingChart in 'ScrollingChart\uWebScrollingChart.pas',
  uWebStage in 'ScrollingChart\uWebStage.pas',
  uScrollingTypes in 'ScrollingChart\uScrollingTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
