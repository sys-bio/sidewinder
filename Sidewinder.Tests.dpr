program Sidewinder.Tests;

uses
  Vcl.Forms,
  WEBLib.Forms,
  ufUnitTests in 'ufUnitTests.pas' {fUnitTests: TWebForm} {*.html},
  LSODA.test in 'math\lsoda\LSODA.test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfUnitTests, fUnitTests);
  Application.Run;
end.
