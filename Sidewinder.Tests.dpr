program Sidewinder.Tests;

uses
  Vcl.Forms,
  WEBLib.Forms,
  LSODA.test in 'math\lsoda\LSODA.test.pas',
  uTestLSODA_JS in 'math\lsoda\uTestLSODA_JS.pas',
  uTestCase in 'tests\uTestCase.pas',
  ufUnitTests in 'tests\ufUnitTests.pas' {fUnitTests: TWebForm} {*.html},
  utests.TestUtils in 'tests\utests.TestUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfUnitTests, fUnitTests);
  Application.Run;
end.
