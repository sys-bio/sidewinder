program Sidewinder.Tests;

uses
  Vcl.Forms,
  WEBLib.Forms,
  LSODA.test in 'math\lsoda\LSODA.test.pas',
  uTestLSODA_JS in 'math\lsoda\uTestLSODA_JS.pas',
  uTestCase in 'tests\uTestCase.pas',
  ufUnitTests in 'tests\ufUnitTests.pas' {fUnitTests: TWebForm} {*.html},
  utests.TestUtils in 'tests\utests.TestUtils.pas',
  uTestSBMLReadWrite in 'sbml\uTestSBMLReadWrite.pas',
  uModel in 'model\uModel.pas',
  uTestSBML_ReadModels in 'sbml\uTestSBML_ReadModels.pas',
  uSBMLReader in 'sbml\uSBMLReader.pas',
  uTestSBMLReadGenerateEqs in 'tests\uTestSBMLReadGenerateEqs.pas',
  uSimulation in 'simulator\uSimulation.pas',
  uSBMLClasses.Render in 'sbml\uSBMLClasses.Render.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfUnitTests, fUnitTests);
  Application.Run;
end.
