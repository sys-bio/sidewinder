program SBMLtest;

uses
  Vcl.Forms,
  WEBLib.Forms,
  Unit1 in 'Unit1.pas' {Form1: TWebForm},
  uMath in '..\math\uMath.pas',
  Adamsbdf in '..\math\lsoda\Adamsbdf.pas',
  LSODA.test in '..\math\lsoda\LSODA.test.pas',
  lsodamat in '..\math\lsoda\lsodamat.pas',
  odeEquations in '..\math\lsoda\odeEquations.pas',
  uMatrix in '..\math\lsoda\uMatrix.pas',
  uVector in '..\math\lsoda\uVector.pas',
  ODE_FormatUtility in '..\simulator\ODE_FormatUtility.pas',
  Simulation in '..\simulator\Simulation.pas',
  GraphP in '..\plot\GraphP.pas',
  plotLayout in '..\plot\plotLayout.pas',
  SBML.helper in '..\sbml\SBML.helper.pas',
  SBML.model in '..\sbml\SBML.model.pas',
  SBML.model.rule in '..\sbml\SBML.model.rule.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
