program sidewinder;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  ufMain in 'ufMain.pas' {mainForm: TWebForm} {*.html},
  SBML.helper in 'sbml\SBML.helper.pas',
  SBML.model in 'sbml\SBML.model.pas',
  uMath in 'math\uMath.pas',
  Adamsbdf in 'math\lsoda\Adamsbdf.pas',
  LSODA.test in 'math\lsoda\LSODA.test.pas',
  lsodamat in 'math\lsoda\lsodamat.pas',
  odeEquations in 'math\lsoda\odeEquations.pas',
  uMatrix in 'math\lsoda\uMatrix.pas',
  uVector in 'math\lsoda\uVector.pas',
  uController in 'rxn_networks\uController.pas',
  uCreateNetworks in 'rxn_networks\uCreateNetworks.pas',
  uDrawReaction in 'rxn_networks\uDrawReaction.pas',
  uDrawTypes in 'rxn_networks\uDrawTypes.pas',
  uGraphUtils in 'rxn_networks\uGraphUtils.pas',
  uLayout in 'rxn_networks\uLayout.pas',
  uNetwork in 'rxn_networks\uNetwork.pas',
  uNetworkCanvas in 'rxn_networks\uNetworkCanvas.pas',
  uNetworkTypes in 'rxn_networks\uNetworkTypes.pas',
  uSelectedObjects in 'rxn_networks\uSelectedObjects.pas',
  ufNodeFrame in 'ufNodeFrame.pas' {Frame1: TFrame},
  ODE_FormatUtility in 'simulator\ODE_FormatUtility.pas',
  Simulation in 'simulator\Simulation.pas',
  GraphP in 'plot\GraphP.pas',
  speciesSelectForm in 'speciesSelectForm.pas' {SpeciesSWForm: TWebForm} {*.html},
  plotLayout in 'plot\plotLayout.pas',
  paramSelectForm in 'paramSelectForm.pas' {Form1: TWebForm} {*.html},
  paramSliderLayout in 'paramSliderLayout.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmainForm, mainForm);
  Application.Run;
end.
