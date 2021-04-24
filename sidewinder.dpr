program sidewinder;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  ufMain in 'ufMain.pas' {mainForm: TWebForm} {*.html},
  uSBMLClasses in 'sbml\uSBMLClasses.pas',
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
  uODE_FormatUtility in 'simulator\uODE_FormatUtility.pas',
  uSimulation in 'simulator\uSimulation.pas',
  uGraphP in 'plot\uGraphP.pas',
  ufSpeciesSelect in 'ufSpeciesSelect.pas' {SpeciesSWForm: TWebForm} {*.html},
  uPlotLayout in 'plot\uPlotLayout.pas',
  ufParamSelect in 'ufParamSelect.pas' {Form1: TWebForm} {*.html},
  uParamSliderLayout in 'uParamSliderLayout.pas',
  uSidewinderTypes in 'uSidewinderTypes.pas',
  uTestModel in 'uTestModel.pas',
  uSBMLClasses.rule in 'sbml\uSBMLClasses.rule.pas',
  uControllerMain in 'uControllerMain.pas',
  uModel in 'model\uModel.pas',
  uSBMLReader in 'sbml\uSBMLReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmainForm, mainForm);
  Application.Run;
end.
