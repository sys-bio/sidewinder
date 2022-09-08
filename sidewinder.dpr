program sidewinder;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  ufMain in 'ufMain.pas' {mainForm: TWebForm} {*.html},
  uSBMLClasses in 'sbml\uSBMLClasses.pas',
  uMath in 'math\uMath.pas',
  Adamsbdf in 'math\lsoda\Adamsbdf.pas',
  lsodamat in 'math\lsoda\lsodamat.pas',
  odeEquations in 'math\lsoda\odeEquations.pas',
  uMatrix in 'math\lsoda\uMatrix.pas',
  uVector in 'math\lsoda\uVector.pas',
  uControllerNetwork in 'rxn_networks\uControllerNetwork.pas',
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
  ufVarSelect in 'ufVarSelect.pas' {VarSelectForm: TWebForm} {*.html},
  uPlotLayout in 'plot\uPlotLayout.pas',
  uParamSliderLayout in 'uParamSliderLayout.pas',
  uSidewinderTypes in 'uSidewinderTypes.pas',
  uTestModel in 'uTestModel.pas',
  uSBMLClasses.rule in 'sbml\uSBMLClasses.rule.pas',
  uControllerMain in 'uControllerMain.pas',
  uModel in 'model\uModel.pas',
  uSBMLReader in 'sbml\uSBMLReader.pas',
  uSBMLWriter in 'sbml\uSBMLWriter.pas',
  uBuildRateLaw in 'rxn_networks\uBuildRateLaw.pas',
  uNetworkToModel in 'rxn_networks\uNetworkToModel.pas',
  uPlotPanel in 'plot\uPlotPanel.pas',
  uSBMLClasses.Layout in 'sbml\uSBMLClasses.Layout.pas',
  uASTNode in 'math\uASTNode.pas',
  uSBMLClasses.Render in 'sbml\uSBMLClasses.Render.pas',
  uSBMLClasses.FuncDefinition in 'sbml\uSBMLClasses.FuncDefinition.pas',
  ufAssignments in 'ufAssignments.pas' {FormAssignments: TWebForm} {*.html},
  ufSelectExample in 'ufSelectExample.pas' {formExamples: TWebForm} {*.html},
  uTestCase in 'tests\uTestCase.pas',
  uScrollingTypes in 'Graph\ScrollingChart\uScrollingTypes.pas',
  uWebComps in 'Graph\ScrollingChart\uWebComps.pas',
  uWebContainer in 'Graph\ScrollingChart\uWebContainer.pas',
  uWebDataSource in 'Graph\ScrollingChart\uWebDataSource.pas',
  uWebGlobalData in 'Graph\ScrollingChart\uWebGlobalData.pas',
  uWebScrollingChart in 'Graph\ScrollingChart\uWebScrollingChart.pas',
  uWebStage in 'Graph\ScrollingChart\uWebStage.pas',
  uGraphPanel in 'Graph\uGraphPanel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TFormAssignments, FormAssignments);
  Application.CreateForm(TformExamples, formExamples);
  Application.Run;
end.
