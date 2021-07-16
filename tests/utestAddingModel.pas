unit uTestAddingModel;

interface
uses System.SysUtils, uSimulation, uModel, uSBMLClasses, uODE_FormatUtility,
     WebLib.Forms, WEBLib.ExtCtrls, Web;

function testAddingModel(): TModel;
// just a test example for generating a model for the simulator,
implementation


function testAddingModel():  TModel; // Test, remove when needed.
var
  sbmlModel: TModel;
  speciesAr: array of TSBMLSpecies;
  paramAr: array of SBMLparameter;
  comp: SBMLcompartment;
  rxnProdStoicAr: array of Double;
  rxnProdsAr: array of String;
  rxnReactsAr: array of String;
  rxnReactsStoicAr: array of Double;
begin
  sbmlModel := TModel.create();

  SetLength(speciesAr, 3);
  speciesAr[0] := TSBMLSpecies.create('S1');
  speciesAr[0].setInitialConcentration(10);
  speciesAr[0].setCompartment('cell');
  speciesAr[1] := TSBMLSpecies.create('S2');
  speciesAr[1].setInitialConcentration(2);
  speciesAr[1].setCompartment('cell');
  speciesAr[2] := TSBMLSpecies.create('S3');
  speciesAr[2].setInitialConcentration(1);
  speciesAr[2].setCompartment('cell');
  sbmlModel.addSBMLspecies(speciesAr[0]);
  sbmlModel.addSBMLspecies(speciesAr[1]);
  sbmlModel.addSBMLspecies(speciesAr[2]);

  comp := SBMLcompartment.create('cell');
  comp.setVolume(2);
  comp.setConstant(true);
  sbmlModel.addSBMLcompartment(comp);

  SetLength(paramAr, 2);
  paramAr[0] := SBMLparameter.create('k1');
  paramAr[0].setValue(0.1);
  paramAr[1] := SBMLparameter.create('k2');
  paramAr[1].setValue(0.05);
  sbmlModel.addSBMLParameter(paramAr[0]);
  sbmlModel.addSBMLParameter(paramAr[1]);

  // Set up reactions:
  rxnProdsAr[0] := speciesAr[1].getID();
  rxnProdStoicAr[0] := 1;
  rxnReactsAr[0] := speciesAr[0].getID();
  rxnReactsStoicAr[0] := 1;
  sbmlModel.addSBMLReaction('S1toS2', rxnProdsAr, rxnProdStoicAr, rxnReactsAr,
    rxnReactsStoicAr, 'k1*S1');

  rxnProdsAr[0] := speciesAr[2].getID();
  rxnProdStoicAr[0] := 1;
  rxnReactsAr[0] := speciesAr[1].getID();
  rxnReactsStoicAr[0] := 1;
  sbmlModel.addSBMLReaction('S2toS3', rxnProdsAr, rxnProdStoicAr, rxnReactsAr,
    rxnReactsStoicAr, 'k2*S2');

   Result := sbmlModel;
end;


end.
