unit uNetworkToModel;
 // *** Currently only one-way reactions.
interface
uses Web, JS, uModel, uSBMLClasses, uNetwork;

const DEFAULT_COMP = 'unit_compartment';
type
TNetworkToModel = class
private
  model: TModel;
  network: TNetwork;

  procedure setSpecies;
  //procedure setParameters; // Currently done in setReactions as all params in rxn equations
  procedure setReactions;
  procedure setCompartments; // Currently only one, unit compartment

public
  constructor create(newModel: TModel; newNetwork: TNetwork);
  function getModel:TModel;
 // procedure setModel(newModel: TModel);
 // procedure setNetwork(newNetwork: TNetwork);   // TODO

end;

implementation

constructor TNetworkToModel.create(newModel: TModel; newNetwork: TNetwork);
begin
  self.model := newModel;
  self.network := newNetwork;
  self.setCompartments;
  self.setSpecies;
  self.setReactions;

end;

procedure TNetworkToModel.setCompartments;
var newComp: TSBMLcompartment;
begin
// NOTE: may use TCompartmentState to get this info once implimented.
   newComp := TSBMLcompartment.create(DEFAULT_COMP);
   newComp.setSize(1.0);
   newComp.setConstant(true);
   self.model.addSBMLcompartment(newComp);
end;

procedure TNetworkToModel.setSpecies();
var i: integer;
    newSpecies: TSBMLSpecies;
    nodes: TListOfNodes;
begin
  nodes := self.network.nodes;
  for i := 0 to Length(nodes)-1 do
  begin
    newSpecies := TSBMLSpecies.create(nodes[i].state.id);
    newSpecies.setInitialConcentration(nodes[i].state.conc);
    newSpecies.setCompartment(DEFAULT_COMP);
    newSpecies.setHasOnlySubstanceUnits(false); // species units is conc.
    self.model.addSBMLspecies(newSpecies);
  end;
end;

procedure TNetworkToModel.setReactions;
var i, j, k: integer;
   newSpecReacts: array of TSBMLSpeciesReference;
   newSpecProds: array of TSBMLSpeciesReference;
   newLaw: SBMLkineticLaw;
   newParams: TSBMLparameter;
   newReaction: SBMLReaction;

begin
  for i := 0 to Length(self.network.reactions)-1 do
  begin
    setLength(newSpecReacts,0);
    setLength(newSpecProds,0);
    for j := 0 to Length(self.network.reactions[i].state.srcId)-1 do
    begin
      k := length(newSpecReacts);
     //
      if self.network.reactions[i].state.srcId[j] <> '' then
      begin
        setLength(newSpecReacts, k+1);
        newSpecReacts[k] := TSBMLSpeciesReference.create(self.network.reactions[i].state.srcId[j] +
                            self.network.reactions[i].state.id,
                            self.network.reactions[i].state.srcStoich[j] );
        newSpecReacts[k].setSpecies(self.network.reactions[i].state.srcId[j]);
        newSpecReacts[k].setConstant(true);  // default: const stoich coeff
      end;
    end;

    k := 0;
    for j := 0 to Length(self.network.reactions[i].state.destId)-1 do
    begin
      k := length(newSpecProds);

      if self.network.reactions[i].state.destId[j] <> '' then
      begin
        setLength(newSpecProds, k+1);
        newSpecProds[k] := TSBMLSpeciesReference.create(self.network.reactions[i].state.destId[j] +
                         self.network.reactions[i].state.id,
                        self.network.reactions[i].state.destStoich[j] );
        newSpecProds[k].setSpecies(self.network.reactions[i].state.destId[j]);
        newSpecProds[k].setConstant(true); // default: const stoich coeff
      end;
    end;
    newReaction := SBMLReaction.create(self.network.reactions[i].state.id, newSpecProds, newSpecReacts );
    newReaction.setReversible(false); // Currently on one-way reactions.
    newLaw := SBMLkineticLaw.create();
    for j := 0 to (self.network.reactions[i].state.rateParams.count -1) do
    begin
      newLaw.addParameter(self.network.reactions[i].state.rateParams[j].getId);
      self.model.addSBMLParameter(self.network.reactions[i].state.rateParams[j]);
    end;
    newLaw.setFormula(self.network.reactions[i].state.rateLaw);
    newReaction.setKineticLaw(newLaw);
    newReaction.setCompartment(DEFAULT_COMP);
    self.model.addSBMLReaction(newReaction);

  end;

end;

function TNetworkToModel.getModel:TModel;
begin
  Result := self.model;
end;

end.
