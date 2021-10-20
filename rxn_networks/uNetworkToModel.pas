unit uNetworkToModel;
 // *** Currently only one-way reactions.
interface
uses Web, JS, uModel, uSBMLClasses, uNetwork, uSBMLClasses.Layout;

const DEFAULT_COMP = 'unit_compartment';
type
TNetworkToModel = class
private
  model: TModel;
  layout: TSBMLLayout;
  network: TNetwork;
 // savingModel: boolean;  // true: model being saved to file, so add layout to file
  procedure setSpecies;
  //procedure setParameters; // Currently done in setReactions as all params in rxn equations
  procedure setReactions;
  procedure setCompartments; // Currently only one, unit compartment
 
public
  constructor create(newModel: TModel; newNetwork: TNetwork);
  function getModel:TModel;
 // procedure setSavingModelFlag(newVal: boolean);
 // function isSavingModel(): boolean;
 // procedure setModel(newModel: TModel);
 // procedure setNetwork(newNetwork: TNetwork);   // TODO

end;

implementation

constructor TNetworkToModel.create(newModel: TModel; newNetwork: TNetwork);
begin
  self.model := newModel;
  self.layout := TSBMLLayout.create();
  self.network := newNetwork;
  self.setCompartments;
  self.setSpecies;
  self.setReactions;
 // self.savingModel := false;

end;
{
procedure TNetworkToModel.setSavingModelFlag(newVal: boolean);
begin
  self.savingModel := newVal;
end;


function TNetworkToModel.isSavingModel(): boolean;
begin
  Result := self.savingModel;
end;
 }

procedure TNetworkToModel.setCompartments;
var newComp: TSBMLcompartment;
begin
// NOTE: may use TCompartmentState to get this info once implimented.
   newComp := TSBMLcompartment.create(DEFAULT_COMP);
   newComp.setSize(1.0);
   newComp.setConstant(true);
   self.model.addSBMLcompartment(newComp);
end;

// Layout: creates species and text glyphs:
procedure TNetworkToModel.setSpecies();
var i: integer;
    newSpecies: TSBMLSpecies;
    nodes: TListOfNodes;
    newSpGlyph: TSBMLLayoutSpeciesGlyph;
    newTextGlyph: TSBMLLayoutTextGlyph;
    newLayoutPt: TSBMLLayoutPoint;
    newDims: TSBMLLayoutDims;
begin
  nodes := self.network.nodes;
  for i := 0 to Length(nodes)-1 do
  begin
    newSpecies := TSBMLSpecies.create(nodes[i].state.id);
    newSpecies.setInitialConcentration(nodes[i].state.conc);
    newSpecies.setCompartment(DEFAULT_COMP);
    newSpecies.setHasOnlySubstanceUnits(false); // species units is conc.
    self.model.addSBMLspecies(newSpecies);
    newSpGlyph := TSBMLLayoutSpeciesGlyph.create(nodes[i].state.id);
    newLayoutPt := TSBMLLayoutPoint.create(nodes[i].getCenter.x, nodes[i].getCenter.y);
    newDims := TSBMLLayoutDims.create(nodes[i].state.w, nodes[i].state.h);
    newSpGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(newLayoutPt, newDims));
    self.layout.addSpGlyph(newSpGlyph);
    newTextGlyph := TSBMLLayoutTextGlyph.create();
    newTextGlyph.setText(nodes[i].state.id);
    newTextGlyph.setId('txt');
    newTextGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(newLayoutPt,newDims));
    self.layout.addTextGlyph(newTextGlyph);

  end;
end;

procedure TNetworkToModel.setReactions;
var i, j, k: integer;
   newSpecReacts: array of TSBMLSpeciesReference;
   newSpecProds: array of TSBMLSpeciesReference;
   newSpRefId: string;
   newLaw: SBMLkineticLaw;
   newParams: TSBMLparameter;
   newReaction: SBMLReaction;
   newRxnGlyph: TSBMLLayoutReactionGlyph;
   newSpRefGlyph: TSBMLLayoutSpeciesReferenceGlyph;
   newLayoutPt: TSBMLLayoutPoint;
   newDims: TSBMLLayoutDims;
   newCurve: TSBMLLayoutCurve;
   newLineSeg: TSBMLLayoutLineSegment;
   newCubicBezier: TSBMLLayoutCubicBezier;

begin
  for i := 0 to Length(self.network.reactions)-1 do
  begin
    newRxnGlyph := TSBMLLayoutReactionGlyph.create(self.network.reactions[i].state.id);
    newCurve := TSBMLLayoutCurve.create;
    newLayoutPt := TSBMLLayoutPoint.create(self.network.reactions[i].state.arcCenter.x,
                                          self.network.reactions[i].state.arcCenter.y);
     // add reaction center to cubic bezier as base point 1, 2:
    newCubicBezier := TSBMLLayoutCubicBezier.create();
    newCubicBezier.setBasePt1(newLayoutPt);
    newCubicBezier.setBasePt2(newLayoutPt);
    newCurve.addCubicBezier(newCubicBezier);

    setLength(newSpecReacts,0);
    setLength(newSpecProds,0);
    for j := 0 to Length(self.network.reactions[i].state.srcId)-1 do
    begin

      k := length(newSpecReacts);

     //
      if self.network.reactions[i].state.srcId[j] <> '' then
      begin
        setLength(newSpecReacts, k+1);
        newSpRefId := self.network.reactions[i].state.srcId[j] + self.network.reactions[i].state.id;
        newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId);
        newSpRefGlyph.setSpeciesGlyphId(self.network.reactions[i].state.srcId[j]);
        newSpRefGlyph.setRole('substrate');
        newDims := TSBMLLayoutDims.create(0,0);
        newLayoutPt := TSBMLLayoutPoint.create(0,0);
        newSpRefGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(newLayoutPt, newDims));
        newRxnGlyph.addSpeciesRefGlyph(newSpRefGlyph);

        newSpecReacts[k] := TSBMLSpeciesReference.create(newSpRefId,
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
        newSpRefId := self.network.reactions[i].state.destId[j] + self.network.reactions[i].state.id;
        newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId);
        newSpRefGlyph.setSpeciesGlyphId(self.network.reactions[i].state.destId[j]);
        newSpRefGlyph.setRole('product');
        newDims := TSBMLLayoutDims.create(0,0);
        newLayoutPt := TSBMLLayoutPoint.create(0,0);
        newSpRefGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(newLayoutPt, newDims));
        newRxnGlyph.addSpeciesRefGlyph(newSpRefGlyph);

        newSpecProds[k] := TSBMLSpeciesReference.create(newSpRefId,
                        self.network.reactions[i].state.destStoich[j] );
        newSpecProds[k].setSpecies(self.network.reactions[i].state.destId[j]);
        newSpecProds[k].setConstant(true); // default: const stoich coeff
      end;
    end;
    newReaction := SBMLReaction.create(self.network.reactions[i].state.id, newSpecProds, newSpecReacts );
    newReaction.setReversible(false); // Currently only one-way reactions.??
    newLaw := SBMLkineticLaw.create();
    for j := 0 to (self.network.reactions[i].state.rateParams.count -1) do
    begin
      newLaw.addParameter(self.network.reactions[i].state.rateParams[j].getId);
      if not self.model.isParameterIdinList(self.network.reactions[i].state.rateParams[j].getId) then
        self.model.addSBMLParameter(self.network.reactions[i].state.rateParams[j]);
    end;
    newLaw.setFormula(self.network.reactions[i].state.rateLaw);
    newReaction.setKineticLaw(newLaw);
    newReaction.setCompartment(DEFAULT_COMP);
    self.model.addSBMLReaction(newReaction);
    self.layout.addRxnGlyph(newRxnGlyph);
  end;

end;

function TNetworkToModel.getModel: TModel;
begin
 // if savingModel then  self.model.setSBMLLayout(self.layout);

  Result := self.model;
end;



end.
