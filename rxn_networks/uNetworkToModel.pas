unit uNetworkToModel;
 // *** Currently only one-way reactions.
interface
uses Web, JS, WEBLib.Graphics, uModel, uSBMLClasses, uNetwork, uSBMLClasses.Layout,
   uSBMLClasses.Render, uNetworkTypes, uSidewinderTypes;

const DEFAULT_COMP = 'unit_compartment';
type
TNetworkToModel = class
private
  model: TModel;
  layout: TSBMLLayout;
  renderInfo: TSBMLRenderInformation;
  network: TNetwork;
  procedure setSpecies;
  //procedure setParameters; // Currently done in setReactions as all params in rxn equations
  procedure setReactions;
  procedure setCompartments; // Currently only one, unit compartment
  procedure setSpeciesRendering( spState: TNodeState; specGlypId: string;
                                 specTextGlyphId:string );
  procedure setReactionRendering( rxnState: TReactionState; rxnGlyphId: string );

public
  constructor create(newModel: TModel; newNetwork: TNetwork;
                     layoutWidth, layoutHeight: integer);

  function getModel:TModel;
  function getLayout(): TSBMLLayout;
  function getRenderInformation(): TSBMLRenderInformation;
 // procedure setModel(newModel: TModel);
 // procedure setNetwork(newNetwork: TNetwork);   // TODO

end;

implementation

constructor TNetworkToModel.create( newModel: TModel; newNetwork: TNetwork;
                                    layoutWidth, layoutHeight: integer );
begin
  self.model := newModel;
  self.layout := TSBMLLayout.create();
  self.renderInfo := TSBMLRenderInformation.create();
  self.layout.setDims( TSBMLLayoutDims.create(layoutWidth, layoutHeight) );
  self.layout.setId('layout' + newNetwork.id);
  self.renderInfo.setId('renderInfo' + newNetwork.id);
  self.network := newNetwork;
  self.setCompartments;
  self.setSpecies;
  self.setReactions;
  self.model.setSBMLLayout(self.layout);
  self.model.setSBMLRenderInfo(self.renderInfo);

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

// Layout: creates species and text glyphs:
procedure TNetworkToModel.setSpecies();
var i: integer;
    newSpecies: TSBMLSpecies;
    nodes: TListOfNodes;
    newSpGlyph: TSBMLLayoutSpeciesGlyph;
    newTextGlyph: TSBMLLayoutTextGlyph;
    newLayoutPt: TSBMLLayoutPoint;
    newDims: TSBMLLayoutDims;
    newColorDef: TSBMLRenderColorDefinition;
    newStyle: TSBMLRenderStyle;
    newRG: TSBMLRenderGroup;
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
    newTextGlyph.setId('txtGlyph' + nodes[i].state.id);
    newTextGlyph.setGraphicalObjId(newSpGlyph.getId);
    newTextGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(newLayoutPt,newDims));
    self.layout.addTextGlyph(newTextGlyph);
    self.setSpeciesRendering( nodes[i].state, newSpGlyph.getId, newTextGlyph.getId );

  end;
end;

procedure TNetworkToModel.setReactions;
// Currently: Rxn lines are used, no bezier curves (once bezier curve is implimented
//            then need to change). One line from node to Rxn mass center. another line
//            from rxn mass center to next node, etc ...
var i, j, k, index: integer;
   newSpecReacts: array of TSBMLSpeciesReference;
   newSpecProds: array of TSBMLSpeciesReference;
   newSpRefId: string;
   newLaw: SBMLkineticLaw;
   newParams: TSBMLparameter;
   newReaction: SBMLReaction;
   newRxnGlyph: TSBMLLayoutReactionGlyph;
   newSpRefGlyph: TSBMLLayoutSpeciesReferenceGlyph;
   newRxnCenterPt: TSBMLLayoutPoint;
   newDims: TSBMLLayoutDims;
   newRxnCurve: TSBMLLayoutCurve;
   newLineSeg: TSBMLLayoutLineSegment;
   newCubicBezier: TSBMLLayoutCubicBezier;
   spRefCenter: TSBMLLayoutPoint;
   massCenter: TPointF;  //Rxn mass center.

begin
  for i := 0 to Length(self.network.reactions)-1 do
  begin
    massCenter := self.network.reactions[i].state.getMassCenter; // update massCenter;
    newRxnGlyph := TSBMLLayoutReactionGlyph.create(self.network.reactions[i].state.id);
    newRxnCurve := TSBMLLayoutCurve.create;
    newRxnCenterPt := TSBMLLayoutPoint.create( massCenter.x, massCenter.y );
    // add reaction center to cubic bezier as start, end pt, base point 1, 2:
    // future, once Cubic Bezier implimented.
    {newCubicBezier := TSBMLLayoutCubicBezier.create();
    newCubicBezier.setStart(st);
    newCubicBezier.setEnd(end)
    newCubicBezier.setBasePt1(pt1);
    newCubicBezier.setBasePt2(pt2);
    newRxnCurve.addCubicBezier(newCubicBezier);  }

    setLength(newSpecReacts,0);
    setLength(newSpecProds,0);
    for j := 0 to self.network.reactions[i].state.nReactants-1 do
    begin

      k := length(newSpecReacts);

     //
      if self.network.reactions[i].state.srcId[j] <> '' then
      begin
        setLength(newSpecReacts, k+1);
        newSpRefId := self.network.reactions[i].state.srcId[j] + self.network.reactions[i].state.id;
        newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId);
        newSpRefGlyph.setSpeciesGlyphId('speciesGlyph' + self.network.reactions[i].state.srcId[j]);
        newSpRefGlyph.setRole('substrate');
        newDims := TSBMLLayoutDims.create(0,0); // Do not need boundingBox, use curve
        if self.network.findNode( self.network.reactions[i].state.srcId[j], index ) then
        begin
          spRefCenter := TSBMLLayoutPoint.create(self.network.nodes[index].state.x,
                                             self.network.nodes[index].state.y );
          newLineSeg := TSBMLLayoutLineSegment.create(newRxnCenterPt, spRefCenter);
          newRxnCurve.addLineSegment(newLineSeg);
        end
        else notifyUser('Node id not found: ' + self.network.reactions[i].state.srcId[j] );
        spRefCenter := TSBMLLayoutPoint.create(0,0);
        newSpRefGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(spRefCenter, newDims));

        newRxnGlyph.addSpeciesRefGlyph(newSpRefGlyph);

        newSpecReacts[k] := TSBMLSpeciesReference.create(newSpRefId,
                            self.network.reactions[i].state.srcStoich[j] );
        newSpecReacts[k].setSpecies(self.network.reactions[i].state.srcId[j]);
        newSpecReacts[k].setConstant(true);  // default: const stoich coeff
      end;
    end;

    k := 0;
    for j := 0 to self.network.reactions[i].state.nProducts-1 do
    begin
      k := length(newSpecProds);
      if self.network.reactions[i].state.destId[j] <> '' then
      begin
        index := -1;
        setLength(newSpecProds, k+1);
        newSpRefId := self.network.reactions[i].state.destId[j] + self.network.reactions[i].state.id;
        newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId);
        newSpRefGlyph.setSpeciesGlyphId('speciesGlyph' + self.network.reactions[i].state.destId[j]);
        newSpRefGlyph.setRole('product');
        newDims := TSBMLLayoutDims.create(0,0);
        if self.network.findNode( self.network.reactions[i].state.destId[j], index ) then
        begin
          spRefCenter := TSBMLLayoutPoint.create(self.network.nodes[index].state.x,
                                             self.network.nodes[index].state.y );
          newLineSeg := TSBMLLayoutLineSegment.create(newRxnCenterPt, spRefCenter);
          newLineSeg.setId('line_' + self.network.reactions[i].state.destId[j] +
                   self.network.reactions[i].state.id );
          newRxnCurve.addLineSegment(newLineSeg);
        end
        else notifyUser('Node id not found: ' + self.network.reactions[i].state.destId[j] );
        spRefCenter := TSBMLLayoutPoint.create(0,0);
        newSpRefGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(spRefCenter, newDims));
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
    newRxnGlyph.setCurve(newRxnCurve);
    self.setReactionRendering( self.network.reactions[i].state, newRxnGlyph.getId );
    self.layout.addRxnGlyph(newRxnGlyph);
  end;

end;

   // set species and text glyph renderings:
procedure TNetworkToModel.setSpeciesRendering( spState: TNodeState; specGlypId: string;
                                               specTextGlyphId:string );
var newStyle: TSBMLRenderStyle;
    newRG: TSBMLRenderGroup;
    newFillColor, newOutlineColor: TColor;
    colorStr: string;
    fillColorDefId, olColorDefId: string;
begin
  newFillColor := spState.fillColor;
  colorStr := colorToHex( newFillColor ); // no # in front of hex number
  console.log('Fill color: ', colorStr );
  fillColorDefId :=  'fillColorSpecies_' + specGlypId;
  self.renderInfo.addColorDef(TSBMLRenderColorDefinition.create( colorStr, fillColorDefId) );
  newOutlineColor := spState.outlineColor;
  colorStr := colorToHex( newOutlineColor );
  olColorDefId := 'outlineColorSpecies_' + specGlypId;
  self.renderInfo.addColorDef(TSBMLRenderColorDefinition.create( colorStr, olColorDefId) );
  // species glyph style:
  newStyle := TSBMLRenderStyle.create;
  newStyle.setId('speciesStyle_' + spState.id);
  newStyle.addType(STYLE_TYPES[1]);
  newStyle.addGoId( specGlypId );
  newRg := TSBMLRenderGroup.create;
  newRg.setStrokeWidth( spState.outlineThickness );
  newRg.setStrokeColor( olColorDefId );
  newRg.setFillColor( fillColorDefId );
  newStyle.setRenderGroup( TSBMLRenderGroup.create(newRg) );
  self.renderInfo.addStyle( TSBMLRenderStyle.create(newStyle) );
  // Text glyph style:
  newRg.Free;
  newStyle.Free;
  newStyle := TSBMLRenderStyle.create;
  newStyle.setId( 'textStyle_' + spState.id );
  newStyle.addType(STYLE_TYPES[4]);
  newStyle.addGoId( specTextGlyphId );
  newRg := TSBMLRenderGroup.create;
  newRg.setFontStyle('normal');
  newStyle.setRenderGroup( TSBMLRenderGroup.create(newRg) );
  self.renderInfo.addStyle( TSBMLRenderStyle.create(newStyle) );
end;

procedure TNetworkToModel.setReactionRendering( rxnState: TReactionState; rxnGlyphId: string );
var newStyle: TSBMLRenderStyle;
    newRG: TSBMLRenderGroup;
    newFillColor: TColor;
    colorStr: string;
    fillColorDefId: string;
begin
  newFillColor := rxnState.fillColor;
  colorStr := colorToHex( newFillColor ); // no # in front of hex number
 // console.log('Fill color: ', colorStr );
  fillColorDefId :=  'fillColorReaction_' + rxnGlyphId;
  self.renderInfo.addColorDef(TSBMLRenderColorDefinition.create( colorStr, fillColorDefId) );
  newStyle := TSBMLRenderStyle.create;
  newStyle.setId('reactionStyle_' + rxnState.id);
  newStyle.addType(STYLE_TYPES[2]);
  newStyle.addType(STYLE_TYPES[3]); // species reference as well?
  newStyle.addGoId( rxnGlyphId );
  newRg := TSBMLRenderGroup.create;
  newRg.setStrokeWidth( rxnState.thickness );
  newRg.setStrokeColor( fillColorDefId ); // same as fill
  newRg.setFillColor( fillColorDefId );
  newStyle.setRenderGroup( TSBMLRenderGroup.create(newRg) );
  self.renderInfo.addStyle( TSBMLRenderStyle.create(newStyle) );
end;

function TNetworkToModel.getModel: TModel;
begin
  Result := self.model;
end;

function TNetworkToModel.getLayout(): TSBMLLayout;
begin
  Result := self.layout;
end;

function TNetworkToModel.getRenderInformation(): TSBMLRenderInformation;
begin
  Result := self.renderInfo;
end;

end.
