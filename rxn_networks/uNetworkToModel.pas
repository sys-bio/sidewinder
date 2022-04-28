unit uNetworkToModel;
 // *** Currently only one-way reactions.
interface
uses Types, Web, JS, WEBLib.Graphics, strUtils, uModel, uSBMLClasses, uNetwork,
   uSBMLClasses.Layout, uSBMLClasses.Render, uNetworkCanvas, uNetworkTypes, uSidewinderTypes;

const DEFAULT_COMP = 'unit_compartment';
type
TNetworkToModel = class
private
  model: TModel;
  layout: TSBMLLayout;
  renderInfo: TSBMLRenderInformation;
  network: TNetwork;
  rxnArrowPts: array of TPointF;  // all rxn arrows are assumed to be the same for now.
  arrowBBoxDims: TSBMLLayoutDims; // contains rxnArrowPts
  procedure setSpecies;
  //procedure setParameters; // Currently done in setReactions as all params in rxn equations
  procedure setReactions;
  procedure setCompartments; // Currently only one, unit compartment
  procedure setSpeciesRendering( spState: TNodeState; specGlypId: string;
                                 specTextGlyphId:string );
  procedure setReactionRendering( rxnState: TReactionState; rxnSpRefGlyphId: string; reactant: boolean );
  function generateRxnLineEnding( id: string; fColorDefId: string;
                            rxnState: TReactionState ): TSBMLRenderLineEnding;
  procedure normalizeLineEndingPts( pts: array of TPointF );

public
  constructor create(newModel: TModel; newNetwork: TNetwork;
               layoutWidth, layoutHeight: integer; rxnEndPts: array of TPointF);

  function getModel:TModel;
  function  createSBMLBezierCurve( newRxnState: TReactionState;
            newRxnCurve: TReactionCurve; srcSide: boolean): TSBMLLayoutCubicBezier;
  function getLayout(): TSBMLLayout;
  function getRenderInformation(): TSBMLRenderInformation;
  procedure setRxnArrowPts( newPts: array of TPointF );
 // procedure setModel(newModel: TModel);
 // procedure setNetwork(newNetwork: TNetwork);   // TODO ?

end;

implementation

constructor TNetworkToModel.create( newModel: TModel; newNetwork: TNetwork;
              layoutWidth, layoutHeight: integer; rxnEndPts: array of TPointF );
begin
  self.model := newModel;
  self.setRxnArrowPts(rxnEndPts);
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

procedure TNetworkToModel.setRxnArrowPts( newPts: array of TPointF );
var i: integer;
begin
  self.normalizeLineEndingPts( newPts );
end;
       // Fit points in box of dimensions: maxX-minX, MaxY-minY
procedure TNetworkToModel.normalizeLineEndingPts( pts: array of TPointF );
var i: integer;
    minX, minY: double;
    maxX, maxY: double;
begin
  if length(pts) > 0 then
  begin
    minX := pts[0].x; minY := pts[0].y;
    maxX := pts[0].x; maxY := pts[0].y;
  end;

  setLength( self.rxnArrowPts, length(pts) );
  for i := 1 to length(pts)-1 do
    begin
      if pts[i].x < minX then minX := pts[i].x;
      if pts[i].y < minY then minY := pts[i].y;
      if pts[i].x > maxX then maxX := pts[i].x;
      if pts[i].y > maxY then maxY := pts[i].y;

    end;
  for i := 0 to length(pts)-1 do
    begin
      self.rxnArrowPts[i].x := pts[i].x - minX;
      self.rxnArrowPts[i].y := pts[i].y - minY;
    end;
  self.arrowBBoxDims := TSBMLLayoutDims.create(maxX-minX, maxY-minY);
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
    newLayoutPt := TSBMLLayoutPoint.create( nodes[i].state.x , nodes[i].state.y );
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
   newRxnCurve, newRxnGlyphCurve : TSBMLLayoutCurve;
   newLineSeg: TSBMLLayoutLineSegment;
   newCubicBezier: TSBMLLayoutCubicBezier;
   spRefCenter: TSBMLLayoutPoint;
   rxnArcCenter: TSBMLLayoutPoint;
   spRefProdCenter: TSBMLLayoutPoint;  // Use for uni-uni reactions, one line segment.
   massCenter: TPointF;  //Rxn mass center.
   nodeCenter: TPointF;
   uniUniRxn: boolean;

begin
  for i := 0 to Length(self.network.reactions)-1 do
  begin
    uniuniRxn := false;
    if (self.network.reactions[i].state.nReactants = 1) and
       (self.network.reactions[i].state.nProducts = 1) then uniUniRxn := true;

    massCenter := self.network.reactions[i].state.getMassCenter; // update massCenter;
    newRxnGlyph := TSBMLLayoutReactionGlyph.create(self.network.reactions[i].state.id);
    //newRxnCurve := TSBMLLayoutCurve.create;
    newRxnCenterPt := TSBMLLayoutPoint.create( massCenter.x, massCenter.y );

    setLength(newSpecReacts,0);
    setLength(newSpecProds,0);
    for j := 0 to self.network.reactions[i].state.nReactants-1 do
    begin
      //if containsText(self.network.reactions[i].state.srcId[i], 'NULL') then break;
      k := length(newSpecReacts);

      if self.network.reactions[i].state.srcId[j] <> '' then
      begin
        setLength(newSpecReacts, k+1);
        newRxnCurve := TSBMLLayoutCurve.create; // create bezier for newSpRefGlyph
        newSpRefId := self.network.reactions[i].state.srcId[j] + self.network.reactions[i].state.id;
        newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId);
        self.setReactionRendering( self.network.reactions[i].state, newSpRefGlyph.getId, true );
        newSpRefGlyph.setSpeciesGlyphId('speciesGlyph' + self.network.reactions[i].state.srcId[j]);
        newSpRefGlyph.setRole(SPECIES_ROLE_SUBSTRATE);
        newDims := TSBMLLayoutDims.create(0,0); // Do not need boundingBox, use curve
        if self.network.findNode( self.network.reactions[i].state.srcId[j], index ) then
        begin
          nodeCenter :=  self.network.nodes[index].state.getCenter;
          spRefCenter := TSBMLLayoutPoint.create(nodeCenter.x, nodeCenter.y);


         // if not uniUniRxn then
         // begin
            if self.network.reactions[i].state.lineType = ltBezier then
              begin
              newCubicBezier := self.createSBMLBezierCurve(self.network.reactions[i].state,
                                    self.network.reactions[i].state.reactantReactionArcs[j], true );
              newCubicBezier.setId('bezier_' + self.network.reactions[i].state.srcId[j] +
                   self.network.reactions[i].state.id);
              newRxnCurve.addCubicBezier(newCubicBezier);
              end
            else  // assume ltLine
              begin
              newLineSeg := TSBMLLayoutLineSegment.create(newRxnCenterPt, spRefCenter);
              newRxnCurve.addLineSegment(newLineSeg);
              end;
         // end;

        end
        else notifyUser('Node id not found: ' + self.network.reactions[i].state.srcId[j] );
        newSpRefGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(spRefCenter, newDims));
        newSpRefGlyph.setCurve(newRxnCurve);
        newRxnGlyph.addSpeciesRefGlyph(newSpRefGlyph);
        if length( self.network.reactions[i].state.srcStoich ) > 0 then
          newSpecReacts[k] := TSBMLSpeciesReference.create( newSpRefId,
                            self.network.reactions[i].state.srcStoich[j] )
        else newSpecReacts[k] := TSBMLSpeciesReference.create(newSpRefId);
        newSpecReacts[k].setSpecies(self.network.reactions[i].state.srcId[j]);
        newSpecReacts[k].setConstant(true);  // default: const stoich coeff
      end;
    end;

    k := 0;
    for j := 0 to self.network.reactions[i].state.nProducts-1 do
    begin
      k := length(newSpecProds);
      newRxnCurve := TSBMLLayoutCurve.create; // create bezier for newSpRefGlyph
      if self.network.reactions[i].state.destId[j] <> '' then
      begin
        index := -1;
        setLength(newSpecProds, k+1);
        newSpRefId := self.network.reactions[i].state.destId[j] + self.network.reactions[i].state.id;
        newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId);
        self.setReactionRendering( self.network.reactions[i].state, newSpRefGlyph.getId, false );
        newSpRefGlyph.setSpeciesGlyphId('speciesGlyph' + self.network.reactions[i].state.destId[j]);
        newSpRefGlyph.setRole(SPECIES_ROLE_PRODUCT);
        newDims := TSBMLLayoutDims.create(0,0);
        if self.network.findNode( self.network.reactions[i].state.destId[j], index ) then
        begin
          spRefProdCenter := TSBMLLayoutPoint.create(self.network.nodes[index].state.x +
                                    (self.network.nodes[index].state.w)  ,
                                             self.network.nodes[index].state.y +
                                             (self.network.nodes[index].state.h) );

         // if uniUniRxn then
         //   newLineSeg := TSBMLLayoutLineSegment.create(spRefCenter, spRefProdCenter)
         // else newLineSeg := TSBMLLayoutLineSegment.create(newRxnCenterPt, spRefProdCenter);
          if self.network.reactions[i].state.lineType = ltBezier then
          begin
            newCubicBezier := self.createSBMLBezierCurve(self.network.reactions[i].state,
                      self.network.reactions[i].state.productReactionArcs[j], false );
            newCubicBezier.setId('bezier_' + self.network.reactions[i].state.destId[j] +
                   self.network.reactions[i].state.id);
            newRxnCurve.addCubicBezier(newCubicBezier);
          end
          else
            begin
            newLineSeg := TSBMLLayoutLineSegment.create(newRxnCenterPt, spRefProdCenter);
            newLineSeg.setId('line_' + self.network.reactions[i].state.destId[j] +
                   self.network.reactions[i].state.id );
            newRxnCurve.addLineSegment(newLineSeg);
            end;
        end
        else notifyUser('Node id not found: ' + self.network.reactions[i].state.destId[j] );
        spRefCenter := TSBMLLayoutPoint.create(0,0);
        newSpRefGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(spRefCenter, newDims));
        newSpRefGlyph.setCurve(newRxnCurve);
        newRxnGlyph.addSpeciesRefGlyph(newSpRefGlyph);
        if length( self.network.reactions[i].state.destStoich ) > 0 then
          newSpecProds[k] := TSBMLSpeciesReference.create(newSpRefId,
                        self.network.reactions[i].state.destStoich[j] )
        else newSpecProds[k] := TSBMLSpeciesReference.create( newSpRefId );
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
    rxnArcCenter := TSBMLLayoutPoint.create(self.network.reactions[i].state.arcCenter.x,
                     self.network.reactions[i].state.arcCenter.y );
    newRxnGlyph.setBoundingBox(TSBMLLayoutBoundingBox.create(rxnArcCenter, newDims)); //libsbmljs does not support
    newRxnGlyphCurve := TSBMLLayoutCurve.create;
    newRxnGlyphCurve.addLineSegment(TSBMLLayoutLineSegment.create(rxnArcCenter,rxnArcCenter)); // point
    newRxnGlyph.setCurve(newRxnGlyphCurve);
    self.layout.addRxnGlyph(newRxnGlyph);
  end;

end;

   // set species and text glyph renderings:
procedure TNetworkToModel.setSpeciesRendering( spState: TNodeState; specGlypId: string;
                                               specTextGlyphId:string );
var newStyle: TSBMLRenderStyle;
    newRG: TSBMLRenderGroup;
    newFillColor, newOutlineColor: TColor;
    newRect: TSBMLRenderRectangle;
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
  //newStyle.addType(STYLE_TYPES[1]);  // either Type or GoId is used when saving SBML, not both
  newStyle.addGoId( specGlypId ); // each species node can have different fill/outline color
  newRect := TSBMLRenderRectangle.create(); // use default x, y
  newRect.setHeight(spState.h);
  newRect.setWidth(spState.w);
  newRect.setRx(0.40 * spState.w); // set radius of rounded corners, estimated, need to find out
  newRect.setRy(0.60 * spState.h);  // how nodes are drawn to get specific radii.
  newRect.setFill( fillColorDefId );
  newRect.setStroke( olColorDefId );
  newRect.setStrokeWidth( spState.outlineThickness );
  newRg := TSBMLRenderGroup.create;
  newRg.setRectangle( newRect );
  newRg.setStrokeWidth( spState.outlineThickness );
  newRg.setStrokeColor( olColorDefId );
  newRg.setFillColor( fillColorDefId );
  newStyle.setRenderGroup( newRg );
  self.renderInfo.addStyle( TSBMLRenderStyle.create(newStyle) );
  // Text glyph style:
  newRg.Free;
  newStyle.Free;
  newStyle := TSBMLRenderStyle.create;
  newStyle.setId( 'textStyle_' + spState.id );
  newStyle.addType(STYLE_TYPES[4]);
 // newStyle.addGoId( specTextGlyphId ); // all Text glyphs are the same
  newRg := TSBMLRenderGroup.create;
  newRg.setFontStyle('normal');
  newRg.setFontSize(DEFAULT_FONT_SIZE + 2);// need scaling factor here: trunc( DEFAULT_FONT_SIZE * scalingFactor )
  newStyle.setRenderGroup( TSBMLRenderGroup.create(newRg) );
  self.renderInfo.addStyle( TSBMLRenderStyle.create(newStyle) );
end;

procedure TNetworkToModel.setReactionRendering( rxnState: TReactionState; rxnSpRefGlyphId: string; reactant: boolean );
var newStyle: TSBMLRenderStyle;
    newRG: TSBMLRenderGroup;
    newFillColor: TColor;
    colorStr: string;
    fillColorDefId: string;
    newLineEndId: string;
    rxnArrowAdded: boolean;
  i: Integer;
begin
  rxnArrowAdded := false;
  newFillColor := rxnState.fillColor;
  colorStr := colorToHex( newFillColor ); // no # in front of hex number
  fillColorDefId :=  'fillColorReaction_' + rxnSpRefGlyphId;
  self.renderInfo.addColorDef(TSBMLRenderColorDefinition.create( colorStr, fillColorDefId) );

  newStyle := TSBMLRenderStyle.create; // for species reference glyphs
  newRg := TSBMLRenderGroup.create;
  newRg.setStrokeWidth( rxnState.thickness );
  newRg.setStrokeColor( fillColorDefId ); // same as fill
  newRg.setFillColor( fillColorDefId );
  newStyle.addGoId( rxnSpRefGlyphId );
  if reactant then
    begin
    newStyle.setId('reaction_reactant_Style_' + rxnState.id);
    //newStyle.addType(STYLE_TYPES[3]); // SPECIESREFERENCEGLYPH
    newStyle.addRole('reactant');
    end
  else    // product species ref glyph style:
    begin
    newStyle.setId('reaction_product_Style_' + rxnState.id);
    newStyle.addRole('product');
    newLineEndId := 'arrowHead_' + 'product'+ rxnState.id; // <-- different arrows for diff rxns.
    newRg.setEndHead(newLineEndId);
    for i := 0 to self.renderInfo.getNumbLineEndings -1 do
      begin
      if self.renderInfo.getLineEnding(i).getId = newLineEndId then
        rxnArrowAdded := true;
      end;
    if rxnArrowAdded = false then
      self.renderInfo.addLineEnding( self.generateRxnLineEnding( newLineEndId,
                                                  fillColorDefId, rxnState ) );
    end;

  newStyle.setRenderGroup( TSBMLRenderGroup.create(newRg) );
  self.renderInfo.addStyle( TSBMLRenderStyle.create(newStyle) );

end;

function TNetworkToModel.generateRxnLineEnding( id: string; fColorDefId: string;
                               rxnState: TReactionState ): TSBMLRenderLineEnding;
var newLineEnd: TSBMLRenderLineEnding;
    newPolygon: TSBMLRenderPolygon;
    newRGroup: TSBMLRenderGroup;
    newBBox: TSBMLLayoutBoundingBox;
    i: integer;
    x,y: double;
begin
  newLineEnd := TSBMLRenderLineEnding.create(id);
  newBBox := TSBMLLayoutBoundingBox.create();
  newBBox.setDims(self.arrowBBoxDims);
  x := self.arrowBBoxDims.getWidth/2;
  y := self.arrowBBoxDims.getHeight/2;
  newBBox.setPoint(x,y);
  newLineEnd.setBoundingBox(newBBox);
  newRGroup := TSBMLRenderGroup.create();
  newRGroup.setStrokeWidth( rxnState.thickness );
  newRGroup.setStrokeColor( fColorDefId );
  newRGroup.setFillColor( fColorDefId );
  newPolygon := TSBMLRenderPolygon.create();
  for i := 0 to length( self.rxnArrowPts ) -1 do
    begin
      console.log('pt: ',i,': ', self.rxnArrowPts[i].x,', ',self.rxnArrowPts[i].y );
      newPolygon.addPt( TSBMLRenderPoint.create(self.rxnArrowPts[i].x, self.rxnArrowPts[i].y ) );
    end;

  newPolygon.setFill( fColorDefId );
  newPolygon.setStroke( fColorDefId );
  newPolygon.setStrokeWidth( rxnState.thickness );
  newRGroup.setPolygon( newPolygon );
  newLineEnd.setRenderGroup( newRGroup );
  Result := newLineEnd;
end;

function  TNetworkToModel.createSBMLBezierCurve( newRxnState: TReactionState;
                            newRxnCurve: TReactionCurve; srcSide: boolean): TSBMLLayoutCubicBezier;
begin
  Result := TSBMLLayoutCubicBezier.create();
  if srcSide then  // from src node to rxn center:
    begin
    Result.setStart( newRxnCurve.nodeIntersectionPt.x, newRxnCurve.nodeIntersectionPt.y );
    Result.setEnd( newRxnState.arcCenter.x, newRxnState.arcCenter.y ) ;
    end
  else   // from rxn center to dest node:
    begin
    Result.setEnd( newRxnCurve.nodeIntersectionPt.x, newRxnCurve.nodeIntersectionPt.y );
    Result.setStart( newRxnState.arcCenter.x, newRxnState.arcCenter.y ) ;
    end;

  Result.setBasePt1( newRxnCurve.h1.x, newRxnCurve.h1.y );
  Result.setBasePt2( newRxnCurve.h2.x, newRxnCurve.h2.y );
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
