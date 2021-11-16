unit uNetwork;

// This file represents methods and objects to represent a simple graph of edges and nodes.

// Functions to add:

// Delete reactions
// Delete nodes
// Set colors, line widths etc
// Add text to node, what to do about fonts as we need a canvas for that. Maybe library
// will have a call back to request this information from an external canvas?

interface


Uses SysUtils, Classes, Types, libthreejs, WEBLib.Graphics, Math, WEBLib.Utils,
     WEBLib.JSON, Web, System.Generics.Collections, uNetworkTypes, uBuildRateLaw,
     uSBMLClasses, uModel, uSBMLClasses.Layout;

const
  // The following constant is the distance between the outer
  // and inner rectangles, the 'dead' space between a node and
  // the launch point for a reaction arc (edge)
  NODE_ARC_DEADSPACE = 8;
  DEFAULT_REACTION_THICKNESS = 3;
  MAGIC_IDENTIFER = 'NM01';  // Identifier for json output, 01 refers to version number

type
  // Replace TColor with something like this.
  // Need utility function to manipulate TARGB    (or RGBA?)
  TARGB = record
      color : int32;
  end;

  TNetworkEvent = procedure(sender: TObject) of object; // Network has been changed.
  TAutoLayoutEvent = procedure(sender: TObject) of object; // Network needs autot layout
    // TODO: need to know if just species or rate/param vals have only changed.

  TParent = class (TObject)
    selected : boolean;
  end;

  TCompartmentState = record
      id : string;
      volume : double;
  end;

  TCompartment = class
      state : TCompartmentState;

  end;

  // This is used to support undo operations and json reading and writing
  TNodeState = record
       id : string;
       conc: double;   // conc of species, assume unit volume
       x, y, w, h : double;
       fillColor, outlineColor : TColor;
       outlineThickness : integer;

       procedure saveAsJSON (nodeObject : TJSONObject);
       procedure loadFromJSON (obj : TJSONObject);
       procedure loadFromSBML (obj : TSBMLLayoutSpeciesGlyph; initVal: double);
  end;

  TNode = class (TParent)
       state : TNodeState;
       addReactionSelected : boolean;
       dx, dy : double; // for autolayout algorithm

       function    overNode (x, y : double) : boolean;
       function    isInRectangle (selectionRect : TRect) : boolean;
       function    getCenter : TPointF;
       function    getNodeBoundingBox : TBoundingBoxSegments;
       procedure   unSelect;
       function    getCurrentState : TNodeState;
       procedure   loadState (node : TNodeState);
       constructor create (id : string);
  end;

  TListOfNodes = array of TNode;
  TListOfNodeStates = array of TNodeState;

  TReactionState = record
      id : string;
      arcCenter: TPointF;
      selected : boolean;
      nReactants, nProducts : integer;
      srcId   : array[0..5] of string;
      destId  : array[0..5] of string;  // Stores node Ids, saved to json
      srcPtr  : array[0..5] of TNode;
      destPtr : array[0..5] of TNode;  // These are not saved to json as they are pointers
      rateLaw : string;                // Mass action rate law for reaction
      srcStoich : array[0..5] of double; // src Stoichiometric coefficients of the rxn.
      destStoich : array[0..5] of double;// dest  "
      rateParams :TList<TSBMLparameter>; // rate and param consts,
      fillColor : TColor;
      thickness : integer;

      procedure saveAsJSON (reactionObject : TJSONObject);
      procedure loadFromJSON (obj : TJSONObject);
      procedure loadFromSBML (glyphRxn : TSBMLLayoutReactionGlyph; modelRxn: SBMLReaction;
                            paramAr: array of TSBMLparameter; compAr: array of TSBMLcompartment);
      function getMassCenter(): TPointF; // Calc mass center for reaction, where each node has mass of 1.
  end;

  TReaction = class (TParent)
       state : TReactionState;
       procedure   unSelect;
       function    getCurrentState : TReactionState;
       procedure   loadState (nodes : TListOfNodes; reactionState : TReactionState);
       procedure   setRateRule;
       procedure   setDefaultParams;
       function    getRateRule: string;
       constructor create; overload;
       constructor create (id : string; src, dest : TNode); overload;
   end;

  TListOfReactions = array of TReaction;

  TListOfReactionStates = array of TReactionState;

  TNetworkSavedState = record
      Id : string;
      savedReactions : TListOfReactionStates;
      savedNodes : TListOfNodeStates;
  end;

  TNetwork = class (TObject)
    private
      procedure buildNetworkFromSBMLLayout(model: TModel); // model has SBML layout described
      procedure autoBuildNetworkFromSBML(model: TModel); // No SBML layout, build own layout
      FNetworkEvent: TNetworkEvent;
      FAutoLayoutEvent: TAutoLayoutEvent;
    public
       id : string;
       nodes : TListOfNodes;
       reactions : TListOfReactions;
       scalingFactor : double;
       savedState : TNetworkSavedState;

       function    unScale (x : double) : double;

       procedure   loadModel (modelStr : string);
       procedure   loadSBMLModel (model: TModel);
       function    convertToJSON : string;
       function    overNode (x, y : double; var node : TNode) : boolean; overload;
       function    overNode (x, y : double; var index : integer) : TNode; overload;
       function    overEdge (x, y : double; var reactionIndex : integer) : TReaction;
       function    overCentroid (x, y : double; var reactionIndex : integer) : TReaction;

       function    findNode (id : string; var index : integer) : boolean;
       function    addNode (id : string) : TNode; overload;
       function    addNode (id : string; x, y : double) : TNode; overload;
       function    addNode (id : string; x, y, w, h : double) : TNode; overload;
       function    addNode (state : TNodeState): TNode; overload;
       function    buildNullNodeState(rxnId: string; num: integer): TNodeState; // used to add null node for one species rxn

       procedure   computeUniUniCoords (reaction : TReaction; srcNode, destNode : TNode);
       procedure   computeAnyToAnyCoordinates (reaction : TReaction; sourceNodes, destNodes : array of TNode);

       function    addUniUniReaction (id : string; src, dest : TNode) : integer; overload;
       function    addReaction (state : TReactionState) : TReaction; overload;
       function    addReaction (reaction : TReaction) : integer; overload;
       procedure   updateReactions(node: TNode);  // Node Id changes
       function    addAnyToAnyEdge (id: string; sourceNodes, destNodes : array of TNode; var edgeIndex : integer) : TReaction;
       procedure   unSelectAll;
       procedure   unReactionSelect;
       procedure   centerNetwork (w, h : integer);
       procedure   clear;
       function    hasReactions (node : TNode) : boolean;
       function    getCurrentState : TNetworkSavedState;
       procedure   loadState (networkState : TNetworkSavedState);
       property OnNetworkEvent: TNetworkEvent read FNetworkEvent write FNetworkEvent;
       property OnAutoLayoutEvent: TAutoLayoutEvent read FAutoLayoutEvent write FAutoLayoutEvent;
       procedure networkEvent(sender: TObject); // Notify listener that Network has changed.
       procedure autoLayoutEvent(sender: TObject); // Notify listener that Network layout needs to be configured.
       constructor Create (id : string);
  end;

implementation

Uses WEBLib.Dialogs, uDrawReaction, uGraphUtils;

function colorToHexString( color: TColor ): string; // hex string for writing to SBML
// From: https://delphi.fandom.com/wiki/Colors_in_Delphi
begin        // Note: no # added to front of hex string.
   result := IntToHex(GetRValue( ColorToRGB(color) ), 2) +
            IntToHex(GetGValue( ColorToRGB(color) ), 2) +
            IntToHex(GetBValue( ColorToRGB(color) ), 2) ;
end;

function HexToTColor(sColor : string) : TColor;
begin
   result :=  RGB( StrToInt('$'+Copy(sColor, 1, 2)),
                   StrToInt('$'+Copy(sColor, 3, 2)),
                   StrToInt('$'+Copy(sColor, 5, 2)) ) ;
end;

function loadColorFromJSON (obj : TJSONObject; colorName : string) : TColor;
var colorPair : TJSONPair; colorObj : TJSONObject;
    R, G, B : byte;
begin
  colorPair := obj.Get(colorName);
  colorObj := colorPair.JsonValue as TJSONObject;
  R := StrToInt (colorObj.getJSONValue ('R'));
  G := StrToInt (colorObj.getJSONValue ('G'));
  B := StrToInt (colorObj.getJSONValue ('B'));
  result := RGB (R, G, B);
end;


function saveColorToJSON (color : TColor) : TJSONObject;
var A : byte;
begin
  console.log('Color: ', colorToHexString(color) );
  result := TJSONObject.Create;
  result.AddPair ('R', TJSONNumber.Create (GetRValue (color)));
  result.AddPair ('G', TJSONNumber.Create (GetGValue (color)));
  result.AddPair ('B', TJSONNumber.Create (GetBValue (color)));
  // There is no opacity, so set to 255
  result.AddPair ('A', TJSONNumber.Create (255));

end;

procedure TNodeState.saveAsJSON (nodeObject : TJSONObject);
var colorObject : TJSONObject;
begin
  nodeObject.AddPair ('id', id);
  nodeObject.AddPair ('conc', TJSONNumber.Create(conc));  // to be added.
  nodeObject.AddPair ('x', TJSONNumber.Create (x));
  nodeObject.AddPair ('y', TJSONNumber.Create (y));
  nodeObject.AddPair ('w', TJSONNumber.Create (w));
  nodeObject.AddPair ('h', TJSONNumber.Create (h));

  nodeObject.AddPair ('fillColor', saveColorToJSON (fillColor));
  nodeObject.AddPair ('outlineColor', saveColorToJSON (outLineColor));;
  nodeObject.AddPair ('outlineThickness', TJSONNumber.Create (outlineThickness));
end;


procedure TNodeState.loadFromJSON (obj : TJSONObject);
begin
   id := obj.GetJSONValue('id');
   conc := strtofloat (obj.GetJSonValue('conc'));  // to be added
   x := strtofloat (obj.GetJSONValue ('x'));
   y := strtofloat (obj.GetJsonValue ('y'));
   h := strtofloat (obj.GetJSONValue ('h'));
   w := strtofloat (obj.GetJsonValue ('w'));

   fillColor := loadColorFromJSON (obj, 'fillColor');
   outlineColor :=  loadColorFromJSON (obj, 'outlineColor');
   outlineThickness := strtoint (obj.GetJsonValue ('outlineThickness'));
end;

procedure TNodeState.loadFromSBML (obj : TSBMLLayoutSpeciesGlyph; initVal: double);
begin
  id := obj.getSpeciesId;
  conc := initVal;
  x := obj.getBoundingBox().getPoint().getX;
  y := obj.getBoundingBox().getPoint().getY;
  h := obj.getBoundingBox().getDims().getHeight;
  w := obj.getBoundingBox().getDims().getWidth;
  // default values, Currently no sbml Render package used:
  fillColor := RGB(255,204,153);// clWebPeachPuff;
  outlineColor := RGB(255,102,0);
  outlineThickness := 3;
end;

procedure TReactionState.saveAsJSON (reactionObject : TJSONObject);
var speciesArray, reactantArray : TJSONArray;
    paramArray: TJSONArray;
    i : integer;
    jso : TJSONObject;
begin
  reactionObject.AddPair ('id', id);
  reactionObject.AddPair ('arcCenterX', floattostr (arcCenter.X));
  reactionObject.AddPair ('arcCenterY', floattostr (arcCenter.Y));

  speciesArray := TJSONArray.Create;

  reactionObject.AddPair ('species', speciesArray);

  jso := TJsonObject.Create();
  for i := 0 to nReactants - 1 do
      jso.AddPair(TJsonPair.Create(srcPtr[i].state.id, TJSONValue.Create (srcStoich[i])));
  speciesArray.Add(jso);

  jso := TJsonObject.Create();
  for i := 0 to nProducts - 1 do
      jso.AddPair(TJsonPair.Create(destPtr[i].state.id, TJSONValue.Create (destStoich[i])));
  speciesArray.Add(jso);

  // **
  paramArray := TJSONArray.Create;
  reactionObject.AddPair('parameters', paramArray);
  jso := TJsonObject.Create();
  for i := 0 to rateParams.count -1 do
    begin
      if rateParams[i].isSetIdAttribute then
        jso.AddPair(TJsonPair.create(rateParams[i].getId, TJSONValue.create(rateParams[i].getValue)));
    end;
  paramArray.Add(jso);

  reactionObject.AddPair ('rateLaw', rateLaw);
  reactionObject.AddPair ('fillColor', saveColorToJSON (fillColor));
  reactionObject.AddPair ('thickness', TJSONNumber.Create (thickness));
end;


procedure TReactionState.loadFromJSON (obj : TJSONObject);
var speciesObject : TJSONObject;
    speciesArray : TJSONArray;
    reactantObject : TJSONObject;
    paramArray : TJSONArray;
    paramObject : TJSONObject;
    newParam : TSBMLparameter;
    pa : TJSONPair;
   // stoich : integer;
    speciesName : string;
    i : integer;

begin
   id := obj.GetJSONValue('id');
   if obj.Get ('species') <> nil then
      begin
      speciesArray := obj.Get ('species').JsonValue as TJSONArray;
      reactantObject := speciesArray.Items[0] as TJSONObject;
      nReactants := reactantObject.count;
      for i:= 0 to Length(srcId) -1 do
        begin
          srcId[i] := '';
          if i < Length(srcStoich) then
            srcStoich[i] := 0.0; // assume same size as srcId
        end;
      for i := 0 to nReactants - 1 do
          begin
          pa := reactantObject.Get(i);
          srcId[i] := pa.JsonString.value;
          srcStoich[i] := trunc (strtofloat (pa.JsonValue.Value));
          end;

      reactantObject := speciesArray.Items[1] as TJSONObject;
      nProducts := reactantObject.count;
      rateParams := TList<TSBMLparameter>.create;
      for i:= 0 to Length(destId)-1 do
        begin
          destId[i] := '';
          if i < Length(destStoich) then
            destStoich[i] := 0.0; // assume same size as destId
        end;
      for i := 0 to nProducts - 1 do
          begin
          pa := reactantObject.Get(i);
          destId[i] := pa.JsonString.value;
          destStoich[i] := trunc (strtofloat (pa.JsonValue.Value));  // stoich not negative
          end;
      end
   else
      raise Exception.Create ('No species in reaction');

   if obj.Get('parameters')<> nil then
     begin
       paramArray := obj.Get('parameters').JsonValue as TJSONArray;
       paramObject := paramArray.Items[0] as TJSONObject;
       rateParams := TList<TSBMLparameter>.create;
       for i := 0 to paramObject.count -1 do
         begin
         pa := paramObject.Get(i);
         newParam := TSBMLparameter.create(pa.JsonString.value );
         newParam.setValue( strtofloat( pa.JsonValue.value));
         rateParams.Add(newParam);
         end;
     end
   else
     raise Exception.Create ('No parameters in reaction');

   rateLaw := obj.GetJSONValue('rateLaw');
   fillColor := loadColorFromJSON (obj, 'fillColor');
   thickness := strtoint (obj.GetJSONValue('thickness'));
end;


procedure TReactionState.loadFromSBML (glyphRxn : TSBMLLayoutReactionGlyph;
            modelRxn: SBMLReaction; paramAr: array of TSBMLparameter; compAr: array of TSBMLcompartment);
var  newParam : TSBMLparameter;
     speciesName : string;
     i, j : integer;
begin
  id := modelRxn.getId;
  rateParams := TList<TSBMLparameter>.create;
  for i := 0 to Length(destStoich) -1 do
     self.destStoich[i] := 0.0; // Initialize array
  for i := 0 to Length(srcStoich) -1 do
     self.srcStoich[i] := 0.0; // Initialize array
  self.nReactants := modelRxn.getNumReactants;
  // get reactants:
  for i := 0 to Length(srcId) -1 do
    srcId[i] := '';
  for i := 0 to modelRxn.getNumReactants - 1 do
    begin
      srcId[i] := '';
      srcId[i] := modelRxn.getReactant(i).getSpecies;
      srcStoich[i] := modelRxn.getReactant(i).getStoichiometry;
    end;
    // get products:
  nProducts := modelRxn.getNumProducts;
  for i := 0 to Length(destId) -1 do
    destId[i] := '';
  for i := 0 to modelRxn.getNumProducts - 1 do
    begin
      destId[i] := modelRxn.getProduct(i).getSpecies;
      destStoich[i] := modelRxn.getProduct(i).getStoichiometry;
    end;

    // For now,  just grab all parameters, not just ones used in reactions:
    // Grab parameters that are used in reaction:
  for j := 0 to Length(paramAr) - 1 do
    begin
     // console.log('param: ',modelRxn.getKineticLaw.getParameter(i),', model param: ',paramAr[j].getId);
      rateParams.Add(paramAr[j]);
    end;
  // Currently just add all compartments as params:
  for i := 0 to Length(compAr) -1 do
    begin
      newParam := TSBMLparameter.create(compAr[i].getId);
      if compAr[i].isSetSize then
        newParam.setValue(compAr[i].getSize)
      else
        newParam.setValue(compAr[i].getVolume);
      rateParams.Add(newParam);
    end;

  rateLaw := modelRxn.getKineticLaw.getFormula;
  fillColor := clWebLightSteelBlue;
  thickness := DEFAULT_REACTION_THICKNESS;

  // Do not need to set arcCenter, remove any arcCenter assignments:
  // arc center: order of assignment:
  //  1.  default, if no layout info available.
  //  2.  grab BasePoint 1 of cubicBez if exists
  //  3.  grap 1st line segment, start point if exists
  //  4.  Grab bounding box center point.
  arcCenter.x := 20; arcCenter.y := 20; // default, need something else here?
  if glyphRxn <> nil then  // check if any layout info available
  begin
    if glyphRxn.getCurve <> nil then
      begin
        if glyphRxn.getCurve.getNumCubicBeziers > 0 then
        begin
          arcCenter.x := glyphRxn.getCurve.getCubicBezier(0).getBasePoint1.getX;
          arcCenter.y := glyphRxn.getCurve.getCubicBezier(0).getBasePoint1.gety;
        end
        else
        begin
          if glyphRxn.getCurve.getNumCurveSegments > 0 then
            begin
              arcCenter.x := glyphRxn.getCurve.getLineSegment(0).getStartPt.getX;
              arcCenter.y := glyphRxn.getCurve.getLineSegment(0).getStartPt.getY;
            end;

        end;
      end
    else
    begin
      if glyphRxn.boundingBoxIsSet then
      begin
        arcCenter.x := glyphRxn.getBoundingBox.getPoint.getX;
        arcCenter.Y := glyphRxn.getBoundingBox.getPoint.getY;
      end;
    end;
  end;

end;

/// Calc mass center for reaction state, where each node has mass of 1.
function TReactionState.getMassCenter: TPointF;
var i: integer;
   xSum, ySum: double;
begin
  xSum := 0; ySum := 0;
  for i := 0 to self.nReactants - 1 do
    begin              // Include width and height of nodes in calcs:
      xSum := xSum + self.srcPtr[i].state.x + self.srcPtr[i].state.w;
      ySum := ySum + self.srcPtr[i].state.y + self.srcPtr[i].state.h;
    end;
  for i := 0 to self.nProducts - 1 do
    begin
      xSum := xSum + self.destPtr[i].state.x + self.destPtr[i].state.w;
      ySum := ySum + self.destPtr[i].state.y + self.destPtr[i].state.h;
    end;
  Result := TPointF.create( xSum/(self.nReactants + nProducts),
                     ySum/(self.nReactants + nProducts) );
end;

procedure AddJSONHeader (obj : TJSONObject);
begin

end;

// ------------------------------------------------------------------------------

constructor TNetwork.create (id : string);
begin
  self.id := id;
end;

procedure TNetwork.networkEvent(sender: TObject);
begin
  if Assigned(FNetworkEvent) then
    FNetworkEvent(sender);
end;

procedure TNetwork.autoLayoutEvent(sender: TObject);
begin
  if Assigned(self.FAutoLayoutEvent) then
    self.FAutoLayoutEvent(sender);
end;

procedure TNetwork.loadModel (modelStr : string);  // JSON string
var JSONRoot, JSONValue1, JSONNodeArray, JSONReactionArray : TJSONValue;
    node : TNode;
    reaction : TReaction;
    nodeState : TNodeState;
    reactionState: TReactionState;
    ar : TJSONArray; nj : TJSONObject;
    i, j, k, index : integer;
    pair : TJSONPair;
begin
  clear;
  JSONRoot := TJSONObject.parseJSONValue (modelStr);
  pair := (JSONRoot as TJSONObject).Get('magicIdentifier');
  if pair = nil then
     raise Exception.Create ('JSON file not a valid network model');

  if (pair.JSONValue as TJSONString).Value <> MAGIC_IDENTIFER then
     raise Exception.Create ('JSON file a value network model, but verison: ' + MAGIC_IDENTIFER + ' not supported');

  JSONValue1 :=(JSONRoot as TJSONObject).Get('id').JSONValue;
  Id := (JSONValue1 as TJSONString).Value;

  if (JSONRoot as TJSONObject).Get ('nodes') <> nil then
      begin
      JSONNodeArray := (JSONRoot as TJSONObject).Get('nodes').JSONValue;
      ar := JSONNodeArray as TJSONArray;
      for i := 0 to ar.Count - 1 do
          begin
          nj := ar.Items[i] as TJSONObject;
          nodeState.loadFromJSON (nj);
          addNode (nodeState);
          end;
     end;

  // Get the array of reactions
  if (JSONRoot as TJSONObject).Get ('reactions') <> nil then
     begin
     JSONreactionArray := (JSONRoot as TJSONObject).Get('reactions').JSONValue;

     ar := JSONreactionArray as TJSONArray;
     for i := 0 to ar.Count - 1 do
         begin
         nj := ar.Items[i] as TJSONObject;
         reactionState.loadFromJSON (nj);
         reaction := addReaction (reactionState);
         // Set the node pointers based on the node Ids
         for j := 0 to reaction.state.nReactants - 1 do
             for k := 0 to length (nodes) - 1 do
                 begin
                 if nodes[k].state.id = reaction.state.srcId[j] then
                    begin
                    reaction.state.srcPtr[j] := nodes[k];
                    break;
                    end;
                 end;

         for j := 0 to reaction.state.nProducts - 1 do
             for k := 0 to length (nodes) - 1 do
                 if nodes[k].state.id = reaction.state.destId[j] then
                    begin
                    reaction.state.destPtr[j] := nodes[k];
                    break;
                    end;
          end;
     end;
     self.networkEvent(nil); // Notify listener
end;

procedure TNetwork.loadSBMLModel (model : TModel);
begin
 // TextGlyphs typically are labels for nodes (they are ignored).
  if (model.getSBMLLayout <> nil) and (model.getSBMLLayout.getNumSpGlyphs >0) then
    begin
      self.buildNetworkFromSBMLLayout(model);
    end
  else self.autoBuildNetworkFromSBML(model);
end;

procedure TNetwork.buildNetworkFromSBMLLayout(model: TModel);
var i, j, k, l: integer;
   modelLayout: TSBMLLayout;
   speciesGlyph: TSBMLLayoutSpeciesGlyph;
   reactionGlyph: TSBMLLayoutReactionGlyph;
   reactionGlyphId: string; // name of reaction, used to find reaction details.
   spAr: array of string;
   initVal: double;
   node : TNode;
   reaction : TReaction;
   nodeState : TNodeState;
   reactionState: TReactionState;
begin
    modelLayout := model.getSBMLLayout;
    // Do not get layout dimensions? not really necessary?
    // if needed then need to set NetworkPB to width and height.
    for i := 0 to modelLayout.getNumSpGlyphs - 1 do
      begin
        initVal := 0;
        spAr := model.getS_Names;
        speciesGlyph := modelLayout.getSpGlyph(i);
        for j := 0 to Length(spAr) -1 do
        begin
         // console.log('loadSBMLModel, species: ',spAr[j],', spglyph: ',speciesGlyph.getSpeciesId);
          if spAr[j] = speciesGlyph.getSpeciesId then
          begin
            initVal := model.getS_initVals[j];
          end;

        end;
        nodeState.loadFromSBML (speciesGlyph, initVal);
        addNode (nodeState);
      end;

   // load reactions:
    for i := 0 to modelLayout.getNumRxnGlyphs - 1 do
      begin
        reactionGlyph := modelLayout.getRxnGlyph(i);
        reactionGlyphId := reactionGlyph.getReactionId;
        for j := 0 to model.getNumReactions - 1 do
          begin
            if reactionGlyphId = model.getReaction(j).getID then
            begin
              reactionState.loadFromSBML (reactionGlyph, model.getReaction(j),
                   model.getSBMLparameterAr, model.getSBMLcompartmentsArr());
              reaction := addReaction (reactionState);
             // Set the node pointers based on the node Ids
              for k := 0 to reaction.state.nReactants - 1 do
                for l := 0 to length (nodes) - 1 do
                  begin
                    if nodes[l].state.id = reaction.state.srcId[k] then
                      begin
                        reaction.state.srcPtr[k] := nodes[l];
                        break;
                      end;
                  end;

              for k := 0 to reaction.state.nProducts - 1 do
                for l := 0 to length (nodes) - 1 do
                  if nodes[l].state.id = reaction.state.destId[k] then
                    begin
                      reaction.state.destPtr[k] := nodes[l];
                      break;
                    end;
            end;

             // ************************
          end;
      end;
end;


procedure TNetwork.autoBuildNetworkFromSBML(model: TModel);
 var i, j, k, l, index: integer;
   spAr: array of string;
   initVal: double;
   node : TNode;
   reaction : TReaction;
   nodeState : TNodeState;
   reactionState: TReactionState;
 begin
    spAr := model.getS_Names;
    for j := 0 to Length(spAr) -1 do
      begin
        nodeState.id := spAr[j];
        nodeState.conc := model.getS_initVals[j];
        nodeState.x := 20 + j; // default values
        nodeState.y := 60 + j; //   "
        nodeState.w := 60;     //   "
        nodeState.h := 40;     //   "
        nodeState.fillColor := RGB(255,204,153);// clWebPeachPuff;
        nodeState.outlineColor := RGB(255,102,0);
        nodeState.outlineThickness := 3;
        self.addNode(nodeState);

      end;

   // load reactions:
    for j := 0 to model.getNumReactions - 1 do
        begin
          reactionState.loadFromSBML (nil, model.getReaction(j), model.getSBMLparameterAr,
                                model.getSBMLcompartmentsArr());

          reaction := addReaction (reactionState);
          reaction.state.arcCenter.x := 20 + j*2;
          reaction.state.arcCenter.y := 22 + j*2;
             // Set the node pointers based on the node Ids
          if reaction.state.nReactants < 1 then
          begin
            nodeState := buildNullNodeState(reaction.state.id, j);
            if self.findNode(nodeState.id, index) then
              begin
                // fill in rest of reactionstate:
                reaction.state.srcId[0] := self.nodes[index].state.id;
                reaction.state.srcPtr[0] := self.nodes[index];
              end
            else
              begin
                self.addNode(nodeState);
                reaction.state.srcId[0] := nodeState.id;
                if self.findNode(nodeState.id, index) then
                  reaction.state.srcPtr[0] := self.nodes[index]
                else
                  begin
                    reaction.state.srcPtr[0] := nil;
                    console.log('null node id not found!');
                  end;
              end;
            reaction.state.nReactants := 1; // add null node.
          end
          else
          begin
            for k := 0 to reaction.state.nReactants - 1 do
              for l := 0 to length (nodes) - 1 do
                begin
                  if nodes[l].state.id = reaction.state.srcId[k] then
                    begin
                      reaction.state.srcPtr[k] := nodes[l];
                      break;
                    end;
                end;
          end;

          if reaction.state.nProducts < 1 then
          begin
            nodeState := buildNullNodeState(reaction.state.id, j);
            if self.findNode(nodeState.id, index) then
              begin
                reaction.state.destId[0] := self.nodes[index].state.id;
                reaction.state.destPtr[0] := self.nodes[index];
              end
            else
              begin
                self.addNode(nodeState);
                reaction.state.destId[0] := nodeState.id;
                if self.findNode(nodeState.id, index) then
                  reaction.state.destPtr[0] := self.nodes[index]
                else
                  begin
                    reaction.state.destPtr[0] := nil;
                    console.log('dest null node id not found!');
                  end;
              end;
            reaction.state.nProducts := 1;  // add null node.
          end
          else
          begin
            for k := 0 to reaction.state.nProducts - 1 do
              for l := 0 to length (nodes) - 1 do
                if nodes[l].state.id = reaction.state.destId[k] then
                  begin
                    reaction.state.destPtr[k] := nodes[l];
                    break;
                  end;
          end;

        end;
     self.autoLayoutEvent(nil); // Generate layout
 end;

 function TNetwork.buildNullNodeState(rxnId: String; num: integer): TNodeState;
 var nodeState: TNodeState;
 begin
   nodeState.id := rxnId + '_Null';
   nodeState.conc := 0.0;
   nodeState.x := 22 + 2*num ; // default values
   nodeState.y := 62 + 2*num; //   "
   nodeState.w := 60;  //   "
   nodeState.h := 40;  //   "
   nodeState.fillColor := clSilver;
   nodeState.outlineColor := RGB(255,102,0);
   nodeState.outlineThickness := 3;
   result := nodeState;
 end;

function  TNetwork.convertToJSON : string;
var JSONRoot, headerObj, modelId, nodeObject, reactionObject : TJSONObject;
    jsonArray : TJSONArray;
    i : integer;
    jsonvalue : TJSONValue;
begin
  JSONRoot := TJSONObject.Create;
  headerObj := TJSONObject.create;
  JSONRoot.AddPair ('magicIdentifier', MAGIC_IDENTIFER);
  JSONRoot.AddPair ('header', headerObj); // Currently empty

  modelId := TJSONObject.Create;
  JSONRoot.AddPair ('id', id);
  jsonArray := TJSONArray.Create;
  for i := 0 to length (nodes) - 1 do
      begin
      nodeObject := TJSONObject.Create;
      nodes[i].state.saveAsJSON (nodeObject);
      jsonArray.Add (nodeObject);
      end;

  if length (nodes) > 0 then
     begin
     jSONRoot.AddPair ('nodes', jsonArray);

     jsonArray := TJSONArray.Create;
     for i := 0 to length (reactions) - 1 do
         begin
         reactionObject := TJSONObject.Create;
         reactions[i].state.saveAsJSON (reactionObject);
         jsonArray.Add (reactionObject);
         end;
     if length (reactions) > 0 then
        jSONRoot.AddPair ('reactions', jsonArray);
     end;

  result := JSONRoot.ToJSON;
end;


// Check if node is connected to any reactions
function TNetwork.hasReactions (node : TNode) : boolean;
var i, j : integer;
begin
  result := False;
  for i := 0 to length (reactions) - 1 do
      begin
      for j := 0 to length(reactions[i].state.srcPtr) do
        begin
          if reactions[i].state.srcPtr[j] = node then
            exit (True);
        end;
      for j := 0 to length(reactions[i].state.destPtr) do
        begin
          if reactions[i].state.destPtr[j] = node then
            exit (True);
        end;

      end;
end;


procedure TNetwork.updateReactions(node: TNode); // nodeId changes
var i, j: integer;
begin
  if self.hasReactions(node) then
  begin
    for i := 0 to length (self.reactions) - 1 do
    begin
      for j := 0 to length(reactions[i].state.srcId) do
      begin
        if reactions[i].state.srcPtr[j] = node then
        begin
          reactions[i].state.srcId[j] := node.state.id;
        end;
      end;
      for j := 0 to length(reactions[i].state.destId) do
      begin
        if reactions[i].state.destPtr[j] = node then
        begin
            reactions[i].state.destId[j] := node.state.id;
        end;
      end;
       // Update rateLaws with new nodeId of all reactions:
      if self.reactions[i].getRateRule <> '' then
        begin
          self.reactions[i].setRateRule;
        end;

    end;

  end;

end;


procedure TNetwork.loadState (networkState : TNetworkSavedState);
var i : integer; ln, b : integer;
begin
  clear;

  self.id := networkstate.Id;
  ln := length (networkState.savedNodes);
  setLength (nodes, length (networkState.savedNodes));
  for i := 0 to length (networkState.savedNodes) - 1 do
      begin
      nodes[i] := TNode.Create;
     // console.log('loadstate: dy,height: ', nodes[i].dy, ' dx:: ',nodes[i].dx);
      nodes[i].state := networkState.savedNodes[i];
      end;

  ln := length (networkState.savedReactions);
  setLength (reactions, ln);
  for i := 0 to ln - 1 do
      begin
      reactions[i] := TReaction.Create;
      reactions[i].loadState (nodes, networkState.savedReactions[i]);
      end;
end;


function TNetwork.getCurrentState : TNetworkSavedState;
var i : integer; ln : integer;
begin
  result.id := id;
  ln := length (nodes);
  setLength (result.savedNodes, ln);
  for i := 0 to ln - 1 do
      result.savedNodes[i] := nodes[i].getCurrentState;

  ln := length (reactions);
  setLength (result.savedReactions, ln);
  for i := 0 to ln - 1 do
     result.savedReactions[i] := reactions[i].getCurrentState;
end;


function TNetwork.unScale (x : double) : double;
begin
  result := trunc (x / scalingFactor);
end;


function TNetwork.findNode (id : string; var index : integer) : boolean;
var i : integer;
begin
  result := False;
  for i := 0 to length (nodes) - 1 do
      if nodes[i].state.id = id then
         begin
         index := i;
         result := True;
         end;
end;

function TNetwork.addNode (id : string; x, y : double) : TNode;
begin
  setlength (nodes, length (nodes) + 1);
  nodes[length (nodes)-1] := TNode.create (id);
  //console.log('Node dx: ', nodes[length(nodes) -1].dx, ', dy: ',nodes[length(nodes) -1].dy);
  //console.log('Node h: ', nodes[length(nodes) -1].state.h, ', width: ',nodes[length(nodes) -1].state.w);
  result := nodes[length (nodes)-1];
  result.state.x := x; result.state.y := y;
  result.state.Id := Id;
  self.networkEvent(nil); // Notify listener
end;


function TNetwork.addNode (id : string; x, y, w, h : double) : TNode;
begin
  setlength (nodes, length (nodes) + 1);
  nodes[length (nodes)-1] := TNode.create (id);
  result := nodes[length (nodes)-1];
  result.state.x := x; result.state.y := y;
  result.state.h := h; result.state.w := w;
  result.state.Id := Id;
  self.networkEvent(nil); // Notify listener
end;


function TNetwork.addNode (state : TNodeState): TNode;
begin
  setlength (nodes, length (nodes) + 1);
  nodes[length (nodes)-1] := TNode.create (id);
  result := nodes[length (nodes)-1];
  result.state := state;
  self.networkEvent(nil); // Notify listener
end;


function TNetwork.addNode (id : string) : TNode;
begin
  setlength (nodes, length (nodes) + 1);
  nodes[length (nodes)-1] := TNode.create (id);
  result := nodes[length (nodes)-1];
  self.networkEvent(nil); // Notify listener

end;


function TNetwork.addUniUniReaction (id : string; src, dest : TNode) : integer;
begin
  setlength (reactions, length (reactions) + 1);
  reactions[length (reactions)-1] := TReaction.create (id, src, dest);
  result := length (reactions) - 1;
  self.networkEvent(nil); // Notify listener
end;


function TNetwork.addReaction (state : TReactionState) : TReaction;
begin
  setlength (reactions, length (reactions) + 1);
  reactions[length (reactions)-1] := TReaction.create;
  result := reactions[length (reactions)-1];
  result.state := state;
  self.networkEvent(nil); // Notify listener
end;


function TNetwork.addReaction (reaction : TReaction) : integer;
begin
  setlength (reactions, length (reactions) + 1);
  reactions[length (reactions)-1] := reaction;
  result := length (reactions);
  self.networkEvent(nil); // Notify listener
end;


procedure TNetwork.computeUniUniCoords (reaction : TReaction; srcNode, destNode : TNode);
begin
  // To be implemented
end;


procedure TNetwork.computeAnyToAnyCoordinates (reaction : TReaction; sourceNodes, destNodes : array of TNode);
begin

end;


//function TNetwork.AddAnyToAnyEdge (sourceNodes, destNodes : array of TNode; var edgeIndex : integer) : TReaction;
function TNetwork.AddAnyToAnyEdge (id: string; sourceNodes, destNodes : array of TNode; var edgeIndex : integer) : TReaction;
var newReaction : TReaction;
    i : integer;
    nSource, nDestination : integer;
begin
  nSource := length (sourceNodes);
  nDestination := length (destNodes);

  newReaction := TReaction.Create;

  newReaction.state.nReactants := nSource;
  newReaction.state.nProducts := nDestination;
  for i := 0 to nSource - 1 do
      begin
      newReaction.state.srcId[i] := sourceNodes[i].state.id;
      newReaction.state.srcPtr[i] := sourceNodes[i];
      end;
  //    newEdge.srcConnectedNodeList.Add (TConnectedNode.Create (sourceNodes[i], -1));
  for i := 0 to nDestination - 1 do
      begin
      newReaction.state.destId[i] := destNodes[i].state.id;
      newReaction.state.destPtr[i] := destNodes[i];
      end;

  //    newEdge.destConnectedNodeList.Add (TConnectedNode.Create (destNodes[i], 1));

  newReaction.state.id := id; //getUniqueReactionName();
  edgeIndex := addReaction (newReaction);

  // Update Node to inform it which edges it is connected to, required when
  // Nodes are moved so that we can adjust surrounding beziers nicely
  //for i := 0 to nSource - 1 do
  //    sourceNodes[i].ConnectedEdgeList.Add (TConnectedEdge.Create (NewEdge));
  //for i := 0 to nDestination - 1 do
  //    destNodes[i].ConnectedEdgeList.Add (TConnectedEdge.Create (NewEdge));

  // Special case for AnyToAny if uniuni, no arccenter for uniuni
  if (nSource = 1) and (nDestination = 1) then
     computeUniUniCoords (newReaction, sourceNodes[0], destNodes[0])
  else
     computeAnyToAnyCoordinates (newReaction, sourceNodes, destNodes);

  // add Rate rule
  if newReaction.getRateRule = '' then
  begin
    newReaction.setDefaultParams;
    newReaction.setRateRule;
  end;
  self.networkEvent(nil); // Notify listener
  result := newReaction;
end;


function TNetwork.overNode (x, y : double; var index : integer) : TNode;
var i : integer;
begin
  result := nil;
  for  i := 0 to length (nodes) - 1 do
      if nodes[i].overNode (x, y) then
         begin
         index := i;
         exit (nodes[i]);
         end;
end;

function TNetwork.overNode (x, y : double; var node : TNode) : boolean;
var i : integer;
begin
  result := False;
  for  i := 0 to length (nodes) - 1 do
      if nodes[i].overNode (x, y) then
         begin
         node := nodes[i];
         exit (True);
         end;
end;


function dist (x1, y1, x2, y2 : double) : double;
begin
  result := sqrt (sqr(x2-x1) + sqr (y2-y1));
end;


// www.jeffreythompson.org/collision-detection/line-point.php
function ptOnLine (p1, p2 : TPointF; px, py : double) : boolean;
var d1, d2 : double; lineLen, buffer : double;
begin
  d1 := dist(px, py, p1.x, p1.y);
  d2 := dist(px, py, p2.x, p2.y);
  lineLen := dist(p1.x, p1.y, p2.x, p2.y);
  buffer := 0.2;    // higher the number = less accurate
  if (d1 + d2 >= lineLen-buffer) and (d1 + d2 <= lineLen+buffer) then
    result := True
  else
    result := False;
end;


function TNetwork.overEdge(x, y : double; var reactionIndex : integer) : TReaction;
var i, j : integer;  p1, p2 : TPointF;
begin
  result := nil;

  for  i := 0 to length (reactions) - 1 do
       begin
       for j := 0 to reactions[i].state.nReactants - 1 do
           begin
           p1 := reactions[i].state.srcPtr[j].getCenter;
           p2 := computeCentroid (reactions[i]);
           if ptOnLine (p1, p2, x, y) then
              begin
              reactionIndex := i;
              exit (reactions[i]);
              end;
           end;

       for j := 0 to reactions[i].state.nProducts - 1 do
           begin
           p2 := reactions[i].state.destPtr[j].getCenter;
           p1 := computeCentroid (reactions[i]);
           if ptOnLine (p1, p2, x, y) then
              begin
              reactionIndex := i;
              exit (reactions[i]);
              end;
           end;
       end;
end;


function TNetwork.overCentroid (x, y : double; var reactionIndex : integer) : TReaction;
begin
  //oldX := x; oldY := y;
end;


procedure TNetwork.clear;
var i : integer;
begin
  for i := 0 to length (reactions) - 1 do
      reactions[i].Free;
  setLength (reactions, 0);

 for i := 0 to length (nodes) - 1 do
      nodes[i].Free;
  setLength (nodes, 0);
  self.networkEvent(nil);
end;


procedure TNetwork.unSelectAll;
var node : TNode; reaction : TReaction;
begin
  for node in nodes do
      begin
      node.selected := false;
      node.addReactionSelected := false;
      end;
  for reaction in reactions do
      reaction.selected := false;
end;


procedure TNetwork.unReactionSelect;
var node : TNode;
begin
  for node in nodes do
      node.addReactionSelected := false;
end;


procedure TNetwork.centerNetwork (w, h : integer);
var i : integer;
    sumx, sumy, cx, cy : double;
begin
  sumx := 0; sumy := 0;
  for i := 0 to length (nodes) - 1 do
      begin
      sumx := sumx + nodes[i].state.x;
      sumy := sumy + nodes[i].state.y;
      end;
  cx := sumx/length (nodes);
  cy := sumy/length (nodes);

for i := 0 to length (nodes) - 1 do
    begin
    if hasReactions (nodes[i]) then
       begin
       nodes[i].state.x := nodes[i].state.x + (w/2 - cx);
       nodes[i].state.y := nodes[i].state.y + (h/2 - cy);
       end;
    end;
end;


// -------------------------------------------------------------------------

constructor TNode.create (id : string);
begin
  self.state.id := id;
  self.state.conc := 0.0;
  state.w := 60; state.h := 40;
  selected := false;
  addReactionSelected := false;
  state.fillColor := RGB(255,204,153);// clWebPeachPuff;
  state.outlineColor := RGB(255,102,0);
end;



procedure TNode.loadState (node : TNodeState);
begin
  state := node;
end;


function TNode.getCurrentState : TNodeState;
begin
  result := state;
end;


function TNode.getCenter : TPointF;
begin
  result.X := state.x + (state.w / 2);
  result.Y := state.y + (state.h / 2);
end;


// Construct the outer rectangle segments which forms the boundary
// where arcs start and stop at nodes.
function TNode.getNodeBoundingBox : TBoundingBoxSegments;
var tx, ty, tw, th : double;
begin
  tx := state.x;
  ty := state.y;
  tw := state.w;
  th := state.h;

  Result[1].p.x := tx - NODE_ARC_DEADSPACE;
  Result[1].p.y := ty - NODE_ARC_DEADSPACE;
  Result[1].q.x := tx + tw + NODE_ARC_DEADSPACE;
  Result[1].q.y := ty - NODE_ARC_DEADSPACE;

  Result[2].p.x := tx + tw + NODE_ARC_DEADSPACE;
  Result[2].p.y := ty - NODE_ARC_DEADSPACE;
  Result[2].q.x := tx + tw + NODE_ARC_DEADSPACE;
  Result[2].q.y := ty + th + NODE_ARC_DEADSPACE;

  Result[3].p.x := tx + tw + NODE_ARC_DEADSPACE;
  Result[3].p.y := ty + th + NODE_ARC_DEADSPACE;
  Result[3].q.x := tx - NODE_ARC_DEADSPACE;
  Result[3].q.y := ty + th + NODE_ARC_DEADSPACE;

  Result[4].p.x := tx - NODE_ARC_DEADSPACE;
  Result[4].p.y := ty + th + NODE_ARC_DEADSPACE;
  Result[4].q.x := tx - NODE_ARC_DEADSPACE;
  Result[4].q.y := ty - NODE_ARC_DEADSPACE;
end;


function TNode.overNode (x, y : double) : boolean;
begin
  if (x > self.state.x) and (y > self.state.y) and (x < self.state.x + self.state.w) and (y < self.state.y + self.state.h) then
     result := True
  else
     result := False;
end;


function TNode.IsInRectangle (selectionRect : TRect) : boolean;
var scalingFactor : double;
    sl, st, sr, sb :  double;
begin
  //scalingFactor := (ParentNode.NetworkRef as TNetwork).scalingFactor;
  result := True;
  sl := selectionRect.Left;///scalingFactor;
  st := selectionRect.Top;///scalingFactor;
  sr := selectionRect.Right;///scalingFactor;
  sb := selectionRect.Bottom;///scalingFactor;

  if (sl < state.x) and (state.x + state.w < sr) and
     (st < state.y)  and (state.y + state.h < sb) then
     result := True
  else result := False;
end;


procedure TNode.UnSelect;
begin
  selected := false;
end;


// -------------------------------------------------------------------------

constructor TReaction.Create;
begin
  state.fillColor := clWebLightSteelBlue;
  state.thickness := DEFAULT_REACTION_THICKNESS;
  selected := False;
end;

constructor TReaction.create (id : string; src, dest : TNode);

begin
  Create;
  state.nReactants := 1; state.nProducts := 1;
  state.id := id;
  state.srcPtr[0] := src;
  state.destPtr[0] := dest;
  // These are used for undo and loading from json file
  state.srcId[0] := src.state.id;
  state.destId[0] := dest.state.id;
  self.setDefaultParams;
  self.setRateRule();
end;

procedure TReaction.setDefaultParams(); // Note: need to just use stoich values of reaction
var newRateConst,newParam: TSBMLparameter;
  i: Integer;
begin
  newRateConst := TSBMLparameter.create(RXN_k_F+ state.id);
  newRateConst.setValue(1.0);
  state.rateParams := TList<TSBMLparameter>.create;
  state.rateParams.Add(newRateConst);
  for i := 0 to state.nReactants -1 do
  begin
    state.srcStoich[i] := 1.0; // default
  end;
  for i := 0 to state.nProducts do
  begin
    state.destStoich[i] := 1.0;
  end;

end;

procedure TReaction.loadState (nodes : TListOfNodes; reactionState : TReactionState);
var i, j, k : integer;
    n : integer;
begin
  state := reactionState;
  //for i := 0 to 5 do
 //     begin
  //    state.srcId[i] := reactionState.srcId[i];
  //    state.destId[i] := reactionState.destId[i];
  //    end;

  n := length (nodes);
  // Set the node pointers based on the node Ids
  for j := 0 to state.nReactants - 1 do
      begin
      for k := 0 to n - 1 do
          begin
           if nodes[k].state.id = state.srcId[j] then
             begin
             state.srcPtr[j] := nodes[k];
             break;
             end;
          end;
      end;

  for j := 0 to state.nProducts - 1 do
      for k := 0 to n - 1 do
          if nodes[k].state.id = state.destId[j] then
              begin
              state.destPtr[j] := nodes[k];
              break;
              end;
end;


function TReaction.getCurrentState : TReactionState;
begin
  result := state;  // This works so long as we don't have dynamic arrays.
end;


procedure TReaction.unSelect;
begin
 selected := false;
end;

procedure TReaction.setRateRule; // Build rate rule
var newRate: tRateLaw;
begin
  newRate := tRateLaw.create(state.nReactants, state.nProducts, state.srcId,
               state.destId, state.srcStoich, state.destStoich, state.rateParams);
  state.rateLaw := newRate.getRateFormula();
//  console.log('setRateRule: ',state.rateLaw);
end;

function TReaction.getRateRule: string;
begin
  Result := state.rateLaw;
end;

end.

