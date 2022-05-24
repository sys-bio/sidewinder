unit uNetwork;

// This file represents methods and objects to represent a
// simple reaction network of reactions and species.

// All coordinates specified here are world coordinates.

// uNetworkCanvas.pas is responible for converting the
// world view into a screen view and should take care of
// things like panning and zooming.

interface


Uses SysUtils, Classes, Types, libthreejs, WEBLib.Graphics, Math, WEBLib.Utils,
     WEBLib.JSON, Web, System.Generics.Collections, uNetworkTypes, uBuildRateLaw,
     uSBMLClasses, uModel, uSBMLClasses.Layout, uSBMLClasses.Render, uSBMLClasses.Rule,
     uSBMLClasses.FuncDefinition, uSidewinderTypes;

const
  // The following constant is the distance between the outer
  // and inner rectangles, the 'dead' space between a node and
  // the launch point for a reaction arc (edge)
  NODE_ARC_DEADSPACE = 8;
  DEFAULT_REACTION_THICKNESS = 3;
  DEFAULT_NODE_BOUNDARY_SPECIES_COLOR = clSkyBlue;
  DEFAULT_REACTION_COLOR = clWebLightSteelBlue;
  DEFAULT_NODE_OUTLINE_THICKNESS = 3;
  MAGIC_IDENTIFER = 'NM01';  // Identifier for json output, 01 refers to version number
  MAX_NODE_CONTROL_POINTS = 4;
  NULL_NODE_TAG = '_Null';

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
       species: string; // typically id and species are the same (aliases will not be)
       conc: double;    // conc of species, assume unit volume
       boundarySp: boolean; // true: boundary species (not part of any reaction)
       x, y, w, h : double;
       fillColor, outlineColor : TColor;
       outlineThickness : integer;
       function  getCenter : TPointF;
       procedure saveAsJSON (nodeObject : TJSONObject);
       procedure loadFromJSON (obj : TJSONObject);
       procedure loadFromSBML (obj : TSBMLLayoutSpeciesGlyph; initVal: double; boundary: boolean;
          nodeStyle: TSBMLRenderStyle; nodeColorDefList: TList<TSBMLRenderColorDefinition>);
       function  clone : TNodeState;
  end;

  TNode = class (TParent)
     private
        //function getControlRects (x, y, w, h : double) : TRectangularArray;
        //function PtInRect (rect : TCanvasRectF; pt : TPointF) : boolean;
     public
       state : TNodeState;
       addReactionSelected : boolean;
       dx, dy : double; // for autolayout algorithm

       function    overNode (x, y : double) : boolean;
       function    overControlRectangle (x, y : double;  var selectedNodeGrabRectangle : integer) : boolean;
       function    isInRectangle (selectionRect : TCanvasRectF) : boolean;
       function    getNodeBoundingBox : TBoundingBoxSegments;
       function    computeStartingPoint (endPt : TPointF) : TPointF;
       procedure   unSelect;
       function    getCurrentState : TNodeState;
       procedure   loadState (node : TNodeState);
       constructor create (id : string);
  end;

  TListOfNodes = array of TNode;
  TListOfNodeStates = array of TNodeState;

  TReactionState = record
      id : string;            // Unique id for the reaction
      arcCenter  : TPointF;    // The point that joins the reactant to product arcs
      selected   : boolean;
      nReactants, nProducts : integer;
      srcId      : array of string;
      destId     : array of string;  // Stores node Ids, saved to json
      srcPtr     : array of TNode;
      destPtr    : array of TNode;  // These are not saved to json as they are pointers
      rateLaw    : string;                // Mass action rate law for reaction
      srcStoich  : array of double; // src Stoichiometric coefficients of the rxn.
      destStoich : array of double; // dest         ''
      lineType   : TReactionLineType;  // line, bezier or line segment
      reactantReactionArcs : array of TReactionCurve;
      productReactionArcs  : array of TReactionCurve;
      rxnLineEnding : TList<TPointF>; // Typically an arrow polygon

      rateParams : TList<TSBMLparameter>; // rate and param consts,
      fillColor  : TColor;
      thickness  : integer;

      procedure createReactantSpace (nReactants : integer);
      procedure createProductSpace (nProducts : integer);
      function  processReactionSpeciesReferenceCurves(newGlyphRxn: TSBMLLayoutReactionGlyph;
               newModelRxn: SBMLReaction; newSpGlyphList: TList<TSBMLLayoutSpeciesGlyph>;
               reactionRenderInfo: TSBMLRenderInformation ): boolean;
      procedure processReactionCurves( newRxn: TSBMLLayoutReactionGlyph ;
                                  intProdNodeIndex, intReactNodeIndex: integer );
      procedure importSBMLBezierCurve( newBezier: TSBMLLayoutCubicBezier; intNode: integer;
                                       reactantSide: boolean );
      procedure convertSBMLLineToBezier( currentLine: TSBMLLayoutLineSegment;
                 totalSegments: integer; intNode: integer; reactantSide: boolean );
      procedure calcArcCenterFromSBMLLayout( newSBMLRxn: TSBMLLayoutReactionGlyph );
      function  setBezierHandles(): boolean;
      procedure saveAsJSON (reactionObject : TJSONObject);
      procedure loadFromJSON (obj : TJSONObject);
      procedure loadFromSBML (glyphRxn : TSBMLLayoutReactionGlyph; modelRxn: SBMLReaction;
         paramAr: array of TSBMLparameter; funcDefList: TList<TSBMLFuncDefinition>;
         spGlyphList: TList<TSBMLLayoutSpeciesGlyph>; compAr: array of TSBMLcompartment;
         modelRender: TSBMLRenderInformation);

      // Convert any SBML func defs that are used for reaction kinetic laws:
      function convertFuncDefsToKineticLaws(funcDefList: TList<TSBMLFuncDefinition>;
                strKinLaw: string ): string; // Returns updated kinetic law
      function getMassCenter(): TPointF; // Calc mass center for reaction, where each node has mass of 1.
      function clone : TReactionState;
  end;

  TReaction = class (TParent)
       state : TReactionState;

       procedure   unSelect;
       function    getCurrentState : TReactionState;
       procedure   loadState (nodes : TListOfNodes; reactionState : TReactionState);
       procedure   setRateRule;
       procedure   setDefaultParams;
       function    getRateRule: string;
       procedure   moveReaction (dx, dy : double);
       function    overCentroid (x, y : double) : boolean;
       procedure   adjustArcCentres(vx, vy: double);
       function    IsInRectangle (selectionBox : TCanvasRectF) : boolean;
       function    overBezierHandle (x, y : double;
                        var isReactantSide : boolean;
                        var bezierId: integer;
                        var handleId : TCurrentSelectedBezierHandle;
                        var handleCoords: TPointF): Boolean;

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
      listOfSBMLRules: TList<TSBMLRule>;
      procedure buildNetworkFromSBMLLayout(model: TModel); // model has SBML layout described
      procedure autoBuildNetworkFromSBML(model: TModel); // No SBML layout, build own layout
      FNetworkEvent: TNetworkEvent;
      FAutoLayoutEvent: TAutoLayoutEvent;
    public
       id : string;
       nodes : TListOfNodes;
       reactions : TListOfReactions;
       savedState : TNetworkSavedState;

       function    loadModel (modelStr : string): string;
       procedure   loadSBMLModel (model: TModel);
       // TODO: attach assignments to nodes??
       function    getSpeciesRenderStyle(newSpeciesGlyph:TSBMLLayoutSpeciesGlyph;
                          newModel: TModel): TSBMLRenderStyle;
       function    getColorDefs(newStyle: TSBMLRenderStyle;
            newRenderInfo: TSBMLRenderInformation): TList<TSBMLRenderColorDefinition>;
       function    convertToJSON : string;
       function    overNode (x, y : double; var node : TNode) : boolean; overload;
       function    overNode (x, y : double; var index : integer) : TNode; overload;
       function    overNodeControlRectangle (x, y : double; node : TNode; var selectedNodeGrabRectangle : integer) : boolean;
       function    overReaction (x, y : double; var reactionIndex, arcId : integer) : boolean;
       function    overBezierHandle (x, y : double;
                       selectedReaction : integer;
                       var isReactant : boolean;
                       var bezierId : integer;
                       var handleId : TCurrentSelectedBezierHandle;
                       var handleCoords: TPointF): Boolean;

       function    findNode (id : string; var index : integer) : boolean;
       function    isNodeinNodeArray(node: TNode; nodeAr: array of TNode): integer; // returns index if found, -1 if not
       function    addNode (id : string) : TNode; overload;
       function    addNode (id : string; x, y : double) : TNode; overload;
       function    addNode (id : string; x, y, w, h : double) : TNode; overload;
       function    addNode (state : TNodeState): TNode; overload;
       function    buildNullNodeState(rxnId: string; num: integer): TNodeState; // used to add null node for one species rxn
       procedure   addRule (newRule: TSBMLRule);
       function    getRule (index: integer): TSBMLRule;
       function    getRuleWithVarId (varId: string): TSBMLRule;
       function    getNumRules(): integer;
       procedure   computeAnyToAnyCoordinates (reaction : TReaction; sourceNodes, destNodes : array of TNode);

       function    findReaction(id: string; var index: integer): boolean;
       function    addReaction (state : TReactionState) : TReaction; overload;
       function    addReaction (reaction : TReaction) : integer; overload;
       procedure   updateReactions(node: TNode);  // Node Id changes
       procedure   updateReactionParamVal( paramId: string; newVal: double );
       function    addAnyToAnyReaction (id: string; sourceNodes, destNodes : array of TNode; var edgeIndex : integer) : TReaction;
       procedure   unSelectAll;
       procedure   unReactionSelect;
       function    adjustNetworkCenterOffset (w, h : integer): TPointF;// get center offset for centering network
       procedure   centerNetwork (w, h : integer);
       procedure   clear;
       function    hasReactions (node : TNode) : boolean;
       function    strReplaceParamName(oldSubStr: string; newSubStr: string): boolean; // simple string replace.
       function    getCurrentState : TNetworkSavedState;
       procedure   loadState (networkState : TNetworkSavedState);
       property OnNetworkEvent: TNetworkEvent read FNetworkEvent write FNetworkEvent;
       property OnAutoLayoutEvent: TAutoLayoutEvent read FAutoLayoutEvent write FAutoLayoutEvent;
       procedure networkEvent(sender: TObject); // Notify listener that Network has changed.
       procedure autoLayoutEvent(sender: TObject); // Notify listener that Network layout needs to be configured.
       constructor Create (id : string);
  end;

implementation

Uses WEBLib.Dialogs, uDrawReaction, uNetworkCanvas, uGraphUtils;

function colorToHexString( color: TColor ): string; // hex string for writing to SBML
// From: https://delphi.fandom.com/wiki/Colors_in_Delphi
begin        // Note: no # added to front of hex string.
   result := IntToHex(GetRValue( ColorToRGB(color) ), 2) +
            IntToHex(GetGValue( ColorToRGB(color) ), 2) +
            IntToHex(GetBValue( ColorToRGB(color) ), 2) ;
end;

function HexToTColor(sColor : string) : TColor;
begin  // Note opacity (alpha) is not implemented here ( 2 rightmost hex digits ).
  // TODO: check if rightmost 2 hexdigits are present and remove.
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
//var colorObject : TJSONObject;
begin
  nodeObject.AddPair ('id', self.id);
  nodeObject.AddPair('species',self.species);
  if self.boundarySp then
    nodeObject.AddPair('boundary', 'true')
  else nodeObject.AddPair('boundary', 'false');
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
var intBC: string;
begin
   intBC := '';
   self.id := obj.GetJSONValue('id');
   self.species := obj.GetJSONValue('species');
   intBC := obj.GetJSONValue('boundary');
   if intBC = 'true' then
     self.boundarySp := true
   else self.boundarySp := false;
   self.conc := strtofloat (obj.GetJSonValue('conc'));  // to be added
   self.x := strtofloat (obj.GetJSONValue ('x'));
   self.y := strtofloat (obj.GetJsonValue ('y'));
   self.h := strtofloat (obj.GetJSONValue ('h'));
   self.w := strtofloat (obj.GetJsonValue ('w'));

   self.fillColor := loadColorFromJSON (obj, 'fillColor');
   self.outlineColor :=  loadColorFromJSON (obj, 'outlineColor');
   self.outlineThickness := strtoint (obj.GetJsonValue ('outlineThickness'));
end;

procedure TNodeState.loadFromSBML (obj : TSBMLLayoutSpeciesGlyph; initVal: double;
  boundary: boolean; nodeStyle: TSBMLRenderStyle; nodeColorDefList: TList<TSBMLRenderColorDefinition>);
var i: integer;
    strokeCFound, fillCFound: boolean;
begin
  strokeCFound := false;
  fillCFound := false;
  self.id := obj.getId;
  self.species := obj.getSpeciesId;
  self.boundarySp := boundary;
  self.conc := initVal;
  self.x := obj.getBoundingBox().getPoint().getX;
  self.y := obj.getBoundingBox().getPoint().getY;
  self.h := obj.getBoundingBox().getDims().getHeight;
  self.w := obj.getBoundingBox().getDims().getWidth;

  if nodeStyle <> nil then
    begin
    for i := 0 to nodeColorDefList.Count -1 do
      begin
      if nodeStyle.getRenderGroup.getStrokeColor = nodeColorDefList[i].getId then
        begin
        self.outlineColor := HexToTColor( nodeColorDefList[i].getValue() );
        strokeCFound := true;
        end;
      if nodeStyle.getRenderGroup.getFillColor = nodeColorDefList[i].getId then
        begin
        self.fillColor := HexToTColor( nodeColorDefList[i].getValue() );
        fillCFound := true;
        end;
      end;

      if strokeCFound = false then
        self.outlineColor := RGB(255,102,0); // default
      if fillCFound = false then
        self.fillColor := RGB(255,204,153);  // default
      self.outLineThickness := nodeStyle.getRenderGroup.getStrokeWidth;
      if self.outlineThickness < 1 then self.outlineThickness := DEFAULT_NODE_OUTLINE_THICKNESS;

     // TODO: render node shape
    end
  else // default values:
    begin
    fillColor := RGB(255,204,153);// clWebPeachPuff;
    outlineColor := RGB(255,102,0);
    outlineThickness := DEFAULT_NODE_OUTLINE_THICKNESS;  //3;
    // TODO call function to draw default node shape
    end;

end;

function TNodeState.getCenter : TPointF;
begin
  result.X := self.x + (self.w / 2);
  result.Y := self.y + (self.h / 2);
end;

procedure TReactionState.saveAsJSON (reactionObject : TJSONObject);
var speciesArray, controlPt : TJSONArray;
    paramArray: TJSONArray;
    i : integer;
    jso : TJSONObject;
begin
  reactionObject.AddPair ('id', id);
  reactionObject.AddPair ('arcCenterX', TJSONNumber.Create (arcCenter.X));
  reactionObject.AddPair ('arcCenterY', TJSONNumber.Create (arcCenter.Y));
  reactionObject.AddPair ('lineType', lineTypeToStr (lineType));

  speciesArray := TJSONArray.Create;
  reactionObject.AddPair ('reactants', speciesArray);

  for i := 0 to nReactants - 1 do
      begin
      jso := TJsonObject.Create();
      jso.AddPair ('species', srcPtr[i].state.id); // this may be an alias node
      jso.AddPair ('stoichiometry', TJSONNumber.Create (srcStoich[i]));

      controlPt := TJSONArray.Create;
      controlPt.Add (reactantReactionArcs[i].h1.x);
      controlPt.Add (reactantReactionArcs[i].h1.y);
      jso.AddPair('h1', controlPt);

      controlPt := TJSONArray.Create;
      controlPt.Add (reactantReactionArcs[i].h2.x);
      controlPt.Add (reactantReactionArcs[i].h2.y);
      jso.addPair ('h2', controlPt);

      speciesArray.Add (jso);
      end;

  speciesArray := TJSONArray.Create;
  reactionObject.AddPair ('products', speciesArray);

  for i := 0 to nProducts - 1 do
      begin
      jso := TJsonObject.Create();
      jso.AddPair ('species', destPtr[i].state.id); // this may be an alias node.
      jso.AddPair ('stoichiometry', TJSONNumber.Create (destStoich[i]));

      controlPt := TJSONArray.Create;
      controlPt.Add (productReactionArcs[i].h1.x);
      controlPt.Add (productReactionArcs[i].h1.y);
      jso.AddPair('h1', controlPt);

      controlPt := TJSONArray.Create;
      controlPt.Add (productReactionArcs[i].h2.x);
      controlPt.Add (productReactionArcs[i].h2.y);
      jso.addPair ('h2', controlPt);

      speciesArray.Add (jso);
      end;
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


function TNodeState.clone : TNodeState;
begin
  result := self;
end;

// ---------------------------------------------------------------------------

procedure TReactionState.createReactantSpace (nReactants : integer);
var i : integer;
begin
  setLength (reactantReactionArcs, nReactants);
  setLength (srcId, nReactants);
  setLength (srcPtr, nReactants);
  setLength (srcStoich, nReactants);

  for i:= 0 to nReactants -1 do
      begin
      srcId[i] := '';
      if i < Length(srcStoich) then
         srcStoich[i] := 0.0; // assume same size as srcId
      end;
end;


procedure TReactionState.createProductSpace (nProducts : integer);
var i : integer;
begin
  setLength (destId, nProducts);
  setLength (destPtr, nProducts);
  setLength (productReactionArcs, nProducts);
  setLength (destStoich, nProducts);

  for i:= 0 to nProducts - 1 do
      begin
      destId[i] := '';
      if i < Length(destStoich) then
         destStoich[i] := 0.0; // assume same size as destId
     end;
end;

procedure TReactionState.loadFromJSON (obj : TJSONObject);
var speciesObject : TJSONObject;
    speciesArray : TJSONArray;
    reactantObject, productObject, reactantArray : TJSONObject;
    paramArray : TJSONArray;
    paramObject : TJSONObject;
    newParam : TSBMLparameter;
    pa : TJSONPair;
    speciesName : string;
    i, j : integer;
    coordPt : TJSONPair;
    coordArray : TJsonArray;
    astr : string;

begin
   id := obj.GetJSONValue('id');

   lineType := strToLineType (obj.GetJSONValue ('lineType'));
   arcCenter.x := (obj.GetValue ('arcCenterX') as TJSONNumber).AsDouble;
   arcCenter.y := (obj.GetValue ('arcCenterY') as TJSONNumber).AsDouble;

   // Load the reactant information
   speciesArray := obj.Get ('reactants').JsonValue as TJSONArray;
   nReactants := speciesArray.count;
   createReactantSpace (nReactants);

   for i := 0 to nReactants - 1 do
       begin
       reactantObject := speciesArray.Items[i] as TJSONObject;
       srcId[i] := reactantObject.GetValue ('species').value;
       srcStoich[i] := (reactantObject.GetValue ('stoichiometry') as TJSONNumber).AsDouble;

       coordPt := reactantObject.Get('h1');
       coordArray :=  coordPt.JsonValue as TJSONArray;
       reactantReactionArcs[i].h1.x := (coordArray.Items[0] as TJSONNumber).AsDouble;
       reactantReactionArcs[i].h1.y := (coordArray.Items[1] as TJSONNumber).AsDouble;

       coordPt := reactantObject.Get('h2');
       coordArray :=  coordPt.JsonValue as TJSONArray;
       reactantReactionArcs[i].h2.x := (coordArray.Items[0] as TJSONNumber).AsDouble;
       reactantReactionArcs[i].h2.y := (coordArray.Items[1] as TJSONNumber).AsDouble;
       end;

   // Load the product information
   speciesArray := obj.Get ('products').JsonValue as TJSONArray;
   nProducts := speciesArray.count;
   createProductSpace (nProducts);

   for i := 0 to nProducts - 1 do
       begin
       reactantObject := speciesArray.Items[i] as TJSONObject;
       destId[i] := reactantObject.GetValue ('species').Value;
       destStoich[i] := (reactantObject.GetValue ('stoichiometry') as TJSONNumber).AsDouble;

       coordPt := reactantObject.Get('h1');
       coordArray :=  coordPt.JsonValue as TJSONArray;
       productReactionArcs[i].h1.x := (coordArray.Items[0] as TJSONNumber).AsDouble;
       productReactionArcs[i].h1.y := (coordArray.Items[1] as TJSONNumber).AsDouble;

       coordPt := reactantObject.Get('h2');
       coordArray :=  coordPt.JsonValue as TJSONArray;
       productReactionArcs[i].h2.x := (coordArray.Items[0] as TJSONNumber).AsDouble;
       productReactionArcs[i].h2.y := (coordArray.Items[1] as TJSONNumber).AsDouble;
       end;

  rateParams := TList<TSBMLparameter>.create;
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


procedure TReactionState.loadFromSBML (glyphRxn : TSBMLLayoutReactionGlyph; modelRxn: SBMLReaction;
         paramAr: array of TSBMLparameter; funcDefList: TList<TSBMLFuncDefinition>;
         spGlyphList: TList<TSBMLLayoutSpeciesGlyph>; compAr: array of TSBMLcompartment;
         modelRender: TSBMLRenderInformation);
var  newParam : TSBMLparameter;
     speciesName, paramName : string;
     updatedKineticLaw: string; // kinetic law with funcDef id replaced with funcDef equation.
     spGlyphId: string;
     i, j, k : integer;
     intNumCurveSeg: integer; // number of line segments
     intReactNode, intProdNode : integer; // node index.
     rxnStyle: TSBMLRenderStyle;
begin
  id := modelRxn.getId;
  speciesName := '';
  intReactNode := 0;
  intProdNode := 0;
  intNumCurveSeg := 0;
  rateParams := TList<TSBMLparameter>.create;
  self.lineType := ltBezier;    // default
  // get reactants:
  self.nReactants := modelRxn.getNumReactants;
  createReactantSpace (nReactants);

  for i := 0 to modelRxn.getNumReactants - 1 do
      begin
      srcId[i] := '';
      if glyphRxn <> nil then
        begin
        for j := 0 to glyphRxn.getNumSpeciesRefGlyphs -1 do
          begin
          speciesName := '';
          spGlyphId := '';
          spGlyphId := glyphRxn.getSpeciesRefGlyph(j).getSpeciesGlyphId;
          speciesName := getSpeciesIdFromSpGlyphId(spGlyphId, spGlyphList);
          if speciesName = modelRxn.getReactant(i).getSpecies then
            srcId[i] := spGlyphId;
          end;
        end;
      if srcId[i] = '' then srcId[i] := modelRxn.getReactant(i).getSpecies;
      srcStoich[i] := modelRxn.getReactant(i).getStoichiometry;
      end;

  // get products:
  nProducts := modelRxn.getNumProducts;
  createProductSpace (nProducts);

  for i := 0 to modelRxn.getNumProducts - 1 do
    begin
    destId[i] := '';
    if glyphRxn <> nil then
      begin
      for j := 0 to glyphRxn.getNumSpeciesRefGlyphs -1 do
          begin
          speciesName := '';
          spGlyphId := '';
          spGlyphId := glyphRxn.getSpeciesRefGlyph(j).getSpeciesGlyphId;
          speciesName := getSpeciesIdFromSpGlyphId(spGlyphId, spGlyphList);
          if speciesName = modelRxn.getProduct(i).getSpecies then
            destId[i] := spGlyphId;
          end;
      end;

    if destId[i] = '' then destId[i] := modelRxn.getProduct(i).getSpecies;
    destStoich[i] := modelRxn.getProduct(i).getStoichiometry;
    end;
  updatedKineticLaw := '';
  updatedKineticLaw := self.convertFuncDefsToKineticLaws( funcDefList,
                                           modelRxn.getKineticLaw.getFormula);
  // Grab parameters that are used in reaction:
  for j := 0 to Length(paramAr) - 1 do
    begin
      paramName := paramAr[j].getId;
      if modelRxn.getKineticLaw.getFormula.Contains(paramName) then
        rateParams.Add(paramAr[j]);
      // console.log('param: ',modelRxn.getKineticLaw.getParameter(i),', model param: ',paramAr[j].getId);
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

  //rateLaw := modelRxn.getKineticLaw.getFormula;
  rateLaw := updatedKineticLaw;

  // Rxn curve/line order of assignment:
  //  1.  default, if no layout info available.
  //  2.  If cubic beziers defined for TSBMLLayoutSpeciesReferenceGlyph.
  //  3.  Cubic beziers defined for glyphRxn (TSBMLLayoutReactionGlyph).
  //  4.  if no Cubic Beziers then look for line segments.
  arcCenter.x := 0; arcCenter.y := 0; // default, need something else here?
  if glyphRxn <> nil then  // check if any layout info available
    begin
    if processReactionSpeciesReferenceCurves(glyphRxn, modelRxn, spGlyphList, modelRender ) = false then
      begin
      if glyphRxn.getCurve <> nil then // Next try to get rxn curves with glyphRxn
        begin
        processReactionCurves( glyphRxn, intProdNode, intReactNode  );
        end
      else // no curves, set arcCenter as boundingBox center:
        begin // TODO: need default reactionArc if none provided in layout....
        if glyphRxn.boundingBoxIsSet then
          begin
          arcCenter.x := glyphRxn.getBoundingBox.getPoint.getX +
                                   (glyphRxn.getBoundingBox.getDims.getWidth/2);
          arcCenter.Y := glyphRxn.getBoundingBox.getPoint.getY +
                                   (glyphRxn.getBoundingBox.getDims.getHeight/2);
          end
        else  // TODO: calc default arcCenter:
          begin
          // arcCenter := computeCentroid(reaction: TReaction): TPointF;
          end;
        end;

      end;

    end // end of if glyphRxn <> nil
  else
    begin
    fillColor := DEFAULT_REACTION_COLOR;
    thickness := DEFAULT_REACTION_THICKNESS;
    end;

end;

 // Convert any SBML func defs that are used for reaction kinetic laws:
function TReactionState.convertFuncDefsToKineticLaws(funcDefList: TList<TSBMLFuncDefinition>;
                strKinLaw: string ): string;
var i: integer;
   strNewKLaw: string;
   formula: string;
begin
  strNewKLaw := '';
  strNewKLaw := strKinLaw;
  console.log(' Initial NewFormula:', strNewKLaw);
  if funcDefList <> nil then
    begin
    for i := 0 to funcDefList.count -1 do
      begin
      formula := '';
      if strNewKLaw.Contains(funcDefList[i].getId) then
        begin
        formula := '(' + funcDefList[i].getFuncFormula + ')';
        strNewKLaw := strNewKLaw.Replace(funcDefList[i].getFullFuncLabel, formula);
        console.log(' Current NewFormula:', strNewKLaw);
        end;

      end;
    end;
  console.log(' Final NewFormula:', strNewKLaw);
  Result := strNewKLaw;
end;

       // Returns true if curve for reaction was found and processed.
function  TReactionState.processReactionSpeciesReferenceCurves(newGlyphRxn: TSBMLLayoutReactionGlyph;
          newModelRxn: SBMLReaction; newSpGlyphList: TList<TSBMLLayoutSpeciesGlyph>;
          reactionRenderInfo: TSBMLRenderInformation ): boolean;
var i, j, k, nodeIndex: integer;
    spRefGlyph: TSBMLLayoutSpeciesReferenceGlyph;
    spRefGlyphStyle: TSBMLRenderStyle;  // Style associated with species reference Glyph
    spGlyphId, spId: string; // SpeciesGlyph id, species id
    reactant: boolean;
    colorFound: boolean;

    arcCenterFound: boolean;
    strMsg: string;
begin
  arcCenterFound := false;
  colorFound := false;

  self.calcArcCenterFromSBMLLayout(newGlyphRxn);
  if (self.arcCenter.x < 1) and (self.arcCenter.y < 1) then arcCenterFound := false
  else arcCenterFound := true;
  for i := 0 to newGlyphRxn.getNumSpeciesRefGlyphs -1 do
    begin
      spGlyphId := '';
      spId := '';   // Actual species id used in reaction
      Result := false;
      reactant := false;
      spRefGlyph := newGlyphRxn.getSpeciesRefGlyph(i);
      if i = 0 then  // Only use style from first spRefGlyph to draw reaction line:
        begin
        spRefGlyphStyle := reactionRenderInfo.getGlyphRenderStyle(newGlyphRxn.getSpeciesRefGlyph(i).getId,
              'SPECIESREFERENCEGLYPH',newGlyphRxn.getSpeciesRefGlyph(i).getRole );
        if spRefGlyphStyle <> nil then
          begin
          if spRefGlyphStyle.getRenderGroup <> nil then
            begin
            for j := 0 to reactionRenderInfo.getNumbColorDefs -1 do
              begin
              if spRefGlyphStyle.getRenderGroup.getFillColor = reactionRenderInfo.getColorDef(j).getId then
                begin
                self.fillColor := HexToTColor( reactionRenderInfo.getColorDef(j).getValue() );
                colorFound := true;
                end;
            end;
            self.thickness := spRefGlyphStyle.getRenderGroup.getStrokeWidth;
            if self.thickness < 1 then self.thickness := DEFAULT_REACTION_THICKNESS;
            if colorFound = false then
              self.fillColor := DEFAULT_REACTION_COLOR;
            end;
          end
        else
          begin
          self.fillColor := DEFAULT_REACTION_COLOR;
          self.thickness := DEFAULT_REACTION_THICKNESS;
          end;
        end;
      spGlyphId := spRefGlyph.getSpeciesGlyphId; // species glyph id used to find spId

      for j := 0 to newModelRxn.getNumReactants -1 do
        begin
        if newModelRxn.getReactant(j).getSpecies = spId then
          begin
          reactant := true;
          for k := 0 to self.nReactants -1 do
            if self.srcId[k] = spGlyphId then nodeIndex := k;
          end
        end;
      if not reactant then
        begin
          for j := 0 to newModelRxn.getNumProducts -1 do
            begin
            if newModelRxn.getProduct(j).getSpecies = spId then
              begin
              reactant := false;
              for k := 0 to self.nProducts -1 do
                if self.destId[k] = spGlyphId then nodeIndex := k;
              end;
            end;
        end;

      if spRefGlyph.isCurveSet then
        begin
          if spRefGlyph.getCurve.getNumCubicBeziers > 0 then
            begin
              Result := true;
              for j := 0 to spRefGlyph.getCurve.getNumCubicBeziers -1 do
                begin
                if spRefGlyph.getCurve.getNumCubicBeziers = 1 then
                  begin
                  if arcCenterFound = false then
                    begin
                    if reactant then
                      begin
                      self.arcCenter.x := spRefGlyph.getCurve.getCubicBezier(0).getEnd.getX;
                      self.arcCenter.y := spRefGlyph.getCurve.getCubicBezier(0).getEnd.getY;
                      end
                    else  // product, RXN arcCenter is beginning of bezier:
                      begin
                      self.arcCenter.x := spRefGlyph.getCurve.getCubicBezier(0).getStart.getX;
                      self.arcCenter.y := spRefGlyph.getCurve.getCubicBezier(0).getStart.getY;
                      end;
                    arcCenterFound := true;
                    end;
                  self.importSBMLBezierCurve(spRefGlyph.getCurve.getCubicBezier(j),
                                           nodeIndex, reactant );
                  Result := true;
                  end
                  else
                    begin
                    strMsg := 'More than one bezier curve used for ' + spId +
                ' side of reaction: ' + newModelRxn.getID + '. Please use only one.';
                    NotifyUser(strMsg);
                    Result := false;
                    end;
                end;
            end
          else // check if line segment, if so, process them:
            begin
            for j := 0 to spRefGlyph.getCurve.getNumCurveSegments -1 do
              begin // Assume one segment for each species in rxn:
              if spRefGlyph.getCurve.getNumCurveSegments = 1 then
                begin
                if arcCenterFound = false then
                  begin
                  if reactant then
                    begin

                    self.arcCenter.x := spRefGlyph.getCurve.getLineSegment(0).getEndPt.getX;
                    self.arcCenter.y := spRefGlyph.getCurve.getLineSegment(0).getEndPt.getY;
                    end
                  else  // product, RXN arcCenter is beginning of line segment:
                    begin
                    self.arcCenter.x := spRefGlyph.getCurve.getLineSegment(0).getStartPt.getX;
                    self.arcCenter.y := spRefGlyph.getCurve.getLineSegment(0).getStartPt.getY;
                    end;
                  arcCenterFound := true;
                  end;
                self.convertSBMLLineToBezier( spRefGlyph.getCurve.getLineSegment(j),
                  2, nodeIndex, reactant );
                Result := true;
                end
              else
                begin
                strMsg := 'More than one line segment used for ' + spId +
                ' side of reaction: ' + newModelRxn.getID + '. Please use bezier curve.';
                NotifyUser(strMsg);
                end;
                Result := false;
              end;

            end;
        end;

    end;
end;

procedure TReactionState.processReactionCurves( newRxn: TSBMLLayoutReactionGlyph ;
                                  intProdNodeIndex, intReactNodeIndex: integer  );
var i, intNumCurveSeg: integer;
 begin
   intNumCurveSeg := 0;
   for i := 0 to newRxn.getCurve.getNumCubicBeziers - 1 do
     begin
     // TODO: what if only 1 bezier? Need to extrapolate arcCenter? Make 2?
     // Assume rxn curve between nodes contains at most 2 subcurves
     // Assume first bezier is a reactantReactionArc:
     if ( arcCenter.x = newRxn.getCurve.getCubicBezier(i).getStart.getX ) and
        ( arcCenter.y = newRxn.getCurve.getCubicBezier(i).getStart.gety ) then
       begin  // productReactionArc
       self.importSBMLBezierCurve(newRxn.getCurve.getCubicBezier(i) , intProdNodeIndex, false);
       inc(intProdNodeIndex);
       end
     else     // reactantReactionArc:
       begin
       self.importSBMLBezierCurve(newRxn.getCurve.getCubicBezier(i) , intReactNodeIndex, true);
       inc(intReactNodeIndex);
       end;

     end;

    // Convert line segments to bezier curves:
   intNumCurveSeg := newRxn.getCurve.getNumCurveSegments;

   for i := 0 to intNumCurveSeg -1 do
     begin
     if (arcCenter.x = newRxn.getCurve.getLineSegment(i).getEndPt.getX) and
       (arcCenter.y = newRxn.getCurve.getLineSegment(i).getEndPt.getY) then
       begin     // add line from reactant to arcCenter
       convertSBMLLineToBezier(newRxn.getCurve.getLineSegment(i), intNumCurveSeg,
                                  intReactNodeIndex, true  );
       inc(intReactNodeIndex);
       end
     else
       begin     // add line from arcCenter to product
       convertSBMLLineToBezier(newRxn.getCurve.getLineSegment(i), intNumCurveSeg,
                                  intProdNodeIndex, false  );
       inc(intProdNodeIndex);
       end;
     end;
 end;


procedure TReactionState.importSBMLBezierCurve( newBezier: TSBMLLayoutCubicBezier; intNode: integer;
                                        reactantSide: boolean );
begin
 // console.log(' Length of self.reactantReactionArcs', length(self.reactantReactionArcs));
   if reactantSide and (length(self.reactantReactionArcs) > intNode) then    // reactantReactionArc:
     begin
     self.reactantReactionArcs[intNode].nodeIntersectionPt.x := newBezier.getStart.getX;
     self.reactantReactionArcs[intNode].nodeIntersectionPt.y := newBezier.getStart.getY;
     self.reactantReactionArcs[intNode].h1.x :=
                                      newBezier.getBasePoint1.getX;
     self.reactantReactionArcs[intNode].h1.y :=
                                      newBezier.getBasePoint1.getY;
     self.reactantReactionArcs[intNode].h2.x :=
                                      newBezier.getBasePoint2.getX;
     self.reactantReactionArcs[intNode].h2.y :=
                                      newBezier.getBasePoint2.getY;
     self.reactantReactionArcs[intNode].arcDirection := adInArc;
     self.reactantReactionArcs[intNode].Merged := false;
     end
  else       // productReactantArc
    begin
    if length(self.productReactionArcs) > intNode then
      begin
      self.productReactionArcs[intNode].nodeIntersectionPt.x :=
                                      newBezier.getEnd.getX;
      self.productReactionArcs[intNode].nodeIntersectionPt.y :=
                                      newBezier.getEnd.getY;
      self.productReactionArcs[intNode].h1.x :=
                                      newBezier.getBasePoint1.getX;
      self.productReactionArcs[intNode].h1.y :=
                                      newBezier.getBasePoint1.getY;
      self.productReactionArcs[intNode].h2.x :=
                                     newBezier.getBasePoint2.getX;
      self.productReactionArcs[intNode].h2.y :=
                                      newBezier.getBasePoint2.getY;
      self.productReactionArcs[intNode].arcDirection := adOutArc;
      self.productReactionArcs[intNode].Merged := false;
     // self.setBezierHandles;   // self.srcPt = nil, not assigned yet
      end;
    end;
end;

procedure TReactionState.convertSBMLLineToBezier( currentLine: TSBMLLayoutLineSegment;
                totalSegments: integer; intNode: integer; reactantSide: boolean );
begin
  if totalSegments = 1 then   // break up segment into two arcs:
    begin // product arc:
    self.productReactionArcs[intNode].nodeIntersectionPt.x := currentLine.getEndPt.getX;
    self.productReactionArcs[intNode].nodeIntersectionPt.y := currentLine.getEndPt.getY;
    self.productReactionArcs[intNode].arcDirection := adOutArc;
    self.productReactionArcs[intNode].Merged := false;
       // reactant arc:
    self.reactantReactionArcs[intNode].nodeIntersectionPt.x := currentLine.getStartPt.getX;
    self.reactantReactionArcs[intNode].nodeIntersectionPt.y := currentLine.getStartPt.getY;
    self.reactantReactionArcs[intNode].arcDirection := adInArc;
    self.reactantReactionArcs[intNode].Merged := false;
   // self.setBezierHandles;
    end
  else
    begin
    if reactantSide and (length( self.reactantReactionArcs ) > intNode) then
      begin // reactant arc:
      self.reactantReactionArcs[intNode].nodeIntersectionPt.x := currentLine.getStartPt.getX;
      self.reactantReactionArcs[intNode].nodeIntersectionPt.y := currentLine.getStartPt.getY;
      self.reactantReactionArcs[intNode].arcDirection := adInArc;
      self.reactantReactionArcs[intNode].Merged := false;
      end
    else   // product arc:
      if length( self.productReactionArcs ) > intNode then
        begin
        self.productReactionArcs[intNode].nodeIntersectionPt.x := currentLine.getEndPt.getX;
        self.productReactionArcs[intNode].nodeIntersectionPt.y := currentLine.getEndPt.getY;
        self.productReactionArcs[intNode].arcDirection := adOutArc;
        self.productReactionArcs[intNode].Merged := false;
      //  self.setBezierHandles;
        end;
    end;

end;

  // Calc rxn arc center from SBML ReactionGlyph BBox or curve
procedure TReactionState.calcArcCenterFromSBMLLayout( newSBMLRxn: TSBMLLayoutReactionGlyph );
var //i: integer;
    //rxnCurve: TSBMLLayoutCurve;
    newCenterPt: TSBMLLayoutPoint;
begin
  newCenterPt := newSBMLRxn.getReactionCenterPoint;
  if newCenterPt <> nil then
    begin
    self.arcCenter.x := newCenterPt.getX;
    self.arcCenter.y := newCenterPt.getY;
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


function TReactionState.clone : TReactionState;
var i : integer;
begin
  result := self;
  setlength (result.reactantReactionArcs, length (self.reactantReactionArcs));
  for i := 0 to length (self.reactantReactionArcs) - 1 do
      begin
      result.reactantReactionArcs[i].nodeIntersectionPt := self.reactantReactionArcs[i].nodeIntersectionPt;
      result.reactantReactionArcs[i].h1 := self.reactantReactionArcs[i].h1;
      result.reactantReactionArcs[i].h2 := self.reactantReactionArcs[i].h2;
      result.reactantReactionArcs[i].merged := self.reactantReactionArcs[i].merged;
      result.reactantReactionArcs[i].arcDirection := self.reactantReactionArcs[i].arcDirection;
      end;

  setlength (result.productReactionArcs, length (self.productReactionArcs));
  for i := 0 to length (self.productReactionArcs) - 1 do
      begin
      result.productReactionArcs[i].nodeIntersectionPt := self.productReactionArcs[i].nodeIntersectionPt;
      result.productReactionArcs[i].h1 := self.productReactionArcs[i].h1;
      result.productReactionArcs[i].h2 := self.productReactionArcs[i].h2;
      result.productReactionArcs[i].merged := self.productReactionArcs[i].merged;
      result.productReactionArcs[i].arcDirection := self.productReactionArcs[i].arcDirection;
      end;

  result.srcId := Copy(self.srcId, 0, Length(self.srcId));
  result.destId := Copy(self.destId, 0, Length(self.destId));
  result.srcStoich := Copy (self.srcStoich, 0, Length (self.srcStoich));
  result.destStoich := Copy (self.destStoich, 0, Length (self.destStoich));

  result.srcPtr := Copy (self.srcPtr, 0, Length (self.srcPtr));
  result.destPtr := Copy (self.destPtr, 0, Length (self.destPtr));
end;


procedure AddJSONHeader (obj : TJSONObject);
begin

end;

// ------------------------------------------------------------------------------

constructor TNetwork.create (id : string);
begin
  self.id := id;
  computeBezierBlendingFuncs;
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

// Returns '' if no issue, else returns string describing problem
function TNetwork.loadModel (modelStr : string): string;  // Loads JSON string
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
  Result := '';
  try
    JSONRoot := TJSONObject.parseJSONValue (modelStr);
    pair := (JSONRoot as TJSONObject).Get('magicIdentifier');
    if pair = nil then
      Result := 'JSON file not a valid network model';
  //   raise Exception.Create ('JSON file not a valid network model');
  except
    Result :='File string not valid JSON';
  end;
  if Result = '' then  // No issues yet
    begin
    if (pair.JSONValue as TJSONString).Value <> MAGIC_IDENTIFER then
    Result := 'JSON file a value network model, but verison: ' + MAGIC_IDENTIFER + ' not supported';
    // raise Exception.Create ('JSON file a value network model, but verison: ' + MAGIC_IDENTIFER + ' not supported');
    end;
  if Result = '' then  // No issues loading json string
  begin
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
   modelRender: TSBMLRenderInformation;
   nodeStyle: TSBMLRenderStyle;
   rxnStyle: TSBMLRenderStyle; // Reaction curve
   speciesGlyph: TSBMLLayoutSpeciesGlyph;
   reactionGlyph: TSBMLLayoutReactionGlyph;
   reactionGlyphId: string; // name of reaction, used to find reaction details.
 //  spAr: array of string;
   spAr: array of TSBMLSpecies;
   initVal: double;
   boundarySp: boolean;
   node : TNode;
   reaction : TReaction;
   nodeState : TNodeState;
   reactionState: TReactionState;
   nodeColorDefList: TList<TSBMLRenderColorDefinition>;
begin
    modelRender := nil;
    modelLayout := model.getSBMLLayout;
    if model.getSBMLRenderInfo <> nil then
      modelRender := model.getSBMLRenderInfo;

    for i := 0 to length(model.getSBMLmodelRules) -1 do
      begin
      if model.getSBMLRule(i).isAssignment then
        self.addRule( model.getSBMLRule(i) );
      end;

    // Do not get layout dimensions? not really necessary?
    // if needed then need to set NetworkPB to width and height.
    for i := 0 to modelLayout.getNumSpGlyphs - 1 do
      begin
        initVal := 0;
       // spAr := model.getS_Names;
        spAr := model.getSBMLspeciesAr;
        speciesGlyph := modelLayout.getSpGlyph(i);
        for j := 0 to Length(spAr) -1 do
        begin
         // console.log('loadSBMLModel, species: ',spAr[j],', spglyph: ',speciesGlyph.getSpeciesId);
          if spAr[j].getID = speciesGlyph.getSpeciesId then
          begin
            if spAr[j].isSetInitialConcentration then
              initVal := spAr[j].getInitialConcentration
            else initVal := spAr[j].getInitialAmount;
            boundarySp := spAr[j].getBoundaryCondition;
          end;

        end;
        if modelRender <> nil then
          begin
          nodeStyle := self.getSpeciesRenderStyle(speciesGlyph, model);
          nodeColorDefList := getColorDefs(nodeStyle, modelRender);
          nodeState.loadFromSBML (speciesGlyph, initVal, boundarySp, nodeStyle, nodeColorDefList);
          end
        else nodeState.loadFromSBML (speciesGlyph, initVal, boundarySp, nil, nil);
        addNode (nodeState);
      end;

   // load reactions:
    for i := 0 to modelLayout.getNumRxnGlyphs - 1 do
      begin
        reactionGlyph := modelLayout.getRxnGlyph(i);
      //  rxnStyle := model.getRenderStyle(reactionGlyph.getId, STYLE_TYPES[2],nil);  //not necessary?
        reactionGlyphId := reactionGlyph.getReactionId;
        for j := 0 to model.getNumReactions - 1 do
          begin
            if reactionGlyphId = model.getReaction(j).getID then
            begin
              reactionState.loadFromSBML (reactionGlyph, model.getReaction(j),
                   model.getSBMLparameterAr, model.getFuncDefList,
                   modelLayout.getSpGlyphList, model.getSBMLcompartmentsArr(),
                   model.getSBMLRenderInfo);
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
              reaction.state.setBezierHandles; // node ptrs have been set.
            end;

             // ************************
          end;
      end;
   console.log('Done loading SBML network layout');
end;

function TNetwork.getColorDefs(newStyle: TSBMLRenderStyle;
            newRenderInfo: TSBMLRenderInformation): TList<TSBMLRenderColorDefinition>;
var i: integer;
begin
  if newStyle <> nil then
    begin
    Result := TList<TSBMLRenderColorDefinition>.create;
    for i := 0 to newRenderInfo.getNumbColorDefs -1 do
      begin
      if newStyle.getRenderGroup.getStrokeColor = newRenderInfo.getColorDef(i).getId then
        Result.Add(newRenderInfo.getColorDef(i))
      else if newStyle.getRenderGroup.getFillColor = newRenderInfo.getColorDef(i).getId then
        Result.Add(newRenderInfo.getColorDef(i));

      end;
    end
  else Result := nil;
end;


function TNetwork.getSpeciesRenderStyle(newSpeciesGlyph:TSBMLLayoutSpeciesGlyph;
                          newModel: TModel): TSBMLRenderStyle;
var i, j: integer;
    renderStyle: TSBMLRenderStyle;
    glyphId: string;
begin
  glyphId := newSpeciesGlyph.getId;
  Result := nil;
  Result := newModel.getRenderStyle( glyphId, 'SPECIESGLYPH', '' );

end;

    // No SBML layout present:
procedure TNetwork.autoBuildNetworkFromSBML(model: TModel);
 var i, j, k, l, index: integer;
   //spAr: array of string;
   spAr: array of TSBMLSpecies;
   initVal: double;
   node : TNode;
   reaction : TReaction;
   nodeState : TNodeState;
   reactionState: TReactionState;
 begin
    //spAr := model.getS_Names;
    spAr := model.getSBMLspeciesAr;
    for j := 0 to Length(spAr) -1 do
      begin
        nodeState.id := spAr[j].getID; // default
        nodeState.species := spAr[j].getID;
        nodeState.boundarySp := spAr[j].getBoundaryCondition;
        if spAr[j].isSetInitialConcentration then
          nodeState.conc := spAr[j].getInitialConcentration
        else nodeState.conc := spAr[j].getInitialAmount;
       // nodeState.conc := spAr[j].getmodel.getS_initVals[j];
        nodeState.x := 20 + j; // default values
        nodeState.y := 60 + j; //   "
        nodeState.w := 60;     //   "
        nodeState.h := 40;     //   "
        if nodeState.boundarySp then
          nodeState.fillColor := DEFAULT_NODE_BOUNDARY_SPECIES_COLOR
        else nodeState.fillColor := RGB(255,204,153);// clWebPeachPuff;
        nodeState.outlineColor := RGB(255,102,0);
        nodeState.outlineThickness := DEFAULT_NODE_OUTLINE_THICKNESS;
        self.addNode(nodeState);

      end;

   // load reactions:
    for j := 0 to model.getNumReactions - 1 do
        begin
          reactionState.loadFromSBML (nil, model.getReaction(j), model.getSBMLparameterAr,
                             nil, nil, model.getSBMLcompartmentsArr(), nil);

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
    // Chk for Assignment rules:
    for i := 0 to length(model.getSBMLmodelRules) -1 do
      begin
      if model.getSBMLRule(i).isAssignment then
        self.addRule( model.getSBMLRule(i) );
      end;
    self.autoLayoutEvent(nil); // Generate layout
 end;

 function TNetwork.buildNullNodeState(rxnId: String; num: integer): TNodeState;
 var nodeState: TNodeState;
 begin
   nodeState.id := rxnId + NULL_NODE_TAG;
   nodeState.species := rxnId + NULL_NODE_TAG;
   nodeState.boundarySp := true;
   nodeState.conc := 0.0;
   nodeState.x := 22 + 2*num ; // default values
   nodeState.y := 62 + 2*num; //   "
   nodeState.w := 60;  //   "
   nodeState.h := 40;  //   "
   nodeState.fillColor := clSilver;
   nodeState.outlineColor := RGB(255,102,0);
   nodeState.outlineThickness := DEFAULT_NODE_OUTLINE_THICKNESS;
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

function TNetwork.findReaction(id: string; var index: integer): boolean;
var i: integer;
begin
  result := False;
  for i := 0 to length (self.reactions) - 1 do
    if self.reactions[i].state.id = id then
       begin
       index := i;
       result := True;
       end;
end;

// returns index if found, -1 if not. Note returns last match only.
function TNetwork.isNodeinNodeArray(node: TNode; nodeAr: array of TNode): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(nodeAr) - 1 do
  begin
    if node.state.id = nodeAr[i].state.id then
      Result := i;
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
  result.state.outlineThickness :=  DEFAULT_NODE_OUTLINE_THICKNESS;
  result.state.id := id;
  result.state.species := id; // default
  self.networkEvent(nil); // Notify listener
end;


function TNetwork.addNode (id : string; x, y, w, h : double) : TNode;
begin
  setlength (nodes, length (nodes) + 1);
  nodes[length (nodes)-1] := TNode.create (id);
  result := nodes[length (nodes)-1];
  result.state.x := x; result.state.y := y;
  result.state.h := h; result.state.w := w;
  result.state.outlineThickness :=  DEFAULT_NODE_OUTLINE_THICKNESS;
  result.state.id := id;
  result.state.species := id;  // default
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

procedure TNetwork.addRule (newRule: TSBMLRule);
begin
  if self.listOfSBMLRules = nil then
    self.listOfSBMLRules := TList<TSBMLRule>.create;
  self.listOfSBMLRules.Add(TSBMLRule.create(newRule));
end;

function TNetwork.getRule (index: integer): TSBMLRule;
begin
  if index < self.listOfSBMLRules.Count then
    Result := self.listOfSBMLRules[index]
  else Result := nil;
end;

// This function returns a rule, typical assignment, for varId
    // varId: any species, parameter, compartment
function TNetwork.getRuleWithVarId (varId: string): TSBMLRule;
 var i: integer;
 begin
   Result := nil;
   for i := 0 to self.listOfSBMLRules.Count -1 do
     begin
     if self.listOfSBMLRules[i].getVariable = varId then
       Result := self.listOfSBMLRules[i];
     end;
 end;

 function TNetwork.getNumRules(): integer;
 begin
   if self.listOfSBMLRules <> nil then
     Result := self.listOfSBMLRules.Count
   else Result := 0;
 end;

 // Note: newSubStr should have all spaces replaced with '_'
function TNetwork.strReplaceParamName(oldSubStr: string; newSubStr: string): boolean;
var i, j: integer;
    newId: string;
begin
  for i := 0 to length(self.reactions) - 1 do
  begin
    for j := 0 to self.reactions[i].state.rateParams.Count -1 do
      begin
      newId := stringReplace(self.reactions[i].state.rateParams[j].getId, oldSubStr, newSubStr, [rfReplaceAll]);
      self.reactions[i].state.rateParams[j].setId(newId);
      end;
  end;
  self.networkEvent(nil);
end;

procedure TNetwork.updateReactionParamVal( paramId: string; newVal: double );
var i, j: integer;
begin
  for i := 0 to length(self.reactions) - 1 do
  begin
    for j := 0 to self.reactions[i].state.rateParams.Count -1 do
      begin
      if self.reactions[i].state.rateParams[j].getId = paramId then
        begin
        self.reactions[i].state.rateParams[j].setValue(newVal);
        end;

      end;
  end;

  self.networkEvent(nil);
end;

procedure TNetwork.computeAnyToAnyCoordinates (reaction : TReaction; sourceNodes, destNodes : array of TNode);
var nReactants, nProducts : integer;
    cx, cy, sumX, sumY, centerX, centerY : double;
    i : integer;
    nDestCount : integer;
    startPt, pt : TPointF;
begin
  nReactants := length (sourceNodes);
  nProducts := length (destNodes);

  reaction.state.arcCenter := computeCentroid (reaction);
  //console.log('Arc center: ',reaction.state.arcCenter.x, ', ', reaction.state.arcCenter.y);
  //cx := reaction.state.arcCenter.x; cy := reaction.state.arcCenter.y;

  setLength (reaction.state.reactantReactionArcs, nReactants);
  setLength (reaction.state.productReactionArcs, nProducts);

  for i := 0 to nReactants - 1 do
      reaction.state.reactantReactionArcs[i].arcDirection := adInArc;
  for i := 0 to nProducts - 1 do
      reaction.state.productReactionArcs[i].arcDirection := adOutArc;

  // Set up start handles on each reactant, these are between
  // the the node and arc center but shifted towards the arccenter
  reaction.state.setBezierHandles;

  // If we want line segments they go here.
  // ...... (See uNetwork.pas in pathwayDesigner line 2311)
end;


function TReactionState.setBezierHandles({ sourceNodes, destNodes : array of TNode}): boolean;
var nReactants, nProducts : integer;
    cx, cy, sumX, sumY, centerX, centerY : double;
    i , reactantCount, productCount: integer;
    nDestCount : integer;
    startPt, pt : TPointF;
begin
  // Set up start handles on each reactant, these are between
  // the the node and arc center but shifted towards the arccenter
  cx := self.arcCenter.x; cy := self.arcCenter.y;
  reactantCount := self.nReactants;
  productCount := self.nProducts;
  for i := 0 to reactantCount - 1 do
    begin
    if self.srcPtr[i] <> nil then
      begin
      pt := self.srcPtr[i].state.getCenter;
      self.reactantReactionArcs[i].h1.x := pt.x + (cx - pt.x) / 1.25;
      self.reactantReactionArcs[i].h1.y := pt.y + (cy - pt.y) / 1.25;
      end;
    end;

  // Compute the common position of the inner control point on the
  // reactant side, the opposite inner control point will be
  // made collinear with this point. We'll make the inner control point
  // on the reactant side the centroid between teh reactants and arccenter

  sumX := 0; sumY := 0;
  for i := 0 to reactantCount - 1 do
    begin
    if self.srcPtr[i] <> nil then
      begin
      pt := self.srcPtr[i].state.getCenter;
      sumX := sumX + pt.x;
      sumY := sumY + pt.y;
      end;
    end;
  // Don't forget to add the centroid
  sumX := sumX + cx; sumY := sumY + cy;
  centerX := sumx/(reactantCount+1);
  centerY := sumy/(reactantCount+1);

  for i := 0 to reactantCount -1 do
      begin
      self.reactantReactionArcs[i].h2.x := centerX;
      self.reactantReactionArcs[i].h2.y := centerY;
      end;

  // Set up start handles on each product, shift handles twards the arccenter
  for i := 0 to productCount - 1 do
    begin
    if self.destPtr[i] <> nil then
      begin
      pt := self.destPtr[i].state.getCenter;
      self.productReactionArcs[i].h2.x := cx + (pt.x - cx) / 3.25;
      self.productReactionArcs[i].h2.y := cy + (pt.y - cy) / 3.25;
      end;
    end;

  // Next compute the collinear coordinate for the inner control points of the products
  nDestCount := 0;
  for i := 0 to productCount - 1 do
      begin
      self.productReactionArcs[i].h1.x := 2*cx - self.reactantReactionArcs[0].h2.x;
      self.productReactionArcs[i].h1.y := 2*cy - self.reactantReactionArcs[0].h2.y;
      end;

  // Record fact that handles are merged
  for i := 0 to productCount - 1 do
      self.productReactionArcs[i].Merged := true;

  Result := true;
end;

function TNetwork.addAnyToAnyReaction (id: string; sourceNodes, destNodes : array of TNode; var edgeIndex : integer) : TReaction;
var newReaction : TReaction;
    i : integer;
    nSource, nDestination : integer;
begin
  nSource := length (sourceNodes);
  nDestination := length (destNodes);

  newReaction := TReaction.Create;

  newReaction.state.nReactants := nSource;
  newReaction.state.nProducts := nDestination;
  newReaction.state.createReactantSpace (newReaction.state.nReactants);
  newReaction.state.createProductSpace (newReaction.state.nProducts);
  for i := 0 to nSource - 1 do
      begin
      newReaction.state.srcId[i] := sourceNodes[i].state.id;
      newReaction.state.srcPtr[i] := sourceNodes[i];
      end;

  for i := 0 to nDestination - 1 do
      begin
      newReaction.state.destId[i] := destNodes[i].state.id;
      newReaction.state.destPtr[i] := destNodes[i];
      end;

  newReaction.state.id := id; //getUniqueReactionName();
  edgeIndex := addReaction (newReaction);

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


function TNetwork.overNodeControlRectangle (x, y : double; node : TNode; var selectedNodeGrabRectangle : integer) : boolean;
var i : integer;
begin
  result := False;
  if node.overControlRectangle (x, y, selectedNodeGrabRectangle) then
     begin
     console.log ('netowork.overNodeControlRectangle: TRUE, i = ' + inttostr (i));
     result := True;
     exit;
     end;
end;



function TNetwork.overReaction(x, y : double; var reactionIndex, arcId : integer) : boolean;
var i, j : integer;  p1, p2 : TPointF;
    parametricDistance : double;
begin
  reactionIndex := -1;
  result := False;
  p2 := TPointF.Create (x, y);
  for i := 0 to length (reactions) -1 do
      begin
      for j := 0 to reactions[i].state.nReactants - 1 do
          begin
          if ptOnBezier ([reactions[i].state.reactantReactionArcs[j].nodeIntersectionPt,
                          reactions[i].state.reactantReactionArcs[j].h1,
                          reactions[i].state.reactantReactionArcs[j].h2,
                          reactions[i].state.arcCenter], p2, parametricDistance) then
             begin
             reactionIndex := i;
             arcId := j;
             console.log ('Found Reactant Bezier');
             exit (True);
             end;
          end;
      for j := 0 to reactions[i].state.nProducts - 1 do
         begin
         if ptOnBezier ([reactions[i].state.arcCenter,
                         reactions[i].state.productReactionArcs[j].h1,
                         reactions[i].state.productReactionArcs[j].h2,
                         reactions[i].state.productReactionArcs[j].nodeIntersectionPt], p2, parametricDistance) then
             begin
             reactionIndex := i;
             arcId := j;
             console.log ('Found it Product Bezier');
             exit (True);
             end;

         end;
      end;

//  for  i := 0 to length (reactions) - 1 do
//       begin
//       for j := 0 to reactions[i].state.nReactants - 1 do
//           begin
//           p1 := reactions[i].state.srcPtr[j].getCenter;
//           p2 := reactions[i].state.arcCenter;
//           if ptOnLine (p1, p2, x, y) then
//              begin
//              reactionIndex := i;
//              exit (reactions[i]);
//              end;
//           end;
//
//       for j := 0 to reactions[i].state.nProducts - 1 do
//           begin
//           p2 := reactions[i].state.destPtr[j].getCenter;
//           p1 := reactions[i].state.arcCenter;
//           if ptOnLine (p1, p2, x, y) then
//              begin
//              reactionIndex := i;
//              exit (reactions[i]);
//              end;
//           end;
//       end;
end;



function TNetwork.overBezierHandle (x, y : double;
                       selectedReaction : integer;
                       var isReactant : boolean;
                       var bezierId : integer;
                       var handleId : TCurrentSelectedBezierHandle;
                       var handleCoords: TPointF): Boolean;
begin
  console.log ('selected reaction overbezier:');
  console.log (selectedReaction);
  if selectedReaction <> -1 then
     begin
     result := reactions[selectedReaction].overBezierHandle (x, y, isReactant, bezierId,
                  handleId, handleCoords);
     end
  else
    result := False;
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

function TNetwork.adjustNetworkCenterOffset (w, h : integer): TPointF;
var i: integer;
    sumx, sumy, cx, cy : double;
begin
  sumx := 0; sumy := 0;
  for i := 0 to length (nodes) - 1 do
      begin
      sumx := sumx + nodes[i].state.x ;
      sumy := sumy + nodes[i].state.y ;
      end;
  cx := sumx/length (nodes);
  cy := sumy/length (nodes);
  Result.x := cx;    // center point offset
  Result.y := cy;
end;

procedure TNetwork.centerNetwork (w, h : integer);
var i, j : integer;
    cx, cy : double;
    centerOffset: TPointF;
begin
  centerOffset := self.adjustNetworkCenterOffset(w, h);
  cx := centerOffset.x;
  cy := centerOffset.y;

for i := 0 to length (nodes) - 1 do
    begin
    if hasReactions (nodes[i]) then
       begin
       nodes[i].state.x := nodes[i].state.x + (w/2 - cx);
       nodes[i].state.y := nodes[i].state.y + (h/2 - cy);
       end;
    end;

  for i := 0 to length (reactions) - 1 do
      begin
      reactions[i].state.arcCenter.x := reactions[i].state.arcCenter.x + (w/2 - cx);
      reactions[i].state.arcCenter.y := reactions[i].state.arcCenter.y + (h/2 - cy);
      for j := 0 to length (reactions[i].state.reactantReactionArcs) - 1 do
          begin
          reactions[i].state.reactantReactionArcs[j].h1.x := reactions[i].state.reactantReactionArcs[j].h1.x + (w/2 - cx);
          reactions[i].state.reactantReactionArcs[j].h1.y := reactions[i].state.reactantReactionArcs[j].h1.y + (h/2 - cy);
          reactions[i].state.reactantReactionArcs[j].h2.x := reactions[i].state.reactantReactionArcs[j].h2.x + (w/2 - cx);
          reactions[i].state.reactantReactionArcs[j].h2.y := reactions[i].state.reactantReactionArcs[j].h2.y + (h/2 - cy);
          end;
      for j := 0 to length (reactions[i].state.productReactionArcs) - 1 do
          begin
          reactions[i].state.productReactionArcs[j].h1.x := reactions[i].state.productReactionArcs[j].h1.x + (w/2 - cx);
          reactions[i].state.productReactionArcs[j].h1.y := reactions[i].state.productReactionArcs[j].h1.y + (h/2 - cy);
          reactions[i].state.productReactionArcs[j].h2.x := reactions[i].state.productReactionArcs[j].h2.x + (w/2 - cx);
          reactions[i].state.productReactionArcs[j].h2.y := reactions[i].state.productReactionArcs[j].h2.y + (h/2 - cy);
          end;
      end;
end;


// -------------------------------------------------------------------------

constructor TNode.create (id : string);
begin
  self.state.id := id;
  self.state.species := id; // default
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


function TNode.computeStartingPoint (endPt : TPointF) : TPointF;
var v : TLineSegment; i : integer; SquareSegs : TBoundingBoxSegments;
begin
  // Define a straight line segment from Node to End Pt
  v.p.x := self.state.x + (self.state.w / 2);
  v.p.y := self.state.y + (self.state.h / 2);

  v.q.x := endPt.x;
  v.q.y := endPt.y;
  // Compute where the point where the segment will start and terminate
  // Gather the boundary segments lines around the node
  SquareSegs := self.getNodeBoundingBox;
  for i := 1 to 4 do
      if segmentIntersects (SquareSegs[i], v, Result) then
         exit;

  // if no intersection then use node centres
  result.x := self.state.x;
  result.y := self.state.y;
end;



function TNode.overNode (x, y : double) : boolean;
begin
  if (x > self.state.x) and (y > self.state.y) and (x < self.state.x + self.state.w) and (y < self.state.y + self.state.h) then
     result := True
  else
     result := False;
end;


function TNode.overControlRectangle (x, y : double; var selectedNodeGrabRectangle : integer) : boolean;
var r : TRectangularArray;
    i : integer;
var sX, sY, sW, sH : double;
    f : integer;
begin
  result := False;

  // Compute the selected box around the node
  f := trunc(4);//*scalingFactor);
  //sX := x*scalingFactor - f;
  //sY := y*scalingFactor - f;
  //sW := state.w*scalingFactor + 2*f;
  //sH := state.h*scalingFactor + 2*f;

  sX := state.x - f;
  sY := state.y - f;
  sW := state.w + 2*f;
  sH := state.h + 2*f;

  // Given the selected box, now compute the control boxes around this rectangle
  r := getControlRects(sX, sY, sW, sH);
  for i := 0 to MAX_NODE_CONTROL_POINTS - 1 do
      begin
      console.log ('TNode.overControlRectangle: ' + inttostr (i));
      result := uGraphUtils.PtInRectF (r[i], TPointF.Create (x,y)) or result;
      if result then
         begin
         console.log ('TNode.overControlRectangle: FOUND');
         selectedNodeGrabRectangle := i;
         exit;
         end;
      end;
end;


function TNode.IsInRectangle (selectionRect : TCanvasRectF) : boolean;
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
  state.lineType := ltBezier;
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

  result := state.clone;
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


procedure TReaction.MoveReaction(dx, dy: double);
var
  dp: TPointF;
  i, j: integer;
begin
  dp := TPointF.Create(dx, dy);
  for i := 0 to state.nReactants - 1 do
      begin
      state.reactantReactionArcs[i].h1.x := state.reactantReactionArcs[i].h1.x + dp.x;
      state.reactantReactionArcs[i].h1.y := state.reactantReactionArcs[i].h1.y + dp.y;
      state.reactantReactionArcs[i].h2.x := state.reactantReactionArcs[i].h2.x + dp.x;
      state.reactantReactionArcs[i].h2.y := state.reactantReactionArcs[i].h2.y + dp.y;
      //for j := 0 to length(SubSegments[i]) - 1 do
      // SubSegments[i][j] := addPtF(SubSegments[i][j], dp);
      end;
  for i := 0 to state.nProducts - 1 do
      begin
      state.productReactionArcs[i].h1.x := state.productReactionArcs[i].h1.x + dp.x;
      state.productReactionArcs[i].h1.y := state.productReactionArcs[i].h1.y + dp.y;
      state.productReactionArcs[i].h2.x := state.productReactionArcs[i].h2.x + dp.x;
      state.productReactionArcs[i].h2.y := state.productReactionArcs[i].h2.y + dp.y;
      //for j := 0 to length(SubSegments[i]) - 1 do
      // SubSegments[i][j] := addPtF(SubSegments[i][j], dp);
      end;

  state.arcCenter.x := state.arcCenter.x + dp.x;
  state.arcCenter.y := state.arcCenter.y + dp.y;
end;


function TReaction.overCentroid (x, y : double) : boolean;
begin
  if ptInCircle (x, y, state.arcCenter)  then
    result := True
  else
    result := False;
end;

// Adjust the arc centre to vx, vy and when doing so also adjust the
// INNER control handles by the same amount, makes things behave
// visually in a nice way.
procedure TReaction.adjustArcCentres(vx, vy: double);
var
  dvx, dvy: double;
  i : integer;
begin
  dvx := vx - state.arcCenter.x;
  dvy := vy - state.arcCenter.y;
  state.arcCenter.x := vx;
  state.arcCenter.y := vy;

  for i := 0 to state.nReactants - 1 do
      begin
      state.reactantReactionArcs[i].h2.x := state.reactantReactionArcs[i].h2.x + dvx;
      state.reactantReactionArcs[i].h2.y := state.reactantReactionArcs[i].h2.y + dvy;
      end;

  for i := 0 to state.nProducts - 1 do
      begin
      state.productReactionArcs[i].h1.x := state.productReactionArcs[i].h1.x + dvx;
      state.productReactionArcs[i].h1.y := state.productReactionArcs[i].h1.y + dvy;
      end;

//  for i := 0 to length(state.reactionArcs) - 1 do
//          case state.reactionArcs[i].arcDirection of
//            adInArc:
//              begin
//                state.reactionArcs[i].h2.x := state.reactionArcs[i].h2.x + dvx;
//                state.reactionArcs[i].h2.y := state.reactionArcs[i].h2.y + dvy;
//              end;
//            adOutArc:
//              begin
//                state.reactionArcs[i].h1.x := state.reactionArcs[i].h1.x + dvx;
//                state.reactionArcs[i].h1.y := state.reactionArcs[i].h1.y + dvy;
//              end;
//          end;
 end;

// Check if mouse in on a particular control handle. If successful it returns:
// isReactantSide : True if the handle is on the reactant side
// bezierId : which bezier arc was it
// handleId : Which handle on the bezier was detected?
// handleCoords : The x/y coords of detected handle
//
function TReaction.overBezierHandle(x, y : double;
           var isReactantSide : boolean;
           var bezierId: integer;
           var handleID: TCurrentSelectedBezierHandle;
           var handleCoords: TPointF): Boolean;
var i : integer;
    h1, h2 : TPointF;
begin
  result := False;
  for i := 0 to state.nReactants - 1 do
      begin
      h1 := state.reactantReactionArcs[i].h1;
      h2 := state.reactantReactionArcs[i].h2;
      console.log ('h1 and h2');
      console.log (h1); console.log (h2);
      if ptInCircle (x, y, h1) then
         begin
         bezierId := i;
         handleId := 0;
         handleCoords := h1;
         result := True;
         isReactantSide := True;
         exit;
         end;
      if ptInCircle (x, y, h2) then
         begin
         bezierId := i;
         handleId := 1;
         handleCoords := h2;
         isReactantSide := True;
         result := True;
         exit;
         end;
      end;

  for i := 0 to state.nProducts - 1 do
      begin
      h1 := state.productReactionArcs[i].h1;
      h2 := state.productReactionArcs[i].h2;
      if ptInCircle (x, y, h1) then
         begin
         bezierId := i;
         handleId := 0;
         handleCoords := h1;
         isReactantSide := False;
         result := True;
         exit;
         end;
      if ptInCircle (x, y, h2) then
         begin
         bezierId := i;
         handleId := 1;
         handleCoords := h2;
         isReactantSide := False;
         result := True;
         exit;
         end;
      end;
end;


function TReaction.isInRectangle (selectionBox : TCanvasRectF) : boolean;
var i :  integer;
begin
  result := True;
  for i := 0 to state.nReactants - 1 do
      result := result and ptInRectF (selectionBox, TPointF.Create(state.srcPtr[i].state.x, state.srcPtr[i].state.y));

  for i := 0 to state.nProducts - 1 do
      result := result and ptInRectF (selectionBox, TPointF.Create(state.destPtr[i].state.x, state.destPtr[i].state.y));
end;


end.

