unit uSBMLClasses.Layout;

interface
   uses System.SysUtils, System.Classes, System.Generics.Collections, Web, JS,
     uSidewinderTypes;
// NOTE: javascript calling overloaded functions choses the first one, period.
//      does not care about type or number of parameters. Must have pascal code
//      calling pascal code for overloads to work. This is then translated to JavaScript.

//  Web quote: There is no real function overloading in JavaScript since it allows you to pass
//   any number of parameters of any type. You have to check inside the function
//   for how many arguments have been passed in and what type they are.
const STRING_SPECIES_REF_ROLES: array[0..8] of string = ( 'undefined', 'substrate', 'product', 'sidesubstrate',
    'sideproduct', 'modifier', 'activator', 'inhibitor', 'invalid');// string version of TSPECIES_REF_ROLE
type
 TSPECIES_REF_ROLE = ( SPECIES_ROLE_UNDEFINED, SPECIES_ROLE_SUBSTRATE, SPECIES_ROLE_PRODUCT,
                SPECIES_ROLE_SIDESUBSTRATE, SPECIES_ROLE_SIDEPRODUCT, SPECIES_ROLE_MODIFIER,
                SPECIES_ROLE_ACTIVATOR, SPECIES_ROLE_INHIBITOR, SPECIES_ROLE_INVALID );
 TSBMLLayoutDims = class
   private
     id: string;  // optional
     width: double;
     height: double;
     depth: double;  // optional
     depthSet: boolean;
   public
   constructor create(); overload;
   constructor create(w: double; h: double) overload;
   constructor create(w: double; h: double; d: double) overload;
   constructor create(cpy: TSBMLLayoutDims) overload;
   function getWidth(): double;
   function getHeight(): double;
   function getDepth(): double;
   procedure setWidth(newW: double);
   procedure setHeight(newH: double);
   procedure setDepth(newD: double); //set to 0 means it is not set.
   function isDepthSet(): boolean;
   function getId(): string;
   procedure setId(newId: string);
   function printStr(): string;

 end;
 TSBMLLayoutPoint = class
   private
     id: string;  // optional
     xPt: double;
     yPt: double;
     zPt: double; // optional
     zSet: boolean;
   public
   constructor create() overload;
   constructor create( x: double; y: double ) overload;
   constructor create( x: double; y: double; z: double ) overload;
   constructor create( cpyPt: TSBMLLayoutPoint) overload;
   function getX(): double;
   function getY(): double;
   function getZ(): double;
   function isZSet(): boolean;
   procedure unSetZFlag(); // disregard z value
   procedure setX(newX: double);
   procedure setY(newY: double);
   procedure setZ(newZ: double);
   function getId(): string;
   procedure setId(newId: string);
   function printStr(): string;

 end;

 TSBMLLayoutBoundingBox = class
   private
     id: string; // optional
     point: TSBMLLayoutPoint;
     dims: TSBMLLayoutDims;

   public
   constructor create() overload;
   constructor create(newPt: TSBMLLayoutPoint; newDims: TSBMLLayoutDims) overload;
   constructor create(cpy: TSBMLLayoutBoundingBox) overload;
   procedure clear(); // Used to empty it out for new values.
   procedure setPoint(newPt: TSBMLLayoutPoint) overload;
   procedure setPoint(x: double; y: double) overload;
   function getPoint(): TSBMLLayoutPoint;
   procedure setDims( newDims: TSBMLLayoutDims) overload;
   procedure setDims(newHt: double; newWidth: double) overload;
   function getDims(): TSBMLLayoutDims;
   procedure setId( newId: string);
   function isSetId(): boolean;
   function getId(): string;
   function printStr(): string;
   end;

 TSBMLLayoutLineSegment = class
   private
     id: string; // optional
     startPt: TSBMLLayoutPoint;
     endPt: TSBMLLayoutPoint;
   public
   constructor create() overload;
   constructor create(sPt: TSBMLLayoutPoint; ePt: TSBMLLayoutPoint) overload;
   constructor create(cpy: TSBMLLayoutLineSegment) overload;
   function getStartPt(): TSBMLLayoutPoint;
   procedure setStartPt( newStart: TSBMLLayoutPoint);
   function getEndPt(): TSBMLLayoutPoint;
   procedure setEndPt(newEnd: TSBMLLayoutPoint);
   function getId(): string;
   procedure setId( newId: string);
   function printStr: string;

 end;

 TSBMLLayoutCubicBezier = class
   private
     startPt: TSBMLLayoutPoint;
     endPt: TSBMLLayoutPoint;
     basePt1: TSBMLLayoutPoint;
     basePt2: TSBMLLayoutPoint;
     id: string;  // optional
   public
   constructor create() overload;
   constructor create(bp1: TSBMLLayoutPoint; bp2: TSBMLLayoutPoint;
               startp: TSBMLLayoutPoint; endp: TSBMLLayoutPoint ) overload;
   constructor create(cpy: TSBMLLayoutCubicBezier) overload;
   function getBasePoint1(): TSBMLLayoutPoint;
   function getBasePoint2(): TSBMLLayoutPoint;
   function getStart(): TSBMLLayoutPoint;
   function getEnd(): TSBMLLayoutPoint;
   procedure setBasePt1(pt: TSBMLLayoutPoint) overload;
   procedure setBasePt1(x: double; y: double) overload;
   procedure setBasePt2(pt: TSBMLLayoutPoint) overload;
   procedure setBasePt2(x: double; y: double) overload;
   procedure setStart(pt: TSBMLLayoutPoint) overload;
   procedure setStart(x: double; y: double) overload;
   procedure setEnd(pt: TSBMLLayoutPoint) overload;
   procedure setEnd(x: double; y: double) overload;
   function getId(): string;
   procedure setId(newId: string);
   function printStr: string;
 end;

 TSBMLLayoutCurve = class
   private
     curveId: string;
     lineSegmentList: TList<TSBMLLayoutLineSegment>;
     cubicBezList: TList<TSBMLLayoutCubicBezier>;
   public
   constructor create() overload;
   constructor create(cpy: TSBMLLayoutCurve) overload;
   procedure clear();
   procedure addCubicBezier(newBez: TSBMLLayoutCubicBezier);
   procedure addLineSegment(newLine: TSBMLLayoutLineSegment);
   function getNumCurveSegments(): integer;
   function getNumCubicBeziers(): integer;
   function getCubicBezier(index: integer): TSBMLLayoutCubicBezier;
   function getLineSegment(index: integer): TSBMLLayoutLineSegment;
   function deleteCubicBezier(index: integer): boolean;
   function deleteLineSegment(index: integer): boolean;
 //  procedure setId(nwId: string);
 //  function getId(): string;
   procedure setCurveId(newId: string);
   function getCurveId(): string;
   function printStr: string;

 end;

 TSBMLLayoutGraphicalObject = class
 private
   id: string;
   boundingBox: TSBMLLayoutBoundingBox;
   boundingBoxSet: boolean;
 public
   constructor create(newId: string) overload;
   constructor create(cpy: TSBMLLayoutGraphicalObject) overload;
   procedure setId(newId: string);
   function getId(): string;
   procedure setBoundingBox(newBox: TSBMLLayoutBoundingBox);
   procedure unsetBoundingBox();
   function getBoundingBox(): TSBMLLayoutBoundingBox;
   function boundingBoxIsSet(): boolean;
   function printStr: string;
 end;

 // 3.11 The GeneralGlyph class: Used to facilitate the representation of elements
 // other than Compartment, Species and Reaction.
 TSBMLLayoutGeneralGlyph = class(TSBMLLayoutGraphicalObject)
 private
   notes: string;
 //TODO: curve: TSBMLLayoutCurve;// describes a curve for the center piece of an additional relationship
 //TODO: curveFlag: boolean;
 public
   constructor create(newId: string) overload;
   constructor create(cpy: TSBMLLayoutGeneralGlyph) overload;
   procedure setNotes(newN: string);
   function getNotes(): string;
   function printStr: string;
 end;

 TSBMLLayoutCompartmentGlyph = class(TSBMLLayoutGraphicalObject)
 private
   order: double;
   compId: string;
 public
   constructor create(newId: string) overload;
   constructor create(cpy: TSBMLLayoutCompartmentGlyph) overload;
   procedure setOrder(newOrder: double);
   function getOrder(): double;
   procedure setCompId(newId: string);
   function getCompId(): string;
   function printStr: string;
 end;

 TSBMLLayoutSpeciesGlyph = class(TSBMLLayoutGraphicalObject)
 private
   speciesId: string;  // associated species ID
 public
   constructor create(newSpId: string) overload;
   constructor create(cpy: TSBMLLayoutSpeciesGlyph) overload;
   procedure setSpeciesId( newSpId: string);
   function getSpeciesId(): string;
   function printStr: string;
 end;

 TSBMLLayoutSpeciesReferenceGlyph = class(TSBMLLayoutGraphicalObject)
 private
   specGlyphId: string;  // id of speciesGlyph associated with this obj
   specRefId: string;  // id of species reference used in reaction ?? needed?
   role: TSPECIES_REF_ROLE;  // optional, default is 'undefined'
   curve: TSBMLLayoutCurve;  // optionally use in place of bounding box
   curveFlag: boolean;
 public
   constructor create() overload;
   constructor create(newSpRefId: string) overload;
   constructor create(cpy: TSBMLLayoutSpeciesReferenceGlyph) overload;
   procedure clear();
   function  getSpeciesGlyphId(): string;
   procedure setSpeciesGlyphId(newId: string);
   function  getSpeciesRefId(): string;
   procedure setSpeciesRefId(newId: string);
   function  getRole(): TSPECIES_REF_ROLE;
   // Allowed values are “substrate”, “product”, “sidesubstrate”, “sideproduct”, “modifier”,
   // “activator”, “inhibitor” and “undefined”.
   function getStringRole(): string;
   procedure setRole(newRole: TSPECIES_REF_ROLE);
   function  setStrRole(newRole: string): boolean;
   procedure setCurve(newCurve: TSBMLLayoutCurve);
   function  getCurve(): TSBMLLayoutCurve;
   function  isCurveSet(): boolean;
   function  printStr: string;
 end;

 TSBMLLayoutReactionGlyph = class(TSBMLLayoutGraphicalObject)
 private
   rxnId: string;  // id of reaction
   speciesRefGlyphList: TList<TSBMLLayoutSpeciesReferenceGlyph>;
   curve: TSBMLLayoutCurve; // center piece of a reaction, or use bounding box
   curveFlag: boolean;
 public
   constructor create(newRxnId: string) overload;
   constructor create(cpy: TSBMLLayoutReactionGlyph) overload;
   procedure clear();
   procedure setReactionId(newId: string);
   function getReactionId(): string;
   function getNumSpeciesRefGlyphs(): integer;
   function getSpeciesRefGlyph(index: integer): TSBMLLayoutSpeciesReferenceGlyph;
   procedure addSpeciesRefGlyph(newGlyph: TSBMLLayoutSpeciesReferenceGlyph);
   function deleteSpeciesRefGlyph(index: integer): boolean;
   function getReactionCenterPoint(): TSBMLLayoutPoint;
   procedure setCurve(newCurve: TSBMLLayoutCurve);
   function getCurve(): TSBMLLayoutCurve;
   function isCurveSet(): boolean;
   function printStr: string;
 end;

 // spec 3.12
 TSBMLLayoutTextGlyph = class(TSBMLLayoutGraphicalObject)
 private
   text: string; // optional, if this exists then do not use originOfText
   originOfText: string;// id of an entity in the SBML model.
   graphicalObjId: string; // corresponds to another graphical object id.
 public
   constructor create() overload;
   constructor create(cpy: TSBMLLayoutTextGlyph) overload;
   procedure clear();
   procedure setText(newText: string);
   function getText(): string;
   procedure setOriginOfText(newText: string);
   function getOriginOfText: string;
   procedure setGraphicalObjId(newId: string);
   function getGraphicalObjId(): string;
   function printStr: string;

 end;

 TSBMLLayout = class
   private
     id: string; // optional
     additionalGraphObjList: TList<TSBMLLayoutGraphicalObject>;
     additionalGenGlyphList: TList<TSBMLLayoutGeneralGlyph>;
     compartmentGlyphList: TList<TSBMLLayoutCompartmentGlyph>;
     speciesGlyphList: TList<TSBMLLayoutSpeciesGlyph>;
     reactionGlyphList: TList<TSBMLLayoutReactionGlyph>;
     textGlyphList: TList<TSBMLLayoutTextGlyph>;
     dims: TSBMLLayoutDims;

   public
   constructor create() overload;
   constructor create(cpy: TSBMLLayout) overload;
   procedure setId(newId: string);
   function getId(): string;
   procedure setDims(newDims: TSBMLLayoutDims);
  // procedure setDims(w: double; h: double) overload;
   //Note: javascript grabs the first overload function
   function  getDims(): TSBMLLayoutDims;
   procedure addGraphObj(newGO: TSBMLLayoutGraphicalObject);
   function  getNumAddionalGraphObjs(): integer;
   function  getAdditionalGraphObj(index:integer): TSBMLLayoutGraphicalObject;
   function  deleteAdditionalGraphObj(index: integer): boolean;
   procedure addGenGlyph(newGenG: TSBMLLayoutGeneralGlyph);
   function  getNumGenGlyphs(): integer;
   function  getGenGlyph(index:integer): TSBMLLayoutGeneralGlyph;
   function  deleteGenGlyph(index: integer): boolean;
   procedure addSpGlyph(newSpG: TSBMLLayoutSpeciesGlyph);
   function  getNumSpGlyphs(): integer;
   function  getSpGlyph(index: integer): TSBMLLayoutSpeciesGlyph;
   function  getSpGlyphList(): TList<TSBMLLayoutSpeciesGlyph>;
   function  deleteSpGlyph(index: integer): boolean;
   procedure addCompGlyph(newCompG: TSBMLLayoutCompartmentGlyph);
   function  getNumCompGlyphs(): integer;
   function  getCompGlyph(index: integer): TSBMLLayoutCompartmentGlyph;
   function  deleteCompGlyph(index: integer): boolean;
   procedure addRxnGlyph(newRxnG: TSBMLLayoutReactionGlyph);
   function  getNumRxnGlyphs(): integer;
   function  getRxnGlyph(index: integer): TSBMLLayoutReactionGlyph;
   function deleteRxnGlyph(index: integer): boolean;
   procedure addTextGlyph(newTextG: TSBMLLayoutTextGlyph);
   function getNumTextGlyphs(): integer;
   function getTextGlyph(index: integer): TSBMLLayoutTextGlyph;
   function deleteTextGlyph(index: integer): boolean;
   function printStr: string;

 end;

 // helper functions:
 function getSpeciesIdFromSpGlyphId(spGlyphId: string; spGlyphList: TList<TSBMLLayoutSpeciesGlyph>): string;

implementation

  constructor TSBMLLayoutDims.create() overload;
  begin
    self.width := -1;
    self.height := -1;
    self.depth := 0;
    self.id := '';
    self.depthSet := false;
  end;
  constructor TSBMLLayoutDims.create(w: double; h: double) overload;
  begin
    self.width := w;
    self.height := h;
    self.depth := 0;
    self.id := '';
    self.depthSet := false;
  end;

  constructor TSBMLLayoutDims.create(cpy: TSBMLLayoutDims) overload;
  begin
    self.id := cpy.getId;
    self.width := cpy.getWidth;
    self.height := cpy.getHeight;
    if cpy.isDepthSet then
    begin
      self.depth := cpy.getDepth;
      self.depthSet := true;
    end
    else self.depthSet := false;

  end;

  constructor TSBMLLayoutDims.create(w: double; h: double; d: double) overload;
  begin
    self.id := '';
    self.width := w;
    self.height := h;
    self.depth := d;
    self.depthSet := true;
  end;

  function TSBMLLayoutDims.printStr: string;
  begin
    Result := ' Layout Dims ID: ' + self.id;
    Result := Result + ' Layout w, h: ' + floattostr(self.width) + ', ' + floattostr(self.height);
    if self.depthSet then Result := Result + ' Layout depth: ' + floattostr(self.depth);
  end;

  procedure TSBMLLayoutDims.setId(newId: string);
  begin
    self.id := newId;
  end;
  function TSBMLLayoutDims.getId(): string;
  begin
    Result := self.id;
  end;
  function TSBMLLayoutDims.getWidth(): double;
  begin
    Result := self.width;
  end;
  function TSBMLLayoutDims.getHeight(): double;
  begin
    Result := self.height;
  end;
  function TSBMLLayoutDims.getDepth(): double;
  begin
    Result := self.depth;
  end;
  procedure TSBMLLayoutDims.setWidth(newW: double);
  begin
    self.width := newW;
  end;
  procedure TSBMLLayoutDims.setHeight(newH: double);
  begin
    self.height := newH;
  end;
  procedure TSBMLLayoutDims.setDepth(newD: double);
  begin
    self.depth := newD;
    if newD = 0 then self.depthSet := false
    else self.depthSet := true;
  end;

  function TSBMLLayoutDims.isDepthSet(): boolean;
  begin
    Result := self.depthSet;
  end;

  constructor TSBMLLayoutPoint.create() overload;
  begin
    self.id := '';
   // self.xPt := 0;
   // self.yPt := 0;
    self.zSet := false;
  end;
  constructor TSBMLLayoutPoint.create( x: double; y: double ) overload;
  begin
    self.id := '';
    self.xPt := x;
    self.yPt := y;
    self.zSet := false;
  end;
  constructor TSBMLLayoutPoint.create( x: double; y: double; z: double ) overload;
  begin
    self.id := '';
    self.xPt := x;
    self.yPt := y;
    self.zPt := z;
    self.zSet := true;
  end;

  constructor TSBMLLayoutPoint.create( cpyPt: TSBMLLayoutPoint) overload;
  begin
    self.id := cpyPt.getId;
    self.xPt := cpyPt.getX;
    self.yPt := cpyPt.getY;
    if cpyPt.isZSet then
    begin
      self.zPt := cpyPt.getZ;
      self.zSet := true;
    end
    else self.zSet := false;

  end;

  function TSBMLLayoutPoint.printStr(): string;
  begin
    Result := '';
    Result := ' Layout Pt ID: ' + self.id;
    Result := Result + ' Layout Pt XY: ' + floattostr(self.xPt) + ', ' +floattostr(self.yPt);
    if self.isZSet then Result := Result + ', Layout Pt Z: ' + floattostr(self.zPt);

  end;

  procedure TSBMLLayoutPoint.setId(newId: string);
  begin
    self.id := newId;
  end;
  function TSBMLLayoutPoint.getId(): string;
  begin
    Result := self.id;
  end;
  function TSBMLLayoutPoint.getX(): double;
  begin
    Result := self.xPt;
  end;
  function TSBMLLayoutPoint.getY(): double;
  begin
    Result := self.yPt
  end;
  function TSBMLLayoutPoint.getZ(): double;
  begin
    Result := self.zPt;
  end;
  procedure TSBMLLayoutPoint.setX(newX: double);
  begin
    self.xPt := newX;
  end;
  procedure TSBMLLayoutPoint.setY(newY: double);
  begin
    self.yPt := newY;
  end;
  procedure TSBMLLayoutPoint.setZ(newZ: double);
  begin
    self.zPt := newZ;
    self.zSet := true;
  end;
  function TSBMLLayoutPoint.isZSet(): boolean;
  begin
    Result := self.zSet;
  end;
  procedure TSBMLLayoutPoint.unSetZFlag(); // disregard z value
  begin
    self.zSet := false;
  end;

  constructor TSBMLLayoutBoundingBox.create() overload;
  begin
    self.id := '';
    self.point := TSBMLLayoutPoint.create();
  end;
  constructor TSBMLLayoutBoundingBox.create(newPt: TSBMLLayoutPoint; newDims: TSBMLLayoutDims) overload;
  begin
    self.id := '';
    self.point := TSBMLLayoutPoint.create(newPt);
   // if newDims.isDepthSet then
   //   self.dims := TSBMLLayoutDims.create(newDims.getWidth, newDims.getHeight, newDims.getDepth)
   // else
   //   self.dims := TSBMLLayoutDims.create(newDims.getWidth, newDims.getHeight);
    self.dims := TSBMLLayoutDims.create(newDims);
  end;
  constructor  TSBMLLayoutBoundingBox.create(cpy: TSBMLLayoutBoundingBox) overload;
  begin
    if cpy <> nil then
    begin
      self.id := cpy.getId;
      self.point := TSBMLLayoutPoint.create(cpy.getPoint);
      self.dims := TSBMLLayoutDims.create(cpy.getDims);
    end
    else notifyUser('TSBMLLayoutBoundingBox: no bounding box to copy');
  end;

  function TSBMLLayoutBoundingBox.printStr: string;
  begin
    Result := '';
    Result := ' BoundingBox ID: ' + self.id + ', BB pt: ' + self.point.printStr;
    Result := Result + self.dims.printStr;
  end;

  procedure TSBMLLayoutBoundingBox.clear();
  begin
    self.id := '';
    self.point.free;
    self.dims.free;
  end;

  procedure TSBMLLayoutBoundingBox.setPoint(newPt: TSBMLLayoutPoint) overload;
  begin
    self.point := TSBMLLayoutPoint.create(newPt);
  end;
  procedure TSBMLLayoutBoundingBox.setPoint(x: double; y: double) overload;
  begin
    self.point.xPt := x;
    self.point.yPt := y;
    self.point.unSetZFlag;

  end;

  function TSBMLLayoutBoundingBox.getPoint(): TSBMLLayoutPoint;
  begin
    Result := self.point;
  end;
  procedure TSBMLLayoutBoundingBox.setDims( newDims: TSBMLLayoutDims) overload;
  begin
    self.dims := TSBMLLayoutDims.create(newDims);
  end;
  procedure TSBMLLayoutBoundingBox.setDims(newHt: double; newWidth: double) overload;
  begin
    self.dims := TSBMLLayoutDims.create(newHt, newWidth);
  end;
  function TSBMLLayoutBoundingBox.getDims(): TSBMLLayoutDims;
  begin
    Result := self.dims;
  end;
  procedure TSBMLLayoutBoundingBox.setId( newId: string);
  begin
    self.id := newId;
  end;
  function TSBMLLayoutBoundingBox.getId(): string;
  begin
    Result := self.id;
  end;
  function TSBMLLayoutBoundingBox.isSetId(): boolean;
  var flag: boolean;
  begin
    flag := false;
    if self.id <> '' then flag := true;
    Result := flag;
  end;

  constructor TSBMLLayoutLineSegment.create() overload;
  begin
    self.id := '';
  end;

  constructor TSBMLLayoutLineSegment.create(sPt: TSBMLLayoutPoint; ePt: TSBMLLayoutPoint) overload;
  begin
    if sPt.isZSet then
      self.startPt := TSBMLLayoutPoint.create(sPt.getX, sPt.getY, sPt.getZ)
    else self.startPt := TSBMLLayoutPoint.create(sPt.getX, sPt.getY);
    if ePt.isZSet then
       self.endPt := TSBMLLayoutPoint.create(ePt.getX, ePt.getY, ePt.getZ)
    else self.endPt := TSBMLLayoutPoint.create(ePt.getX, ePt.getY);

    self.id := '';
  end;

  constructor TSBMLLayoutLineSegment.create(cpy: TSBMLLayoutLineSegment) overload;
  begin
    self.id := cpy.getId;
    self.startPt := TSBMLLayoutPoint.create(cpy.getStartPt);
    self.endPt := TSBMLLayoutPoint.create(cpy.getEndPt);
  end;

  function TSBMLLayoutLineSegment.printStr: string;
  begin
    Result := ', Layout LineSeg ID: '+ self.id +', Start: ' + self.getStartPt.printStr +
     ', End: ' + self.getEndPt.printStr;
  end;

  function TSBMLLayoutLineSegment.getStartPt(): TSBMLLayoutPoint;
  begin
    Result := self.startPt;
  end;

  function TSBMLLayoutLineSegment.getEndPt(): TSBMLLayoutPoint;
  begin
    Result := self.endPt;
  end;

  procedure TSBMLLayoutLineSegment.setStartPt( newStart: TSBMLLayoutPoint);
  begin
    self.startPt :=  TSBMLLayoutPoint.create(newStart);
  end;

   procedure TSBMLLayoutLineSegment.setEndPt( newEnd: TSBMLLayoutPoint);
  begin
    self.endPt :=  TSBMLLayoutPoint.create(newEnd);
  end;

  function TSBMLLayoutLineSegment.getId(): string;
  begin
    Result := self.id;
  end;
  procedure TSBMLLayoutLineSegment.setId(newId: string);
  begin
    self.id := newId;
  end;


  constructor TSBMLLayoutCubicBezier.create() overload;
  begin
    self.id := '';
    self.startPt := TSBMLLayoutPoint.create(0,0);
    self.endPt := TSBMLLayoutPoint.create(0,0);
    self.basePt1 := TSBMLLayoutPoint.create(0,0);
    self.basePt2 := TSBMLLayoutPoint.create(0,0);

  end;
  constructor TSBMLLayoutCubicBezier.create(bp1: TSBMLLayoutPoint; bp2: TSBMLLayoutPoint;
               startp: TSBMLLayoutPoint; endp: TSBMLLayoutPoint ) overload;
  begin
    self.id := '';
    self.startPt := TSBMLLayoutPoint.create(startp);
    self.endPt := TSBMLLayoutPoint.create(endp);
    self.basePt1 := TSBMLLayoutPoint.create(bp1);
    self.basePt2 := TSBMLLayoutPoint.create(bp2);
  end;

  constructor TSBMLLayoutCubicBezier.create(cpy: TSBMLLayoutCubicBezier) overload;
  begin
    self.id := cpy.getId;
    self.startPt := TSBMLLayoutPoint.create(cpy.getStart);
    self.endPt := TSBMLLayoutPoint.create(cpy.getEnd);
    self.basePt1 := TSBMLLayoutPoint.create(cpy.getBasePoint1);
    self.basePt2 := TSBMLLayoutPoint.create(cpy.getBasePoint2);
  end;

  function TSBMLLayoutCubicBezier.printStr: string;
  begin
    Result := ', Layout Bezier ID: ' + self.getId +', Bezier Start: ' + self.getStart.printStr +
     ', Bezier End: ' + self.getEnd.printStr;
     Result := Result + 'Bezier BPt 1: ' + self.getBasePoint1.printStr +
     ', Bezier BPt 2: ' + self.getBasePoint2.printStr;
  end;

  function TSBMLLayoutCubicBezier.getBasePoint1(): TSBMLLayoutPoint;
  begin
    Result := self.basePt1;
  end;
  function TSBMLLayoutCubicBezier.getBasePoint2(): TSBMLLayoutPoint;
  begin
    Result := self.basePt2;
  end;
  function TSBMLLayoutCubicBezier.getStart(): TSBMLLayoutPoint;
  begin
    Result := self.startPt;
  end;
  function TSBMLLayoutCubicBezier.getEnd(): TSBMLLayoutPoint;
  begin
    Result := self.endPt;
  end;
  procedure TSBMLLayoutCubicBezier.setBasePt1(pt: TSBMLLayoutPoint) overload;
  begin
    self.basePt1 := TSBMLLayoutPoint.create(pt);
  end;
  procedure TSBMLLayoutCubicBezier.setBasePt1(x: double; y: double) overload;
  begin
    self.basePt1 := TSBMLLayoutPoint.create(x, y);
  end;
  procedure TSBMLLayoutCubicBezier.setBasePt2(pt: TSBMLLayoutPoint) overload;
  begin
    self.basePt2 := TSBMLLayoutPoint.create(pt);
  end;
  procedure TSBMLLayoutCubicBezier.setBasePt2(x: double; y: double) overload;
  begin
    self.basePt2 := TSBMLLayoutPoint.create(x, y);
  end;
  procedure TSBMLLayoutCubicBezier.setStart(pt: TSBMLLayoutPoint) overload;
  begin
    self.startPt := TSBMLLayoutPoint.create(pt);
  end;
  procedure TSBMLLayoutCubicBezier.setStart(x: double; y: double) overload;
  begin
    self.startPt := TSBMLLayoutPoint.create(x, y);
  end;
  procedure TSBMLLayoutCubicBezier.setEnd(pt: TSBMLLayoutPoint) overload;
  begin
    self.endPt := TSBMLLayoutPoint.create(pt);
  end;
  procedure TSBMLLayoutCubicBezier.setEnd(x: double; y: double) overload;
  begin
    self.endPt := TSBMLLayoutPoint.create(x, y);
  end;
  function TSBMLLayoutCubicBezier.getId(): string;
  begin
    Result := self.id;
  end;
  procedure TSBMLLayoutCubicBezier.setId(newId: string);
  begin
    self.id := newId;
  end;

  constructor TSBMLLayoutCurve.create();
  begin
    self.curveId := '';
    self.lineSegmentList := TList<TSBMLLayoutLineSegment>.create;
    self.cubicBezList := TList<TSBMLLayoutCubicBezier>.create;
  end;

  constructor TSBMLLayoutCurve.create(cpy: TSBMLLayoutCurve) overload;
  var i: integer;
  begin
    self.curveId := cpy.getCurveId;
    self.lineSegmentList := TList<TSBMLLayoutLineSegment>.create;
    self.cubicBezList := TList<TSBMLLayoutCubicBezier>.create;
    for i := 0 to cpy.getNumCurveSegments-1 do
    begin
      self.lineSegmentList.Add(cpy.getLineSegment(i));
    end;
    for i := 0 to cpy.getNumCubicBeziers-1 do
    begin
      self.cubicBezList.Add(cpy.getCubicBezier(i));
    end;
  end;

  function TSBMLLayoutCurve.printStr: string;
  var i: integer;
  begin
    Result := ', Layout Curve ID: ' + self.getCurveId;
    for i := 0 to self.lineSegmentList.Count -1 do
      Result := Result + 'Layout Curve lineSeg:' + self.getLineSegment(i).printStr;
    for i := 0 to self.cubicBezList.Count -1 do
      Result := Result + 'Layout Curve Bezier:' + self.getCubicBezier(i).printStr;
  end;

  procedure TSBMLLayoutCurve.clear();
  var
  i: Integer;
  begin
    self.curveId := '';
    for i := 0 to self.lineSegmentList.Count -1 do
      self.deleteLineSegment(i);
    for i := 0 to self.cubicBezList.Count -1 do
      self.deleteCubicBezier(i);
  end;

  procedure TSBMLLayoutCurve.addCubicBezier(newBez: TSBMLLayoutCubicBezier);
  begin
    if self.cubicBezList = nil then
      self.cubicBezList := TList<TSBMLLayoutCubicBezier>.create;
    self.cubicBezList.Add(TSBMLLayoutCubicBezier.create(newBez));
  end;
  procedure TSBMLLayoutCurve.addLineSegment(newLine: TSBMLLayoutLineSegment);
  begin
    if self.lineSegmentList = nil then
      self.lineSegmentList := TList<TSBMLLayoutLineSegment>.create;
    self.lineSegmentList.Add(TSBMLLayoutLineSegment.create(newLine));
  end;
  function TSBMLLayoutCurve.getNumCurveSegments(): integer;
  begin
    Result := self.lineSegmentList.Count;
  end;
  function TSBMLLayoutCurve.getNumCubicBeziers(): integer;
  begin
    Result := self.cubicBezList.Count;
  end;
  function TSBMLLayoutCurve.getCubicBezier(index: integer): TSBMLLayoutCubicBezier;
  begin
    Result := self.cubicBezList[index];
  end;
  function TSBMLLayoutCurve.getLineSegment(index: integer): TSBMLLayoutLineSegment;
  begin
    Result := self.lineSegmentList[index];
  end;
  function TSBMLLayoutCurve.deleteCubicBezier(index: integer): boolean;
  var success: boolean;
  begin
    try
      self.cubicBezList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;
  function TSBMLLayoutCurve.deleteLineSegment(index: integer): boolean;
  var success: boolean;
  begin
    try
      self.lineSegmentList.Delete(index);
      success := true;
    except
    on E: Exception do
      success := false;
    end;
    Result := success;
  end;
  procedure TSBMLLayoutCurve.setCurveId(newId: string);
  begin
    self.curveId := newId;
  end;
  function TSBMLLayoutCurve.getCurveId(): string;
  begin
    Result := self.curveId;
  end;

  constructor TSBMLLayoutGraphicalObject.create(newId: string) overload;
  begin
    self.id := newId;
    self.boundingBoxSet := false;
  end;

  constructor TSBMLLayoutGraphicalObject.create(cpy: TSBMLLayoutGraphicalObject) overload;
  begin
    self.id := cpy.getId;
    if cpy.boundingBoxSet then
    begin
      self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
      self.boundingBoxSet := true;
    end
    else self.boundingBoxSet := false;
  end;

  function TSBMLLayoutGraphicalObject.printStr: string;
  begin
    Result := ', Layout Graph Object ID:' + self.getId;
    if self.boundingBoxSet then Result := Result + self.getBoundingBox.printStr
    else Result := Result + ' No bounding box';
  end;

  procedure TSBMLLayoutGraphicalObject.setId(newId: string);
  begin
    self.id := newId;
  end;
  function TSBMLLayoutGraphicalObject.getId(): string;
  begin
    Result := self.id;
  end;
  procedure TSBMLLayoutGraphicalObject.setBoundingBox(newBox: TSBMLLayoutBoundingBox);
  begin
    self.boundingBox := TSBMLLayoutBoundingBox.create(newBox);
    self.boundingBoxSet := true;
  end;
  function TSBMLLayoutGraphicalObject.getBoundingBox(): TSBMLLayoutBoundingBox;
  begin
    Result := self.boundingBox;
  end;

  function TSBMLLayoutGraphicalObject.boundingBoxIsSet(): boolean;
  begin
    Result := self.boundingBoxSet;
  end;

  procedure TSBMLLayoutGraphicalObject.unsetBoundingBox();
  begin
    self.boundingBoxSet := false;
  end;

  constructor TSBMLLayoutGeneralGlyph.create(newId: string) overload;
  begin
    inherited create('generalGlyph' + newId);
    self.notes := '';
  end;

  constructor TSBMLLayoutGeneralGlyph.create(cpy: TSBMLLayoutGeneralGlyph) overload;
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.notes := cpy.getNotes;
  end;

  function TSBMLLayoutGeneralGlyph.printStr: string;
  begin
    Result := ', Layout General Glyph ID: ' + inherited printstr;
    Result := Result + ' General glyph Notes: ' + self.getNotes;
  end;

  procedure TSBMLLayoutGeneralGlyph.setNotes(newN: string);
  begin
    self.notes := newN;
  end;
  function TSBMLLayoutGeneralGlyph.getNotes(): string;
  begin
    Result := self.getNotes;
  end;

  constructor TSBMLLayoutCompartmentGlyph.create(newId: string) overload;
  begin
    inherited create('compGlyph' + newId);
    self.order := 0;
  end;
  constructor TSBMLLayoutCompartmentGlyph.create(cpy: TSBMLLayoutCompartmentGlyph) overload;
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.order := cpy.getOrder;
    self.compId := cpy.getCompId;
  end;

  function TSBMLLayoutCompartmentGlyph.printStr: string;
  begin
    Result := ', Layout Comp Glyph: ' + inherited printstr;
    Result := Result + ', Comp Glyph compartment Id: ' + self.getCompId;
    Result := Result + ', Comp Glyph order: ' + floattostr(self.getOrder);
  end;

  procedure TSBMLLayoutCompartmentGlyph.setOrder(newOrder: double);
  begin
    self.order := newOrder;
  end;
  function TSBMLLayoutCompartmentGlyph.getOrder(): double;
  begin
    Result := self.order;
  end;

  procedure TSBMLLayoutCompartmentGlyph.setCompId(newId: string);
  begin
    self.compId := newId;
  end;
  function TSBMLLayoutCompartmentGlyph.getCompId(): string;
  begin
    Result := self.compId;
  end;

  constructor TSBMLLayoutSpeciesGlyph.create(newSpId: string) overload;
  begin
    inherited create('speciesGlyph' + newSpId);
    self.speciesId := newSpId;
  end;

  constructor TSBMLLayoutSpeciesGlyph.create(cpy: TSBMLLayoutSpeciesGlyph) overload;
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.boundingBoxSet := cpy.boundingBoxIsSet;
    self.speciesId := cpy.getSpeciesId;

  end;

  function TSBMLLayoutSpeciesGlyph.printStr: string;
  begin
    Result := ' Layout species glyph: ' + inherited printStr;
    Result := Result + ', species Glyph: species id: ' + self.getSpeciesId;
  end;

  procedure TSBMLLayoutSpeciesGlyph.setSpeciesId( newSpId: string);
  begin
    self.speciesId := newSpId;
  end;
  function TSBMLLayoutSpeciesGlyph.getSpeciesId(): string;
  begin
    Result := self.speciesId;
  end;

  constructor TSBMLLayoutSpeciesReferenceGlyph.create() overload;
  begin
    inherited create('specRefGlyph');
    self.specGlyphId := '';
    self.specRefId := '';
    self.role := SPECIES_ROLE_UNDEFINED;
    self.curve := nil;
    self.curveFlag := false;
  end;

  constructor TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId: string);
  begin
    inherited create('specRefGlyph'+ newSpRefId);
    self.specGlyphId := '';
    self.specRefId := newSpRefId;
    self.role := SPECIES_ROLE_UNDEFINED;
    self.curve := nil;
    self.curveFlag := false;
  end;

  constructor TSBMLLayoutSpeciesReferenceGlyph.create(cpy: TSBMLLayoutSpeciesReferenceGlyph);
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.specGlyphId := cpy.getSpeciesGlyphId;
    self.specRefId := cpy.getSpeciesRefId;
    self.role := cpy.getRole;
    if cpy.isCurveSet then
    begin
      self.curve := TSBMLLayoutCurve.create(cpy.getCurve);
      self.curveFlag := true;
    end
    else self.curveFlag := false;
  end;

  function TSBMLLayoutSpeciesReferenceGlyph.printStr: string;
  begin
    Result := ', Layout Species Ref Glyph: ' + inherited printStr;
    Result := Result + ', Sp Glyph ID: ' + self.getSpeciesGlyphId;
    Result := Result + ', Sp Ref ID: ' + self.getSpeciesRefId;
    Result := Result + ', Role: ' + self.getStringRole;
    if self.isCurveSet then Result := Result + ' Sp Ref Glyph Curve: ' + self.getCurve.printStr
    else Result := Result + ' Sp Ref Glyph Curve: None';
  end;

  procedure TSBMLLayoutSpeciesReferenceGlyph.clear;
  begin
    self.id := 'specRefGlyph';
    self.specGlyphId := '';
    self.specRefId :='';
    self.role := SPECIES_ROLE_UNDEFINED; //'undefined';
    self.curve.Free;
    self.boundingBox.Free;
    self.curveFlag := false;
  end;

  function TSBMLLayoutSpeciesReferenceGlyph.getSpeciesGlyphId(): string;
  begin
    Result := self.specGlyphId;
  end;
  procedure TSBMLLayoutSpeciesReferenceGlyph.setSpeciesGlyphId(newId: string);
  begin
    self.specGlyphId := newId;
  end;
  function TSBMLLayoutSpeciesReferenceGlyph.getSpeciesRefId(): string;
  begin
    Result := self.specRefId;
  end;
  procedure TSBMLLayoutSpeciesReferenceGlyph.setSpeciesRefId(newId: string);
  begin
    self.specRefId := newId;
  end;
  function TSBMLLayoutSpeciesReferenceGlyph.getRole(): TSPECIES_REF_ROLE;
  begin
    Result := self.role;
  end;
  procedure TSBMLLayoutSpeciesReferenceGlyph.setRole(newRole: TSPECIES_REF_ROLE);
  begin
    self.role := newRole;
  end;
     // Not tested:
  function TSBMLLayoutSpeciesReferenceGlyph.setStrRole(newRole: string): boolean;
  var i: integer;
  begin
    Result := false;
    for i := 0 to Length(STRING_SPECIES_REF_ROLES) -1 do
    begin
      if LowerCase(newRole) = STRING_SPECIES_REF_ROLES[i] then
        begin
        self.role := TSPECIES_REF_ROLE(i);
        Result := true;
        end;
    end;
  end;

  function TSBMLLayoutSpeciesReferenceGlyph.getStringRole: string;
  begin
    Result := STRING_SPECIES_REF_ROLES[ord(self.role)];
  end;

  function TSBMLLayoutSpeciesReferenceGlyph.isCurveSet(): boolean;
  begin
    Result := self.curveFlag;
  end;
  procedure TSBMLLayoutSpeciesReferenceGlyph.setCurve(newCurve: TSBMLLayoutCurve);
  begin
    self.curve := TSBMLLayoutCurve.create(newCurve);
    self.curveFlag := true;
  end;
  function TSBMLLayoutSpeciesReferenceGlyph.getCurve(): TSBMLLayoutCurve;
  begin
    Result := self.curve;
  end;

  constructor TSBMLLayoutReactionGlyph.create(newRxnId: string) overload;
  begin
    inherited create( 'reactionGlyph' + newRxnId );
    self.rxnId := newRxnId ;
    self.curveFlag := false;
    self.speciesRefGlyphList := TList<TSBMLLayoutSpeciesReferenceGlyph>.create;
  end;

  constructor TSBMLLayoutReactionGlyph.create(cpy: TSBMLLayoutReactionGlyph) overload;
var
  i: Integer;
  begin
    if cpy.boundingBoxIsSet then
      begin
      self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
      self.boundingBoxSet := true;
      end;
    self.rxnId := cpy.getReactionId;
    if cpy.isCurveSet then
      begin
        self.curve := TSBMLLayoutCurve.create(cpy.getCurve);
        self.curveFlag := true;
      end
    else self.curveFlag := false;

    self.speciesRefGlyphList := TList<TSBMLLayoutSpeciesReferenceGlyph>.create;
    for i := 0 to cpy.getNumSpeciesRefGlyphs -1 do
      begin
        self.speciesRefGlyphList.Add(TSBMLLayoutSpeciesReferenceGlyph.create(cpy.getSpeciesRefGlyph(i)));
      end;
    self.id := cpy.getId;
    if cpy.getBoundingBox <> nil then
      self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
  end;

  function TSBMLLayoutReactionGlyph.printStr: string;
  var i: integer;
  begin
    Result := ', Layout Reaction Glyph: ' + inherited printStr;
    Result := Result + ', Reaction Glyph Rxn Id: ' + self.getReactionId;
    if self.isCurveSet then
      Result := Result + ', Reaction Glyph Curve: ' + self.getCurve.printStr
    else Result := Result + ', Reaction Glyph No Curve ';
    Result := Result + ', Reaction Glyph SpRefGlyphs: ';
    for i := 0 to self.getNumSpeciesRefGlyphs -1 do
      Result := Result + self.getSpeciesRefGlyph(i).printStr + sLineBreak;
  end;

  procedure TSBMLLayoutReactionGlyph.clear();
  var
  i: Integer;
  begin
    self.rxnId := '';
    if self.speciesRefGlyphList <> nil then
    begin
      for i := 0 to self.speciesRefGlyphList.Count -1 do
      begin
        self.deleteSpeciesRefGlyph(i);
      end;
    end;
    self.speciesRefGlyphList.Free;
    self.speciesRefGlyphList := TList<TSBMLLayoutSpeciesReferenceGlyph>.create;
    if self.curve <> nil then
      self.curve.clear;
    //inherited:
    self.setId('');
    self.boundingBox.free;
    self.boundingBoxSet := false;
  end;

  procedure TSBMLLayoutReactionGlyph.setReactionId(newId: string);
  begin
    self.rxnId := newId;
  end;
  function TSBMLLayoutReactionGlyph.getReactionId(): string;
  begin
    Result := self.rxnId;
  end;
  function TSBMLLayoutReactionGlyph.getNumSpeciesRefGlyphs(): integer;
  begin
    Result := self.speciesRefGlyphList.Count;
  end;
  function TSBMLLayoutReactionGlyph.getSpeciesRefGlyph(index: integer): TSBMLLayoutSpeciesReferenceGlyph;
  begin
    Result := self.speciesRefGlyphList[index];
  end;
  procedure TSBMLLayoutReactionGlyph.addSpeciesRefGlyph(newGlyph: TSBMLLayoutSpeciesReferenceGlyph);
  begin
    if self.speciesRefGlyphList = nil then
      self.speciesRefGlyphList := TList<TSBMLLayoutSpeciesReferenceGlyph>.create;
    self.speciesRefGlyphList.Add(TSBMLLayoutSpeciesReferenceGlyph.create(newGlyph));
  end;

  function TSBMLLayoutReactionGlyph.deleteSpeciesRefGlyph(index: integer): boolean;
  var success: boolean;
  begin
    success := false;
    try
      self.speciesRefGlyphList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;

  function TSBMLLayoutReactionGlyph.isCurveSet(): boolean;
  begin
    Result := self.curveFlag;
  end;
  procedure TSBMLLayoutReactionGlyph.setCurve(newCurve: TSBMLLayoutCurve);
  begin
    self.curve := TSBMLLayoutCurve.create(newCurve);
    self.curveFlag := true;
  end;
  function TSBMLLayoutReactionGlyph.getCurve(): TSBMLLayoutCurve;
  begin
    Result := self.curve;
  end;

  function TSBMLLayoutReactionGlyph.getReactionCenterPoint(): TSBMLLayoutPoint;
  var i: integer;
    rxnCurve: TSBMLLayoutCurve;
    centerPt: TSBMLLayoutPoint;
  begin
  centerPt := TSBMLLayoutPoint.create;
  if self.isCurveSet() then
    begin
    rxnCurve := self.getCurve();
    if rxnCurve.getNumCurveSegments = 1 then
      begin
      centerPt.setX((rxnCurve.getLineSegment(0).getStartPt.getX +
                                   rxnCurve.getLineSegment(0).getEndPt.getX)/2);
      centerPt.setY((rxnCurve.getLineSegment(0).getStartPt.getY +
                                   rxnCurve.getLineSegment(0).getEndPt.getY)/2);
      end

    else //assume 2 bezier curves, endpoint of first is arcCenter:
      begin // Typically bezier curves are not use to define center area of reaction.
      if rxnCurve.getNumCubicBeziers > 1 then
        begin //assume 2 bezier curves and endpoint of first is arcCenter:
        centerPt.setX( rxnCurve.getCubicBezier(0).getEnd.getX );
        centerPt.setY( rxnCurve.getCubicBezier(0).getEnd.getY );
        end
      else  // Set arcCenter to midpoint of start and endpoints:
        begin
        if rxnCurve.getNumCubicBeziers = 1 then
          begin
          centerPt.setX( (rxnCurve.getCubicBezier(0).getStart.getX +
                            rxnCurve.getCubicBezier(0).getEnd.getX)/2 );
          centerPt.setY( (rxnCurve.getCubicBezier(0).getStart.getY +
                            rxnCurve.getCubicBezier(0).getEnd.getY)/2 );
          end;
        end;
      end;
    end
  else
    begin
    if self.boundingBoxIsSet then
      begin  // get center of reaction BBox:
        centerPt.setX( self.getBoundingBox.getPoint.getX +
                       (self.getBoundingBox.getDims.getWidth/2) );
        centerPt.setY( self.getBoundingBox.getPoint.getY +
                       (self.getBoundingBox.getDims.getHeight/2) );
      end;
    end;
  Result := centerPt;
end;



  constructor TSBMLLayoutTextGlyph.create() overload;
  begin
    inherited create('textGlyph');
    self.text := '';  // has precedence over originOfText.
    self.originOfText := '';
    self.graphicalObjId := '';
  end;

  constructor TSBMLLayoutTextGlyph.create(cpy: TSBMLLayoutTextGlyph) overload;
  begin
    self.id := cpy.getId;
    self.text := cpy.getText;
    self.originOfText := cpy.getOriginOfText;
    self.graphicalObjId := cpy.getGraphicalObjId;

    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
  end;

  function TSBMLLayoutTextGlyph.printStr: string;
  begin
    Result := ', Layout TextGlyph: ' + inherited printStr;
    Result := Result + ', TextGlyph text: ' + self.getText;
    Result := Result + ', TextGlyph text origin: ' + self.getOriginOfText;
    Result := Result + ', TextGlyph GraphObj ID: ' + self.getGraphicalObjId;
  end;

  procedure TSBMLLayoutTextGlyph.clear();
  begin
    self.text := '';
    self.originOfText := '';
    self.graphicalObjId := '';
    //inherited:
    self.setId('');
    self.boundingBox.clear;
    self.boundingBoxSet := false;
  end;
  procedure TSBMLLayoutTextGlyph.setText(newText: string);
  begin
    self.text := newText;
  end;
  function TSBMLLayoutTextGlyph.getText(): string;
  begin
    Result := self.text;
  end;
  procedure TSBMLLayoutTextGlyph.setOriginOfText(newText: string);
  begin
    self.originOfText := newText;
  end;
  function TSBMLLayoutTextGlyph.getOriginOfText: string;
  begin
    Result := self.originOfText;
  end;
  procedure TSBMLLayoutTextGlyph.setGraphicalObjId(newId: string);
  begin
    self.graphicalObjId := newId;
  end;
  function TSBMLLayoutTextGlyph.getGraphicalObjId(): string;
  begin
    Result := self.graphicalObjId;
  end;

  constructor TSBMLLayout.create() overload;
  begin
    self.additionalGraphObjList := TList<TSBMLLayoutGraphicalObject>.create;
    self.additionalGenGlyphList := TList<TSBMLLayoutGeneralGlyph>.create;
    self.compartmentGlyphList := TList<TSBMLLayoutCompartmentGlyph>.create;
    self.speciesGlyphList := TList<TSBMLLayoutSpeciesGlyph>.create;
    self.reactionGlyphList := TList<TSBMLLayoutReactionGlyph>.create;
    self.textGlyphList := TList<TSBMLLayoutTextGlyph>.create;
    self.id := '';
  end;

  constructor TSBMLLayout.create(cpy: TSBMLLayout) overload;
  var i: integer;
  begin
    self.additionalGraphObjList := TList<TSBMLLayoutGraphicalObject>.create;
    for i := 0 to cpy.getNumAddionalGraphObjs -1 do
      begin
        self.additionalGraphObjList.Add(TSBMLLayoutGraphicalObject.create(cpy.getAdditionalGraphObj(i)));
      end;
    self.additionalGenGlyphList := TList<TSBMLLayoutGeneralGlyph>.create;
    for i := 0 to cpy.getNumGenGlyphs -1 do
      begin
        self.additionalGenGlyphList.Add(TSBMLLayoutGeneralGlyph.create(cpy.getGenGlyph(i)));
      end;
    self.compartmentGlyphList := TList<TSBMLLayoutCompartmentGlyph>.create;
    for i := 0 to cpy.getNumCompGlyphs -1 do
      begin
        self.compartmentGlyphList.Add(TSBMLLayoutCompartmentGlyph.create(cpy.getCompGlyph(i)));
      end;
    self.speciesGlyphList := TList<TSBMLLayoutSpeciesGlyph>.create;
    for i := 0 to cpy.getNumSpGlyphs - 1 do
      begin
        self.speciesGlyphList.Add(TSBMLLayoutSpeciesGlyph.create(cpy.getSpGlyph(i)));
      end;
    self.reactionGlyphList := TList<TSBMLLayoutReactionGlyph>.create;
    for i := 0 to cpy.getNumRxnGlyphs - 1 do
      begin
        self.reactionGlyphList.Add(TSBMLLayoutReactionGlyph.create(cpy.getRxnGlyph(i)));
      end;
    self.textGlyphList := TList<TSBMLLayoutTextGlyph>.create;
    for i := 0 to cpy.getNumTextGlyphs - 1 do
      begin
        self.textGlyphList.Add(TSBMLLayoutTextGlyph.create(cpy.getTextGlyph(i)));
      end;
    self.id := cpy.getId;
    self.dims := TSBMLLayoutDims.create(cpy.getDims);

  end;

  function TSBMLLayout.printStr: string;
  var i: integer;
  begin
    Result := ' Layout ID: ' + self.getId + ', Layout dims: ' + self.getDims.printStr + sLineBreak;
    Result := Result + ' Additional Graphical Glyphs: ';
    for i := 0 to self.getNumAddionalGraphObjs -1 do
      Result := Result + self.getAdditionalGraphObj(i).printStr;
    Result := Result + sLineBreak + ' Additional General Glyphs: ';
    for i := 0 to self.getNumGenGlyphs -1 do
      Result := Result + self.getGenGlyph(i).printStr;
    Result := Result + sLineBreak + ' Compartment Glyphs: ';
    for i := 0 to self.getNumCompGlyphs -1 do
      Result := Result + self.getCompGlyph(i).printStr;
    Result := Result + sLineBreak + ' Species Glyphs: ';
    for i := 0 to self.getNumSpGlyphs -1 do
      Result := Result + self.getSpGlyph(i).printStr + sLineBreak;
    Result := Result + ' Reaction and Sp Ref Glyphs: ';
    for i := 0 to self.getNumRxnGlyphs -1 do
      Result := Result + self.getRxnGlyph(i).printStr;
    Result := Result + sLineBreak + ' Text Glyphs: ';
    for i := 0 to self.getNumTextGlyphs -1 do
      Result := Result + self.getTextGlyph(i).printStr;
    Result := Result + sLineBreak;

  end;


  procedure TSBMLLayout.setId(newId: string);
  begin
    self.id := newId;
  end;
  function TSBMLLayout.getId(): string;
  begin
    Result := self.id;
  end;

  procedure TSBMLLayout.setDims(newDims: TSBMLLayoutDims);
  begin
    self.dims := TSBMLLayoutDims.create(newDims);
  end;

  {
  procedure TSBMLLayout.setDims(w: double; h: double) overload;
  begin
    //self.dims := TSBMLLayoutDims.create(w, h);
    self.dims := TSBMLLayoutDims.create();
    self.dims.setWidth(w);
    self.dims.setHeight(h);
    self.dims.setDepth(0);
  end;        }

  function TSBMLLayout.getDims(): TSBMLLayoutDims;
  begin
    Result := self.dims;
  end;
  procedure TSBMLLayout.addGraphObj(newGO: TSBMLLayoutGraphicalObject);
  begin
    if self.additionalGraphObjList = nil then
      self.additionalGraphObjList := TList<TSBMLLayoutGraphicalObject>.create;
    self.additionalGraphObjList.Add(newGO);
  end;
  function TSBMLLayout.getNumAddionalGraphObjs(): integer;
  begin
    Result := self.additionalGraphObjList.Count;
  end;
  function TSBMLLayout.getAdditionalGraphObj(index: Integer): TSBMLLayoutGraphicalObject;
  begin
    Result := self.additionalGraphObjList[index];
  end;
  function TSBMLLayout.deleteAdditionalGraphObj(index: integer): boolean;
  var success: boolean;
  begin
   success := false;
    try
      self.additionalGraphObjList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;

  procedure TSBMLLayout.addGenGlyph(newGenG: TSBMLLayoutGeneralGlyph);
  begin
    if self.additionalGenGlyphList = nil then
      self.additionalGenGlyphList := TList<TSBMLLayoutGeneralGlyph>.create;
    self.additionalGenGlyphList.Add(TSBMLLayoutGeneralGlyph.create(newGenG));
  end;
  function TSBMLLayout.getNumGenGlyphs(): integer;
  begin
    Result := self.additionalGenGlyphList.Count;
  end;
  function TSBMLLayout.getGenGlyph(index: Integer): TSBMLLayoutGeneralGlyph;
  begin
    Result := self.additionalGenGlyphList[index];
  end;

  function TSBMLLayout.deleteGenGlyph(index: integer): boolean;
  var success: boolean;
  begin
    success := false;
    try
      self.additionalGenGlyphList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;

  procedure TSBMLLayout.addSpGlyph(newSpG: TSBMLLayoutSpeciesGlyph);
  begin
   // self.speciesGlyphList.Add(newSpG);
     if self.speciesGlyphList = nil then
       self.speciesGlyphList := TList<TSBMLLayoutSpeciesGlyph>.create;
     self.speciesGlyphList.Add(TSBMLLayoutSpeciesGlyph.create(newSpG));
  end;

  function TSBMLLayout.getNumSpGlyphs(): integer;
  begin
    Result := self.speciesGlyphList.count;
  end;
  function TSBMLLayout.getSpGlyph(index: Integer): TSBMLLayoutSpeciesGlyph;
  begin
    Result := self.speciesGlyphList[index];
  end;

  function  TSBMLLayout.getSpGlyphList(): TList<TSBMLLayoutSpeciesGlyph>;
  begin
    Result := self.speciesGlyphList;
  end;

  function TSBMLLayout.deleteSpGlyph(index: integer): boolean;
  var success: boolean;
  begin
    success := false;
    try
      self.speciesGlyphList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;

  procedure TSBMLLayout.addCompGlyph(newCompG: TSBMLLayoutCompartmentGlyph);
  begin
    self.compartmentGlyphList.Add(TSBMLLayoutCompartmentGlyph.create(newCompG));
  end;
  function TSBMLLayout.getNumCompGlyphs(): integer;
  begin
    Result := self.compartmentGlyphList.Count;
  end;
  function TSBMLLayout.getCompGlyph(index: integer): TSBMLLayoutCompartmentGlyph;
  begin
    Result := self.compartmentGlyphList[index];
  end;

  function TSBMLLayout.deleteCompGlyph(index: integer): boolean;
  var success: boolean;
  begin
    success := false;
    try
      self.compartmentGlyphList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;

  procedure TSBMLLayout.addRxnGlyph(newRxnG: TSBMLLayoutReactionGlyph);
  begin
    self.reactionGlyphList.Add(TSBMLLayoutReactionGlyph.create(newRxnG));
  end;

  function TSBMLLayout.getNumRxnGlyphs(): integer;
  begin
    Result := self.reactionGlyphList.Count;
  end;

  function TSBMLLayout.getRxnGlyph(index: Integer): TSBMLLayoutReactionGlyph;
  begin
    Result := self.reactionGlyphList[index];
  end;

  function TSBMLLayout.deleteRxnGlyph(index: integer): boolean;
  var success: boolean;
  begin
    success := false;
    try
      self.reactionGlyphList.Delete(index);
      success := true;
    except
    on E: Exception do
      //notifyUser(E.message);
      success := false;
    end;
    Result := success;
  end;

  procedure TSBMLLayout.addTextGlyph(newTextG: TSBMLLayoutTextGlyph);
  begin
   // self.textGlyphList.Add(newTextG);

    if self.textGlyphList = nil then
      self.textGlyphList := TList<TSBMLLayoutTextGlyph>.create;
    self.textGlyphList.Add(TSBMLLayoutTextGlyph.create(newTextG));


  end;
  function TSBMLLayout.getNumTextGlyphs(): integer;
  begin
    result := self.textGlyphList.Count;
  end;

  function TSBMLLayout.getTextGlyph(index: Integer): TSBMLLayoutTextGlyph;
  begin
    Result := self.textGlyphList[index];
  end;

  function TSBMLLayout.deleteTextGlyph(index: integer): boolean;
  var success: boolean;
  begin
      success := false;
    try
      self.textGlyphList.Delete(index);
      success := true;
    except
    on E: Exception do
      success := false;
    end;
    Result := success;
  end;


  function getSpeciesIdFromSpGlyphId(spGlyphId: string; spGlyphList: TList<TSBMLLayoutSpeciesGlyph>): string;
  var i: integer;
  begin
  Result := '';
  for i := 0 to spGlyphList.Count -1 do
    begin
    if spGlyphList[i].getId = spGlyphId then
      Result := spGlyphList[i].getSpeciesId;
    end;
  end;

end.
