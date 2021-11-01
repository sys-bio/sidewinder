unit uSBMLClasses.Layout;

interface
   uses System.SysUtils, System.Classes, System.Generics.Collections, Web, JS;
// NOTE: javascript calling overloaded functions choses the first one, period.
//      does not care about type or number of parameters. Must have pascal code
//      calling pascal code for overloads to work. This is then translated to JavaScript.

//  Web quote: There is no real function overloading in JavaScript since it allows you to pass
//   any number of parameters of any type. You have to check inside the function
//   for how many arguments have been passed in and what type they are.
type
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
   procedure setPoint(newPt: TSBMLLayoutPoint) overload;
   procedure setPoint(x: double; y: double) overload;
   function getPoint(): TSBMLLayoutPoint;
   procedure setDims( newDims: TSBMLLayoutDims) overload;
   procedure setDims(newHt: double; newWidth: double) overload;
   function getDims(): TSBMLLayoutDims;
   procedure setId( newId: string);
   function getId(): string;
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
 end;

 TSBMLLayoutGeneralGlyph = class(TSBMLLayoutGraphicalObject)
 private
   notes: string;
 public
   constructor create(newId: string) overload;
   constructor create(cpy: TSBMLLayoutGeneralGlyph) overload;
   procedure setNotes(newN: string);
   function getNotes(): string;

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
 end;

 TSBMLLayoutSpeciesGlyph = class(TSBMLLayoutGraphicalObject)
 private
   speciesId: string;  // associated species ID
 public
   constructor create(newSpId: string) overload;
   constructor create(cpy: TSBMLLayoutSpeciesGlyph) overload;
   procedure setSpeciesId( newSpId: string);
   function getSpeciesId(): string;

 end;

 TSBMLLayoutSpeciesReferenceGlyph = class(TSBMLLayoutGraphicalObject)
 private
   specGlyphId: string;  // id of speciesGlyph associated with this obj
   specRefId: string;  // id of species reference used in reaction
   role: string;       // optional, default is 'undefined'
   curve: TSBMLLayoutCurve;  // optionally use use in place of bounding box
   curveFlag: boolean;
 public
   constructor create() overload;
   constructor create(newSpRefId: string) overload;
   constructor create(cpy: TSBMLLayoutSpeciesReferenceGlyph) overload;
   function getSpeciesGlyphId(): string;
   procedure setSpeciesGlyphId(newId: string);
   function getSpeciesRefId(): string;
   procedure setSpeciesRefId(newId: string);
   function getRole(): string; // Currently does not check for allowed values.
   // Allowed values are “substrate”, “product”, “sidesubstrate”, “sideproduct”, “modifier”,
   // “activator”, “inhibitor” and “undefined”.
   procedure setRole(newRole: string);
   procedure setCurve(newCurve: TSBMLLayoutCurve);
   function getCurve(): TSBMLLayoutCurve;
   function isCurveSet(): boolean;

 end;

 TSBMLLayoutReactionGlyph = class(TSBMLLayoutGraphicalObject)
 private
   rxnId: string;  // id of reaction
   speciesRefGlyphList: TList<TSBMLLayoutSpeciesReferenceGlyph>;
   curve: TSBMLLayoutCurve;
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
   procedure setCurve(newCurve: TSBMLLayoutCurve);
   function getCurve(): TSBMLLayoutCurve;
   function isCurveSet(): boolean;
 end;

 // spec 3.12
 TSBMLLayoutTextGlyph = class(TSBMLLayoutGraphicalObject)
 private
   text: string;
   originOfText: string;// id of an entity in the SBML model. Use this instead of text.
   //graphicalObjId: string; // corresponds to another graphical object id.
 public
   constructor create() overload;
   constructor create(cpy: TSBMLLayoutTextGlyph) overload;
   procedure clear();
   procedure setText(newText: string);
   function getText(): string;
   procedure setOriginOfText(newText: string);
   function getOriginOfText: string;
  // procedure setGraphicalObjId(newId: string);
  // function getGraphicalObjId(): string;

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
   function getDims(): TSBMLLayoutDims;
   procedure addGraphObj(newGO: TSBMLLayoutGraphicalObject);
   function getNumAddionalGraphObjs(): integer;
   function getAdditionalGraphObj(index:integer): TSBMLLayoutGraphicalObject;
   function deleteAdditionalGraphObj(index: integer): boolean;
   procedure addGenGlyph(newGenG: TSBMLLayoutGeneralGlyph);
   function getNumGenGlyphs(): integer;
   function getGenGlyph(index:integer): TSBMLLayoutGeneralGlyph;
   function deleteGenGlyph(index: integer): boolean;
   procedure addSpGlyph(newSpG: TSBMLLayoutSpeciesGlyph);
   function getNumSpGlyphs(): integer;
   function getSpGlyph(index: integer): TSBMLLayoutSpeciesGlyph;
   function deleteSpGlyph(index: integer): boolean;
   procedure addCompGlyph(newCompG: TSBMLLayoutCompartmentGlyph);
   function getNumCompGlyphs(): integer;
   function getCompGlyph(index: integer): TSBMLLayoutCompartmentGlyph;
   function deleteCompGlyph(index: integer): boolean;
   procedure addRxnGlyph(newRxnG: TSBMLLayoutReactionGlyph);
   function getNumRxnGlyphs(): integer;
   function getRxnGlyph(index: integer): TSBMLLayoutReactionGlyph;
   function deleteRxnGlyph(index: integer): boolean;
   procedure addTextGlyph(newTextG: TSBMLLayoutTextGlyph);
   function getNumTextGlyphs(): integer;
   function getTextGlyph(index: integer): TSBMLLayoutTextGlyph;
   function deleteTextGlyph(index: integer): boolean;

 end;
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
    self.id := cpy.getId;
    self.point := TSBMLLayoutPoint.create(cpy.getPoint);
    self.dims := TSBMLLayoutDims.create(cpy.getDims);
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
    inherited create(newId);
    self.notes := '';
  end;

  constructor TSBMLLayoutGeneralGlyph.create(cpy: TSBMLLayoutGeneralGlyph) overload;
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.notes := cpy.getNotes;
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
    inherited create(newId);
    self.order := 0;
  end;
  constructor TSBMLLayoutCompartmentGlyph.create(cpy: TSBMLLayoutCompartmentGlyph) overload;
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.order := cpy.getOrder;
    self.compId := cpy.getCompId;
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
    inherited create('speciesGlyph');
    self.speciesId := newSpId;
  end;

  constructor TSBMLLayoutSpeciesGlyph.create(cpy: TSBMLLayoutSpeciesGlyph) overload;
  begin
    self.id := cpy.getId;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
    self.boundingBoxSet := cpy.boundingBoxIsSet;
    self.speciesId := cpy.getSpeciesId;

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
    self.role := 'undefined';
    self.curve := nil;
    self.curveFlag := false;
  end;

  constructor TSBMLLayoutSpeciesReferenceGlyph.create(newSpRefId: string);
  begin
    inherited create('specRefGlyph');
    self.specGlyphId := '';
    self.specRefId := newSpRefId;
    self.role := 'undefined';
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
  function TSBMLLayoutSpeciesReferenceGlyph.getRole(): string;
  begin
    Result := self.role;
  end;
  procedure TSBMLLayoutSpeciesReferenceGlyph.setRole(newRole: string);
  begin
    self.role := newRole; //TODO: chk if valid role.
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
    inherited create('reactionGlyph');
    self.rxnId := rxnId;
    self.curveFlag := false;
    self.speciesRefGlyphList := TList<TSBMLLayoutSpeciesReferenceGlyph>.create;
  end;

  constructor TSBMLLayoutReactionGlyph.create(cpy: TSBMLLayoutReactionGlyph) overload;
var
  i: Integer;
  begin
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
    if self.curve <> nil then
      self.curve.clear;
    //inherited:
    self.setId('');
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

  constructor TSBMLLayoutTextGlyph.create() overload;
  begin
    inherited create('');
    self.text := '';  // has precedence over originOfText.
    self.originOfText := '';
  end;

  constructor TSBMLLayoutTextGlyph.create(cpy: TSBMLLayoutTextGlyph) overload;
  begin
    self.id := cpy.getId;
    self.text := cpy.getText;
    self.originOfText := cpy.getOriginOfText;
    self.boundingBox := TSBMLLayoutBoundingBox.create(cpy.getBoundingBox);
  end;

  procedure TSBMLLayoutTextGlyph.clear();
  begin
    self.text := '';
    self.originOfText := '';
    //inherited:
    self.setId('');
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
  {procedure TSBMLLayout.setDims(w: double; h: double) overload;
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


end.
