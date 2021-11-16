unit uSBMLClasses.Render;
// Classes based on SBML Level 3 Render package: v1.1, 2017
interface
 uses System.SysUtils, System.Classes, System.Generics.Collections, Web;

 const STYLE_TYPES: array [0..7] of string = ( 'COMPARTMENTGLYPH', 'SPECIESGLYPH',
   'REACTIONGLYPH', 'SPECIESREFERENCEGLYPH', 'TEXTGLYPH', 'GENERALGLYPH', 'GRAPHICALOBJECT',
   'ANY' );

 type
 TVTextAnchorTypes = ( V_ZERO, V_TOP, V_MIDDLE, V_BOTTOM, V_BASELINE );
 THTextAnchorTypes = ( H_ZERO, H_START, H_MIDDLE, H_END );
    // pas2js compiler currently does not support enum constant where you sent
    // initial val to something other than 0;
    // Would like this:
 //  TVTextAnchorTypes = ( V_TOP = 1, V_MIDDLE, V_BOTTOM, V_BASELINE );

 TSBMLRenderColorDefinition = class
  private
    id: string;
    value: string;  // hex value representing color., 'none' allowed
  public
    constructor create() overload;
    constructor create( cpy: TSBMLRenderColorDefinition ) overload;
    constructor create( newColorStr: string; newId: string ) overload;
    procedure setId( sNewId: string);
    function  getId(): string;
    procedure setValue( sNewValue: string );
    function  getValue(): string;
    function  containsValue( sCmpValue: string ): boolean;

 end;

 TSBMLRenderPrimitive1D = class
   private
     id: string;
     stroke: string; // specify the color of the stroke. Can either hold a hex color value
                     // or id of a predefined TSBMLRenderColorDefinition object
     strokeWidth: double;
   public
     constructor create() overload;
     constructor create( cpy: TSBMLRenderPrimitive1D ) overload;
     procedure setId( newId: string );
     function getId(): string;
     procedure setStroke( newS: string );
     function getStroke(): string;
     procedure setStrokeWidth( newW: double );
     function getStrokeWidth(): double;
     function isStrokeWidthSet(): boolean;

 end;

 TSBMLRenderRectangle = class( TSBMLRenderPrimitive1D )
   private
   x, y: double;// position within the bounding box of the enclosing Layout object.
   height, width: double; // specify the width and height of the rectangle, either in absolute
            // values or as a percentage of the width and height of the enclosing bounding box.
   rx, ry: double; // optional, specify the radius of the corner curvature.
   //z: double;    // depth within the bounding box of the enclosing Layout object. not yet.
   ratio: double;// optional , the biggest rectangle with the desired ratio of width to
   // height is to be drawn centered in the objects bounding box. Sqr: ratio of '1'.
   fill: string;// fill style can either be a hexadecimal color value or the id
                // of a TSBMLRenderColorDefinition object. 'none' is used for no fill.
   public
     constructor create() overload;
     constructor create( cpy: TSBMLRenderRectangle ) overload;
     procedure setX( newX: double );
     function getX(): double;
     procedure setY( newY: double );
     function getY(): double;
     procedure setHeight( newH: double );
     function getHeight(): double;
     procedure setWidth( newW: double );
     function getWidth(): double;
     procedure setRx( newRx: double );
     function getRx(): double;
     procedure setRy( newRy: double );
     function getRy(): double;
     procedure setRatio( newR: double );
     function getRatio(): double;
     procedure setFill( newFill: string );
     function getFill(): string;

 end;

 TSBMLRenderGroup = class
   private
     iStrokeWidth: integer;
     sStrokeColor: string;
     sFillColor: string;
     iFontSize: integer;
     sFontStyle: string;
     sVTextAnchor: TVTextAnchorTypes; // how text elements are to be vertically aligned within
                                      // their bounding box,
     sHTextAnchor: THTextAnchorTypes; // horizontally aligned in BBox,
     rRectangle: TSBMLRenderRectangle;
   public
     constructor create() overload;
     constructor create( cpy: TSBMLRenderGroup ) overload;
     procedure setStrokeWidth( iNewSW: integer );
     function  getStrokeWidth(): integer;
     function isStrokeWidthSet(): boolean;
     procedure setStrokeColor( sNewSW: string );
     function  getStrokeColor(): string;
     procedure setFontSize( newSize: integer );
     function getFontSize(): integer;
     function isFontSizeSet(): boolean;
     procedure setFontStyle( sNewFS: string );
     function getFontStyle(): string;
     procedure setFillColor( sNewFC: string );
     function getFillColor(): string;
     procedure setRectangle( newR: TSBMLRenderRectangle );
     function getRectangle(): TSBMLRenderRectangle;
     function isRectangleSet(): boolean;
     procedure setVTextAnchor( newAnchor: TVTextAnchorTypes );
     function getVTextAnchor(): TVTextAnchorTypes;
     procedure setHTextAnchor( newAnchor: THTextAnchorTypes );
     function getHTextAnchor(): THTextAnchorTypes;
 end;

 TSBMLRenderStyle = class
  private
    id: string;
    typeList: TList<string {STYLE_TYPES}>; // Optional, Which glyph types to apply this style
   // roleList: TList<string>; // not used, list of roles for which this style applies.
    goIdList: TList<string>;// Optional, List of layout graphical object ids for which this style applies.
    rg: TSBMLRenderGroup;
  public
    constructor create() overload;
    constructor create( cpy: TSBMLRenderStyle ) overload;
    procedure setId( newId: string);
    function  getId(): string;
    procedure setRenderGroup( newRG: TSBMLRenderGroup );
    function  getRenderGroup(): TSBMLRenderGroup;
    function addType( sNewType: string ): boolean;
    function  getType( index: integer ): string;
    function  getNumbTypes(): integer;
   // procedure addRole( newRole: string);
   // function  getRole( index: integer ): string;
   // function  getNumbRoles(): integer;
    procedure addGoId( sNewId: string );  // add id og GO to list.
    function  getGoId( index: integer ): string;
    function  getNumbGoIds(): integer;

 end;
 {
 TSBMLRenderGradientDefinition = class

 end;

 TSBMLRenderLineEnding = class

 end;
     }

 TSBMLRenderInformation = class
 private
   colorDefList: TList<TSBMLRenderColorDefinition>;
  // gradientDefList: TList<TSBMLRenderGradientDefinition>;
  // lineEndingList: TList<TSBMLRenderLineEnding>;
   styleList: TList<TSBMLRenderStyle>;

   id: string;
 public
   constructor create() overload;
   constructor create( cpy: TSBMLRenderInformation ) overload;
   function  getNumbColorDefs(): integer;
   procedure setId( sNewId: string );
   function  getId(): string;
   procedure addColorDef( newColor: TSBMLRenderColorDefinition );
   function  getColorDef( index: integer): TSBMLRenderColorDefinition;
 //  function  getNumbGradientDefs(): integer;
 //  procedure addGradientDef( newGradient: TSBMLRenderGradientDefinition );
 //  function  getGradientDef( index: integer ): TSBMLRenderGradientDefinition;
 //  function  getNumbLineEndings(): integer;
 //  procedure addLineEnding( newLineEnding: TSBMLRenderLineEnding );
 //  function  getLineEnding( index: integer ): TSBMLRenderLineEnding;
   function  getNumberStyles(): integer;
   procedure addStyle( newStyle: TSBMLRenderStyle );
   function  getStyle( index: integer ): TSBMLRenderStyle;


 end;

 {     DO not use these classes for now:
 TGlobalRenderInformation = class(TSBMLRenderInformation)
 private
   globalStyleList: TList<TSBMLRenderStyle>;

 public
   function  getNumberGlobalStyles(): integer;
   procedure addGlobalStyle( newStyle: TSBMLRenderStyle );
   function  getGlobalStyle( index: integer ): TSBMLRenderStyle;
 end;

 TLocalRenderInformation = class(TSBMLRenderInformation)
 private
   styleList: TList<TSBMLRenderStyle>;

 public
   function  getNumberStyles(): integer;
   procedure addStyle( newStyle: TSBMLRenderStyle );
   function  getStyle( index: integer ): TSBMLRenderStyle;
 end;
    }

implementation

  constructor TSBMLRenderColorDefinition.create() overload;
  begin
    self.id := '';
    self.value := '';
  end;
  constructor TSBMLRenderColorDefinition.create( cpy: TSBMLRenderColorDefinition ) overload;
  begin
    self.id := cpy.getId;
    self.value := cpy.getValue;
  end;

  constructor TSBMLRenderColorDefinition.create( newColorStr: string; newId: string ) overload;
  begin
     self.id := newId;
     self.value := '#' + newColorStr;
  end;

  procedure TSBMLRenderColorDefinition.setId( sNewId: string);
  begin
    self.id := sNewId;
  end;
  function  TSBMLRenderColorDefinition.getId(): string;
  begin
    Result := self.id;
  end;
  procedure TSBMLRenderColorDefinition.setValue( sNewValue: string );
  begin
    self.value := sNewValue; // hex string
  end;
  function  TSBMLRenderColorDefinition.getValue(): string;
  begin
    Result := self.value;
  end;
  function  TSBMLRenderColorDefinition.containsValue( sCmpValue: string ): boolean;
  begin
    if self.value = sCmpValue then Result := true
    else Result := false;
  end;

  constructor TSBMLRenderGroup.create() overload;
  begin
    self.iStrokeWidth := -1;   // not set
    self.sStrokeColor := '';
    self.sFillColor := '';
    self.iFontSize := -1;
    self.sFontStyle := 'normal';
    self.sVTextAnchor := V_MIDDLE;
    self.sHTextAnchor := H_MIDDLE;
    self.rRectangle := nil;
  end;

  constructor TSBMLRenderGroup.create( cpy: TSBMLRenderGroup ) overload;
  begin
    self.iStrokeWidth := cpy.getStrokeWidth;
    self.sStrokeColor := cpy.getStrokeColor;
    self.sFillColor := cpy.getFillColor;
    self.iFontSize := cpy.getFontSize;
    self.sFontStyle := cpy.getFontStyle;
    self.sVTextAnchor := cpy.getVTextAnchor;
    self.sHTextAnchor := cpy.getHTextAnchor;
    if cpy.isRectangleSet then
    begin
      self.rRectangle := TSBMLRenderRectangle.create( cpy.getRectangle );
    end;
  end;

  procedure TSBMLRenderGroup.setStrokeWidth( iNewSW: integer );
  begin
    if iNewSW > -1 then
      self.iStrokeWidth := iNewSW;
  end;
  function  TSBMLRenderGroup.getStrokeWidth(): integer;
  begin
    Result := self.iStrokeWidth;
  end;
  function TSBMLRenderGroup.isStrokeWidthSet(): boolean;
  begin
    if self.iStrokeWidth > -1 then Result := true
    else Result := false;

  end;
  procedure TSBMLRenderGroup.setStrokeColor( sNewSW: string );
  begin
    self.sStrokeColor := sNewSW;
  end;
  function  TSBMLRenderGroup.getStrokeColor(): string;
  begin
    Result := self.sStrokeColor;
  end;
  procedure TSBMLRenderGroup.setFontSize( newSize: integer );
  begin
    if newSize > -1  then self.iFontSize := newSize;
  end;
  function TSBMLRenderGroup.getFontSize(): integer;
  begin
    Result := self.iFontSize;
  end;
  function TSBMLRenderGroup.isFontSizeSet(): boolean;
  begin
    if self.iFontSize > -1 then Result := true
    else Result := false;
  end;
  procedure TSBMLRenderGroup.setFillColor( sNewFC: string );
  begin
    self.sFillColor := sNewFc;
  end;
  function TSBMLRenderGroup.getFillColor(): string;
  begin
    Result := self.sFillColor;
  end;
  procedure TSBMLRenderGroup.setFontStyle( sNewFS: string );
  begin
    // spec 3.10.4 text class:
    if (sNewFS = 'italic') or (sNewFs = 'normal') then self.sFontStyle := sNewFS;

  end;
  function TSBMLRenderGroup.getFontStyle(): string;
  begin
    Result := self.sFontStyle;
  end;

  procedure TSBMLRenderGroup.setVTextAnchor( newAnchor: TVTextAnchorTypes );
  begin
    self.sVTextAnchor := newAnchor;
  end;
  function TSBMLRenderGroup.getVTextAnchor(): TVTextAnchorTypes;
  begin
    Result := self.sVTextAnchor;
  end;
  procedure TSBMLRenderGroup.setHTextAnchor( newAnchor: THTextAnchorTypes );
  begin
    self.sHTextAnchor := newAnchor;
  end;
  function TSBMLRenderGroup.getHTextAnchor(): THTextAnchorTypes;
  begin
    Result := self.sHTextAnchor;
  end;

  procedure TSBMLRenderGroup.setRectangle( newR: TSBMLRenderRectangle );
  begin
    self.rRectangle := TSBMLRenderRectangle.create( newR );
  end;
  function TSBMLRenderGroup.getRectangle(): TSBMLRenderRectangle;
  begin
    Result := self.rRectangle;
  end;
  function TSBMLRenderGroup.isRectangleSet(): boolean;
  begin
    if self.rRectangle = nil then Result := false
    else Result := true;
  end;


  constructor TSBMLRenderStyle.create();
  begin
    self.id := '';
    self.typeList := TList<string>.create;
    self.goIdList := TList<string>.create;

  end;

  constructor TSBMLRenderStyle.create( cpy: TSBMLRenderStyle ) overload;
  var i: integer;
  begin
    self.id := cpy.getId;
    self.typeList := TList<string>.create;
    self.goIdList := TList<string>.create;
    for i := 0 to cpy.getNumbTypes -1 do
      begin
        self.typeList.Add( cpy.getType(i) );
      end;
    for i := 0 to cpy.getNumbGoIds -1 do
      begin
        self.goIdList.Add( cpy.getGoId(i) );
      end;
    self.rg := TSBMLRenderGroup.create(cpy.getRenderGroup);
  end;

  procedure TSBMLRenderStyle.setId( newId: string);
  begin
    self.id := newId;
  end;

  function  TSBMLRenderStyle.getId(): string;
  begin
    Result := self.id;
  end;

  procedure TSBMLRenderStyle.setRenderGroup( newRG: TSBMLRenderGroup );
  begin
    if self.rg <> nil then self.rg.Free;
    self.rg := TSBMLRenderGroup.create;
    if newRG.isStrokeWidthSet then self.rg.setStrokeWidth( newRG.getStrokeWidth );
    if newRG.getStrokeColor <> '' then self.rg.setStrokeColor( newRG.getStrokeColor );
    if newRG.isFontSizeSet then self.rg.setFontSize( newRG.getFontSize );
    if newRG.getFillColor <> '' then self.rg.setFillColor( newRG.getFillColor );
    if newRG.isRectangleSet then self.rg.rRectangle := TSBMLRenderRectangle.create( newRG.getRectangle );

  end;

  function  TSBMLRenderStyle.getRenderGroup(): TSBMLRenderGroup;
  begin
    Result := self.rg;
  end;

  function TSBMLRenderStyle.addType( sNewType: string ): boolean;
  var i: integer;
      success: boolean;
  begin
    success := false;
    for i := 0 to Length(STYLE_TYPES) -1 do
    begin
      if sNewType = STYLE_TYPES[i] then self.typeList.Add(sNewType);
      success := true;
    end;
    Result := success;
  end;

  function  TSBMLRenderStyle.getType( index: integer ): string;
  begin
    Result := self.typeList[index];
  end;
  function  TSBMLRenderStyle.getNumbTypes(): integer;
  begin
    Result := self.typeList.Count;
  end;
   // procedure addRole( newRole: string);
   // function  getRole( index: integer ): string;
   // function  getNumbRoles(): integer;
  procedure TSBMLRenderStyle.addGoId( sNewId: string );  // add id og GO to list.
  begin
    self.goIdList.Add( sNewId );
  end;
  function  TSBMLRenderStyle.getGoId( index: integer ): string;
  begin
    Result := self.goIdList[index];
  end;
  function  TSBMLRenderStyle.getNumbGoIds(): integer;
  begin
    Result := self.goIdList.Count;
  end;


  constructor TSBMLRenderInformation.create() overload;
  begin
    self.id := '';
    self.colorDefList := TList<TSBMLRenderColorDefinition>.create;
  //  self.gradientDefList := TList<TSBMLRenderGradientDefinition>.create;
  //  self.lineEndingList := TList<TSBMLRenderLineEnding>.create;
    self.styleList := TList<TSBMLRenderStyle>.create;
  end;

  constructor TSBMLRenderInformation.create( cpy: TSBMLRenderInformation ) overload;
  var i: integer;
  begin
    self.id := cpy.getId;
    self.colorDefList := TList<TSBMLRenderColorDefinition>.create;
    self.styleList := TList<TSBMLRenderStyle>.create;

    for i := 0 to cpy.getNumbColorDefs -1 do
      begin
        self.colorDefList.Add( TSBMLRenderColorDefinition.create(cpy.getColorDef(i)) );
      end;
    for i := 0 to cpy.getNumberStyles  -1 do
      begin
        self.styleList.Add(TSBMLRenderStyle.create(cpy.getStyle(i)) );
      end;

  end;

  function  TSBMLRenderInformation.getNumbColorDefs(): integer;
  begin
    Result := self.colorDefList.Count;
  end;
  procedure TSBMLRenderInformation.setId( sNewId: string );
  begin
    self.id := sNewId;
  end;
  function  TSBMLRenderInformation.getId(): string;
  begin
    Result := self.id;
  end;
  procedure TSBMLRenderInformation.addColorDef( newColor: TSBMLRenderColorDefinition );
  begin
    self.colorDefList.add( TSBMLRenderColorDefinition.create(newColor) );
  end;
  function  TSBMLRenderInformation.getColorDef( index: integer): TSBMLRenderColorDefinition;
  begin
    Result := self.colorDefList[index];
  end;

  procedure TSBMLRenderInformation.addStyle( newStyle: TSBMLRenderStyle );
  //var i: integer;
  //   nStyle: TSBMLRenderStyle;
  begin
    if newStyle <> nil then
    begin
      //nStyle.create();
     // nStyle.id := newStyle.getId;
      self.styleList.Add( newStyle );
    end;
  end;

  function TSBMLRenderInformation.getStyle(index: Integer): TSBMLRenderStyle;
  begin
    Result := self.styleList[index];
  end;

  function  TSBMLRenderInformation.getNumberStyles(): integer;
  begin
    Result := self.styleList.Count;
  end;

  constructor TSBMLRenderPrimitive1D.create() overload;
  begin
    self.id := '';
    self.stroke := '';
    self.strokeWidth := -1;
  end;

  constructor TSBMLRenderPrimitive1D.create( cpy: TSBMLRenderPrimitive1D ) overload;
  begin
    self.id := cpy.getId;
    self.stroke := cpy.getStroke;
    self.strokeWidth := cpy.strokeWidth;
  end;

  procedure TSBMLRenderPrimitive1D.setId( newId: string );
  begin
    self.id := newId;
  end;

  function TSBMLRenderPrimitive1D.getId(): string;
  begin
    Result := self.id;
  end;
  procedure TSBMLRenderPrimitive1D.setStroke( newS: string );
  begin
    self.stroke := newS;
  end;
  function TSBMLRenderPrimitive1D.getStroke(): string;
  begin
    Result := self.stroke;
  end;
  procedure TSBMLRenderPrimitive1D.setStrokeWidth( newW: double );
  begin
    self.strokeWidth := newW;
  end;
  function TSBMLRenderPrimitive1D.getStrokeWidth(): double;
  begin
    Result := self.strokeWidth;
  end;
  function TSBMLRenderPrimitive1D.isStrokeWidthSet(): boolean;
  begin
    if self.strokeWidth > -1 then Result := true
    else Result := false;
  end;

  constructor TSBMLRenderRectangle.create() overload;
  begin
    Inherited create();
    self.fill := '';
    self.x := 0;
    self.y := 0;
    self.height := 0;
    self.width := 0;
    self.rx := 0;
    self.ry := 0;
    self.ratio := 0;
  end;
  constructor TSBMLRenderRectangle.create( cpy: TSBMLRenderRectangle ) overload;
  begin
    Inherited create( cpy );
    self.fill := cpy.getFill;
    self.x := cpy.getX;
    self.y := cpy.getY;
    self.height := cpy.getHeight;
    self.width := cpy.getWidth;
    self.rx := cpy.getRx;
    self.ry := cpy.getRy;
    self.ratio := cpy.getRatio;

  end;
  procedure TSBMLRenderRectangle.setX( newX: double );
  begin
    self.x := newX;
  end;
  function TSBMLRenderRectangle.getX(): double;
  begin
    Result := self.x;
  end;
  procedure TSBMLRenderRectangle.setY( newY: double );
  begin
    self.y := newY;
  end;
  function TSBMLRenderRectangle.getY(): double;
  begin
    Result := self.y;
  end;
  procedure TSBMLRenderRectangle.setHeight( newH: double );
  begin
    if newH <0 then self.height := 0
    else self.height := newH;
  end;
  function TSBMLRenderRectangle.getHeight(): double;
  begin
    Result := self.height;
  end;
  procedure TSBMLRenderRectangle.setWidth( newW: double );
  begin
    if newW < 0 then self.width := 0
    else self.width := newW;
  end;
  function TSBMLRenderRectangle.getWidth(): double;
  begin
    Result := self.width;
  end;
  procedure TSBMLRenderRectangle.setRx( newRx: double );
  begin
    if newRx <0 then self.rx := 0
    else self.rx := newRx;
  end;
  function TSBMLRenderRectangle.getRx(): double;
  begin
    Result := self.rx;
  end;
  procedure TSBMLRenderRectangle.setRy( newRy: double );
  begin
    if newRy <0 then self.ry := 0
    else self.ry := newRy;
  end;
  function TSBMLRenderRectangle.getRy(): double;
  begin
    Result := self.ry;
  end;
  procedure TSBMLRenderRectangle.setRatio( newR: double );
  begin
    if newR <0 then self.ratio := 0
    else self.ratio := newR;
  end;
  function TSBMLRenderRectangle.getRatio(): double;
  begin
    Result := self.ratio;
  end;
  procedure TSBMLRenderRectangle.setFill(newFill: string);
  begin
    self.fill := newFill;
  end;
  function TSBMLRenderRectangle.getFill: string;
  begin
    Result := self.fill;
  end;

end.
