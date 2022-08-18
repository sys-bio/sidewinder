unit uScrollingTypes;

// System.Types, UITypes that are not supported in TMS Webcore

interface

uses JS, web;

type
 TPointF = record  // TMS only supports TPoint
    x, y : double;
    constructor create (x, y : double);
    function Add(b: TPointF): TPointF;
    function Subtract(b: TPointF): TPointF;
  end;

 TSizeF = record  // TMS does not support this.
    cx: Single;
    cy: Single;
  public
    constructor Create(P: TSizeF); overload;
    constructor Create(const X, Y: Single); overload;
    property Width: Single read cx write cx;
    property Height: Single read cy write cy;
 end;


 TRectF = record
  private
    FWidth: single;
    FHeight: single;
    fLocation: TPointF;  // upper left corner
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetSize: TSizeF;
   // procedure SetSize(const Value: TSizeF);
//    function  GetLocation: TPointF; // upper left corner
//    procedure SetLocation(const X, Y: Single); overload; // upper left corner
//    procedure SetLocation(const newPoint: TPointF); overload;

  public
    constructor Create(const Origin: TPointF); overload;   // empty rect at given origin (upper left corner)
    constructor Create(const Origin: TPointF; const Width, Height: Single); overload; // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Single); overload;             // at Left, Top, Right, and Bottom
//    constructor Create(const P1, P2: TPointF; Normalize: Boolean = False); overload;  // with corners specified by p1 and p2
//    constructor Create(const R: TRectF; Normalize: Boolean = False); overload;
//    constructor Create(const R: TRect; Normalize: Boolean = False); overload;
//    class operator Equal(const Lhs, Rhs: TRectF): Boolean;
//    class operator NotEqual(const Lhs, Rhs: TRectF): Boolean;
//    class operator Implicit(const Source: TRect): TRectF;
//    class operator Explicit(const Source: TRectF): TRect;
//    class operator Add(const Lhs, Rhs: TRectF): TRectF;
//    class operator Multiply(const Lhs, Rhs: TRectF): TRectF;
//    class function Empty: TRectF; inline; static;
    { This method is to be deprecated. It stretches current rectangle into designated area similarly to FitInto,
      but only when current rectangle is bigger than the area; otherwise, it only centers it. }
//    function Fit(const BoundsRect: TRectF): Single; // deprecated 'Please consider using FitInto instead.';
    { Stretches current rectangle into the designated area, preserving aspect ratio. Note that unlike Fit, when designated
      area is bigger than current rectangle, the last one will be stretched to fill designated area (while Fit would only
      center it). }
//    function FitInto(const ADesignatedArea: TRectF; out Ratio: Single): TRectF; overload;
//    function FitInto(const ADesignatedArea: TRectF): TRectF; overload;
 //   function CenterAt(const ADesignatedArea: TRectF): TRectF;
  //  function PlaceInto(const ADesignatedArea: TRectF; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
 //     const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;
 //   function SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean = True): TRectF;
  //  procedure NormalizeRect;
  //  function IsEmpty: Boolean;
    function  Contains(const Pt: TPointF): Boolean; overload;
  //  function Contains(const R: TRectF): Boolean; overload;
 //   function IntersectsWith(const R: TRectF): Boolean;
  //  class function Intersect(const R1: TRectF; const R2: TRectF): TRectF; overload; static;
  //  procedure Intersect(const R: TRectF); overload;
   // class function Union(const R1: TRectF; const R2: TRectF): TRectF; overload; static;
 //   procedure Union(const R: TRectF); overload;
 //   class function Union(const Points: Array of TPointF): TRectF; overload; static;
  //  procedure Offset(const DX, DY: Single); overload;
  //  procedure Offset(const Point: TPointF); overload;
//    function  GetLocation: TPointF; // upper left corner
//    procedure SetLocation(const X, Y: Single); overload; // upper left corner
//    procedure SetLocation(const Point: TPointF); overload;
  //  procedure Inflate(const DX, DY: Single); overload;
  //  procedure Inflate(const DL, DT, DR, DB: Single); overload;
  //  function CenterPoint: TPointF;
  //  function Ceiling: TRect;
  //  function Truncate: TRect;
  //  function Round: TRect;
  //  function EqualsTo(const R: TRectF; const Epsilon: Single = 0): Boolean;

    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
  //  property Size: TSizeF read GetSize write SetSize;
    property Size: TSizeF read GetSize ;
   // property Location: TPointF read GetLocation write SetLocation;  // upper left corner
    function  GetLocation: TPointF; // upper left corner
    procedure SetLocation(const X, Y: Single); overload; // upper left corner
    procedure SetLocation(const newPoint: TPointF); overload;
  {case Integer of
    0: (Left, Top, Right, Bottom: Single);
    1: (TopLeft, BottomRight: TPointF);
  end; }
 end;


implementation

constructor TPointF.create (x, y : double);
begin
  self.x := x; self.y := y;
end;

function TPointF.Add(b: TPointF): TPointF;
begin
  Result.x := self.x + b.x;
  Result.y := self.y + b.y;
end;
function TPointF.Subtract(b: TPointF): TPointF;
begin
  Result.x := self.x - b.x;
  Result.y := self.y - b.y;
end;
constructor TSizeF.Create(P: TSizeF) overload;
begin
  self.cx := P.cx;  // ??
  self.cy := P.cy;
end;

constructor TSizeF.Create(const X, Y: Single) overload;
begin
  self.cx := X;
  self.cy := Y;
end;

constructor TRectF.Create(const Origin: TPointF) overload;
begin
  self.SetLocation(Origin.x, Origin.y); // upper left corner

end;

constructor TRectF.Create(const Origin: TPointF; const Width, Height: Single) overload;
begin
  self.SetWidth( Width );
  self.SetHeight( Height );
  self.SetLocation( Origin );  // upper left corner

end;

constructor TRectF.Create(const Left, Top, Right, Bottom: Single) overload;
var newSize: TSizeF;
begin
  self.SetWidth( abs(Right - Left) );
  self.SetHeight( abs(Bottom - Top) );
  self.SetLocation( Left, Top );
  newSize.Width := self.GetWidth;
  newSize.Height := self.GetHeight;
  //self.SetSize(newSize);
end;

function TRectF.GetWidth: Single;
begin
  Result := self.FWidth;
end;
procedure TRectF.SetWidth(const Value: Single);
begin
 // console.log('TRectF.setWidth, current value: ', self.Width);
  self.FWidth := Value;
end;
function TRectF.GetHeight: Single;
begin
  Result := self.FHeight;
end;
procedure TRectF.SetHeight(const Value: Single);
begin
  self.FHeight := Value;
end;
function TRectF.GetSize: TSizeF;
var curSize: TSizeF;
begin
  curSize := TSizeF.Create(self.FWidth, self.FHeight);
  Result := curSize;
end;
{procedure TRectF.SetSize(const Value: TSizeF);
begin
  self.Size := Value;
end; }
function TRectF.GetLocation: TPointF; // Upper left corner
begin
  Result := self.fLocation;
end;

function TRectF.Contains(const Pt: TPointF): Boolean;
var fLeft, fRight, fTop, fBottom: single;
begin
  fLeft := self.GetLocation.x;
  fRight := self.GetLocation.x + self.GetWidth;
  fTop := self.GetLocation.y;
  fBottom := self.GetLocation.y + self.GetHeight;
  if (Pt.x >= fLeft) and (fRight >= Pt.x ) and (Pt.y >= fTop) and (fBottom >= Pt.y) then
    Result := true
  else Result := false;
end;

procedure TRectF.SetLocation(const X, Y: Single) overload;
begin

  self.fLocation := TPointF.create(X,Y);
end;

procedure TRectF.SetLocation(const newPoint: TPointF) overload;
begin
 // console.log('TRectF.SetLocation', newPoint.x, ', ', newPoint.y);
  self.fLocation := TPointF.create(newPoint.x, newPoint.y);
end;

end.
