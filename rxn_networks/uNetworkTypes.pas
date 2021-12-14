unit uNetworkTypes;

interface

Uses System.Types, Classes, SysUtils, Web, WEBLib.Graphics;

const
   MaxControlPoints = 4;

type
  TReactionType = (eUniUni, eUniBi, eAnyToAny);

  TPointF = record
      x, y : double;
      constructor create (x, y : double);
  end;
  TPointDynArray = array of TPointF;

  TRectangularArray = array[0..MaxControlPoints-1] of TCanvasRectF;

  TReactionLineType = (ltLine, ltSegmentedLine, ltBezier);
  TArcDirection = (adInArc, adOutArc);
  TBezierCurve = record h1, h2 : TPointF; Merged : boolean; arcDirection : TArcDirection; end;
  TLineSegment = record p, q : TPointF; end;
  TBoundingBoxSegments = array[1..4] of TLineSegment;   // A Square
  TCurrentSelectedBezierHandle = -1..1;  // -1 = no handle, 0 (h1), 1 (h2)

  function getReactionTypeString (atype : TReactionType) : string;
  function getReactionType (atype : string) : TReactionType;

implementation


constructor TPointF.create (x, y : double);
begin
  self.x := x; self.y := y;
end;


function getReactionTypeString (atype : TReactionType) : string;
begin
  case atype of
    eUniUni : result := 'eUniUni';
    eUniBi  : result := 'eUniBi';
    eAnyToAny : result := 'eAnyToAny';
  end;
end;

function getReactionType (atype : string) : TReactionType;
begin
  if atype = 'eUniUni' then
     result := eUniUni
  else
  if atype = 'eUniBi' then
     result := eUniBi
  else
  if atype = 'eAnyToAny' then
     result := eAnyToAny
  else
     raise Exception.Create ('Unable to recognized raction type');
end;

end.
