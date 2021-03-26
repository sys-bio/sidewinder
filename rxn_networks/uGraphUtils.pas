unit uGraphUtils;

interface

Uses SysUtils, Graphics, Types, uNetworkTypes, uNetwork;

function Line (pt1, pt2 : TPointF) : TLineSegment;
function Angle (x, y : double) : Double;
function ScalePt (const p1 : TPointF; s : double) : TPointF;
function MinusPt (const p1, p2 : TPointF) : TPointF;
function  OriginAdjustToInt (r : TPointF; const Origin : TPointF) : TPoint;
function pointWithinCircle (x, y : double; pt : TPointF) : boolean;
function computeCentroid (reaction: TReaction): TPointF;
function segmentIntersects (v1, v2 : TLineSegment; var v : TPointF) : Boolean;
function computeLineIntersection (node : TNode; scalingFactor : double; var pt : TPointF; line : TLineSegment) : boolean;


implementation

Uses uNetwork;

const
  HANDLE_RADIUS = 5.0;

// Find angle between x and y
function Angle (x, y : double) : Double;
begin
  if abs (x) < 1e-5 then
     if abs (y) < 1e-5 then begin result := 0.0; exit; end
     else if (y > 0.0) then begin result := 3.1415*0.5; exit; end
          else begin result := 3.1415*1.5; exit; end
  else if (x < 0.0) then begin result := arctan (y/x) + 3.1415; exit; end
       else result := arctan (y/x);
end;


// Translate P1 by p2
function MinusPt (const p1, p2 : TPointF) : TPointF;
begin
  Result.x := p1.x - p2.x;
  Result.y := p1.y - p2.y;
end;


function ScalePt (const p1 : TPointF; s : double) : TPointF;
begin
  result.x := trunc (p1.x * s);
  result.y := trunc (p1.y * s);
end;


// Given two points, pt1 and pt2, Line returns the LineSegment
function Line (pt1, pt2 : TPointF) : TLineSegment;
begin
  Result.p.x := pt1.x;
  Result.p.y := pt1.y;

  Result.q.x := pt2.x;
  Result.q.y := pt2.y;
end;



// Check if pt is within a circle with centre x,y.
// Bit of a fudge, assumes a circle is a square!
// Circle is assumed to have a 'diameter' of three units
function pointWithinCircle (x, y : double; pt : TPointF) : boolean;
begin
  //if PtinRect (TRectF.Create (pt.x-HANDLE_RADIUS, pt.y-HANDLE_RADIUS, pt.x+HANDLE_RADIUS, pt.y+HANDLE_RADIUS), TPointF.Create (x, y)) then
  //   result := true
  //else
  //   result := false;
end;


// Compute the centroid for the Edge
function computeCentroid(reaction: TReaction): TPointF;
var
  nSrcNodes, nDestNodes: integer;
  TotalNodes, i: integer;
  pt, arcCentre: TPointF;
begin
  // # of nodes that go into the arc
  nSrcNodes := reaction.state.nReactants;
  nDestNodes := reaction.state.nProducts;  // # of nodes that come off the arc

  // Calculate the centroid from the nodes associated with the Edge
  arcCentre.x := 0.0; arcCentre.y := 0.0;

  for i := 0 to nSrcNodes - 1 do
      begin
      pt := reaction.state.srcPtr[i].getCenter;
      arcCentre.x := arcCentre.x + pt.x;
      arcCentre.y := arcCentre.y + pt.y;
      end;

  for i := 0 to nDestNodes - 1 do
      begin
     pt := reaction.state.destPtr[i].getCenter;
     ArcCentre.x := ArcCentre.x + pt.x;
     ArcCentre.y := ArcCentre.y + pt.y;
     end;

  TotalNodes := nSrcNodes + nDestNodes;
  result.x := arcCentre.x / TotalNodes;
  result.y := arcCentre.y / TotalNodes;
end;


// Check if the target line (v2) intersects the SEGMENT line (v1). Returns
// true if lines intersect with intersection coordinate returned in v, else
// returns false
function segmentIntersects (v1, v2 : TLineSegment; var v : TPointF) : Boolean;
var xlk, ylk, xnm, ynm, xmk, ymk, det : double;//longint;
    detinv, s, t : double;
begin
  xlk := v2.q.x - v2.p.x;
  ylk := v2.q.y - v2.p.y;
  xnm := v1.p.x - v1.q.x;
  ynm := v1.p.y - v1.q.y;
  xmk := v1.q.x - v2.p.x;
  ymk := v1.q.y - v2.p.y;

  det := xnm*ylk - ynm*xlk;
  if abs (det) < 1e-6 then
     result := false
  else
     begin
     detinv := 1.0/det;
     s := (xnm*ymk - ynm*xmk)*detinv;
     t := (xlk*ymk - ylk*xmk)*detinv;
     if (s < 0.0) or (s > 1.0) or (t < 0.0) or (t > 1.0) then
        result := false
     else
        begin
        v.x := v2.p.x + trunc (xlk*s);
        v.y := v2.p.y + trunc (ylk*s);
        result := true;
        end;
     end;
end;


function computeLineIntersection (node : TNode; scalingFactor : double; var pt : TPointF; line : TLineSegment) : boolean;
var i : integer; OuterSegs : TBoundingBoxSegments;

begin
  // Construct the outer rectangle for node #, src
  outerSegs := node.getNodeBoundingBox;
  for i := 1 to 4 do
      begin
      OuterSegs[i].p := scalePt (OuterSegs[i].p, scalingFactor);
      OuterSegs[i].q := scalePt (OuterSegs[i].q, scalingFactor);
      end;

  result := false;
  for i := 1 to 4 do
      if SegmentIntersects (OuterSegs[i], line, pt) then
         begin
         result := true;
         exit;
         end;
end;

function OriginAdjustToInt (r : TPointF; const Origin : TPointF) : TPoint;
begin
  result.x := trunc (r.x - Origin.x);
  result.y := trunc (r.y - Origin.y);
end;

end.


