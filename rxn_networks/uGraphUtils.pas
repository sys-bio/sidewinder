unit uGraphUtils;

interface

Uses SysUtils, Graphics, Types, uNetworkTypes, uNetwork, Web;


const
  MAX_BEZIER_SEGMENTS = 29;
  HANDLE_RADIUS = 5;

type
    TBezierPts = array[0..MAX_BEZIER_SEGMENTS] of TPointF;

function  line (pt1, pt2 : TPointF) : TLineSegment;
function  angle (x, y : double) : Double;
function  scalePt (const p1 : TPointF; s : double) : TPointF;
function  minusPt (const p1, p2 : TPointF) : TPointF;
function  originAdjustToInt (r : TPointF; const Origin : TPointF) : TPoint;
function  ptOnLine (p1, p2 : TPointF; px, py : double) : boolean;
function  ptInCircle (x, y : double; pt : TPointF) : boolean;
function  computeCentroid (reaction: TReaction): TPointF;
function  segmentIntersects (v1, v2 : TLineSegment; var v : TPointF) : Boolean;
function  computeLineIntersection (node : TNode; scalingFactor : double; var pt : TPointF; line : TLineSegment) : boolean;
function  computeBezierPoints (p : array of TPointF) : TPointF;
procedure computeBezierBlendingFuncs;
function  computeBezierLineIntersection(node: TNode; const scalingFactor : double; var pt: TPointF;   var t: double; var Segn: integer): Boolean;
procedure drawBezier (canvas : TCanvas);
function  ptOnBezier (p : array of TPointF; pt : TPointF; var t : double) : Boolean;
//function  overControlRectangle (x, y : double; const state : TNodeState; var selectedNodeGrabRectangle : integer) : boolean;
function  getControlRects (x, y, w, h : double) : TRectangularArray;
function  ptInRectF (rect : TCanvasRectF; pt : TPointF) : boolean;
function  getSlope(startPt, endPt: TPointF): double;

implementation

Uses Math;

 var
  BezierPoints    : TBezierPts;
  // blending function variables
  BezJ, BezJPrime : array[0..MAX_BEZIER_SEGMENTS, 0..4] of double;

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
function minusPt (const p1, p2 : TPointF) : TPointF;
begin
  Result.x := p1.x - p2.x;
  Result.y := p1.y - p2.y;
end;


function scalePt (const p1 : TPointF; s : double) : TPointF;
begin
  result.x := trunc (p1.x * s);
  result.y := trunc (p1.y * s);
end;


// Given two points, pt1 and pt2, Line returns the LineSegment
function line (pt1, pt2 : TPointF) : TLineSegment;
begin
  Result.p.x := pt1.x;
  Result.p.y := pt1.y;

  Result.q.x := pt2.x;
  Result.q.y := pt2.y;
end;


function dist (x1, y1, x2, y2 : double) : double;
begin
  result := sqrt (sqr(x2-x1) + sqr (y2-y1));
end;

function  getSlope(startPt, endPt: TPointF): double;
begin
  Result := (endPt.y - startPt.y)/(endPt.x - startPt.x);
end;

// www.jeffreythompson.org/collision-detection/line-point.php
function ptOnLine (p1, p2 : TPointF; px, py : double) : boolean;
var d1, d2 : double; lineLen, buffer : double;
begin
  d1 := dist(px, py, p1.x, p1.y);
  d2 := dist(px, py, p2.x, p2.y);
  lineLen := dist(p1.x, p1.y, p2.x, p2.y);
  buffer := 8;    // higher the number = less accurate
  if (d1 + d2 >= lineLen-buffer) and (d1 + d2 <= lineLen+buffer) then
    result := True
  else
    result := False;
end;


// Check if pt is within a circle with centre x,y.
// Bit of a fudge, assumes a circle is a square!
// Circle is assumed to have a 'diameter' of three units
function ptInCircle (x, y : double; pt : TPointF) : boolean;
begin
  if PtinRect (Rect (trunc (pt.x-HANDLE_RADIUS), trunc (pt.y-HANDLE_RADIUS), trunc (pt.x+HANDLE_RADIUS), trunc (pt.y+HANDLE_RADIUS)), Point (trunc (x), trunc (y))) then
     result := true
  else
     result := false;
end;


// Compute the centroid for the Edge
function computeCentroid(reaction: TReaction): TPointF;
var
  nSrcNodes, nDestNodes: integer;
  TotalNodes, i: integer;
  pt, arcCentre: TPointF;
begin
  console.log ('Compute centroid');
  // # of nodes that go into the arc
  nSrcNodes := reaction.state.nReactants;
  nDestNodes := reaction.state.nProducts;  // # of nodes that come off the arc

  // Calculate the centroid from the nodes associated with the Edge
  arcCentre.x := 0.0; arcCentre.y := 0.0;

  for i := 0 to nSrcNodes - 1 do
      begin
      pt := reaction.state.srcPtr[i].state.getCenter;
      arcCentre.x := arcCentre.x + pt.x;
      arcCentre.y := arcCentre.y + pt.y;
      end;

  for i := 0 to nDestNodes - 1 do
      begin
     pt := reaction.state.destPtr[i].state.getCenter;
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


function PtInRectF (rect : TCanvasRectF; pt : TPointF) : boolean;
begin
  result := (pt.X >= Rect.Left) and (pt.X < Rect.Right) and (pt.Y >= Rect.Top)
    and (pt.Y < Rect.Bottom);
end;


// Assumes that x, y, w and h have been prescaled
function getControlRects (x, y, w, h : double) : TRectangularArray;
var grabW,  grabH : double;
    grabW2, grabH2 : double;
begin
  grabW := 8;
  grabH := 8;
  grabW2 := grabW / 2;
  grabH2 := grabH / 2;

  result[0] := createCanvasRectF(x - grabW2, y - grabW2, x + grabW2, y + grabH2);
  result[1] := createCanvasRectF(x + w - grabW2, y - grabH2, x + w + grabW2, y + grabH2);
  result[2] := createCanvasRectF(x - grabW2, y + h - grabH2, x + grabW2, y + h + grabH2);
  result[3] := createCanvasRectF(x + w - grabW2, y + h - grabH2, x + w + grabW2, y + h + grabH2);
end;



// Compute factorial of n.  I know, there is a better way but this will do for
// now, in any case it only has to be computed once
function fact (n : integer) : integer;
var i : integer;
begin
  result := 1;
  if n = 0 then
     exit
  else
     for i := 1 to n do
         result := result * i;
end;


// Combination function
function Comb (n, i : integer) : integer;
begin
  result := fact (n) div (fact (i) * fact (n-i));
end;

// Compute the Bezier blending functions here, ready for when we need them
// Assume 2 control points, MAXSEGS is number of steps to draw bezier curve
procedure computeBezierBlendingFuncs;
var t, tm : double; ti, i : integer;
begin
  for ti := 0 to MAX_BEZIER_SEGMENTS do
      begin
      t := ti/MAX_BEZIER_SEGMENTS;
      for i := 0 to 3 do
          BezJ[ti,i] := Comb(3, i) * power (t, i) * power (1-t, 3-i);
      // At the moment hard-wired for n = 3
      tm := 1 - t;
      BezJPrime[ti, 0] := -3*tm*tm;
      BezJPrime[ti, 1] := 3*tm*tm - 6*t*tm;
      BezJPrime[ti, 2] := 6*t*tm - 3*t*t;
      BezJPrime[ti, 3] := 3*t*t;
      end;
end;

// Compute points along bezier curve and store in global var BezierPoints
// p holds the start, two control points and end point
function computeBezierPoints (p : array of TPointF) : TPointF;
type TLongPoint = record x, y : Int64 end;
var i, j, c : integer;
    lpoints : array[0..MAX_BEZIER_SEGMENTS] of TPointF;
    lp : array[0..3] of TPointF;
begin
  // Scale up dimensions to get a smooth curve
  for i := 0 to 3 do
      begin
      lp[i].x := p[i].x * 1000;
      lp[i].y := p[i].y * 1000;
      end;

  // Now compute the bezier points
  for i := 0 to MAX_BEZIER_SEGMENTS do
      begin
      lpoints[i].x := 0; lpoints[i].y := 0;
      for j := 0 to 3 do
          begin
          lpoints[i].x := lpoints[i].x + trunc (lp[j].x*BezJ[i, j]);
          lpoints[i].y := lpoints[i].y + trunc (lp[j].y*BezJ[i, j]);
          end;
      BezierPoints[i].x := lpoints[i].x / 1000;  // and scale back down again
      BezierPoints[i].y := lpoints[i].y / 1000;
      end;
  c := MAX_BEZIER_SEGMENTS div 2;
  result := BezierPoints[c];
end;


// Returns true if current bezier intersects line segment, intersection
// point returned in pt, parametric value in t, and the segment number in segn
function computeBezierLineIntersection(node: TNode; const scalingFactor : double; var pt: TPointF; var t: double; var Segn: integer): Boolean;
var
  i, j: integer;
  BezierSegments: TLineSegment;
  outerSegs: TBoundingBoxSegments;
begin
  // Construct the outer rectangle for node #, src
  outerSegs := node.getNodeBoundingBox ();
  for i := 1 to 4 do
    begin
    outerSegs[ i ].p := scalePt (outerSegs[ i ].p, scalingFactor);
    outerSegs[ i ].q := scalePt (outerSegs[ i ].q, scalingFactor);
    end;

  result := False;
  try
    for i := 1 to MAX_BEZIER_SEGMENTS do
      begin
      t := i / MAX_BEZIER_SEGMENTS;
      BezierSegments.p := BezierPoints[ i - 1 ];
      BezierSegments.q := BezierPoints[ i ];
      for j := 1 to 4 do
        if segmentIntersects (outerSegs[ j ], BezierSegments, pt) then
          begin
          result := True;
          Segn := i;
          exit;
          end;
      end;
  except
    on EIntOverflow do
      begin
      result := False;
      exit;
      end;
  end;
  result := False;
end;


procedure drawBezier (canvas : TCanvas);
var i : integer;
    pi : array[0..MAX_BEZIER_SEGMENTS] of TPoint;
begin
  for i := 0 to MAX_BEZIER_SEGMENTS do
      begin
      pi[i].x := trunc (BezierPoints[i].x);
      pi[i].y := trunc (BezierPoints[i].y);
      end;
  canvas.Polyline (pi);
end;



// Determine whether the point 'pt' is on the bezier curved defined by the
// control points 'p'. This brute force method works through each segment
// making up the bezier checking to see if pt in on one of the segments
// - seems to be quick enough. t is the parametric distance along the bezier }
function ptOnBezier (p : array of TPointF; pt : TPointF; var t : double) : Boolean;
type TLongPoint = record x, y : Int64; end;
var i, j : integer;
    points : array[0..MAX_BEZIER_SEGMENTS] of TPointF;
    lpoints : array[0..MAX_BEZIER_SEGMENTS] of TPointF;
    lp : array[0..3] of TPointF;
begin
  // Scale up dimensions to get smooth curve
  for i := 0 to 3 do
      begin
      lp[i].x := p[i].x * 1000;
      lp[i].y := p[i].y * 1000;
      end;

  // Compute the first point because later on I pass i-1 point to PtOnLine
  lpoints[0].x := 0; lpoints[0].y := 0;
  for j := 0 to 3 do
      begin
      lpoints[0].x := lpoints[0].x + trunc (lp[j].x*BezJ[0, j]);
      lpoints[0].y := lpoints[0].y + trunc (lp[j].y*BezJ[0, j]);
      end;
  points[0].x := lpoints[0].x / 1000;
  points[0].y := lpoints[0].y / 1000;
  for i := 1 to MAX_BEZIER_SEGMENTS do
      begin
      lpoints[i].x := 0; lpoints[i].y := 0;
      for j := 0 to 3 do
          begin
          lpoints[i].x := lpoints[i].x + trunc (lp[j].x*BezJ[i, j]);
          lpoints[i].y := lpoints[i].y + trunc (lp[j].y*BezJ[i, j]);
          end;
      points[i].x := lpoints[i].x / 1000;
      points[i].y := lpoints[i].y / 1000;
      // Check if pt in on the line segment, i-1 to i
      if PtOnLine (points[i-1], points[i], pt.x, pt.y) then
         begin
         result := true;
         t := i/MAX_BEZIER_SEGMENTS;
         exit;
         end;
      end;
  result := false;
end;


end.


