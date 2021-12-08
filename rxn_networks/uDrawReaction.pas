unit uDrawReaction;

interface

Uses SysUtils, Classes, Web, WEBLib.Graphics, Types, uNetwork, Dialogs, uNetworkTypes, uGraphUtils;

type
   TReactionRender = class
       private
         canvas : TCanvas;
         origin : TPointF;
         scalingFactor : double;
         arrowPts: array of TPointF;
         procedure drawArrow (tip : TPointF; dxdt, dydt : double);
         procedure drawBezierArcHandles(p: array of TPointF);
         procedure drawBezierToCentroid (arcId: integer; reaction : TReaction; centroid: TPointF; scalingFactor: double) ;
         procedure drawBezierFromCentroid(arcId: integer; reaction : TReaction; centroid: TPointF;  scalingFactor: double);
         procedure drawStraightLineToCentroid(ArcId: integer; reaction : TReaction; centroid: TPointF; scalingFactor: double);
         procedure drawStraightLineFromCentroid (arcId: integer; reaction : TReaction; centroid: TPointF; scalingFactor: double);
         procedure drawCentroidPoint(adjustedArcCenter : TPointF; color : TColor);
         procedure drawUniUniLine (scalingFactor : double; reaction : TReaction);
         procedure drawAnyToAny (reaction : TReaction);

       public
         procedure draw (origin : TPointF; scalingFactor : double; reaction : TReaction);
         function getRxnAPts(): array of TPointF; // Get reaction arrow pts
         constructor Create (canvas : TCanvas);
   end;

implementation

const
  CENTROID_RADIUS = 3;

constructor TReactionRender.Create (canvas : TCanvas);
begin
  self.canvas := canvas;
end;


procedure TReactionRender.drawArrow (tip : TPointF; dxdt, dydt : double);
var dx, dy : integer;
    alpha, cosine, sine : double;
    pg : TPointDynArray;  // defines the vertices of an arrow
    adX, adY : double;
    scalingFactor : double;
    scale : double;
    fpt : array of TPoint;
    i : integer;
begin
  setLength (pg, 4);

  scale := 1; scalingFactor := 1;
  alpha := -Angle (dxdt, dydt);
  cosine := cos (alpha); sine := sin (alpha);
  // Adjust the tip so that it moves forward slightly: I didn't do this in the end.
  adX := trunc (1*scalingFactor)*cos (-alpha);
  adY := trunc (1*scalingFactor)*sin (-alpha);
  tip.x := tip.x + trunc (adX);
  tip.y := tip.y + trunc (adY);

  pg[0].x := trunc (0*Scale*cosine + 14*Scale*sine);
  pg[0].y := trunc(-0*Scale*sine + 14*Scale*cosine);

  //pg[1].x := trunc(7*Scale*cosine + 7*Scale*sine);
  //pg[1].y := trunc(-7*Scale*sine + 7*Scale*cosine);
  pg[1].x := trunc(3*Scale*cosine + 7*Scale*sine);
  pg[1].y := trunc(-3*Scale*sine + 7*Scale*cosine);

  pg[2].x := trunc(0*Scale*cosine + 0*Scale*sine);
  pg[2].y := trunc(-0*Scale*sine + 0*Scale*cosine);

  pg[3].x := trunc(9*Scale*cosine + 7*Scale*sine);
  pg[3].y := trunc(-9*Scale*sine + 7*Scale*cosine);

  // Compute the distance of the tip of the arrow to the end point on the line
  // where the arrow should be placed. Then use this distance to translate S and T
  dx := integer (trunc (tip.x - pg[3].x));
  dy := integer (trunc (tip.y - pg[3].y));

  // Translate the remaining coordinates of the arrow, note tip = V
  pg[0].x := pg[0].x + dx; pg[0].y := pg[0].y + dy;  // S
  pg[1].x := pg[1].x + dx; pg[1].y := pg[1].y + dy;  // T
  pg[2].x := pg[2].x + dx; pg[2].y := pg[2].y + dy;  // U
  pg[3].x := tip.x;
  pg[3].y := tip.y;

  //if selected then
  //   solidBrush.SetColor (aclSelectedLineColor)
  //else solidBrush.SetColor (aclArrowColor);
  // pg is a pointer, drawpolygon expect one, so cast it to the pointer it expects
  setlength (fpt, 4);
  setLength(self.arrowPts, length(fpt));
  for i := 0 to 3 do
      begin
      fpt[i].x := trunc (pg[i].x);
      self.arrowPts[i].x := pg[i].x;  // assume all arrows are the same
      fpt[i].y := trunc (pg[i].y);
      self.arrowPts[i].y := pg[i].y;
      end;
  //setLength(self.arrowPts, length(fpt));
 // for i := 0 to length(fpt)-1 do
 //   self.arrowPts[i] := fpt[i]; //

  canvas.polygon (fpt);
end;

function TReactionRender.getRxnAPts(): array of TPointF;
begin
  Result := self.arrowPts;
end;


// Used if the line type is ltLine
procedure TReactionRender.drawUniUniLine (scalingFactor : double; reaction : TReaction);
var startPt, endPt : TPointF;
    alpha, adX, adY : double;
    //centreOfLine : TPoint;
    srcPtIntersect, destPtIntersect : TPointF;
    pSrc, pDest : TPointF;
begin
  pSrc  := minusPt (scalePt (reaction.state.srcPtr[0].getCenter, scalingFactor), origin);
  pDest := minusPt (scalePt (reaction.state.destPtr[0].getCenter,scalingFactor), origin);

  computeLineIntersection (reaction.state.srcPtr[0], scalingFactor, srcPtIntersect, Line (pSrc, pDest));
  computeLineIntersection (reaction.state.destPtr[0], scalingFactor, destPtIntersect, Line (pSrc, pDest));

  // Save this intersection point because we'll need it at other times to work
  // out whether the user has clicked on the curve with the mouse, see IsOnEdge
  //intersect[0] := destPtIntersect;
  //intersect[1] := srcPtIntersect;
  startPt := srcPtIntersect;
  endPt := destPtIntersect;

  startPt := srcPtIntersect; //minusPt (srcPtIntersect, Origin);
  endPt := destPtIntersect; //minusPt (destPtIntersect, Origin);

  // Move the end point slightly back to make room for the arrow
  alpha := Angle (endPt.x - startPt.x, endPt.y - startPt.y);
  adX := trunc (8*scalingFactor)*cos (alpha);
  adY := trunc (8*scalingFactor)*sin (alpha);
  adX := endPt.x - trunc (adX);
  adY := endPt.y - trunc (adY);

  if reaction.selected then
     canvas.pen.color := clRed
  else
     canvas.pen.color := reaction.state.fillColor;

  canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
  canvas.brush.color := reaction.state.fillColor;

  Canvas.moveTo (startPt.x, startPt.y);
  Canvas.lineTo (adX, adY);

  drawArrow (endPt, pDest.x - pSrc.x, pDest.y - pSrc.y);

     //if edgeLabel.visible then
     //   begin
     //   centreOfLine.x := srcPtIntersect.x + (destPtIntersect.x - srcPtIntersect.x) div 2;
     //   centreOfLine.y := srcPtIntersect.y + (destPtIntersect.y - srcPtIntersect.y) div 2;
     //   edgeLabel.Paint(Canvas, self, (NetworkRef as TNetwork).printerScale, scalingFactor, minusPt (minusPt (centreOfLine, point (-10,0)), Origin));
     //   end;
end;


// Draw the bezier control points
procedure TReactionRender.drawBezierArcHandles(p: array of TPointF);
var
  oldcolour: TColor;
  oldThickness: integer;
  oldPenStyle: TPenStyle;
  oldBrushStyle: TBrushStyle;
  oldSolidPenThickness: single;
begin
  // Must remember brush colour because arrow heads are filled in with
  // foreground colour which is different from background colour.
  oldcolour := Canvas.Brush.color;
  oldThickness := Canvas.Pen.Width;
  oldBrushStyle := Canvas.Brush.style;
  oldPenStyle := Canvas.Pen.style;
  oldSolidPenThickness := canvas.pen.width;
  try
    Canvas.Brush.color := clRed; // makes them stand out
    Canvas.Pen.Width := 2;
    Canvas.Brush.style := bsSolid;
    Canvas.Pen.style := psSolid;

    canvas.moveto (p[0].x, p[0].y); canvas.lineto (p[1].x, p[1].y);
    canvas.ellipse (p[1].x - HANDLE_RADIUS, p[1].y - HANDLE_RADIUS,
                    p[1].x + HANDLE_RADIUS, p[1].y + HANDLE_RADIUS );

    canvas.moveto (p[2].x, p[2].y); canvas.lineto (p[3].x, p[3].y);
    canvas.ellipse (p[2].x - HANDLE_RADIUS, p[2].y - HANDLE_RADIUS,
                    p[2].x + HANDLE_RADIUS, p[2].y + HANDLE_RADIUS );
  finally
    Canvas.Brush.color := oldcolour;
    Canvas.Pen.Width := oldThickness;
    Canvas.Brush.style := oldBrushStyle;
    Canvas.Pen.style := oldPenStyle;
  end;
end;


procedure TReactionRender.drawBezierToCentroid (arcId: integer; reaction : TReaction; centroid: TPointF; scalingFactor: double);
var h1, h2, pt: TPointF;
    pSrc, pDest : TPointF;
    node : TNode;
    par : double;
    segmentNumber : integer;
begin
  node := reaction.state.srcPtr[arcId];

  // Get the bezier control handle coordinates for the first subarc
  h1 := scalePt(reaction.state.reactantReactionArcs[arcId].h1, scalingFactor);
  h2 := scalePt(reaction.state.reactantReactionArcs[arcid].h2, scalingFactor);

  // We will draw an arc form the node to the arc center
  // First calculate the center of the src node, then the arc centre
  pSrc := scalePt(node.getCenter(), scalingFactor);
  pDest := scalePt(centroid, scalingFactor);

  // Compute the points along the bezier from node center to centroid
  // bezier pts needed by computeBezierLineIntersection
  computeBezierPoints([pSrc, h1, h2, pDest]);

  // Clip the src starting point by returning the bezier segment number which intersects with the node outer rectangle
  if computeBezierLineIntersection(node, scalingFactor, pt, par, segmentNumber) then
    // Only draw if successful }
    begin
    reaction.intersectionPts[arcId] := pt;

    // Adjust for changes in the origin
    h1 := minusPt(h1, Origin);
    h2 := minusPt(h2, Origin);
    pDest := minusPt(pDest, Origin);

    // Store the new start point for the bezier, and draw it
    pSrc := minusPt(pt, Origin);

    computeBezierPoints([pSrc, h1, h2, pDest]);
    // See UniUni for explanation of following line
    //Intersect[ArcId] := pt;

    if reaction.selected then
       canvas.pen.color := clRed
    else
       canvas.pen.color := reaction.state.fillColor;

    canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
    canvas.brush.color := reaction.state.fillColor;

    drawBezier (canvas);

    if reaction.selected then
       drawBezierArcHandles([pSrc, h1, h2, pDest]);
    // ***** HMS
    //if reversible then
    //  arrowTip.Paint(self, pSrc, pDest.x - pSrc.x, pDest.y - pSrc.y, true); // true = reverse arrow
   end;
end;


procedure TReactionRender.drawBezierFromCentroid(arcId: integer; reaction : TReaction; centroid: TPointF;  scalingFactor: double);
var
  pSrc, pDest, h1, h2, pt: TPointF;
  par: double;
  Segn: integer;
  alpha: double;
  adX, adY: double;
  node : TNode;
begin
  // Nasty hack, destPtr ranges from 0 to nProducts but arcId includes reactant and
  // product arcs so we must subtract the nReactants to get the indexing right.
  node := reaction.state.destPtr[arcId];

  // This time we're going to draw from the arccentre to the node
  h1 := scalePt(reaction.state.productReactionArcs[arcId].h1, scalingFactor);
  h2 := scalePt(reaction.state.productReactionArcs[arcId].h2, scalingFactor);
  pSrc := scalePt(centroid, scalingFactor);
  pDest := scalePt(node.getCenter, scalingFactor);

  computeBezierPoints([pSrc, h1, h2, pDest]);

  h1 := minusPt(h1, Origin);
  h2 := minusPt(h2, Origin);
  pSrc := minusPt(pSrc, Origin);

  if computeBezierLineIntersection(node, scalingFactor, pt, par, Segn) then
  begin
    reaction.intersectionPts[arcId] := pt;

    pDest := minusPt(pt, Origin);

    // Move the end point slightly back to make room for the arrow
    alpha := Angle(pDest.x - h2.x, pDest.y - h2.y);
    adX := trunc(8 * scalingFactor) * cos(alpha);
    adY := trunc(8 * scalingFactor) * sin(alpha);
    adX := pDest.x - trunc(adX);
    adY := pDest.y - trunc(adY);

    // Store the new start point for the bezier, and draw it
    //pSrc := minusPt(pt, Origin);

    computeBezierPoints([pSrc, h1, h2, pDest]);

    //Intersect[ArcId] := pt;

    if reaction.selected then
       canvas.pen.color := clRed
    else
       canvas.pen.color := reaction.state.fillColor;

    canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
    canvas.brush.color := reaction.state.fillColor;

    drawBezier (canvas);

    //if arrowTip.visible then
    //   arrowTip.Paint(self, pDest, pDest.x - h2.x, pDest.y - h2.y);
    if reaction.selected then
       begin
       drawBezierArcHandles ([pSrc, h1, h2, pDest]);
       end;
  end;
end;


procedure TReactionRender.drawStraightLineToCentroid(arcId: integer; reaction : TReaction;
             centroid: TPointF; scalingFactor: double);
var
  pSrc, pt: TPointF;
  startPt: TPointF;
  node : TNode;
  oldWidth : integer;
begin
  oldWidth := canvas.pen.width;

  node := reaction.state.srcPtr[arcId];

  pSrc := scalePt(node.getCenter, scalingFactor);
  centroid := scalePt (centroid, scalingFactor);
  if computeLineIntersection(node, scalingFactor, pt, line(pSrc, centroid)) then
     begin
     reaction.intersectionPts[arcId] := pt;

     if reaction.selected then
        canvas.pen.color := clRed
     else
        canvas.pen.color := reaction.state.fillColor;

     canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
     canvas.brush.color := reaction.state.fillColor;
     //Intersect[ArcId] := pt;
     startPt := minusPt(pt, Origin);
     centroid := minusPt(centroid, Origin);
     canvas.moveTo (startPt.x, startPt.y);
     canvas.lineTo (centroid.x, centroid.y);
     end;
  //if reversible then
  //   arrowTip.Paint(self, startPt, pDest.x - startPt.x, pDest.y - startPt.y, true); // true = reverse arrow
  canvas.pen.width := oldWidth;
end;


procedure TReactionRender.drawStraightLineFromCentroid (arcId: integer; reaction : TReaction; centroid: TPointF; scalingFactor: double);
var
  pDest, pt: TPointF;
  startPt, endPt: TPointF;
  alpha, adX, adY: double;
  node : TNode;
  oldWidth : integer;
begin
  oldWidth := canvas.pen.width;

  node := reaction.state.destPtr[arcId-reaction.state.nReactants];

  pDest := scalePt(node.getCenter, scalingFactor);
  centroid := scalePt (centroid, scalingFactor);

  if computeLineIntersection(Node, scalingFactor, pt, line(centroid, pDest)) then
     begin
     reaction.intersectionPts[arcId] := pt;

     if reaction.selected then
        canvas.pen.color := clRed
     else
        canvas.pen.color := reaction.state.fillColor;

     canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
     canvas.brush.color := reaction.state.fillColor;

     reaction.intersectionPts[arcId] := pt;

     endPt := minusPt(pt, Origin);
     startPt := minusPt(centroid, Origin);
     canvas.moveTo (startPt.x, startPt.y);
     canvas.lineto (endPt.x, endPt.y);

     // Move the end point slightly back to make room for the arrow
     alpha := Angle(pDest.x - centroid.x, pDest.y - centroid.y);
     adX := trunc(8 * scalingFactor) * cos(alpha);
     adY := trunc(8 * scalingFactor) * sin(alpha);
     adX := pDest.x - trunc(adX);
     adY := pDest.y - trunc(adY);

     drawArrow (endPt, pDest.x - centroid.x, pDest.y - centroid.y);

    //pDest := minusPt(pt, Origin);
    //pSrc := minusPt(pSrc, Origin);
    //if arrowTip.visible then
    //   arrowTip.Paint(self, pDest, pDest.x - pSrc.x, pDest.y - pSrc.y);
  end;
  canvas.pen.width := oldWidth;
end;



function isMouseOnArcCentre(reaction: TReaction; x, y: double): Boolean;
begin
  if ptInCircle(x, y, reaction.state.arcCenter) then
    result := True
  else
    result := False;
end;


procedure TReactionRender.drawCentroidPoint(adjustedArcCenter : TPointF; color : TColor);
begin
  canvas.pen.color := color;
  canvas.brush.color := color;
  canvas.ellipse(adjustedArcCenter.x - CENTROID_RADIUS, adjustedArcCenter.y - CENTROID_RADIUS, adjustedArcCenter.x + CENTROID_RADIUS, adjustedArcCenter.y + CENTROID_RADIUS);
end;



procedure TReactionRender.drawAnyToAny (reaction : TReaction);
var
  i: integer;
  centroid: TPointF;
  oldSize: integer;
begin
  centroid:= reaction.state.arcCenter;
  try
   // To arc center
   for i := 0 to reaction.state.nReactants - 1 do
       begin
        case reaction.state.lineType of
         ltBezier :
           drawBezierToCentroid (i, reaction, centroid, scalingFactor);
         ltLine :
           drawStraightLineToCentroid (i, reaction, centroid, scalingFactor);
         //ltSegmentedLine :
         //  drawLineSegmentToCentroid (i, (srcConnectedNodeList[ i ] as TConnectedNode).SubNode, AdjustedArcCentre, Origin, scalingFactor);
       end;
       end;

  // From arc center to products
  for i := 0 to reaction.state.nProducts - 1 do
      begin
        case reaction.state.lineType of
         ltBezier :
            drawBezierFromCentroid (i, reaction, centroid, scalingFactor);
         ltLine :
            drawStraightLineFromCentroid(reaction.state.nReactants + i, reaction, centroid,  scalingFactor);
         end;
      end;

  if reaction.selected then
     drawCentroidPoint (minusPt (centroid, origin), clRed);

   //reactionLabel.Paint(self, addPtF (AdjustedArcCentre, TPointF.Create (reactionLabel.textValue.rx, reactionLabel.textValue.ry)));

  finally
    //Canvas.font.size := oldSize;
  end;
end;


procedure TReactionRender.draw (origin : TPointF; scalingFactor : double; reaction : TReaction);
begin
  self.origin := origin;
  self.scalingFactor := scalingFactor;
  drawAnyToAny (reaction);
end;


end.
