unit uDrawReaction;

interface

Uses SysUtils, Classes, Web, WEBLib.Graphics, Types, uNetwork, Dialogs, uNetworkTypes, uGraphUtils,
     System.Generics.Collections;

type
   TReactionRender = class
       private
         canvas : TCanvas;
         origin : TPointF;
         scalingFactor : double;
         arrowPts: TList<TPointF>;  // holds basic arrow shape at 0 degrees (rad)
         arrowTipPt: integer; // position of arrow tip in arrowPts
         procedure generateArrow(); // builds arrow if needed.
         function findArrowTip(): TPointF;
         function translateArrow(tip : TPointF; dxdt, dydt : double): TList<TPoint>;  // translates arrow based on rxn line slope

         procedure renderArrow (trArrowPts: TList<TPoint>); // converts to array for canvas.polygon
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
  CENTROID_RADIUS = 4;

constructor TReactionRender.Create (canvas : TCanvas);
begin
  self.canvas := canvas;
  self.arrowPts := TList<TPointF>.create;
end;

procedure TReactionRender.generateArrow(); // builds 4 pt arrow if needed.
var alpha, cosine, sine : double;
    pt: TPointF;
    i: integer;
    scale: double;
begin
  alpha := Angle(1, 0);  //(x,y)
  cosine := cos (alpha); sine := sin (alpha);
  scale := 1;
  pt.x := trunc (0*Scale*cosine + 14*Scale*sine);
  pt.y := trunc(-0*Scale*sine + 14*Scale*cosine);
  self.arrowPts.add(pt);
  pt.x := trunc(3*Scale*cosine + 7*Scale*sine);
  pt.y := trunc(-3*Scale*sine + 7*Scale*cosine);
  self.arrowPts.add(pt);
  pt.x := trunc(0*Scale*cosine + 0*Scale*sine);
  pt.y := trunc(-0*Scale*sine + 0*Scale*cosine);
  self.arrowPts.add(pt);
  pt.x := trunc(9*Scale*cosine + 7*Scale*sine);
  pt.y := trunc(-9*Scale*sine + 7*Scale*cosine);
  self.arrowTipPt := 3;
  self.arrowPts.add(pt);
end;

function TReactionRender.findArrowTip(): TPointF;
begin
  // Todo
  Result.x := 1;
  Result.y := 2;
end;

function TReactionRender.translateArrow(tip : TPointF; dxdt, dydt : double): TList<TPoint>;
var i: integer;
    alpha, cosine, sine : double;
    dx, dy : integer;
    newPt: TPoint;
    scale: double;
    initPts: TList<TPoint>;
begin
  Result := TList<TPoint>.create;
  initPts := TList<TPoint>.create;
  alpha := -Angle(dxdt, dydt);  //(x,y)
  cosine := cos (alpha); sine := sin (alpha);
  scale := 1;    // not used for now
  for i := 0 to self.arrowPts.count -1 do
    begin
      newPt.x := trunc( self.arrowPts[i].x * scale* cosine + self.arrowPts[i].y * scale * sine );
      newPt.y := trunc( self.arrowPts[i].y * scale * cosine - self.arrowPts[i].x * scale * sine );
      initPts.add( newPt );
    end;
  // Compute the distance of the tip of the arrow to the end point on the line
  // where the arrow should be placed. Then use this distance to translate S and T
  dx := integer (trunc (tip.x - initPts[arrowTipPt].x));
  dy := integer (trunc (tip.y - initPts[arrowTipPt].y));
  for i := 0 to initPts.count -1 do
    begin
      if i = self.arrowTipPt then
        begin    // Translate the coordinates of the arrow
          newPt.x := trunc(tip.x);
          newPt.y := trunc(tip.y);
          Result.add( newPt );
        end
      else
        begin
          newPt.x := trunc( initPts[i].x + dx );
          newPt.y := trunc( initPts[i].y + dy );
          Result.add( newPt );
        end;
    end

end;

procedure TReactionRender.drawArrow (tip : TPointF; dxdt, dydt : double);
begin
  if self.arrowPts.count < 2 then self.generateArrow;
  self.renderArrow (self.translateArrow(tip, dxdt, dydt));
end;


procedure TReactionRender.renderArrow(trArrowPts: TList<TPoint>);
var i: integer;
    renderPts: array of TPoint;
begin
  setLength(renderPts, trArrowPts.count);
  for i := 0 to trArrowPts.count -1 do
    begin
      renderPts[i].x := trArrowPts[i].x;
      renderPts[i].y := trArrowPts[i].y;
    end;
  self.canvas.polygon(renderPts);
end;

function TReactionRender.getRxnAPts(): array of TPointF;
var i: integer;
begin
  setLength(Result, self.arrowPts.count);
  for i := 0 to self.arrowPts.count -1 do
    begin
      Result[i].x := self.arrowPts[i].x;
      Result[i].y := self.arrowPts[i].y;
    end;

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

     //if edgeLabel.visible then    ?? needed??
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


// Draw the reactant side arcs
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
    reaction.state.reactantReactionArcs[arcId].nodeIntersectionPt := pt;

    // Adjust for changes in the origin
    h1 := minusPt(h1, Origin);
    h2 := minusPt(h2, Origin);
    pDest := minusPt(pDest, Origin);

    // Store the new start point for the bezier, and draw it
    pSrc := minusPt(pt, Origin);

    computeBezierPoints([pSrc, h1, h2, pDest]);

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
    reaction.state.productReactionArcs[arcId].nodeIntersectionPt := pt;
    //reaction.intersectionPts[arcId] := pt;

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
    drawArrow(pDest,pDest.x - h2.x, pDest.y - h2.y);
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

  pSrc     := scalePt(node.getCenter, scalingFactor);
  centroid := scalePt (centroid, scalingFactor);
  if computeLineIntersection(node, scalingFactor, pt, line(pSrc, centroid)) then
     begin
     reaction.state.reactantReactionArcs[arcId].nodeIntersectionPt.create (pt.x, pt.y);
     //reaction.state.reactantReactionArcs[arcId].nodeIntersectionPt := pt;

     if reaction.selected then
        canvas.pen.color := clRed
     else
        canvas.pen.color := reaction.state.fillColor;

     canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
     canvas.brush.color := reaction.state.fillColor;
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

  node := reaction.state.destPtr[arcId];

  pDest := scalePt(node.getCenter, scalingFactor);
  centroid := scalePt (centroid, scalingFactor);

  if computeLineIntersection(Node, scalingFactor, pt, line(centroid, pDest)) then
     begin
     reaction.state.productReactionArcs[arcId].nodeIntersectionPt := pt;
     reaction.state.productReactionArcs[arcId].nodeIntersectionPt.create (pt.x, pt.y);

     if reaction.selected then
        canvas.pen.color := clRed
     else
        canvas.pen.color := reaction.state.fillColor;

     canvas.pen.width := trunc (reaction.state.thickness * scalingFactor);
     canvas.brush.color := reaction.state.fillColor;

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
  canvas.rectangle (adjustedArcCenter.x - CENTROID_RADIUS, adjustedArcCenter.y - CENTROID_RADIUS, adjustedArcCenter.x + CENTROID_RADIUS, adjustedArcCenter.y + CENTROID_RADIUS);
  //canvas.ellipse(adjustedArcCenter.x - CENTROID_RADIUS, adjustedArcCenter.y - CENTROID_RADIUS, adjustedArcCenter.x + CENTROID_RADIUS, adjustedArcCenter.y + CENTROID_RADIUS);
end;



procedure TReactionRender.drawAnyToAny (reaction : TReaction);
var
  i: integer;
  centroid: TPointF;
  oldSize: integer;
begin
  centroid := reaction.state.arcCenter;
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
            drawStraightLineFromCentroid(i, reaction, centroid,  scalingFactor);
         end;
      end;

  if reaction.selected then
     begin
     centroid := scalePt (centroid, scalingFactor);
     drawCentroidPoint (minusPt (centroid, origin), clRed);
     end;

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
