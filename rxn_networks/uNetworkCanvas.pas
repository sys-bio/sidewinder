unit uNetworkCanvas;

interface

Uses SysUtils, Classes, Graphics, Types, Dialogs, uNetwork, uDrawReaction,
  uNetworkTypes, Math, uGraphUtils, Web, WEBLib.Graphics;

const
   DEFAULT_FONT_SIZE = 10;

type
  TNetworkCanvas = class

  private
    //function  getControlRects(x, y, w, h: integer): TRectangularArray;
    procedure drawMouseGrabPoints(x, y, w, h: integer);

  public
    network: TNetwork;
    reactionRenderer: TReactionRender;
    bolDrawSelectionBox: boolean;
    selectionBoxPt : TPoint;
    selectionBox : TCanvasRectF;
    MousePt : TPoint;

    bitmap: TBitmap;
    origin: TPointF;
    scalingFactor : double;
    procedure paint;// (origin: TPointF; scalingFactor: double);

    procedure drawSelectionBox;
    procedure drawReactions;
    procedure drawNodes;
    procedure drawNode;
    function  drawNodeCaption (node : TNode; scaledX, scaledY : double) : TPointF;

    function  unScale (x : double) : double;  inline;

    function  scaleWorldDim_X (x : double) : integer; inline;
    function  scaleWorldDim_Y (y : double) : integer; inline;

    function  worldToScreen_X (wx : double) : integer; inline;
    function  worldToScreen_Y (wy : double) : integer; inline;
    function  worldToScreen (wx, wy : double) : TPoint;  inline;

    constructor Create (network: TNetwork);

  end;

implementation

constructor TNetworkCanvas.Create (network: TNetwork);
begin
  bitmap := TBitmap.Create;
  self.network := network;
  bolDrawSelectionBox := False;
  scalingFactor := 1;
  origin := TPointF.Create (0, 0);
  reactionRenderer := TReactionRender.Create(bitmap.canvas);
end;

// ---------------------------------------------------------
// WORLD / SCREEN CONVERSION METHODS
// ---------------------------------------------------------


function TNetworkCanvas.unScale (x : double) : double;
begin
  result := trunc (x / scalingFactor);
end;

// Use this for lengths such as widths and heigths
// For point coorindates use worldToScreen methds
function TNetworkCanvas.scaleWorldDim_X (x : double) : integer;
begin
  result := trunc (x * scalingFactor);
end;

function TNetworkCanvas.scaleWorldDim_Y (y : double) : integer;
begin
  result := trunc (y * scalingFactor);
end;


function TNetworkCanvas.worldToScreen (wx, wy : double) : TPoint;
begin
  result.x := trunc(wx * scalingFactor - origin.x);
  result.y := trunc(wy * scalingFactor - origin.y);
end;

function TNetworkCanvas.worldToScreen_X (wx : double) : integer;
begin
  result := trunc(wx * scalingFactor - origin.x);
end;

function TNetworkCanvas.worldToScreen_Y (wy : double) : integer;
begin
  result := trunc(wy * scalingFactor - origin.y);
end;

// -------------------------------------------------------

procedure TNetworkCanvas.paint;
var dest: TRect;
begin
  dest.Left := 0;
  dest.top := 0;
  dest.Right := bitmap.Width - 0;
  dest.Bottom := bitmap.Height - 0;
  bitmap.canvas.pen.color := clWhite;
  bitmap.canvas.Brush.color := clWhite;
  bitmap.canvas.FillRect(dest);

  drawNodes;
  drawReactions;
  if bolDrawSelectionBox then
    drawSelectionBox;
end;


procedure TNetworkCanvas.drawSelectionBox;
var dest : TRect; oldColor : TColor; oldStyle : TPenStyle;
    p1, p2, p3, p4, p5 : TPoint;
begin
  // min/max stuff allows user to draw box upwards or downwards
  selectionBox.left := min (MousePt.x, trunc (selectionBoxPt.x));
  selectionBox.Top := min (MousePt.y, trunc (selectionBoxPt.y));
  selectionBox.right := max (MousePt.x, trunc (selectionBoxPt.x));
  selectionBox.bottom := max (MousePt.y, trunc (selectionBoxPt.y));

  oldColor := Bitmap.Canvas.Pen.Color;
  oldStyle := Bitmap.Canvas.Pen.Style;

  Bitmap.Canvas.Pen.Color := clBlack;
  Bitmap.Canvas.Pen.Style := psDot;
  Bitmap.Canvas.Pen.Width := 1;

  p1 := OriginAdjustToInt (TPointF.Create (MousePt.x*scalingFactor, MousePt.y*scalingFactor), origin);
  p2 := OriginAdjustToInt (TPointF.Create (selectionBoxPt.x*scalingFactor, MousePt.y*scalingFactor), Origin);
  p3 := OriginAdjustToInt (TPointF.Create (selectionBoxPt.x*scalingFactor, selectionBoxPt.y*scalingFactor), Origin);
  p4 := OriginAdjustToInt (TPointF.Create (MousePt.x*scalingFactor, selectionBoxPt.y*scalingFactor), origin);
  p5 := OriginAdjustToInt (TPointF.Create (MousePt.x*scalingFactor, MousePt.y*scalingFactor), origin);
  Bitmap.Canvas.PolyLine([p1, p2, p3, p4, p5]);

  Bitmap.Canvas.Pen.Color := oldColor;
  Bitmap.Canvas.Pen.Style := oldStyle;
end;


procedure TNetworkCanvas.drawMouseGrabPoints(x, y, w, h: integer);
var rectList: TRectangularArray;
begin
  rectList := getControlRects(x, y, w, h);

  bitmap.canvas.Brush.color := clRed;
  bitmap.canvas.Brush.style := bsSolid;
  bitmap.canvas.FillRect(rectList[0]);
  bitmap.canvas.FillRect(rectList[1]);
  bitmap.canvas.FillRect(rectList[2]);
  bitmap.canvas.FillRect(rectList[3]);
end;


procedure TNetworkCanvas.drawNode;
begin

end;


// Returns the unscaled rectangle coords for the caption
function TNetworkCanvas.drawNodeCaption (node : TNode; scaledX, scaledY : double) : TPointF;
var p, q : double;
    oldBrushStyle : TBrushStyle;
    //bbox : TGPRectF;
    xp, yp : double;
    relativeDisplacment : double;
begin
  // scaledX and scaledY is the corrdinate of the top left corner of the bounding box

  oldBrushStyle := bitmap.canvas.Brush.Style;
  bitmap.canvas.Brush.Style := bsClear;

  // Center x and y relative to top left corner adjusted by size of text
  p := scaleWorldDim_X (node.state.w) / 2 - (bitmap.canvas.TextWidth (node.state.species) / 2);
  q := scaleWorldDim_Y (node.state.h) / 2 - (bitmap.canvas.TextHeight (node.state.species) / 2);

  relativeDisplacment := 8/100; // 8/100 = 8% adjustment
  xp := scaledX + p;
//  case shapeObj.captionPosition of
//      npBottom : begin
//                 if shapeObj.boundaryNode then
//                    relativeDisplacment := 40/100;
//                 yp := scaledY  + shapeObj.h*scalingFactor + relativeDisplacment*shapeObj.h*scalingFactor;
//                 end;
//
//         npTop : begin
//                 if shapeObj.boundaryNode then
//                    relativeDisplacment := 32/100;
//                 yp := scaledY - Canvas.TextHeight (name) - relativeDisplacment*shapeObj.h*scalingFactor;  // 8/100 = 8% upwards adjustment
//                 end;

  yp := scaledY + q;

//        npLeft : begin yp := scaledY + q; xp := xp - (shapeObj.w/2*scalingFactor) - Canvas.TextWidth (name)/2;  end;
//
//       npRight : begin yp := scaledY + q; xp := xp + (shapeObj.w/2*scalingFactor) + Canvas.TextWidth (name)/2 + (12*scalingFactor);  end;
//  else
     // yp := scaledY + (shapeObj.h + 5)*scalingFactor;
  //end;

  //if shapeObj.captionVisible then
  bitmap.canvas.TextOut (trunc (xp), trunc (yp), node.state.species);

  //extValue.absoluteCoordinateX := trunc (xp);
  //textValue.absoluteCoordinateY := trunc (yp);
  //textValue.textWidth := bitmap.canvas.TextWidth (name);
  //textValue.textHeight := bitmap.canvas.TextHeight (name);

  bitmap.canvas.Brush.Style := oldBrushStyle;

  // Not currently used:
  result.x := p/scalingFactor;
  result.y := q/scalingFactor;
end;



procedure TNetworkCanvas.drawNodes;
var
  i: integer;
  oldWidth: integer;
  oldColor: TColor;
  f, sX, sY: integer;
  scaledX, scaledY, scaledW, scaledH: integer;
  scaledPt : TPoint;
  sizeOfText : TCanvasSizeF;
begin
  oldWidth := bitmap.canvas.pen.Width;
  oldColor := bitmap.canvas.pen.color;

  bitmap.canvas.Font.Size := trunc (DEFAULT_FONT_SIZE * scalingFactor);
  if bitmap.canvas.Font.Size = 0 then bitmap.canvas.Font.Size := 2;

  try
    for i := 0 to length(network.nodes) - 1 do
        begin
        // convert the node positions to screen value
        scaledX := worldToScreen_X (network.nodes[i].state.x);
        scaledY := worldToScreen_Y (network.nodes[i].state.y);
        scaledW := scaleWorldDim_X (network.nodes[i].state.w);
        scaledH := scaleWorldDim_Y (network.nodes[i].state.h);

        // Get the size of the text
        sizeOfText := bitmap.canvas.TextExtent (network.nodes[i].state.species);

        if sizeOfText.cx/scalingFactor + 1 >= network.nodes[i].state.w then
           network.nodes[i].state.w := unscale (SizeOfText.cx) + 0.1*unscale (sizeOfText.cx);

        if network.nodes[i].selected then
           begin
           bitmap.canvas.pen.color := clRed;
           bitmap.canvas.pen.Width := 1;
           bitmap.canvas.Brush.style := bsClear;
           f := scaleWorldDim_X (4);

           sX := scaledX - f;
           sY := scaledY - f;

           bitmap.canvas.Rectangle(sX, sY, sX + scaledW + 2 * f, sY + scaledH + 2 * f);
           drawMouseGrabPoints(sX, sY, scaledW + 2 * f, scaledH + 2 * f);
           end;

       if network.nodes[i].addReactionSelected then
          begin
          bitmap.canvas.pen.color := clRed;
          bitmap.canvas.Brush.style := bsClear;
          bitmap.canvas.pen.Width := 1;
          bitmap.canvas.pen.style := psDash;
          bitmap.canvas.RoundRect(
                     worldToScreen_X (network.nodes[i].state.x - 7),
                     worldToScreen_Y (network.nodes[i].state.y - 7),
                     scaledX + scaleWorldDim_X (network.nodes[i].state.w + 7),
                     scaledY + scaleWorldDim_Y (network.nodes[i].state.h + 7), 25, 25);

          bitmap.canvas.pen.style := psSolid;
          bitmap.canvas.pen.color := network.nodes[i].state.outlineColor;
          end;

       bitmap.canvas.pen.color := network.nodes[i].state.outlineColor;
       bitmap.canvas.pen.Width := 3;
       bitmap.canvas.Brush.color := network.nodes[i].state.fillColor;
       bitmap.canvas.Brush.style := bsSolid;
       bitmap.canvas.RoundRect(scaledX, scaledY,
                 scaledX + scaleWorldDim_X (network.nodes[i].state.w),
                 scaledY + scaleWorldDim_Y (network.nodes[i].state.h), 25, 25);

      drawNodeCaption (network.nodes[i], scaledX, scaledY);
    end;
  finally
    bitmap.canvas.pen.Width := oldWidth;
    bitmap.canvas.pen.color := oldColor;
    bitmap.canvas.pen.style := psSolid;
  end;
end;

procedure TNetworkCanvas.drawReactions;
var
  i: integer;
  scaledLineThickness: integer;
begin
  bitmap.canvas.pen.Width := 2;
  try
    for i := 0 to length(network.reactions) - 1 do
    begin
      if network.reactions[i].selected then
      begin
        bitmap.canvas.pen.color := clRed;
      end
      else
      begin
        bitmap.canvas.pen.Width := trunc(2 * scalingFactor);
        bitmap.canvas.pen.color := clBlack;
      end;
      reactionRenderer.draw(origin, scalingFactor, network.reactions[i]);
    end;
  finally
    bitmap.canvas.pen.color := clBlack;
  end;
end;

end.
