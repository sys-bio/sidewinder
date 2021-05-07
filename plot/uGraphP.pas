unit uGraphP;

interface

uses SysUtils, Classes, Graphics, Controls, System.Types,
     Dialogs, WEBLib.StdCtrls, WEBLib.ExtCtrls, JS, Web, uSidewinderTypes;

const   MAXSIZE = 100;   // Used to set plot window width (if 1 msec step then 10 sec window.)
        Colors: array[0..9] of integer = (clOlive, clRed, clBlue, clGreen,clBlack,
                                        clAqua, clGray,clPurple, clYellow,clFuchsia);
type
  TDataType = array[1..MAXSIZE] of integer;
  TDouble_MAXSIZE_Array = array[1..MAXSIZE] of double;

  TPlotGraph = class
  private
    endp: integer ; startp: integer; size: integer;
    a, b, a1, b1 : single;
    maxpoints : integer;
    lsxmin, lsxmax : integer;
    x1: TDataType;
    y_vals : array of TDouble_MAXSIZE_Array;
    sim_Ymax: double; // Current largest y value for all variables of plot, do not change during run.
    graphScreenWidth, canvasHeight : integer;
    canvasWidth: integer;
    yAxisHt: integer; // height of yaxis in pixels.
    y_Mult: array of double; // array of y axis multipliers, default is 1. NOT used currently.
    step: double;     // step size
    simLength: double; // simulation length
    xScaleHeight : integer;
    yScaleWidth: integer; // new
    paintBox : TWebPaintBox;
    procedure drawGraph (canvas : TCanvas; x : TDataType;  y : TDouble_MAXSIZE_Array; size, startp, endp : integer);
    procedure change_xorigin (wxmin, wxmax : integer);
    procedure draw_x_axis (const x : TDataType; canvas : TCanvas; currTime: double);
    procedure draw_y_axis (const x : TDataType; canvas : TCanvas);
    function  world_to_screen (wx, wy : double): TIntegerArray;
    function  normalizeY(y_new, y_max: double):double;
  public
    bitmap : TBitmap;
    constructor create;
    procedure initGraph (wxmin, wxmax, wymin, wymax : integer; sxmin, sxmax,
                symin, symax : integer; xScaleHeight, yScaleWidth : integer; runTime : double; stepSize : double);
    procedure addPoint (time: integer; newYValues : array of double;
                   plot_var: array of boolean; display : boolean; currTime:double);
    procedure resetGraph (canvas : TCanvas);
    procedure redrawGraph(canvas : TCanvas; plot_var: array of boolean; display : boolean);
   end;


 implementation

 constructor TPlotGraph.create;
 begin
   endp:= 0; startp:= 1; size:= 0; sim_Ymax := 0;
   bitmap := TBitmap.Create;
 end;


// xscale height is the distance above the base of the graph at which the
// x axis is drawn, therefore it is a y coordinate
procedure TPlotGraph.initGraph (wxmin, wxmax, wymin, wymax : integer;
               sxmin, sxmax,  symin, symax : integer;
               xScaleHeight, yScaleWidth : integer;
               runTime : double; stepSize : double);
begin
  // NOTE:
  // y_vals should ideally be sized here but we don't know the size until addPoint

  simLength := runTime;
  step := stepSize;
  canvasHeight := symax - symin; // Get canvas height before adjusting symax
  canvasWidth  := sxmax - sxmin;

  bitmap.Width := canvasWidth;
  bitmap.Height := canvasHeight;

  bitmap.canvas.brush.color := clWhite;
  bitmap.canvas.FillRect(rect(0, 0, canvasWidth-1, canvasHeight - 1));

  // Now adjust for y axis label:
  self.yScaleWidth := yScaleWidth;
  self.xScaleHeight := xScaleHeight;

  sxmin := sxmin + yScaleWidth;   // Move the y axis to the right
  symax := symax - xScaleHeight;  // Move the x axis *up* (note y screens coords are inverted)

  yAxisHt := symax;
  graphScreenWidth := sxmax - sxmin;  // screen coordinates !

  // Conversion factors to convert world to screen coordinates
  lsxmax := sxmax; lsxmin := sxmin;
  a := graphScreenWidth/(wxmax - wxmin);
  // top - bottom !
  b := (symin - symax)/(wymax - wymin);
  a1 := -wxmin*a + lsxmin;
  b1 := -wymin*b + symax;
  maxpoints := trunc(runTime/stepSize);
  if sim_Ymax < 0 then
     sim_Ymax := 0;    // Do not reset ...
end;


function TPlotGraph.world_to_screen (wx, wy : double) : TIntegerArray;
begin
  setLength (result, 2);
  result[0] := trunc (wx*a + a1);
  result[1] := trunc (wy*b + b1);
end;


// If the xorigin has changed, update the workd to screen conversion factors
procedure TPlotGraph.change_xorigin (wxmin, wxmax : integer);
begin
  a := graphScreenWidth/(wxmax - wxmin);
  a1 := -wxmin*a + lsxmin;
end;


procedure TPlotGraph.resetGraph (canvas : TCanvas);
begin
  canvas.Brush.Color := clWhite;
  canvas.Rectangle(0, 0, graphScreenWidth-1, canvasHeight-1);
  endp := 0; startp := 1; size := 0;
end;


// Internal method
procedure TPlotGraph.drawGraph (canvas : TCanvas; x : TDataType; y : TDouble_MAXSIZE_Array; Size, startp, endp : integer);
var i, j, xi, yi : integer; xyAr: TIntegerArray;
begin
  // we must recompute x axis scaling as graph pans to left
  if Size = MAXSIZE then
     change_xorigin (x[startp], x[endp])
  else
     change_xorigin (1, MAXSIZE);

  xyAr := world_to_screen (x[startp], y[startp]);
  xi := xyAr[0]; yi := xyAr[1];
  canvas.Pen.Width := 2;
  canvas.moveto (xi, yi);
  j := startp+1; if j > MAXSIZE then j := 1;  // watch for overflow
  // Note since we've done the first point there will only be Size-1 left to do
  for i := 1 to size-1 do
      begin
      xyAr := world_to_screen (x[j], y[j]);
      xi := xyAr[0]; yi:= xyAr[1];
      canvas.lineto (xi, yi);
      j := (j MOD Size) + 1;
      end;
end;


procedure TPlotGraph.draw_x_axis (const x : TDataType; canvas : TCanvas; currTime: double);
var i, xi, yi, mxi, myi, minor, xstart, xend, scale, divMk, divMinor: integer;
    xyAr: TIntegerArray;
    leftEdge : integer;
begin
  divMk := 20;
  divMinor := 5;
  xstart := -1;
  xstart := integer(x[startp]);
  // now draw x scale
  canvas.pen.color := clBlack;
  canvas.Pen.Width := 1;
  xyAr := world_to_screen (xstart, 0);  xi := xyAr[0]; yi := xyAr[1];
  canvas.moveto (xi, yi);
  leftEdge := xi;
  xyAr := world_to_screen (xstart + MAXSIZE, 0);  xi := xyAr[0]; yi := xyAr[1];
  canvas.lineto (xi, yi);

  // major division interval = 20, minor division interval = 5
  xend  := xstart + MAXSIZE;
  scale := ((xstart div divMk) * divMk);  // place the first division at the easliest 20th mark on the graph
  canvas.font.name := 'Courier New';
  canvas.font.Size := 8;

  while scale < xend do
    begin
      // major ticks
      xyAr := world_to_screen (scale, 0); xi := xyAr[0]; yi := xyAr[1];
      if xi > leftEdge then
         begin
         canvas.moveto (xi+1, yi);
         canvas.lineto (xi+1, yi+divMinor);  // use screen coord on y, it might be quicker
         end;
      minor := scale + divMinor;
      for i := 1 to 3 do
          begin
          xyAr := world_to_screen (minor, 0); mxi := xyAr[0]; myi := xyAr[1];
          if mxi > leftEdge then
             begin
             canvas.moveto (mxi+1, myi);
             canvas.lineto (mxi+1, myi+3);  // use screen coord on y, it might be quicker
             end;
          inc (minor, divMinor);
          end;
      if xi > leftEdge-4 then  // -4 to make sure 0 is displayed
         canvas.TextOut (xi+1, yi+6, floattostrf ((scale*step),ffGeneral,6,4));
    inc (scale, divMk);
    end;
end;


procedure TPlotGraph.draw_y_axis (const x : TDataType; canvas : TCanvas);
var i, xi, yi, mxi, myi, xstart, ystart, yend, scale, divMk, divMinor: integer;
      xyAr: TIntegerArray;
 // TODO: add tick marks
var yposition, stepSize, minor, minorStepSize : double;
begin
  divMk := 18;
  divMinor := 4;
  ystart := yScaleWidth;
  xstart := x[startp];
  yend := 10;

  // now draw y scale
  canvas.pen.color := clBlack;
  xyAr := world_to_screen (xstart, 0); xi:= xyAr[0]; yi:= xyAr[1];
  canvas.moveto (xi, yi+1);
  xyAr := world_to_screen (xstart, yend); xi:= xyAr[0]; yi:= xyAr[1];
  canvas.lineto (xi, yi+1);

  // major division inter val = 20, minor division interval = 5
  //yend  := ystart + yaxisHt;
  scale := ((ystart div divMk) * divMk);   // place the first division at the easliest 20th mark on the graph
  canvas.font.name := 'Courier New';
  canvas.font.Size := 8;
  yPosition := 0;
  stepSize := yend/5;
  minorStepSize := stepSize / 3;

  while yPosition < yend do
    begin
      // major ticks
      xyAr := world_to_screen (xstart, yPosition); xi:= xyAr[0]; yi:= xyAr[1];
      canvas.moveto (xi, yi);
      canvas.lineto (xi - divMinor, yi);
      // minor ticks
      minor := yPosition + minorStepSize;
      for i := 1 to 3 do
          begin
          xyAr := world_to_screen (xstart, minor);
          mxi := xyAr[0]; myi := xyAr[1];
          canvas.moveto (mxi+1, myi);
          canvas.lineto (mxi+3, myi);
          minor := minor + minorStepSize;
          end;
      canvas.TextOut (xi-15, yi-4, floattostrf (yPosition,ffGeneral, 4, 4));
      //canvas.TextOut (xi+5, yi-4, floattostrf (((scale/yAxisHt)*sim_Ymax),ffGeneral, 4, 4));
      yposition := yposition + stepSize;
      //inc (scale, divMk);
    end;
end;


// time is currentGeneration
// newYValues cantains the Y values for the next time point
procedure TPlotGraph.addPoint (time: integer; newYValues : array of double;
                  plot_var: array of boolean; display : boolean; currTime : double);
var
  i, j : integer;
begin
  // Init size of y_vals if not already done:
  //sim_Ymax:= 10;//y_curr[0];
  if length(y_vals) <> length(newYValues) then
     setLength (y_vals, length(newYValues));

  //for i := 0 to length(newYValues)-1 do
  //  begin
  //    if (sim_Ymax <newYValues[i]) and plot_var[i] then sim_Ymax:= newYValues[i];
  //  end;


  // flood fill appears to be slighly quicker and eliminates the dots bug

  if display then
     begin
     bitmap.canvas.brush.color := clWhite;
     bitmap.canvas.Rectangle(0, 0, yscaleWidth + graphScreenWidth-1, canvasHeight-1);
     end;

  if size = MAXSIZE then
     begin
     endp   := (endp MOD MAXSIZE) + 1;
     startp := (startp MOD MAXSIZE) + 1;
     end
  else
     begin
     inc (endp); inc (size);
     end;
  x1[endp] := time;

  for i := 0 to Length(newYValues)-1 do
   y_vals[i][endp] := newYValues[i]; //  round(normalizeY(newYValues[i],sim_yMax));

  if display then
     begin
     for j := 0 to length(y_vals)-1 do
         begin
         if plot_var[j] then
           begin
           // TODO: if j > length(COLORS) then i:= j-
           if j < Length(COLORS) then bitmap.canvas.pen.color := COLORS[j]  // assume plots <10
           else bitmap.canvas.pen.color := COLORS[1];
           drawGraph (bitmap.canvas, x1, y_vals[j], Size, startp, endp);
           end;
         end;

     draw_x_axis (x1, bitmap.canvas, currTime);
     draw_y_axis (x1, bitmap.canvas);
     end;
end;


function TPlotGraph.normalizeY(y_new, y_max: double) : double;
begin
  result:= (y_new/y_max) *  yAxisHt; //(usable height, in pixels)
end;


procedure TPlotGraph.redrawGraph(canvas : TCanvas; plot_var: array of boolean; display : boolean);
var j: integer;
begin
  // flood fill appears to be slighly quicker and eliminates the dots bug
  if display then
     begin
      canvas.brush.color := clWhite;
      canvas.Rectangle(0, 0, yScaleWidth + graphScreenWidth-1, xScaleHeight + canvasHeight-1);

      for j := 0 to length(y_vals)-1 do
      begin
      if plot_var[j] then
        begin
          // TODO: if j > length(COLORS) then i:= j-
          if j<Length(COLORS) then canvas.pen.Color:= COLORS[j]
          else canvas.pen.color := COLORS[1];  // or COLORS[j mod length(COLORS)]
          drawGraph (canvas, x1, y_vals[j], Size, startp, endp);
        end;
      end;

      draw_x_axis (x1, canvas,10);
     end;
end;


end.
