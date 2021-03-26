unit Graphp;

 // TODO: turn this into a class so that there is a separate instantiation for each plot.
 // Currently all plots share all values set in this unit. Ok for most but not all, such as Size and sim_Ymax
interface

uses SysUtils, Classes, Graphics, Controls, Dialogs, JS, Web;

procedure initGraph (wxmin, wxmax, wymin, wymax : integer; sxmin, sxmax,
                symin, symax : integer; xscaleHeight : integer; runTime:double; stepSize: double);

procedure addPoint (canvas : TCanvas; time: integer; y_curr : array of double;
                 plot_var: array of boolean; display : boolean; currTime:double);
procedure resetGraph (canvas : TCanvas);
procedure redrawGraph(canvas : TCanvas; plot_var: array of boolean; display : boolean);
function normalizeY(y_new, y_max: double):double;

implementation

const
  MAXSIZE = 100;   // Used to set plot window width (if 1 msec step then 10 sec window.)
  OFFSET = 120;

type
  DataType = array[1..MAXSIZE] of integer;
  const Colors: array[0..9] of integer = (clOlive, clRed, clBlue, clGreen,clBlack,
                                        clAqua, clGray,clPurple, clYellow,clFuchsia  );

var a, b, a1, b1 : single;
    maxpoints : integer;
    x1: DataType;
    y_vals: array of DataType;
    lsxmin, lsxmax : integer;
    sim_Ymax: double; // Current largest y value for all variables, can change during run.
    GraphScrnWidth, CanvasHeight : integer;
    CanvasWidth: integer;
    yAxisHt: integer; // height of yaxis in pixels.
    y_Mult: array of double; // array of y axis multipliers, default is 1. NOT used currently.
    step: double;     // step size
    simLength: double; // simulation length
    yscaleHeight: integer;

    var endp : integer = 0; startp : integer = 1; Size : integer = 0;


procedure world_to_screen (wx, wy : integer; var sx, sy : integer);
begin
  sx := trunc (wx*a + a1);
  sy := trunc (wy*b + b1);
end;


{ xscale height is the distance above the base of the graph at which the
x axis is drawn, therefore it is a y coordinate }
procedure initGraph (wxmin, wxmax, wymin, wymax : integer;
                     sxmin, sxmax, symin, symax : integer;
                     xscaleHeight : integer; runTime:double; stepSize: double);
begin
  simLength:= runTime;
  step:= stepSize;
  CanvasHeight := (symax - symin); // Get canvas height before adjusting symax
  symax := symax - xscaleHeight;  // max pixel height
  // Now adjust for y axis label:
  yScaleHeight:= xscaleHeight;
  CanvasWidth:= (sxmax - sxmin);
//  sxmin:= sxmin + yScaleHeight;  // adjust width with pixel width of y axis label.

  yAxisHt:= symax;
  GraphScrnWidth  := sxmax - sxmin;    { screen coordinates ! }

  lsxmax := sxmax; lsxmin := sxmin;
  a := GraphScrnWidth/(wxmax - wxmin);
  { top - bottom ! }
  b := (symin - symax)/(wymax - wymin);
  a1 := -wxmin*a + lsxmin;
  b1 := -wymin*b + symax;
  maxpoints:= trunc(runTime/stepSize);
  if sim_Ymax <0 then sim_Ymax:= 0;    // Do not reset ...

end;


procedure change_xorigin (wxmin, wxmax : integer);
begin
  a := GraphScrnWidth/(wxmax - wxmin);
  a1 := -wxmin*a + lsxmin;
end;

procedure resetGraph (canvas : TCanvas);
begin
  canvas.Brush.Color:=clWhite;
  canvas.Rectangle(0,0,GraphScrnWidth,CanvasHeight);
  endp := 0; startp := 1; Size := 0;
end;


procedure draw_Graph (canvas : TCanvas; x, y : DataType; Size, startp, endp : integer);
var i, j, xi, yi : integer;
begin
  { we must recompute x axis scaling as graph pans to left }
  if Size = MAXSIZE then
     change_xorigin (x[startp], x[endp])
  else
     change_xorigin (1, MAXSIZE);

  world_to_screen (x[startp], y[startp], xi, yi);
  canvas.moveto (xi, yi);
  j := startp+1; if j > MAXSIZE then j := 1;  { watch for overflow }
  { Note since we've done the first point there will only be Size-1 left to do }
  for i := 1 to Size-1 do
      begin
      world_to_screen (x[j], y[j], xi, yi);
      canvas.lineto (xi, yi);
      j := (j MOD Size) + 1;
      end;
end;


procedure draw_x_axis (const x : DataType; canvas : TCanvas; currTime: double);
var i, xi, yi, mxi, myi, minor, xstart, xend, scale, divMk, divMinor: integer;

begin
  divMk:= 20;
  divMinor:= 5;
  xstart := x[startp];
  { now draw x scale }
  canvas.pen.color := clBlack;
  world_to_screen (xstart, 0, xi, yi);    canvas.moveto (xi, yi);
  world_to_screen (xstart+MAXSIZE, 0, xi, yi); canvas.lineto (xi, yi);

  { major division interval = 20, minor division interval = 5 }
  xend  := xstart + MAXSIZE;
  scale := ((xstart div divMk) * divMk);   { place the first division at the easliest 20th mark on the graph}
  canvas.font.name := 'Courier New';
  canvas.font.Size := 8;

  while scale < xend do
        begin
        { major ticks }
        world_to_screen (scale, 0, xi, yi);   canvas.moveto (xi+1, yi);
        canvas.lineto (xi+1, yi+divMinor);  { use screen coord on y, it might be quicker }
        minor := scale + divMinor;
        for i := 1 to 3 do
            begin
            world_to_screen (minor, 0, mxi, myi);   canvas.moveto (mxi+1, myi);
            canvas.lineto (mxi+1, myi+3);  { use screen coord on y, it might be quicker }
            inc (minor, divMinor);
            end;

        //console.log('scale: ',scale);
        canvas.TextOut (xi+1, yi+6, floattostrf ((scale*step),ffGeneral,6,4));
        inc (scale, divMk);
        end;
end;

procedure draw_y_axis ( const x : DataType; canvas : TCanvas);
var i, xi, yi, mxi, myi, minor, xstart, ystart, yend, scale, divMk, divMinor: integer;
 // TODO: add tick marks
begin
  divMk:= 18;
  divMinor:= 2;
  ystart := yScaleHeight;
  xstart := x[startp];
  { now draw y scale }
  canvas.pen.color := clBlack;
  world_to_screen (xstart, ystart, xi, yi);    canvas.moveto (xi, yi+1);
 // world_to_screen (0, ystart+MAXSIZE, xi, yi); canvas.lineto (xi, yi);
 world_to_screen (xstart, yScaleHeight-scale, xi, yi); canvas.lineto (xi, yi+1);

  { major division inter val = 20, minor division interval = 5 }
  yend  := ystart + yaxisHt;
  scale := ((ystart div divMk) * divMk);   { place the first division at the easliest 20th mark on the graph}
  canvas.font.name := 'Courier New';
  canvas.font.Size := 8;
  //console.log('...scale:', scale,',  ',scale/yAxisHt);
  while scale < yAxisHt do
        begin
        { major ticks }
        world_to_screen (xstart, scale, xi, yi);   canvas.moveto (xi+1, yi);
        canvas.lineto (xi+divMinor, yi);  { use screen coord on y, it might be quicker? }
        //minor := scale + divMinor;
        //for i := 1 to 3 do
         //   begin
          //  world_to_screen (xstart, minor, mxi, myi);  canvas.moveto (mxi+1, myi);
           // canvas.lineto (mxi+3, mxi);  { use screen coord on y, it might be quicker }
            //inc (minor, divMinor);
         //   end;
        canvas.TextOut (xi+5, yi-4, floattostrf (((scale/yAxisHt)*sim_Ymax),ffGeneral,4,4));
        inc (scale, divMk);
        end;
end;

                    // time is currentGeneration
procedure addPoint (canvas : TCanvas; time: integer; y_curr : array of double;
                  plot_var: array of boolean; display : boolean; currTime:double);
var
  i: Integer;
  j: Integer;

begin
// Init size of y_vals if not already done:
  if length(y_vals) <> length(y_curr) then setLength(y_vals,length(y_curr));
  for i := 0 to length(y_curr)-1 do
    begin
      if sim_Ymax <y_curr[i] then sim_Ymax:= y_curr[i];
    end;
  { flood fill appears to be slighly quicker and eliminates the dots bug }
  if display then
     begin
     canvas.brush.color := clWhite;
     canvas.Rectangle(0,0,GraphScrnWidth-1,CanvasHeight-1);
     end;

  if Size = MAXSIZE then
     begin
     endp   := (endp MOD MAXSIZE) + 1;
     startp := (startp MOD MAXSIZE) + 1;
     end
  else
     begin
     inc (endp); inc (Size);
    // console.log('Size: ',Size);
     end;

  x1[endp]  := time;

  for i := 0 to Length(y_curr)-1 do
  begin
     y_vals[i][endp]:= round(normalizeY(y_curr[i],sim_yMax));
  end;

  if display then
     begin
     for j := 0 to Length(y_vals)-1 do
      begin
      if plot_var[j] then
        begin
          // TODO: if j > length(COLORS) then i:= j-
           if j<Length(COLORS) then canvas.pen.color := COLORS[j]    // assume plots <10
           else canvas.pen.color := COLORS[1];

          draw_graph (canvas, x1, y_vals[j], Size, startp, endp);
        end;
      end;

   //  draw_x_axis (x1, canvas);
     draw_x_axis (x1, canvas, currTime);
     draw_y_axis (x1, canvas);
     end;
end;

function normalizeY(y_new, y_max: double):double;
begin
  Result:= (y_new/y_max) *  yAxisHt; //(usable height, in pixels)
end;

procedure redrawGraph(canvas : TCanvas; plot_var: array of boolean; display : boolean);
var j: integer;
begin
  { flood fill appears to be slighly quicker and eliminates the dots bug }
  if display then
     begin
      canvas.brush.color := clWhite;
      canvas.Rectangle(0,0,GraphScrnWidth,CanvasHeight);

      for j := 0 to Length(y_vals)-1 do
      begin
      if plot_var[j] then
        begin
          // TODO: if j > length(COLORS) then i:= j-
          if j<Length(COLORS) then canvas.pen.Color:= COLORS[j]
          else canvas.pen.color := COLORS[1];  // or COLORS[j mod length(COLORS)]

          draw_graph (canvas, x1, y_vals[j], Size, startp, endp);
        end;
      end;

     
      draw_x_axis (x1, canvas,10);
     end;
end;




end.
