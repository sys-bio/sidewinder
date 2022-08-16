unit uWebContainer;

interface
uses
  System.Classes, System.UITypes, {System.UIConsts,} System.SysUtils, JS, Web,
  WebLib.Graphics, {Vcl.Graphics,} System.Types, uWebGlobalData, uScrollingTypes;

  type
    TOrientation = (horizontal, vertical);
    TPosition = (start, half, last);

    TContainer = class (TObject)
      FRoot: TObject;
      //fdata: TGlobalData;
      x, y, width, height: double;
      //scaleX, scaleY: double;
      parent: TObject;
      name: string;
      //childs: TObjectList;
     // color, backgroundColor : TAlphaColor;
      color, backgroundColor : TColor;
      lineWidth: single;
      constructor Create(w, h: double; P: TObject); virtual;

      procedure Clear;
      function getFRoot: TObject;
      function getGlobalPosition: TPointF;

      function getPlane: TPlaneXY;
      procedure setPlane(value: TPlaneXY);

      function getLegend: TLegend;
      procedure setLegend(value: TLegend);


      function getAGlobalData: TGlobalData;
      procedure setGlobalData(value: TGlobalData);
      property data: TGlobalData read getAGlobalData write setGlobalData;
      property FPlane: TPlaneXY read getPlane write setPlane;
      property legend: TLegend read getLegend write setLegend;


      property Root: TObject read getFRoot;
      procedure DrawRect(RF: TRectF; cl: TColor);
    //  procedure DrawRect(RF: TRect; cl: TColor);
  end;

  TMyLine = class (TContainer)
      forientation: TOrientation;
      fpos: TPosition;
      constructor Create(orientation: TOrientation; pos: TPosition; P: TObject); reintroduce; overload;
      procedure drawLine;
  end;

  TMask = class(TContainer)
      clientWindow: TContainer;
      constructor Create(w, h: double; client: TContainer; P: TObject); reintroduce; overload;
      procedure Draw;
  end;

  TMyText = class (TContainer)
      FText: String;
      hAlign: THAlign;
      vAlign: TVAlign;
      margin: Single;
      rotate: Boolean;
      P: TPointF;
      AB: TSizeF;
      fontName: String;
      fontSize: Integer;
      constructor Create(fName: String; Size: Integer; _hAlign: THAlign; _vAlign: TVAlign;_margin: Single; P: TObject); reintroduce; overload;
      procedure writeText;
      function MakeFromText(W, H: Integer): TPointF;
  end;

implementation

uses
  uWebScrollingChart;

constructor TMask.Create(w, h: double; client: TContainer; P: TObject);
begin
   parent := P;
   width := w;
   height := h;
   clientWindow := client;
end;

procedure TContainer.DrawRect(RF: TRectF; cl: TColor);
var
 // R: TRect;

begin
  data.Canvas.Brush.Style := bsSolid;
  data.Canvas.Brush.Color := cl;
  data.canvas.pen.Width := 1;
  data.canvas.pen.color := cl;
 // R := TRect.Create(TPoint.Create(Round(RF.TopLeft.X), Round(RF.TopLeft.Y)), Round(RF.Width), Round(RF.Height));
 // data.canvas.Rectangle(R);
  data.canvas.Rectangle(RF.getLocation.X, RF.getLocation.Y, RF.Width, RF.Height);
end;

procedure TMask.Draw;
var
  R: TRectF;
  topLeft: TPointF;
  P: TPointF;
begin

 // P := clientWindow.getGlobalPosition + TPointF.Create(x, y);
  P.x := clientWindow.getGlobalPosition.x + self.x;
  P.x := clientWindow.getGlobalPosition.y + self.y;
  //paint := TSkPaint.Create;
  //blender := TSkBlender.MakeMode(TSkBlendMode.Clear);
  //paint.Blender := blender;

  topLeft := TPointF.Create(0, 0);
  R := TRectF.Create(topLeft, data.chartWidth, P.y);
  DrawRect(R, data.BackgroundColor); // ??

  {topLeft := TPointF.Create(0, P.y + height);
  R := TRectF.Create(topLeft, data.chartWidth, data.chartHeight - P.y - height);
  DrawRect(R, data.BackgroundColor); }// right middle side rect, right of actual graph gris

  topLeft := TPointF.Create(0, P.y);
  R := TRectF.Create(topLeft, P.x, height);
  DrawRect(R, data.BackgroundColor); // Top, left rect, above y axis

 { topLeft := TPointF.Create(P.x + width, P.y);
  R := TRectF.Create(topLeft, data.chartWidth - P.x - width, height);
  DrawRect(R, data.BackgroundColor);} // top rect, above actual graph grid

end;

procedure TContainer.setGlobalData(value: TGlobalData);
var
  chart: TWebScrollingChart;
begin
  chart := Root as TWebScrollingChart;
  chart.globaldata := value;
end;

function TContainer.getFRoot: TObject;
var
  C: TContainer;
begin
  if FRoot = nil then
    begin
      FRoot := parent;
      while not (FRoot is TWebScrollingChart) do
        begin
          C := FRoot as TContainer;
          FRoot := C.parent;
        end;
    end;

  Result := FRoot;
end;

function TContainer.getLegend: TLegend;
var
  chart: TWebScrollingChart;
begin
  chart := Root as TWebScrollingChart;
  Result := chart.legend;
end;

procedure TContainer.setLegend(value: TLegend);
var
  chart: TWebScrollingChart;
begin
  chart := Root as TWebScrollingChart;
  chart.legend := value;
end;

function TContainer.getPlane: TPlaneXY;
var
  chart: TWebScrollingChart;
begin
  chart := Root as TWebScrollingChart;
  Result := chart.plane;
end;

procedure TContainer.setPlane(value: TPlaneXY);
var
  chart: TWebScrollingChart;
begin
  chart := Root as TWebScrollingChart;
  chart.plane := value;
end;

function TContainer.getAGlobalData: TGlobalData;
var
  chart: TWebScrollingChart;
begin
  chart := Root as TWebScrollingChart;
  chart.globalData.series := chart.series;
  chart.globaldata.autoScaleUp := chart.autoScaleUp;
  chart.globaldata.autoScaleDown := chart.autoScaleDown;
  Result := chart.globalData;
end;

function TContainer.getGlobalPosition: TPointF;
var
  P: TContainer;
  Q: TObject;
begin
  //Result := TPoint.Zero;
  Result.x := 0;
  Result.y := 0;

  Q := Parent;
  while Q is TContainer do
    begin
      P := Q as TContainer;
      //Result := Result + TPointF.Create(P.x, P.y);
      Result.x := Result.x + P.x;
      Result.y := Result.y + P.y;
      Q := P.parent;
    end;
end;

constructor TContainer.Create(w, h: double; P: TObject);
begin
   parent := P;
   width := w;
   height := h;
   backgroundColor := clWhite; //Default color
   x := 0;
   y := 0;
   color := clBlack;
   lineWidth := 1;
end;

procedure TContainer.Clear;
var
  R: TRectF;
  topLeft: TPointF;
begin
//  topLeft := getGlobalPosition + TPointF.Create(x, y);
  topLeft.x := getGlobalPosition.x + self.x;
  topLeft.y := getGlobalPosition.y + self.y;

 // R := TRectF.Create(topLeft, width, height);  ??
 // DrawRect(R, backgroundColor);    ??
end;

constructor TMyLine.Create(orientation: TOrientation; pos: TPosition; P: TObject);
begin
  parent := P;
  forientation := orientation;
  fpos := pos;
end;

procedure TMyLine.drawLine;
var
  origen, destino, P: TPointF;
begin
  P := getGlobalPosition;
  if (forientation = vertical ) and (fpos = half) then
    begin
       origen := TPointF.Create(x + width/2, y);
       destino := TPointF.Create(x + width/2, y + height);
    end
  else
  if (forientation = vertical ) and (fpos = start) then
    begin
      origen := TPointF.Create(x, y);
      destino := TPointF.Create(x, y + height);
    end
  else
  if (forientation = vertical ) and (fpos = last) then
    begin
      origen := TPointF.Create(x + width, y);
      destino := TPointF.Create(x + width, y + height);
    end
  else
  if (forientation = horizontal ) and (fpos = start) then
    begin
      origen := TPointF.Create(x, y);
      destino := TPointF.Create(x + width, y);
    end
  else
  if (forientation = horizontal ) and (fpos = half) then
    begin
      origen := TPointF.Create(x, y + height/2);
      destino := TPointF.Create(x + width, y + height/2);
    end
  else
  if (forientation = horizontal ) and (fpos = last) then
    begin
      origen := TPointF.Create(x, y + height);
      destino := TPointF.Create(x + width, y + height);
    end;

  //origen := P + origen;
  //destino := P + destino;
  origen.x := P.x + origen.x;
  origen.y := P.y + origen.y;
  destino.x := P.x + destino.x;
  destino.y := P.y + destino.y;

  data.canvas.pen.Color := color;
  data.canvas.Pen.Width := Round(lineWidth);
  data.canvas.MoveTo(Round(origen.X), Round(origen.Y));
  data.canvas.LineTo(Round(destino.X), Round(destino.Y));
end;

constructor TMyText.Create(fName: String; Size: Integer; _hAlign: THAlign; _vAlign: TVAlign;_margin: Single; P: TObject);
begin
  parent := P;
  fontName := fName;
  fontSize := Size;
  hAlign := _hAlign;
  vAlign := _vAlign;
 // backgroundColor := claAqua;
  backgroundColor := clAqua;
  rotate := false;
  FText := '';
 // color := claBlack;
  color := clBlack;
  margin := _margin;
end;

function TMyText.MakeFromText(W, H: Integer): TPointF;
var
  tx, ty: single;
begin
  tx := P.x + x;
  ty := P.y + y{ + ABounds.Height};

  if (hAlign = center) and (vAlign = top) then
    begin
       tx := tx + (width - W)/2;
       ty := ty + margin;
    end
  else
  if (hAlign = left) and (vAlign = top) then
    begin
       tx := tx + margin;
       ty := ty + margin;
    end
  else
  if (hAlign = right) and (vAlign = top) then
    begin
       tx := tx + width - W - margin;
       ty := ty + margin;
    end
  else
  if (hAlign = left) and (vAlign = middle) then
    begin
       tx := tx + margin;
       ty := ty + (height - H)/2;
    end
  else
  if (hAlign = center) and (vAlign = middle) then
    begin
       tx := tx + (width - W)/2;
       ty := ty + (height - H)/2;
    end
  else
  if (hAlign = right) and (vAlign = middle) then
    begin
       tx := tx + width - W - margin;
       ty := ty + (height - H)/2;
    end
  else
  if (hAlign = left) and (vAlign = bottom) then
    begin
       tx := tx + margin;
       ty := ty + height - margin;
    end
  else
  if (hAlign = center) and (vAlign = bottom) then
    begin
       tx := tx + (width - W)/2;
       ty := ty + height - margin;
    end
  else
  if (hAlign = right) and (vAlign = bottom) then
    begin
       tx := tx + width - W - margin;
       ty := ty + height - margin;
    end;

   Result.x := tx;
   Result.y := ty;
end;

procedure TMyText.writeText;
var
  t : TPointF;
begin
  P := getGlobalPosition;
  data.canvas.font.Name := fontName;
  data.canvas.font.Size := fontSize;
 // data.canvas.font.Quality := fqAntialiased;
  data.canvas.Font.Color := color;
  data.canvas.Font.Orientation := 0;
  data.canvas.Brush.color := data.BackgroundColor;
  data.canvas.Brush.Style := bsSolid;
  //AB := data.canvas.TextExtent(FText);
  AB.Width := data.canvas.TextExtent(FText).cx;
  AB.Height := data.canvas.TextExtent(FText).cy;

  if rotate then
    begin
      t := MakeFromText(round(AB.Height), round(AB.Width));
      data.canvas.Font.Orientation := 900;
      t.y := t.y + AB.Width;
      t.x := t.x - AB.Height;
    end
  else
    begin
      t := MakeFromText(round(AB.Width), round(AB.Height));
      data.canvas.Font.Orientation := 0;
    end;

  data.canvas.TextOut(Round(t.x), Round(t.y), FText);
end;


end.
