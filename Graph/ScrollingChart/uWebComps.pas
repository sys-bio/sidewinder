unit uWebComps;

interface
  uses                                               { maybe Generic.collections ?}
    Math, Classes, Types, System.UITypes, {System.UIConsts,} System.Contnrs, JS, Web, WebLib.Graphics,
    {Vcl.Graphics,} System.SysUtils, uWebContainer, uWebDataSource, uWebGlobalData,
    uScrollingTypes;

  type

  TBlockY = class (TContainer)
     tick: TMyLine;
     number: TMyText;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure resize;
  end;

  TAxisY = class (TContainer)
      hLine: TMyLine;
      block: TBlockY;
      constructor Create(w, h: double; P: TObject); override;
      destructor Destroy; override;
      procedure Draw;
      procedure Resize;
      procedure Reset;
  end;

  TGridAxisY = class (TContainer)
      hLine: TMyLine;
      zeroIndex: Integer;
   //   cl: TAlphaColor;
      cl: TColor;
      constructor Create(w, h: double; P: TObject); override;
      destructor Destroy; override;
      procedure Draw;
      function getZeroIndex: Integer;
      procedure resize;
      procedure Reset;
  end;

  TLeftBox = class (TContainer)
     axisY: TAxisY;
     labelY: TMyText;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure resize;
  end;

  TContBlocksX = class (TContainer)
     childs: TObjectList;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure addChild(posX, posY: double; index: Integer; child: TContainer);
     procedure removeChild(index: Integer);
     procedure  eraseAll;
     procedure Draw;
  end;

  TGridXContainer = class (TContainer)
     childs: TObjectList;
     constructor Create(w, h: double; P: TObject); override;
     procedure addChild(posX, posY: double; child: TContainer);
     procedure removeChild(index: Integer);
     procedure  eraseAll;
     procedure Draw;
     destructor Destroy; override;
  end;

  TBlockX = class (TContainer)
     tick: TMyLine;
     number: TMyText;
     life, index: Integer;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure resize;
  end;

  TAxisX = class (TContainer)
      hLine: TMyLine;
      block: TBlockX;
      blocksContainer: TContBlocksX;
      offsetX: double;
      dtbyTick, widthBlock: double;
      rBlack, lBlack: TContainer;
      u: TPoint;
      dir: byte;
      life: Integer;
      constructor Create(w, h: double; P: TObject); override;
      procedure reset;
      destructor Destroy; override;
      procedure Draw;
      procedure resize;
      procedure update;
      procedure initBlocks;
  end;

  TBottomBox = class (TContainer)
     axisX: TAxisX;
     labelX: TMyText;
     constructor Create(w, h: double; P: TObject); override;
     procedure Draw;
     destructor Destroy; override;
     procedure resize;
  end;

  TLegendBox = class (TContainer)
     pivot, offSet: TPointF;
     box: TRect;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure draw;
     procedure resize;
  end;

  TTitleBox = class (TContainer)
     title: TTitle;
     pivotF, offSet: TPointF;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     //procedure resize;
  end;


  TMyBlockLine = class(TMyline)
    index, life: Integer;
  end;

  TGridBox = class (TContainer)
     const
       a: Single = 0.05;
     var
       block: TMyBlockLine;
       offsetX: double;
       dtbyTick, widthBlock: double;
       gridXContainer: TGridXContainer;
       life: Integer;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     procedure resize;
     procedure initBlocks;
     destructor Destroy; override;
     procedure update;
     procedure reset;
  end;

  TGraph = class (TContainer)
     FOnComplete: TOnCompleteEvent;
     scale, globalPos: TPointF;
     mask: TMask;
     legendBox: TLegendBox;
     gridX: TGridBox;
     gridY: TGridAxisY;
     xMouse, yMouse: Single;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     procedure resize;
     function calculateScale: TPointF;
     function realtoScreen(x_w, y_w: double): TPointF;
     function ScreenToReal(locX, locY: double): TPointF; //Real
     destructor Destroy; override;
     function isInside(globalX, globalY: Single): Boolean;
     function worldCoordinates(globalX, globalY: Single): TPointF;
     procedure scanXY(globalX, globalY: Single);
     procedure BorderDraw;
  end;

  TRightBox = class (TContainer)
     graph: TGraph;
     bottomBox: TBottomBox;
     constructor Create(w, h: double; P: TObject); override;
     procedure Draw;
     destructor Destroy; override;
     procedure resize;
  end;


implementation

constructor TGridAxisY.Create(w, h: double; P: TObject);
begin
  parent := P;
  width := w;
  height := h;

  hLine := TMyLine.Create(horizontal, start, self);
  hLine.x := 0;
  hLine.y := 0;
  hLine.width := width;
  hLine.height := height/FPlane.yAxis.maxTicks;
end;

function TGridAxisY.getZeroIndex: Integer;
var
  i: Integer;
  val, delta, nicenum: double;
  plane: TPlaneXY;
begin
  plane := FPlane;
  delta := plane.height/FPlane.yAxis.maxTicks;
  val := plane.y + plane.height;
  for i := 0 to FPlane.yAxis.maxTicks do
    begin
      nicenum := StrToFloat(plane.niceNumY(val));
      if IsZero(nicenum) then
        begin
          Result := i;
          Exit;
        end;
      val := val - delta;
    end;
  Result := -1;
end;

procedure TGridAxisY.resize;
begin
  hLine.width := width;
  hLine.height := height/FPlane.yAxis.maxTicks;
end;

procedure TGridAxisY.Reset;
begin
  Resize;
  Draw;
end;

procedure TGridAxisY.Draw;
var
  i: Integer;
  dy: double;
begin

  //cl := MakeColor(FPlane.grid.color, 0.90);  // No MakeColor for TMS js
  cl := FPlane.grid.color;
  dy := 0;
  zeroIndex := getZeroIndex;
  for i := 0 to FPlane.yAxis.maxTicks do
    begin
       hLine.x := 0;
       hLine.y := dy;
       if zeroIndex = i then
         begin
           hLine.lineWidth := FPlane.yAxis.lineWidth;
           hLine.color := FPlane.yAxis.color;
         end
       else
         begin
           hLine.lineWidth := FPlane.grid.lineWidth;
           hLine.color := cl;
         end;

       hLine.drawLine;
       dy := dy  + hLine.height;
    end;
end;

destructor TGridAxisY.Destroy;
begin
   hLine.Destroy;
end;

constructor TGridXContainer.Create(w, h: double; P: TObject);
begin
  parent := P;
  childs := TObjectList.Create;
  childs.OwnsObjects := false;
end;

procedure TGridXContainer.addChild(posX, posY: double; child: TContainer);
begin
   child.x := posX;
   child.y := posY;
   childs.Add(child);
end;

procedure TGridXContainer.removeChild(index: Integer);
var
  block: TMyBlockLine;
begin
  block := childs[index] as TMyBlockLine;
  block.Destroy;
  childs.Delete(index);
end;

procedure TGridXContainer.eraseAll;
var
  i: Integer;
begin
  for i := childs.Count - 1 downto 0 do removeChild(i);
end;

procedure TGridXContainer.Draw;
var
  i: Integer;
  block: TMyBlockLine;
  mcl: TColor;
begin
 // mcl := MakeColor(FPlane.grid.color, 0.90);
  mcl := FPlane.grid.color;
  //console.log(' +++ TGridXContainer.Draw ..');
  for i := 0 to childs.Count - 1 do
    begin
      block := childs[i] as TMyBlockLine;
      //block.clear;
      block.color := mcl;
      block.lineWidth := FPlane.grid.lineWidth;
      block.DrawLine;
    end;
end;

destructor TGridXContainer.Destroy;
begin
  eraseAll;
  childs.Free;
end;

procedure TContBlocksX.addChild(posX, posY: double; index: Integer; child: TContainer);
begin
   child.x := posX;
   child.y := posY;
   if index = 0 then
     childs.Insert(0, child)
   else
     childs.Add(child);
end;

procedure TContBlocksX.removeChild(index: Integer);
var
  block: TBlockX;
begin
  block := childs[index] as TBlockX;
  block.Destroy;
  childs.Delete(index);
end;

constructor TContBlocksX.Create(w, h: double; P: TObject);
begin
  parent := P;
  childs := TObjectList.Create;
  childs.OwnsObjects := false;
end;

procedure TContBlocksX.Draw;
var
  i: Integer;
  block: TBlockX;
begin

  for i := 0 to childs.Count - 1 do
    begin
      block := childs[i] as TBlockX;
      block.tick.color := Fplane.xAxis.color;
      block.tick.lineWidth := Fplane.xAxis.lineWidth;
      block.number.color := Fplane.xAxis.color;
      block.Draw;
    end;

end;

destructor TContBlocksX.Destroy;
begin
  eraseAll;
  childs.Free;
end;

procedure  TContBlocksX.eraseAll;
var
  i: Integer;
begin
  for i := childs.Count - 1 downto 0 do removeChild(i);
end;


constructor TLeftBox.Create(w, h: double; P: TObject);
var
  yAxis: TInfoAxis;
begin
  parent := P;
  yAxis := FPlane.yAxis;
  width := w;
  height := h;

  axisY := TAxisY.Create(w/2, h - TConst.HEIGHT_X_AXIS, self);
  //axisY.backgroundColor := claWhite;
  axisY.backgroundColor := clWhite;
  axisY.x := w/2;
  axisY.y := 0;


  labelY := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, left, middle, 0, self);
  labelY.width := w/2;
  labelY.height := h - TConst.HEIGHT_X_AXIS;
  labelY.x := 0;
  labelY.y := 0;
  labelY.color := yAxis.color;
  labelY.FText := yAxis.caption;
  labelY.rotate := true;
end;

procedure TLeftBox.resize;
begin
  axisY.width := width/2;
  axisY.height := height - TConst.HEIGHT_X_AXIS;
  axisY.x := width/2;
  axisY.y := 0;

  axisY.resize;

  labelY.width := width/2;
  labelY.height := height - TConst.HEIGHT_X_AXIS;
  labelY.x := 0;
  labelY.y := 0;
end;

destructor TLeftBox.Destroy;
begin
   axisY.Destroy;
   labelY.Destroy;
end;

procedure TLeftBox.Draw;
begin
   //clear;
   console.log(' ++++ TLeftBox.Draw ..');
   axisY.Draw;
   labelY.color := FPlane.yAxis.color;
   labelY.FText := FPlane.yAxis.caption;
   labelY.writeText;
end;

constructor TAxisY.Create(w, h: double; P: TObject);
begin
   parent := P;
   width := w;
   height := h;

   hLine := TMyLine.Create(vertical, last, self);
   hLine.width := TConst.LENGTH_BOX_TICK_Y;
   hLine.height := h;
  // hLine.backgroundColor := claYellow;
   hLine.backgroundColor := clYellow;
   hLine.color := FPlane.yAxis.color;

   hLine.lineWidth := FPlane.yAxis.lineWidth;
   hLine.x := w - hLine.width;
   hLine.y := 0;


   block := TBlockY.Create(w, h/FPlane.yAxis.maxTicks, self);
   block.width := w;
   block.height := h/FPlane.yAxis.maxTicks;
   block.x := hLine.x + hLine.width - block.width;
end;

procedure TAxisY.Reset;
begin
  Resize;
  Draw;
end;

procedure TAxisY.resize;
begin
   hLine.width := TConst.LENGTH_BOX_TICK_Y;
   hLine.height := height;
   hLine.x := width - hLine.width;
   hLine.y := 0;

   block.width := width;
   block.height := height/FPlane.yAxis.maxTicks;
   block.x := hLine.x + hLine.width - block.width;

   block.resize;
end;

procedure TAxisY.Draw;
var
  i: Integer;
  val, delta, dy: double;
  plane: TPlaneXY;
begin
 // hLine.Clear;
  plane := FPlane;
  console.log(' ++ TAxisY.Draw ..');
  delta := plane.height/FPlane.yAxis.maxTicks;
  val := plane.y + plane.height;
  dy := -block.height/2;

  for i := 0 to FPlane.yAxis.maxTicks do
    begin
       block.x := 0;
       block.y := dy;
     //  block.backgroundColor := claBlanchedalmond;
       block.backgroundColor := clCream;
       block.number.FText := plane.niceNumY(val);

       //block.Clear;
       block.tick.lineWidth := plane.yAxis.lineWidth;
       block.tick.color := plane.yAxis.color;
       block.number.color := plane.yAxis.color;

       block.Draw;

       val := val - delta;
       dy := dy  + block.height;
    end;

  hLine.color := plane.yAxis.color;
  hLine.lineWidth := plane.yAxis.lineWidth;
  hLine.drawLine;
end;

destructor TAxisY.Destroy;
begin
   hLine.Destroy;
   block.Destroy;
end;

constructor TBlockY.Create(w, h: double; P: TObject);
begin
   parent := P;
   width := w;
   height := h;

   tick := TMyLine.Create(horizontal, half, self);
   tick.width := w*TConst.tickPercentAxisY;
   tick.height := h;
   tick.x := w - tick.width;
   tick.y := 0;
   tick.color := FPlane.yAxis.color;
   tick.lineWidth := FPlane.yAxis.lineWidth;
  // tick.backgroundColor := claWhite;
   tick.backgroundColor := clWhite;

   number := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, right, middle, TConst.MARGIN_FONT, self);
   number.width := w - tick.width;
   number.height := h;
   number.x := 0;
   number.y := 0;
   number.color := tick.color;
end;

procedure TBlockY.resize;
begin
   tick.width := width*TConst.tickPercentAxisY;
   tick.height := height;
   tick.x := width - tick.width;
   tick.y := 0;

   number.width := width - tick.width;
   number.height := height;
   number.x := 0;
   number.y := 0;
end;

procedure TBlockY.Draw;
begin
  tick.drawLine;
  number.writeText;
end;

destructor TBlockY.Destroy;
begin
   tick.destroy;
   number.destroy;
end;

constructor TTitleBox.Create(w, h: double; P: TObject);
begin
  parent := P;
  width := w;
  height := h;
  title := data.title;
  backgroundColor := title.backgroundColor;
end;

procedure TTitleBox.Draw;
var
  P, t: TPointF;
  ref: TPivot;
  ABounds: TSize;
  fABounds: TSizeF;
begin
  title := data.title;
  console.log(' *++ TTitleBox.Draw ..');
  P := getGlobalPosition;
  data.canvas.Font.Name := title.fontName;
  data.canvas.Font.Size := title.fontSize;
  data.canvas.Font.Color := title.fontcolor;
  data.canvas.Font.Orientation := 0;
 // data.canvas.Font.Quality := fqAntialiased;  // ??
  data.canvas.Brush.Style := bsClear;
  //ABounds := data.canvas.TextExtent(title.text);
  fABounds.cx := data.canvas.TextExtent(title.text).cx;
  fABounds.cy := data.canvas.TextExtent(title.text).cy;
  ABounds.cx := round(fABounds.cx);
  ABounds.cy := round(fABounds.cy);

 // height := title.pad + ABounds.Height + title.pad;
  self.height := title.pad + fABounds.Height + title.pad;
  ref.horizontal := title.align;
  ref.vertical := middle;

  offSet := data.getPivot(width, height, ref);
 // pivotF := data.getPivot(ABounds.Width, ABounds.Height, ref);
  pivotF := data.getPivot(fABounds.Width, fABounds.Height, ref);

  backgroundColor := title.backgroundColor;
  //clear;
 // t := P + offSet - pivotF;
  t.x:= P.x + offSet.x - pivotF.x;
  t.y:= P.x + offSet.y - pivotF.y;
  data.canvas.TextOut(Round(t.x), Round(t.y{ + ABounds.Height}), title.text);
end;


constructor TLegendBox.Create(w, h: double; P: TObject);
begin
  parent := P;
  width := w;
  height := h;
  backgroundColor := legend.backgroundColor;
  offSet := data.getPivot(width, height, legend.reference);
end;

destructor TLegendBox.Destroy;
begin
  inherited;
end;

procedure TLegendBox.resize;
begin
  offSet := data.getPivot(width, height, legend.reference);
end;

procedure TLegendBox.draw;
 var
   d: TGlobalData;
   w, h, tx, ty, widthBox, heightBox: double;
   i, n: Integer;
   P, topLeft: TPointF;
  // tempPt: TPoint;
   ABounds: TSize;
   fABounds: TSizeF;
   leg: TLegend;
 begin
   d := data;
   leg := legend;
   w := 0;
   h := 0;
   P := getGlobalPosition;
   n := 0;

   d.canvas.font.Name := leg.fontName;
   d.canvas.font.Size := leg.fontSize;
  // d.canvas.font.Quality := fqAntialiased;
   d.canvas.Font.Color := leg.fontcolor;
   d.canvas.Font.Orientation := 0;
   d.canvas.Brush.Style := bsClear;

   for i := 0 to length(d.series) - 1 do
     if d.series[i].visible then
       begin
         //ABounds := data.canvas.TextExtent(d.series[i].name);
         //if ABounds.Width > w then w := ABounds.Width;
         //h := h + ABounds.Height;
         fABounds.cx := data.canvas.TextExtent(d.series[i].name).cx;
         fABounds.cy := data.canvas.TextExtent(d.series[i].name).cy;
         if fABounds.Width > w then w := fABounds.Width;
         h := h + fABounds.Height;
         n := n + 1;
       end;

   widthBox := leg.pad + w + leg.space + leg.widthLine + leg.pad;
   heightBox := leg.pad + h + (n - 1)*leg.gap + leg.pad;
   if heightBox < 2*leg.pad then heightBox := 2*leg.pad;

   offSet := data.getPivot(width, height, leg.reference);
   pivot := data.getPivot(widthBox, heightBox, leg.pivot);
   console.log('LegendBox.draw, width, height: ', widthBox,', ', heightBox);

//   topLeft := P + offSet - pivot + TPointF.Create(leg.x, leg.y);
   topLeft := P.Add( offSet.Subtract( pivot.Add(TPointF.Create(leg.x, leg.y))) );

   d.canvas.Brush.Style := bsSolid;
   //d.canvas.Brush.Color := MakeColor(leg.backgroundColor, 0.80);
   d.canvas.Brush.Color := leg.backgroundColor;
   d.canvas.Pen.Width := 1;
   d.canvas.Pen.Color := d.canvas.Brush.Color;

   // self.box := TRect.Create(TPoint.Create(Round(topLeft.X), Round(topLeft.Y)), Round(widthBox), Round(heightBox));
   //d.canvas.Rectangle(self.box);
   d.canvas.Rectangle(topLeft.X, topLeft.Y, topLeft.X + widthBox, topLeft.Y + heightBox);

 //  tx := box.topLeft.X + leg.pad;
 //  ty := box.topLeft.Y + leg.pad;
   tx := topLeft.X + leg.pad;
   ty := topLeft.Y + leg.pad;


   for i := 0 to length(d.series) - 1 do
     if d.series[i].visible then
       begin
  //       ABounds := data.canvas.TextExtent(d.series[i].name);
         fABounds.cx := data.canvas.TextExtent(d.series[i].name).cx;
         fABounds.cy := data.canvas.TextExtent(d.series[i].name).cy;
         d.canvas.TextOut(Round(tx), Round(ty), d.series[i].name);
         d.canvas.Pen.Width := 2;
         d.canvas.Pen.Color := d.series[i].color;
         d.canvas.MoveTo(Round(tx + w + leg.space), Round(ty + fABounds.Height/2));
         d.canvas.LineTo(Round(tx + w + leg.space + leg.widthLine), Round(ty + fABounds.Height/2));
         ty := ty + fABounds.Height + leg.gap;
       end;
end;

constructor TGridBox.Create(w, h: double; P: TObject);
begin
   parent := P;
   width := w;
   height := h;
   gridXContainer := TGridXContainer.Create(w, h, self);
   initBlocks;
end;

procedure TGridBox.initBlocks;
var
  i: Integer;
  //val: double;
  plane: TPlaneXY;
  mcl: TColor;
begin
  plane := FPlane;
  //maxTicks := plane.xAxis.maxTicks;
  offsetX := 0;
  widthBlock := width/plane.xAxis.maxTicks;
  //val := plane.x;
  dtbyTick := plane.width/plane.xAxis.maxTicks;
  life := round(dtbyTick/plane.DeltaX);

  mcl := FPlane.grid.color;

  for i := 0 to plane.xAxis.maxTicks do
     begin
       block := TMyBlockLine.Create(TOrientation.vertical, TPosition.half, gridXContainer);
       block.width := widthBlock;
       block.height := height;
       if i = 0 then block.life := Round(0.5*life) else block.life := life;
       block.index := i;
       //block.backgroundColor := data.plotPanelBackgroundColor;
       block.color := mcl;
       block.lineWidth := plane.grid.lineWidth;
       gridXContainer.addChild(block.width*i, 0, block);
     end;

   gridXContainer.x := -widthBlock/2 - offsetX;
   gridXContainer.y := 0;
end;

 procedure TGridBox.draw;
 begin
   console.log(' **++ TGridBox.Draw ++**');
   widthBlock := width/fplane.xAxis.maxTicks;
   gridXContainer.x := -widthBlock/2 - offsetX;
   gridXContainer.Draw;
 end;

 procedure TGridBox.resize;
 var
   i: Integer;
 begin
   offsetX := (fplane.X*width)/fplane.width;

   widthBlock := width/fplane.xAxis.maxTicks;
   gridXContainer.x := -widthBlock/2 - offsetX;
   gridXContainer.y := 0;
   for i := 0 to gridXContainer.childs.Count - 1 do
    begin
      block := gridXContainer.childs[i] as TMyBlockLine;
      block.width := widthBlock;
      block.height := height;
      block.x := block.width*block.index;
      block.y := 0;
      //block.resize;
    end;
end;


procedure TGridBox.reset;
begin
  gridXContainer.eraseAll;
  initBlocks;
end;

procedure TGridBox.update;
var
  b, last: TMyBlockLine;
  plane: TPlaneXY;
  mcl: TColor;
begin
  plane := FPlane;
  offsetX := (plane.X*width)/plane.width;
  b := gridXContainer.childs[0] as TMyBlockLine;
  b.life := b.life - 1;
  mcl := FPlane.grid.color;

  if b.life = 1 then
    begin
      widthBlock := width/plane.xAxis.maxTicks;
      b := TMyBlockLine.Create(TOrientation.vertical, TPosition.half, gridXContainer);
      b.width := widthBlock;
      b.height := height;
      b.life := life;
      last := gridXContainer.childs[gridXContainer.childs.Count - 1] as TMyBlockLine;
      b.index := last.index + 1;
     // b.backgroundColor := data.plotPanelBackgroundColor;
      b.color := mcl;
      b.lineWidth := plane.grid.lineWidth;
      gridXContainer.addChild(b.width*b.index, 0, b);
    end
  else if b.life = 0 then
    begin
      gridXContainer.removeChild(0);
    end;
end;

destructor TGridBox.Destroy;
begin
  gridXContainer.Destroy;
end;


destructor TGraph.Destroy;
begin
  gridX.Destroy;
  gridY.Destroy;
  mask.Destroy;
  legendBox.Destroy;
end;

constructor TGraph.Create(w, h: double; P: TObject);
begin
  parent := P;
  width := w;
  height := h;
  console.log('TGraph.create, width, height: ', width,', ', height);
  mask := TMask.Create(w, h, self, self);
  mask.x := 0;
  mask.y := 0;

  legendBox := TLegendBox.Create(w, h, self);
  legendBox.x := 0;
  legendBox.y := 0;

  gridX := TGridBox.Create(w, h, self);
  gridX.x := 0;
  gridX.y := 0;

  gridY := TGridAxisY.Create(w, h, self);
  gridY.x := 0;
  gridY.y := 0;
end;

procedure TGraph.resize;
begin
  mask.x := 0;
  mask.y := 0;
  mask.width := width;
  mask.height := height;
  console.log('**TGraph.resize, mask width, height: ', mask.width,', ', mask.height);
  legendBox.x := 0;
  legendBox.y := 0;
  legendBox.width := width;
  legendBox.height := height;
  legendBox.resize;

  gridX.x := 0;
  gridX.y := 0;
  gridX.width := width;
  gridX.height := height;
  gridX.resize;


  gridY.x := 0;
  gridY.y := 0;
  gridY.width := width;
  gridY.height := height;
  gridY.resize;

  scale := calculateScale;
  globalPos := getGlobalPosition;
   console.log('TGraph.resize, width, height: ', width,', ', height);
end;

function TGraph.RealToScreen(x_w, y_w: double): TPointF; //Pixels
begin
  Result.x := x + (x_w - FPlane.X)/scale.X;
  Result.y := height - (y + (y_w - FPlane.Y)/scale.y);
  Result.x := globalPos.x + Result.x;
  Result.y := globalPos.y + Result.y;
 // Result := globalPos + Result;
end;

function TGraph.ScreenToReal(locX, locY: double): TPointF; //Real
begin
  Result.x := locX*scale.X + FPlane.x;
  Result.y := (Height - locY)*scale.Y + FPlane.y;
end;

function TGraph.calculateScale: TPointF;
begin
  result.x := FPlane.width / width;
  result.y := FPlane.Height / height;
end;

function TGraph.isInside(globalX, globalY: Single): Boolean;
begin
   globalPos := getGlobalPosition;
   console.log('TGraph.isInside: creating TRectF...');
   Result := TRectF.Create(globalPos, width, height).Contains(TPointF.Create(globalX, globalY));
end;

function TGraph.worldCoordinates(globalX, globalY: Single): TPointF;
begin
  scale := calculateScale;
  Result := ScreenToReal(globalX - globalPos.X, globalY - globalPos.Y);
end;

procedure TGraph.scanXY(globalX, globalY: Single);
begin
  xMouse := globalX;
  yMouse := globalY;
end;

procedure TGraph.BorderDraw;
var
//  R: TRect;
  R: TRectF;
begin
  data.Canvas.Brush.Style := bsClear;
  data.canvas.pen.Width := Round(FPlane.yAxis.lineWidth);
  data.canvas.pen.color := FPlane.yAxis.color;
  console.log('TGraph.BorderDraw, calling TrectF.create');
  R := TRectF.Create(TPointf.Create(globalPos.X, globalPos.Y), width, height);
  data.canvas.MoveTo(round(R.getLocation.X), round(R.getLocation.Y));
//  data.canvas.LineTo(R.Location.X + R.Width, R.Location.Y);
//  data.canvas.LineTo(R.Location.X + R.Width, R.Location.Y + R.Height);
  data.canvas.LineTo( round(R.getLocation.X + R.Width), round(R.getLocation.Y) );
  data.canvas.LineTo( round(R.getLocation.X + R.Width), round(R.getLocation.Y + R.Height) );
  //data.canvas.Rectangle(R);
end;

procedure TGraph.draw;
var
  i, j: Integer;
//  Q: PDataCol;
 // D: PData;
  Q: TDataCol;
  D: TData;
  serie: TDataSerie;
  pt: TPointF;
  temp: TGlobalData;
begin
  temp := data;
  clear;

  gridX.draw;
  gridY.draw;  // X, Y grids are visible
  console.log(' **** TGraph.Draw ****');
  //if temp.dataSource.cols.Count < 2 then Exit;

  scale := calculateScale;
  globalPos := getGlobalPosition;

//  for i := 0 to length(temp.dataSource.cols) - 1 do
  for i := 0 to temp.dataSource.cols.Count - 1 do
    begin
      Q := temp.dataSource.cols[i];
      //for j := 0 to length(Q^.rows) - 1 do
      for j := 0 to Q.rows.Count - 1 do
        begin
         // D := Q^.rows[j];
         // serie := D^.serie;
          D := Q.rows[j];
          serie := D.serie;
          if (serie.visible) and (serie.count >= 2) then
            begin
              //pt := RealToScreen(Q^.x, D^.y);
              pt := RealToScreen(Q.x, D.y);
              if serie.fromPoint.X = -1 then
                begin
                  serie.fromPoint.X := round(pt.X);
                  serie.fromPoint.Y := round(pt.Y);
                end
              else
                begin
                  temp.canvas.Pen.Color := serie.color;
                  temp.canvas.Pen.Width := Round(serie.lineWidth);
                  temp.canvas.MoveTo(serie.fromPoint.X, serie.fromPoint.Y);
                  serie.fromPoint.X := round(pt.X);
                  serie.fromPoint.Y := round(pt.Y);
                  temp.canvas.LineTo(serie.fromPoint.X, serie.fromPoint.Y);
                end;
            end;
        end;
    end;


  for j := 0 to length(temp.series) - 1 do
    begin
      serie := temp.series[j];
      if (serie.visible) and (serie.count >= 2) then serie.fromPoint.X := -1;
    end;

   mask.Draw; // This causes background color to cover grid lines.....

   if legend.visible then legendBox.draw;

   BorderDraw;

   if Assigned(FOnComplete) then
     begin
       pt := ScreenToReal(xMouse - globalPos.X, yMouse - globalPos.Y);
       FOnComplete(pt.X, pt.Y);
     end;

end;

constructor TBottomBox.Create(w, h: double; P: TObject);
var
  xAxis: TInfoAxis;
begin
  parent := P;
  xAxis := FPlane.xAxis;
  width := w;
  height := h;

  axisX := TAxisX.Create(w, h/2, self);
  axisX.backgroundColor := data.BackgroundColor;


  axisX.x := 0;
  axisX.y := 0;

  labelX := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, center, bottom, TConst.MARGIN_FONT, self);
  labelX.width := w;
  labelX.height := h/2;
  labelX.x := 0;
  labelX.y := axisX.height;
  labelX.color := xAxis.color;
  labelX.FText := xAxis.caption;
end;

procedure TBottomBox.resize;
begin
  axisX.width := width;
  axisX.height := height/2;
  axisX.x := 0;
  axisX.y := 0;
  axisX.resize;

  labelX.width := width;
  labelX.height := height/2;
  labelX.x := 0;
  labelX.y := axisX.height;
end;

destructor TBottomBox.Destroy;
begin
  axisX.Destroy;
  labelX.Destroy;
end;

procedure TBottomBox.Draw;
begin
  // Clear; // clears plot area as well  ??
   console.log(' ++** TBottomBox.Draw ..');
   //axisX.backgroundColor := data.BackgroundColor;
   axisX.Draw;
   labelX.color := FPlane.xAxis.color;
   labelX.FText := FPlane.xAxis.caption;
   labelX.writeText;
end;

procedure TAxisX.update;
var
  b, last: TBlockX;
  v: TPoint;
begin
  v := data.switch([0, blocksContainer.childs.Count - 1], dir);
  u := data.switch([1, -1], dir);

  offsetX := u.Y*(FPlane.X*width)/FPlane.width;

  b := blocksContainer.childs[v.X] as TBlockX;
  b.life := b.life - 1;

  if b.life = 1 then
    begin
      b := TBlockX.Create(width/FPlane.xAxis.maxTicks, height, blocksContainer);
      b.life := life;
      last := blocksContainer.childs[v.Y] as TBlockX;
      b.index := last.index + u.X;
    //  b.backgroundColor := claBlanchedalmond;
      b.backgroundColor := clCream;
      b.color := hLine.color;
      b.number.FText := fplane.NiceNumX(b.index*dtbyTick);
      blocksContainer.addChild(b.width*b.index, 0, v.Y, b);
    end
  else if b.life = 0 then
    begin
      blocksContainer.removeChild(v.X);
    end;
end;

procedure TAxisX.Draw;
var
  widthBlock: double;
begin
  //clear;
  console.log(' $ TAxisX.Draw $');
  hLine.color := FPlane.xAxis.color;
  hLine.lineWidth := FPlane.xAxis.lineWidth;
  hLine.drawLine;
  widthBlock := width/FPlane.xAxis.maxTicks;
  blocksContainer.x := -widthBlock/2 + offsetX;
  blocksContainer.Draw;

  rBlack.backgroundColor := data.BackgroundColor;
  lBlack.backgroundColor := data.BackgroundColor;
  rBlack.clear;
  LBlack.clear;
end;

procedure TAxisX.reset;
begin
  blocksContainer.eraseAll;
  initBlocks;
end;

procedure TAxisX.initBlocks;
var
  i: Integer;
  val: double;
begin
  u := data.switch([1, -1], dir);
  offsetX := 0;

  widthBlock := width/FPlane.xAxis.maxTicks;
  val := FPlane.x;
  dtbyTick := FPlane.width/FPlane.xAxis.maxTicks;
  life := round(dtbyTick/FPlane.DeltaX);

  for i := 0 to FPlane.xAxis.maxTicks do
     begin
       block := TBlockX.Create(widthBlock, height, blocksContainer);

       if i = 0 then block.life := Round(0.5*life) else block.life := life;
       block.index := i;
//       block.backgroundColor := claBlanchedalmond;
       block.backgroundColor := clCream;
       block.color := hLine.color;

       block.number.FText := fplane.niceNumX(val);
       val := val + dtbyTick;
       blocksContainer.addChild(block.width*i, 0, i, block);
     end;

   blocksContainer.x := -widthBlock/2 + offsetX;
   blocksContainer.y := 0;
end;

constructor TAxisX.Create(w, h: double; P: TObject);
begin
   parent := P;
   dir := 0;

   width := w;
   height := h;
  // maxTicks := FPlane.xAxis.maxTicks;

   hLine := TMyLine.Create(horizontal, start, self);
   hLine.width := w;
   hLine.height := TConst.LENGTH_BOX_TICK_X;
   hLine.parent := self;

 //  hLine.backgroundColor := claYellow;
   hLine.backgroundColor := clYellow;
   hLine.color := FPlane.xAxis.color;

   hLine.lineWidth := FPlane.XAxis.lineWidth;
   hLine.x := 0;
   hLine.y := 0;

   blocksContainer := TContBlocksX.Create(w, h, self);
   blocksContainer.parent := self;

   initBlocks;


   rBlack := TContainer.Create(TConst.MARGIN_X, h*2 + TConst.MARGIN_Y, self);
   rBlack.backgroundColor := data.BackgroundColor;
   rBlack.y := 0;
   rBlack.x := width + TConst.FONT_SIZE;


   LBlack := TContainer.Create(TConst.MARGIN_X + TConst.WIDTH_Y_AXIS, h*2 + TConst.MARGIN_Y, self);
   LBlack.backgroundColor := data.BackgroundColor;
   LBlack.y := 0;
   LBlack.x := -LBlack.width - TConst.FONT_SIZE;

end;

procedure TAxisX.resize;
var
  i: Integer;
begin
   u := data.switch([1, -1], dir);
   offsetX := u.Y*(FPlane.X*width)/FPlane.width;


   hLine.width := width;
   hLine.height := TConst.LENGTH_BOX_TICK_X;

   widthBlock := width/FPlane.xAxis.maxTicks;
   blocksContainer.x := -widthBlock/2 + offsetX;
   blocksContainer.y := 0;


   for i := 0 to blocksContainer.childs.Count - 1 do
    begin
      block := blocksContainer.childs[i] as TBlockX;
      block.width := widthBlock;
      block.height := height;
      block.x := block.width*block.index;
      block.y := 0;
      block.resize;
    end;


   rBlack.y := 0;
   rBlack.x := width + TConst.FONT_SIZE;
   rBlack.width := TConst.MARGIN_X;
   rBlack.height := height*2 + TConst.MARGIN_y;


   LBlack.width := TConst.MARGIN_X + TConst.WIDTH_Y_AXIS;
   LBlack.height := height*2 + TConst.MARGIN_Y;

   LBlack.y := 0;
   LBlack.x := -LBlack.width - TConst.FONT_SIZE;

end;

destructor TAxisX.Destroy;
begin
   hLine.Destroy;
   blocksContainer.Destroy;
   rBlack.Destroy;
   LBlack.Destroy;
end;

constructor TBlockX.Create(w, h: double; P: TObject);
begin
   parent := P;

   width := w;
   height := h;

   tick := TMyLine.Create(vertical, half, self);
   tick.width := w;
   tick.height := TConst.tickPercentAxisX*h;
   tick.x := 0;
   tick.y := 0;
   tick.color := FPlane.xAxis.color;
   tick.lineWidth := FPlane.XAxis.lineWidth;
   //tick.backgroundColor := claWhite;
   tick.backgroundColor := clWhite;


   number := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, center, top, TConst.MARGIN_FONT, self);
   number.width := w;
   number.height := h - tick.height;
   number.x := 0;
   number.y := tick.height;
  // number.backgroundColor := claGainsboro;
   number.backgroundColor := clLtGray;
   number.color := tick.color;
end;

procedure TBlockX.resize;
begin
   tick.width := width;
   tick.height := TConst.tickPercentAxisX*height;

   number.width := width;
   number.height := height - tick.height;
   number.x := 0;
   number.y := tick.height;
end;

procedure TBlockX.Draw;
begin
  //tick.Clear;
  tick.drawLine;

  //number.Clear;
  number.writeText;
end;

destructor TBlockX.Destroy;
begin
   tick.destroy;
   number.destroy;
end;

destructor  TRightBox.Destroy;
begin
  graph.Destroy;
  bottomBox.Destroy;
end;

constructor TRightBox.Create(w, h: double; P: TObject);
var
  hBottombox: double;
begin
  parent := P;
  name := 'TRightBox';
  width := w;
  height := h;
  hBottombox := TConst.HEIGHT_X_AXIS;

  bottomBox := TBottomBox.Create(w, hBottombox, self);
  bottomBox.backgroundColor := data.BackgroundColor;
  bottomBox.x := 0;
  bottomBox.y := h - hBottombox;

  graph := TGraph.Create(w, h - hBottombox, self);
  graph.name := 'graph';
  graph.backgroundColor := data.plotPanelBackgroundColor;
  graph.x := 0;
  graph.y := 0;
end;

procedure TRightBox.resize;
begin
  bottomBox.width := width;
  bottomBox.height := TConst.HEIGHT_X_AXIS;
  bottomBox.x := 0;
  bottomBox.y := height - bottomBox.height;

  graph.width := width;
  graph.height := bottomBox.y;
  graph.x := 0;
  graph.y := 0;

  bottomBox.resize;
  graph.resize;
end;

procedure TRightBox.Draw;
begin
  clear;   // This draws correct rectangle on top of graph panel ??
  console.log(' ^^++ TRightBox.Draw ++^^ ');
  self.graph.backgroundColor := self.data.plotPanelBackgroundColor;  // ISSUE ???  data?
  self.graph.Draw;
  bottomBox.backgroundColor := self.data.BackgroundColor;
  bottomBox.Draw;
end;



end.
