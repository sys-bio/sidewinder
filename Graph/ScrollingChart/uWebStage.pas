unit uWebStage;

interface
  uses
   Math, System.Types, System.UITypes, System.Contnrs,
   System.SysUtils, JS, Web, WebLib.Graphics, uWebContainer,
   uWebComps, uWebDataSource, uWebGlobalData;

type

  TStage = class (TContainer)
     leftBox: TLeftBox;
     rightBox: TRightBox;
     titleBox: TTitleBox;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure checkLimits;
     procedure resize;
     procedure borderDraw;
  end;

implementation

destructor TStage.Destroy;
begin
   leftBox.Destroy;
   rightBox.Destroy;
   titleBox.Destroy;
end;

procedure TStage.checkLimits;
var
  xMin, xMax: Double;
  plane: TPlaneXY;
  dataSource: TDataSource;
begin
  dataSource := data.dataSource;
  if dataSource.cols.Count = 0 then Exit;

  plane := FPlane;
 //console.log('TStage.checkLimits, FPlane height, width: ', self.FPlane.height, ', ', self.FPlane.width);
  if dataSource.isOut(plane.width - plane.DeltaX) then // <-- -step size needed tostay inside grid plane
    begin
      dataSource.removeFirst;
      dataSource.getNewLimits(xMin, xMax);
      plane.x := xMin;
     // rightBox.bottomBox.axisX.update;
      rightBox.bottomBox.axisX.update(xMin);
      rightBox.graph.gridX.update;
    end;

end;

procedure TStage.resize;
begin
   titleBox.x := 0;
   titleBox.y := 0;
   titleBox.width := width;
 //  console.log(' title box width, height: ', self.titleBox.width, ', ', self.titleBox.height);

   leftBox.width := TConst.WIDTH_Y_AXIS;
   leftBox.height := height;// - titleBox.height;
   leftBox.x := 0;
   leftBox.y := 0; //titleBox.height;
 //  console.log(' Left box width, height: ', self.leftBox.width, ', ', self.leftBox.height);

   rightBox.width := width - leftBox.width;
   rightBox.height := height;// - titleBox.height;
   rightBox.x := leftBox.width;
   rightBox.y := 0; //titleBox.height;
 //  console.log(' right box width, height: ', self.rightBox.width, ', ', self.rightBox.height);
   leftBox.resize;
   rightBox.resize;

end;

constructor TStage.Create(w, h: double; P: TObject);
var
  wLeftBox: single;
  title: TTitle;
begin
  parent := P;
  width := w;
  height := h;
  title := data.title;
  //console.log(' TStage.create, width, height: ', self.width, ', ', self.height);
  //backgroundColor := clayellow; //DEFAULT_BACKGROUND_COLOR;
 // title.text := '';
  titleBox := TTitleBox.Create(w, {2*}title.pad + title.fontSize, self);
 // titleBox.height := 0; // added

  titleBox.x := 0;
  titleBox.y := 0; // this maybe it, set to bottom of chart - height -NO

  wLeftBox := TConst.WIDTH_Y_AXIS;

  leftBox := TLeftBox.Create(wLeftBox, h, self);
  leftBox.backgroundColor := clSkyBlue;
  leftBox.x := 0;
  leftBox.y := titleBox.height;
  rightBox := TRightBox.Create(w - wLeftBox, h , self);
  rightBox.backgroundColor := self.getAGlobalData.plotPanelBackgroundColor;
  rightBox.x := wLeftBox;
  rightBox.y := titleBox.height;
end;

procedure TStage.borderDraw;   // Fills all of chart
begin
  data.Canvas.Brush.Style := bsClear;
  data.canvas.pen.Width := Round(FPlane.yAxis.lineWidth);
  data.canvas.pen.color := FPlane.yAxis.color;
  data.canvas.Rectangle( 0, 0,Round(data.chartWidth), Round(data.chartHeight) );
end;

procedure TStage.Draw;
var
  plane: TPlaneXY;
  dataSource: TDataSource;
begin
  plane := FPlane;
  dataSource := data.dataSource;  // get TGlobalData
  if (abs(dataSource.maxY - dataSource.minY) < abs(MAX_VALUE_AXIS_Y - MIN_VALUE_AXIS_Y)) then
    begin

      if (data.autoScaleUp) and (not data.autoScaleDown) then
          plane.setYAxisRange(plane.initialValueYMin, dataSource.maxY)
      else
      if (data.autoScaleUp) and (data.autoScaleDown) then
          plane.setYAxisRange(dataSource.minY, dataSource.maxY)
      else
      if (not data.autoScaleUp) and (data.autoScaleDown) then
          plane.setYAxisRange(dataSource.minY, plane.initialValueYMax)
      else
          plane.setYAxisRange(plane.initialValueYMin, plane.initialValueYMax);

    end;

  //clear;
  rightBox.Draw; // Including legend
  titleBox.draw;
  leftBox.Draw;
  borderDraw;
end;

end.
