unit uWebGlobalData;
interface
  uses
    SysUtils, System.StrUtils, Classes, System.UITypes, contnrs, Types, WebLib.ExtCtrls,
    WebLib.Utils, WebLib.Buttons, WebLib.Graphics, WebLib.Controls,
    Vcl.StdCtrls, WebLib.StdCtrls, math, Vcl.Graphics, uWebDataSource, uScrollingTypes{System.UIConsts,};
  type
    THAlign = (left, center, right);
    TVAlign = (top, middle, bottom);
    TPivot = record
      horizontal: THAlign;
      vertical: TVAlign;
    end;

    TOnCompleteEvent = procedure(X, Y: single) of object;
    TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single) of object;
    TSwitch = reference to function (b: array of const; m: byte): TPoint;
    TFuncPivot = reference to function (w, h: double; pivot: TPivot): TPointF;
    TValuesLegend = 0..8;
    TConst = record
      const
        AlignMat: Array[0..8] of TPivot =
        (
          (horizontal: left; vertical: top),
          (horizontal: center; vertical: top),
          (horizontal: right; vertical: top),
          (horizontal: left; vertical: middle),
          (horizontal: center; vertical: middle),
          (horizontal: right; vertical: middle),
          (horizontal: left; vertical: bottom),
          (horizontal: center; vertical: bottom),
          (horizontal: right; vertical: bottom)
        );
        VUnits : array[0..8] of TPoint =
        (
          (X : 1; Y: 1),
          (X : 0; Y: 1),
          (X : -1; Y: 1),
          (X : 1; Y: 0),
          (X : 0; Y: 0),
          (X : -1; Y: 0),
          (X : 1; Y: -1),
          (X : 0; Y: -1),
          (X : -1; Y: -1)
        ) ;
        MARGIN_X = 20; //40;
        MARGIN_y = 20;
        ERROR_NAME_SERIE = 'Error!. The Identifier already exists. The series was not added';
        DEFAULT_NAME_SERIES = 'Serie %d';
        HEIGHT_X_AXIS = 60;
        WIDTH_Y_AXIS = 40; //80;
        DEFAULT_X_MIN = 0;
        DEFAULT_X_MAX = 20;
        DEFAULT_Y_MIN = -2;
        DEFAULT_Y_MAX =  2;
        DEFAULT_X_POINTS_DISPLAYED = 100;
        DEFAULT_LINEWIDTH_AXIS = 2;
        DEFAULT_COLOR_AXIS = clWhite;
        MAX_TICKS_X = 10;
        MAX_TICKS_Y = 10;
        DEFAULT_XAXIS_CAPTION = 'Time (sec)';
        DEFAULT_YAXIS_CAPTION = 'Y Axis';
        DEFAULT_FONTCOLOR_LEGEND = clBlack;
        DEFAULT_FONTSIZE_LEGEND = 8; //10;
        DEFAULT_FONTNAME_LEGEND = 'Arial';
        DEFAULT_BACKGROUNDCOLOR_LEGEND = clRed;
        DEFAULT_PAD_LEGEND = 4;// 8;
        DEFAULT_WIDTH_LINE_LEGEND = 40;
        DEFAULT_PIVOT_HOR_LEGEND = right;
        DEFAULT_PIVOT_VERT_LEGEND = top;
        DEFAULT_POSITION_LEGEND = 2;
        DEFAULT_MARGIN_LEGEND = 10;
        DEFAULT_SPACE_TEXT_LINES_LEGEND = 10;
        DEFAULT_GAP_LEGEND = 2;
        DEFAULT_PAD_TITLE = 16;
        DEFAULT_TEXT_TITLE = 'Chart Title';
        DEFAULT_FONTCOLOR_TITLE = clwhite;
        DEFAULT_BACKGROUNDCOLOR_TITLE = clBlack;
        DEFAULT_FONTNAME_TITLE = 'Arial';
        DEFAULT_FONTSIZE_TITLE = 12; //14;
        DEFAULT_ALIGN_TITLE = center;
        DEFAULT_COLOR_GRID = clWhite;
        DEFAULT_LINEWIDTH_GRID = 1;
        DEFAULT_VISIBLE_GRID = true;
        DEFAULT_COLOR_SERIE = clYellow;
        DEFAULT_LINEWIDTH_SERIE = 2;
        DEFAULT_BACKGROUND_COLOR = clBlack;
        DEFAULT_PLOT_AREA_BACKGROUND_COLOR = clBlack;//$FF100c08;
        LENGTH_BOX_TICK_X = 24;
        LENGTH_BOX_TICK_Y = 24;
        tickPercentAxisX = 0.5;
        tickPercentAxisY = 0.5;
        FONT_COLOR = clWhite;
        FONT_NAME = 'ARIAL';
        FONT_SIZE = 10; //12;
        MARGIN_FONT = 6;
        DEFAULT_DELTA_X = 0.1;
    end;
    TSeries = Array of TDataSerie;
    TLegend = class (TObject)
      FPosition: TValuesLegend;
      FMargin: double;
      x, y: double;
      pivot, reference: TPivot;
      fontColor, backgroundColor : TColor;
      fontSize: byte;
      fontName: String;
      pad, space, gap: double;
      widthLine: Byte;
      visible: Boolean;
      constructor Create;
      function getAlignIndex(ref: TPivot): Integer;
      function getPos(val: TPivot): Integer;
      procedure setPos(val: TValuesLegend);
      procedure SetMargin(val: Double);
      property Position: TValuesLegend read FPosition write SetPos default 2;
      property Margin: double read FMargin write SetMargin;
    end;
    TInfoAxis = class (TObject)
        color: TColor;
        caption: String;
        maxTicks: Integer;
        lineWidth: Byte;
        constructor Create;
    end;
    TTitle = class (TObject)
      pad: double;
      text: String;
      fontcolor, backgroundColor: TColor;
      fontName: String;
      fontSize: Integer;
      align: THAlign;
      constructor Create;
    end;
    TGridInfo = class (TObject)
      visible: Boolean;
      color: TColor;
      lineWidth: Byte;
      constructor Create;
    end;
    TPlaneXY = class (TObject)
      x, y: double;
      width, height: double;
      xAxis, yAxis: TInfoAxis;
      initialValueYMin, initialValueYMax: double;
      grid: TGridInfo;
      FDeltaX: Single;
      fm: String;
      constructor Create;
      destructor Destroy; override;
      procedure SetDeltaX(val: Single);
      function GetXAxisTicks: Integer;
      procedure SetXAxisTicks(val: Integer);
      function GetYAxisTicks: Integer;
      procedure SetYAxisTicks(val: Integer);
      procedure setXAxisRange(xMin, xMax: double);
      procedure setYAxisRange(yMin, yMax: double);
      property DeltaX: Single read FDeltaX write SetDeltaX;
      function NiceNumX(val: double): String;
      function NiceNumY(val: double): String;
      procedure SetFormatNumber;
      property XAxisTicks: Integer read GetXAxisTicks write SetXAxisTicks;
      property YAxisTicks: Integer read GetYAxisTicks write SetYAxisTicks;
    end;
    TGlobalData = class (TObject)
      dataSource: TDataSource;
      autoScaleUp, autoScaleDown: Boolean;
      chartWidth, chartHeight: double;
      canvas: TCanvas;
      series: TSeries;
      title: TTitle;
      plane: TPlaneXY;
      BackgroundColor: TColor;
      plotPanelBackgroundColor: TColor;
      constructor Create;
      destructor Destroy; override;
      function getPivot(w, h: double; pivot: TPivot): TPointF;
      function Switch(b: array of const; m: byte): TPoint;
    end;
implementation
uses
  uWebComps;
function TPlaneXY.GetXAxisTicks: Integer;
begin
  Result := xAxis.maxTicks;
end;
procedure TPlaneXY.SetXAxisTicks(val: Integer);
begin
  if val <>  xAxis.maxTicks then
    begin
      xAxis.maxTicks := val;
    end;
end;
function TPlaneXY.GetYAxisTicks: Integer;
begin
  Result := yAxis.maxTicks;
end;
procedure TPlaneXY.SetYAxisTicks(val: Integer);
begin
   if val <> yAxis.maxTicks then
    begin
      yAxis.maxTicks := val;
    end;
end;
function TPlaneXY.niceNumX(val: double): String;
begin
  Result := Format(fm, [val]);
  Result := Format('%g', [StrToFloat(Result)]);
end;
function TPlaneXY.niceNumY(val: double): String;
begin
  Result := Format('%.2f', [val]);
  Result := Format('%g', [StrToFloat(Result)]);
end;
function countZeros(s: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for I := 1 to length(s) do
    begin
      Result := Result + 1;
      if s[i] <> '0' then Exit;
    end;
end;
procedure TPlaneXY.SetFormatNumber;
var
  s: String;
  n: Integer;
  Splitted: TArray<String>;
begin
  s := FloatToStr(FDeltaX);
 // Splitted := s.Split(['.'], 2);

  Splitted := SplitString(s, '.'); // assume only 2 elements
  s := Splitted[1];
  n := countZeros(s);
  if n < 0 then n := 0;
  fm := '%.' + IntToStr(n) + 'f';
end;

procedure TPlaneXY.SetDeltaX(val: Single);
begin
  if val <> FDeltaX then
    begin
      FDeltaX := val;
      SetFormatNumber;
    end;
end;
function TGlobalData.Switch(b: array of const; m: byte): TPoint;
begin
  Result.X := b[m].VInteger;
  Result.Y := b[1 - m].VInteger;
end;
constructor TGridInfo.Create;
begin
  color :=  TConst.DEFAULT_COLOR_GRID;
  lineWidth := TConst.DEFAULT_LINEWIDTH_GRID;
  visible := TConst.DEFAULT_VISIBLE_GRID;
end;
constructor TGlobalData.Create;
begin
  inherited;
  dataSource := TDataSource.Create;
  plane := TPlaneXY.Create;
  title := TTitle.Create;
  BackgroundColor := TConst.DEFAULT_BACKGROUND_COLOR;
  plotPanelBackgroundColor := TConst.DEFAULT_PLOT_AREA_BACKGROUND_COLOR;
end;
destructor TGlobalData.Destroy;
begin
  title.Destroy;
  plane.Destroy;
  dataSource.Destroy;
  inherited;
end;
constructor TTitle.Create;
begin
  pad := TConst.DEFAULT_PAD_TITLE;
  text := TConst.DEFAULT_TEXT_TITLE;
  fontColor := TConst.DEFAULT_FONTCOLOR_TITLE;
  backgroundColor := TConst.DEFAULT_BACKGROUNDCOLOR_TITLE;
  fontName := TConst.DEFAULT_FONTNAME_TITLE;
  fontSize := TConst.DEFAULT_FONTSIZE_TITLE;
  align := TConst.DEFAULT_ALIGN_TITLE;
end;
constructor TLegend.Create;
begin
  Position := TConst.DEFAULT_POSITION_LEGEND;
 // Margin := TConst.DEFAULT_MARGIN_LEGEND;
  fontColor := TConst.DEFAULT_FONTCOLOR_LEGEND;
  backgroundColor := TConst.DEFAULT_BACKGROUNDCOLOR_LEGEND;
  fontSize := TConst.DEFAULT_FONTSIZE_LEGEND;
  fontName := TConst.DEFAULT_FONTNAME_LEGEND;
  pad := TConst.DEFAULT_PAD_LEGEND;
  widthLine := TConst.DEFAULT_WIDTH_LINE_LEGEND;
  space := TConst.DEFAULT_SPACE_TEXT_LINES_LEGEND;
  gap := TConst.DEFAULT_GAP_LEGEND;
  visible := true;
end;
function TLegend.getAlignIndex(ref: TPivot): Integer;
var
  I: Integer;
begin
  for I := 0 to 8 do
    if (TConst.AlignMat[i].horizontal = ref.horizontal) and
       (TConst.AlignMat[i].vertical = ref.vertical) then
      begin
        Result := i;
        Exit;
      end;
  Result := -1;
end;
function TLegend.getPos(val: TPivot): Integer;
begin
  Result := getAlignIndex(val);
  //Result := FPosition;
end;
procedure TLegend.setPos(val: TValuesLegend);
begin
  FPosition := val;
  Margin := TConst.DEFAULT_MARGIN_LEGEND;
  pivot := TConst.AlignMat[val];
  reference := pivot;
end;
procedure TLegend.setMargin(val: Double);
begin
  FMargin := val;
  x := FMargin * TConst.VUnits[FPosition].X;
  y := FMargin * TConst.VUnits[FPosition].Y;
end;
constructor TInfoAxis.Create;
begin
  caption := '';
  color := TConst.DEFAULT_COLOR_AXIS;
  lineWidth := TConst.DEFAULT_LINEWIDTH_AXIS;
end;
constructor TPlaneXY.Create;
begin
  xAxis := TInfoAxis.Create;
  xAxis.maxTicks := TConst.MAX_TICKS_X;
  xAxis.caption := TConst.DEFAULT_XAXIS_CAPTION;
  yAxis := TInfoAxis.Create;
  yAxis.maxTicks := TConst.MAX_TICKS_Y;
  yAxis.caption := TConst.DEFAULT_YAXIS_CAPTION;
  grid := TGridInfo.Create;
  DeltaX := TConst.DEFAULT_DELTA_X;
end;
destructor TPlaneXY.Destroy;
begin
  xAxis.Destroy;
  yAxis.Destroy;
  grid.Destroy;
end;
procedure TPlaneXY.setXAxisRange(xMin, xMax: double);
begin
  x := xMin;
  width := xMax - xMin;
end;
procedure TPlaneXY.setYAxisRange(yMin, yMax: double);
begin
  y := yMin;
  height := yMax - yMin;
end;
function TGlobalData.getPivot(w, h: double; pivot: TPivot): TPointF;
var
  pivotX, pivotY: Double;
begin
   pivotX := 0;
   pivotY := 0;
  if (pivot.horizontal = left) and (pivot.vertical = top) then
    begin
      pivotX := 0;
      pivotY := 0;
    end
  else if (pivot.horizontal = center) and (pivot.vertical = top) then
    begin
      pivotX := w/2;
      pivotY := 0;
    end
  else if (pivot.horizontal = right) and (pivot.vertical = top) then
    begin
      pivotX := w;
      pivotY := 0;
    end
  else if (pivot.horizontal = left) and (pivot.vertical = middle) then
    begin
      pivotX := 0;
      pivotY := h/2;
    end
  else if (pivot.horizontal = center) and (pivot.vertical = middle) then
    begin
      pivotX := w/2;
      pivotY := h/2;
    end
  else if (pivot.horizontal = right) and (pivot.vertical = middle) then
    begin
      pivotX := w;
      pivotY := h/2;
    end
  else if (pivot.horizontal = left) and (pivot.vertical = bottom) then
    begin
      pivotX := 0;
      pivotY := h;
    end
  else if (pivot.horizontal = center) and (pivot.vertical = bottom) then
    begin
      pivotX := w/2;
      pivotY := h;
    end
  else if (pivot.horizontal = right) and (pivot.vertical = bottom) then
    begin
      pivotX := w;
      pivotY := h;
    end;
  Result.X := pivotX;
  Result.Y := pivotY;
end;

initialization
end.
