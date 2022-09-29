unit uWebDataSource;
interface
uses
  System.Classes, JS, Web, WebLib.Graphics,  System.UITypes, {System.UIConsts,} System.Types,
  System.SysUtils, System.Generics.Collections, math, uScrollingTypes;
const
    MAX_VALUE_AXIS_Y = 1E10;
    MIN_VALUE_AXIS_Y = -1E10;
type
  TDataSerie = class;
  TFunctionTime = reference to function(t: double): double;
  TOnRedraw = procedure of object;
 
 // PData = ^TData;    // *** pointer
  TData = record
        y: double;
    serie: TDataSerie;
  end;
 // TListY = Array of PData;
  TListY = TList<TData>;
 // PDataCol = ^TDataCol;  // *** pointer
  TDataCol = record
      x: double;
      rows: TListY;
  end;
  //PDataCol = ^TDataCol;
  //TListX = Array of PDataCol;
  TListX = TList<TDataCol>;
  TDataSource = class (TObject)
      Redraw: TOnRedraw;
      cols: TListX;
      minY, maxY: double;
      procedure add(x, y: double; serie: TObject);
      procedure addX(x: Double);
      procedure addY(y: double; serie: TObject);
      constructor Create;
      procedure deleteSerie(serie: TObject);
      procedure deleteAllSeries;
    //  procedure DeleteCol(Q: PDataCol);// [Error] uWebDataSource.pas(43): Not supported: pointer
    //  procedure DeleteCol(Q: TDataCol);// [Error] uWebDataSource.pas(43): Not supported: pointer
      procedure DeleteAll;
      destructor  Destroy; override;
      function isOut(wPlane: double): Boolean;
      procedure getNewLimits(var minX, maxX: double);
      procedure removeFirst;
      procedure init;
      function isEmpty: Boolean;
      procedure removeAll;
      procedure reset;
      procedure getYMinAndYMax;
      procedure updateYMinAndYMax(Q: TData);
  end;
  TDataSerie = class (TObject)
      functionTime: TFunctionTime;
      dataSource: TDataSource;
      FColor: TColor;
      FlineWidth: Byte;
      name: String;
      FVisible: Boolean;
      count: Integer;
      fromPoint: TPoint;
      procedure add(x, y: double);
      procedure addX(x: double);
      procedure addY(y: double);
      constructor Create;
      destructor  Destroy; override;
      procedure Init;
      procedure SetlineWidth(val: Byte);
      procedure SetVisible(val: Boolean);
      procedure SetColor(val: TColor);
      property visible: Boolean read FVisible write SetVisible;
      property color: TColor read FColor write SetColor;
      property lineWidth: Byte read FlineWidth write SetlineWidth;
      procedure Redraw;
  end;

implementation
uses
  uWebGlobalData;

procedure TDataSource.addX(x: Double);
var
 Data: TDataCol;
begin
  if (self.cols.Count = 0) or (x > self.cols[self.cols.count - 1].x) then
    begin
      Data.x := x;
      Data.rows := TListY.create;
      self.cols.Add(Data);
    end;
end;
procedure TDataSource.addY(y: double; serie: TObject);
var
  D: TData;
begin
  D.y := y;
  D.serie := serie as TDataSerie;
  D.serie.count := D.serie.count + 1;
  self.updateYMinAndYMax(D);
  self.cols[self.cols.count -1].rows.Add(D);
end;

procedure TDataSource.add(x, y: double; serie: TObject);
begin
  addX(x);
  addY(y, serie);
end;

procedure TDataSource.removeFirst;
begin
  self.cols.Delete(0);
end;

procedure TDataSource.updateYMinAndYMax(Q: TData);
begin
  if Q.serie.visible then
    begin
      if Q.y < self.minY then self.minY := Q.y;
      if Q.y > self.maxY then self.maxY := Q.y;
    end;
end;
procedure TDataSource.getYMinAndYMax;
var
  i, j: Integer;
  col: TDataCol;
begin
  minY := MAX_VALUE_AXIS_Y;   // ? Want first y value to set max and min
  maxY := MIN_VALUE_AXIS_Y;   // ?

  for i := 0 to self.cols.Count - 1 do
    begin
      col := self.cols[i];
      for j := 0 to col.rows.Count - 1 do updateYMinAndYMax(col.rows[j]);
    end;
end;
function TDataSource.isEmpty: Boolean;
var
  i: Integer;
  col: TDataCol;
begin
  for i := self.cols.count - 1 downto 0 do
    begin
      col := self.cols[i];
      if col.rows.count > 0 then
        begin
          Result := false;
          Exit;
        end;
    end;
  Result := true;
end;
procedure TDataSource.deleteAllSeries;
var
  i, j: Integer;
begin
  for i := self.cols.Count - 1 downto 0 do
    begin
      for j := self.cols[i].rows.Count - 1 downto 0 do
        begin
        self.cols[i].rows.Delete(j);
        end;
    end;
  init;
end;
procedure TDataSource.deleteSerie(serie: TObject);
var
  i, j: Integer;
  col: TDataCol;
 // row: TData;
begin
    for i := self.cols.Count - 1 downto 0 do
    begin
      col := self.cols[i];
      for j := col.rows.Count - 1 downto 0 do
        begin
          if serie = col.rows[j].serie then // is this equal?
            begin
            //  Dispose(row);
              self.cols[i].rows.Delete(j);
            end;
        end;
    end;
    if isEmpty then init;
end;
{procedure TDataSource.DeleteCol(Q: TDataCol); // NOT needed
var
  j: Integer;
  //D: PData;
  D: TData;
begin
 { for j := 0 to length(Q^.rows) - 1 do
    begin
      D := Q^.rows[j];
      D^.serie.count := D^.serie.count - 1;
      Dispose(D);
    end;
  Q^.rows := Nil;
  Dispose(Q);  }
 { for j := 0 to Q.rows.Count - 1 do
    begin
    Q.rows.Delete(j);
    end;
  Q.rows := Nil;
  //Dispose(Q);
end;
      }
procedure TDataSource.DeleteAll;
begin
  removeAll;
  cols := nil;
end;
procedure TDataSource.removeAll;
var
  i: Integer;
begin
  for i := cols.Count - 1 downto 0 do
    begin
    self.cols.Delete(i);
    end;
end;
procedure TDataSource.reset;
begin
   removeAll;
   Init;
end;
constructor TDataSource.Create;
begin
  self.cols := TListX.Create;
  Init;
end;
procedure TDataSource.Init;
begin
   minY := MAX_VALUE_AXIS_Y;
   maxY := MIN_VALUE_AXIS_Y;
end;
procedure TDataSource.getNewLimits(var minX, maxX: double);
begin
  minX :=self.cols[0].x;
  maxX := self.cols[self.cols.count-1].x;
end;

function TDataSource.isOut(wPlane: double): Boolean;
begin
  if self.cols.Count >= 2 then
    begin
    if self.cols[self.cols.count -1].x - self.cols[0].x > wPlane then
      begin
        Result := true;
        Exit;
      end;
    end;
  Result:= false;
end;
destructor TDataSource.Destroy; // needed??
begin
   DeleteAll;
end;
constructor TDataSerie.Create;
begin
  inherited;
  color := TConst.DEFAULT_COLOR_SERIE;
  lineWidth:= TConst.DEFAULT_LINEWIDTH_SERIE;
  Fvisible := true;
  init;
end;
procedure TDataSerie.init;
begin
  count := 0;
  fromPoint.X := -1;
end;
destructor  TDataSerie.Destroy;
begin
end;
procedure TDataSerie.addX(x: double);
begin
  dataSource.addX(x);
end;
procedure TDataSerie.addY(y: double);
begin
  dataSource.addY(y, self);
end;
procedure TDataSerie.add(x, y: double);
begin
  dataSource.add(x, y, self);
end;
procedure TDataSerie.Redraw;
begin
  if (dataSource <> Nil) and (Assigned(dataSource.Redraw)) then dataSource.Redraw;
end;
procedure TDataSerie.SetlineWidth(val: Byte);
begin
if val <> FlineWidth then
    begin
      FlineWidth := val;
      Redraw;
    end;
end;
procedure TDataSerie.SetColor(val: TColor);
begin
   if val <> FColor then
    begin
      self.FColor := val;
      Redraw;
    end;
end;
procedure TDataSerie.SetVisible(val: Boolean);
begin
  if val <> FVisible then
    begin
      FVisible := val;
      dataSource.getYMinAndYMax;
      Redraw;
    end;
end;

end.
