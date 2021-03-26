unit uMatrix;

interface
      type rx= array of double;
type
  TMatrix = class
     nr, nc : integer;

     //mx : array of array of double;

     mx: array of rx;

     constructor  create (r, c : integer);

     procedure setVal (ri, ci : integer; v : double);
     function  getVal (ri, ci : integer) : double;

     property  element[x, y : Integer] : double read getVal write setVal; default;

     property  r : integer read nr;
     property  c : integer read nc;

  end;

implementation

constructor TMatrix.create (r, c : integer);
begin
  nr := r; nc := c;
  setlength (mx, r+1, c+1);
end;

procedure TMatrix.setVal (ri, ci : integer; v : double);
begin
  mx[ri,ci] := v;
end;

function TMatrix.getVal (ri, ci : integer) : double;
begin
  result := mx[ri,ci];
end;

end.
