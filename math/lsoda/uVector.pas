unit uVector;

interface

type
  TVector = class
    vx : array of double;

    procedure   setVal (i : integer; v : double);
    function    getVal (i : integer) : double;
    function    getSize : integer;

    property    element[x : Integer] : double read getVal write setVal; default;
    property    size : integer read getSize;
    constructor create(size : integer);
  end;

  TVectori = class
     vx : array of integer;

     procedure   setVal (i : integer; v : integer);
     function    getVal (i : integer) : integer;

     property    element[x : Integer] : integer read GetVal write SetVal; default;
     constructor create(size : integer);
  end;


implementation


constructor TVector.create(size : integer);
begin
  setLength (vx, size+1);
end;

// used internally but is also accessible from the outside
procedure TVector.setVal (i : integer; v : double);
begin
  vx[i] := v;
end;


// used internally but is also accessible from the outside
function  TVector.getVal (i : integer) : double;
begin
  result := vx[i];
end;


function TVector.getSize : integer;
begin
  result := length (vx)-1;
end;


// --------------------------------------------------------

constructor TVectori.create(size : integer);
begin
  setLength (vx, size+1);
end;


// used internally but is also accessible from the outside
procedure TVectori.Setval (i : integer; v : integer);
begin
  vx[i] := v;
end;


{ used internally but is also accessible from the outside }
function  TVectori.Getval (i : integer) : integer;
begin
  result := vx[i];
end;


end.
