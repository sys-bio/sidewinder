unit odeEquations;

interface

uses SysUtils, uVector, Web;

type
 TODE_eqs = class(TObject)
 class function ComputeODEs (t: double; y, p: array of double):TVector;
 end;


implementation

class function TODE_eqs.ComputeODEs (t : double; y, p:array of double):TVector;
begin
  Result:= TVector.create(Length(y));
  Result[1] := p[0]*y[0] + p[1]*y[1]*y[2];
  Result[3] := p[2]*y[1]*y[1];
  Result[2] := -Result[1] - Result[3];
// console.log(' ComputeODES, y[0], [1], [2]: ', y[0],', ', y[1],', ', y[2]);

end;



end.


