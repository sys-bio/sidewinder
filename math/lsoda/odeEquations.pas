unit odeEquations;

interface

uses SysUtils, uVector, Web;

type
 TODE_eqs = class(TObject)
 //  class function ComputeODEs (t: double; y : TVector; p: array of double):TVector;
 class function ComputeODEs (t: double; y, p: array of double):TVector;
 end;


implementation
 // return dy vector
class function TODE_eqs.ComputeODEs (t : double; y, p:array of double):TVector;
begin
  Result:= TVector.create(Length(y));
 // Result[1] := -0.04*y[1] + 10000*y[2]*y[3];
 // Result[3] := 3E7*y[2]*y[2];
 // Result[2] := -Result[1] - Result[3];

  Result[1] := p[0]*y[0] + p[1]*y[1]*y[2];
  Result[3] := p[2]*y[1]*y[1];
  Result[2] := -Result[1] - Result[3];
// console.log(' ComputeODES, y[0], [1], [2]: ', y[0],', ', y[1],', ', y[2]);

end;


end.


