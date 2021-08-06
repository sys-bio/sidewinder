unit LSODA.test;

// TEST LSODA web version (compiled to js).

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  uVector, adamsbdf, odeEquations;

  procedure LSODA_Test;

implementation

procedure LSODA_Test;
var
  y : TVector;
  t, tout : double;
  p: array of double;
  iout : integer;
  l : TLsoda;
  odeClass : TODE_eqs;

begin
  y := TVector.Create (3);  // 3 is the number of variables
  l := TLsoda.Create (3);
  p[0]:= -0.04;
  p[1]:= 10000;
  p[2]:= 3E7;
  l.p:= p;

  odeClass:= TODE_eqs.create;    // added

  try
    l.rtol[1] := 1e-4; l.rtol[2] := 1e-4; l.rtol[3] := 1e-4;
    l.atol[1] := 1e-6; l.atol[2] := 1e-10; l.atol[3] := 1e-6;

    y[1] := 1.0; y[2] := 0.0; y[3] := 0.0;
    t := 0.0; tout := 0.1;
    l.itol := 2;
    l.itask := 1;
    l.istate := 1;
    l.iopt := 0;
    l.jt := 2;

    l.SetfcnPascal (odeClass);   // Set all pascal code function to solve.

     console.log('t,        y_1,        y_2,        y_3 ');
    for iout := 1 to 20 do
        begin
        l.execute (y, t, tout);
        //console.log('t: ',t, 'y_1: ', floattostr(y[1]), 'y_2: ',floattostr(y[2]), 'y_3: ',floattostr (y[3]));
        console.log(t, ', ', y[1], ', ',y[2], ', ',y[3]);
        //Form1.WebListBox1.Items.Add (floattostr (t) + ': ' + floattostr (y[1]) + ',  ' + floattostr (y[2]) + ',  ' + floattostr (y[3]));
        if l.istate < 0 then
           begin

           console.log ('Error, istate = ', l.istate);
           break;
           end;

        tout := tout + 1; // 1 'sec' step
        end;
  finally
    l.free; y.free;
  end;
end;


end.