unit LSODA.test;

// TEST LSODA web version (compiled to js).

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  uVector, adamsbdf, odeEquations;

  procedure test;

implementation

procedure test;
var
  y : TVector;
  t, tout : double;
  p: array of double;
  iout : integer;
  l : TLsoda;
  odeClass : TODE_eqs;      // added
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
    t := 0.0; tout := 0.4;
    l.itol := 2;
    l.itask := 1;
    l.istate := 1;
    l.iopt := 0;
    l.jt := 2;

   // l.Setfcn (odeClass);   // Set for all pascal code

    for iout := 1 to 12 do
        begin
        l.execute (y, t, tout);
        console.log('t: ',t, 'y_1: ', floattostr(y[1]), 'y_2: ',floattostr(y[2]), 'y_3: ',floattostr (y[3]));
        //Form1.WebListBox1.Items.Add (floattostr (t) + ': ' + floattostr (y[1]) + ',  ' + floattostr (y[2]) + ',  ' + floattostr (y[3]));
        if l.istate < 0 then
           begin

           console.log ('Error, istate = ', l.istate);
           break;
           end;

        tout := tout + 1;
        end;
  finally
    l.free; y.free;
  end;
end;


end.