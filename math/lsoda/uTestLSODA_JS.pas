unit uTestLSODA_JS;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections, LSODA.test, adamsbdf, uVector;

const TESTS = 3;
type
  TArrD = array of double;
  TLSODA_JSTests = class

  private
    LSODAeqs: TList<string>;
    LSODAspecies: TList<TArrD>;
    LSODAparams: TList<TArrD>;
    lode: TLsoda;
    function getInitialValues(values:array of double): array of double;
  public
    constructor create;
    procedure LSODATests();
    procedure runTest(eqs: string; species: array of double; params: array of double);
  end;

var
 

implementation

{$R *.dfm}

constructor TLSODA_JSTests.create;
var i: integer;
    a,b: array of double;

begin
  LSODAspecies := TList<TArrD>.create;
  LSODAparams := TList<TArrD>.create;
  LSODAeqs := TList<string>.create;
  // TEST 1:
  LSODAeqs.Add('let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]);'+
              'dydt_s.setVal(2, (1)*(1)* (p[0] * Math.pow(s[0], 1)));' +
              'dydt_s.setVal(3, (1)*(1)* (p[0] * Math.pow(s[0], 1))+  (-1)*(1)* (p[1] * Math.pow(s[2], 1)));' +
              'dydt_s.setVal(1, (-1)*(1)* (p[0] * Math.pow(s[0], 1)));' +
              'dydt_s.setVal(4, (1)*(1)* (p[1] * Math.pow(s[2], 1))); return dydt_s;');
  SetLength(a,4);
  a[0] := 11; a[1] := 0.5; a[2] := 0; a[3] := 0;
  LSODAspecies.Add(Copy(a,0,MaxInt));
  setLength(b, 2);
  b[0] := 1.0; b[1] := 1.0;
  LSODAparams.Add(Copy(b,0,MaxInt));

  // TEST 2:   Same eq as 1.
  LSODAeqs.Add(LSODAeqs.Items[0]);
  a[0] := 11; a[1] := 0.5; a[2] := 0.2; a[3] := 0.1;
  LSODAspecies.Add(Copy(a,0,MaxInt));
  b[0] := 1.0; b[1] := 1.0;
  LSODAparams.Add(Copy(b,0,MaxInt));

  // TEST 3:
  LSODAeqs.Add('let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]); ' +
               'dydt_s.setVal(1, (1)*(1)* (p[13] * p[0])+  (1)*(1)* (p[13] * p[1] * p[2])+  (-1)*(1)* (p[13] * (p[3] * Math.pow(s[0], p[4]) / (Math.pow(p[5], p[4]) + Math.pow(s[0], p[4]))))+' +
               '  (1)*(1)* (p[14] * (p[6] * Math.pow(s[1], p[7]) * Math.pow(s[0], p[12]) / ((Math.pow(p[8], p[7]) + Math.pow(s[1], p[7])) * (Math.pow(p[9], p[12]) + Math.pow(s[0], p[12])))))+  (1)*(1)* (p[14] * p[10] * s[1])+  (-1)*(1)* (p[13] * p[11] * s[0])); ' +
               'dydt_s.setVal(2, (1)*(1)* (p[13] * (p[3] * Math.pow(s[0], p[4]) / (Math.pow(p[5], p[4]) + Math.pow(s[0], p[4]))))+ '+
               ' (-1)*(1)* (p[14] * (p[6] * Math.pow(s[1], p[7]) * Math.pow(s[0], p[12]) / ((Math.pow(p[8], p[7]) + Math.pow(s[1], p[7])) * (Math.pow(p[9], p[12]) + Math.pow(s[0], p[12])))))+  (-1)*(1)* (p[14] * p[10] * s[1])); return dydt_s;');
  SetLength(a,0);
  SetLength(a,2);
  a[0] := 0.15; a[1] := 1.6;
  LSODAspecies.Add(Copy(a,0,MaxInt));
  setLength(b,0);
  setLength(b, 15);
  b[0] := 1.0; b[1] := 7.3; b[2] := 0.301; b[3] := 65; b[4] := 2.0; b[5] := 1.0;
  b[6] :=500; b[7] := 2; b[8] := 2; b[9] := 0.9; b[10] := 1; b[11] := 10; b[12] := 4;
  b[13] := 1; b[14] := 1;
  LSODAparams.Add(Copy(b,0,MaxInt));
  end;

 {procedure TLSODA_JSTests.LSODATests;
begin

  LSODA_Test; // PASCAL ODE eq
  self.LSODATests();
end;  }

procedure TLSODA_JSTests.LSODATests();
var i: integer;
    s: array of double;
    p: array of double;
begin

  for i := 0 to TESTS -1 do
    begin
     console.log('*** Beginning Test # ',i+1);
      // Load settings for test:
      s := self.getInitialValues(LSODAspecies.Items[i]);
      p := self.getInitialValues(LSODAparams.Items[i]);
      // Run test:
      self.runTest(self.LSODAeqs.Items[i], s, p);

      // Save results to file.
    end;

end;

function TLSODA_JSTests.getInitialValues(values: array of double): array of double;
var i, j: integer;
    val: array of double;

begin
  setLength(val,Length(values));
  for i := 0 to Length(values) -1 do
    begin
      val[i] := values[i];
    end;
  Result := val;
end;

procedure TLSODA_JSTests.runTest(eqs: string; species: array of double; params: array of double);
var i, j: integer;
    stepSize: double;
    runTime: double;
    numSteps: Integer;
    dydt_s: TVector;
    y: TVector;
    tNext: double;
    time: double;
    outputStr: string;
  k: Integer;
begin
  stepSize := 0.1;
  runTime := 10;
  time := 0.0; // current time
  self.lode :=  TLsoda.create(length(species));
  for i := 1 to Length(species) do
  begin
    self.lode.rtol[i] := 1e-4;
    self.lode.atol[i] := 1e-6;
  end;
  self.lode.itol := 2;
  self.lode.itask := 1;
  self.lode.istate := 1;
  self.lode.iopt := 0;
  self.lode.jt := 2;
  asm
   console.log('TForm1.runTest: LSODA Funct: ', eqs);
   var ODE_func = new Function('time', 's','p', eqs);
   this.lode.Setfcn (ODE_func);
  end;

  numSteps := Round(runTime/stepSize);
  lode.p := params; // set param vals.
  outputStr := '';
  outputStr := floattostr(time) + ': ';
  for k := 0 to Length(species)-1 do
    begin
      outputStr := outputStr + floattostr(species[k])+ ', ';
    end;
  console.log(outputStr);
  for i := 0 to numSteps -1 do
    begin
      tNext:= time + stepSize;
      y:= TVector.create(Length(species));
      for j := 1 to Length(species) do
      begin
        y[j]:= species[j-1];
      end;
      self.lode.Execute (y, time, tNext);
      time := tNext;

     // Convert TVector back to array of double ( y ->species)
      for j:= 0 to Length(species)-1 do
      begin
       species[j]:= y[j+1];
      end;
      outputStr := '';
      outputStr := floattostr(time) + ': ';
      for k := 0 to Length(species)-1 do
      begin
        outputStr := outputStr + floattostr(species[k])+ ', ';
      end;
      console.log(outputStr);
      if self.lode.istate < 0 then
      begin

        console.log ('Error, istate = ', self.lode.istate);
      end;
      tNext:= tNext + stepSize;
     end;  // for i loop
end;

end.









