unit LSODA.test;

// TEST LSODA web version (compiled to js).

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  System.Generics.Collections, uVector, adamsbdf, odeEquations, uTestCase,
  utests.TestUtils;

  function LSODA_Test(currentTestResultList: TList<TTestCase>): TList<TTestCase>;
  function getReferenceResults(): string;

implementation

function LSODA_Test(currentTestResultList: TList<TTestCase>):TList<TTestCase>;
var
  y : TVector;
  t, tout : double;
  p: array of double;
  i, iout : integer;
  l : TLsoda;
  odeClass : TODE_eqs;
  solverOutput: string;
  refOutput: string;
  testResult: TTestCase;
  resultInfo: TList<string>;
begin
  resultInfo := TList<string>.create;
  solverOutput := '';
  y := TVector.Create (3);  // 3 is the number of variables
  l := TLsoda.Create (3);
  p[0]:= -0.04;
  p[1]:= 10000;
  p[2]:= 3E7;
  l.p:= p;
  testResult := TTestCase.create(1, 'LSODA Pascal test');
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

    // console.log('t,        y_1,        y_2,        y_3 ');
     solverOutput := 't,        y_1,        y_2,        y_3' + sLineBreak; // line break
    for iout := 1 to 20 do
        begin
        l.execute (y, t, tout);
        //console.log(t, ', ', y[1], ', ',y[2], ', ',y[3]);
        solverOutput := solverOutput + floattostr(t) + ', ' + floattostr(y[1]) + ', ' + floattostr(y[2]) + ', ' + floattostr(y[3]) + sLineBreak;
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
  resultInfo := compareStrResults(solverOutput, getReferenceResults());
  if resultInfo.count < 1 then
    begin
    testResult.testPass;
    end
  else
    begin
    testResult.testFail;
    for i := 0 to resultInfo.count -1 do
      testResult.sTestInfoList.Add(resultInfo[i]);
    end;
  currentTestResultList.Add(testResult);
 // console.log('Results:', resultInfo );
  Result := currentTestResultList;
end;

function getReferenceResults(): string;
begin
  Result := 't,        y_1,        y_2,        y_3 ' + sLineBreak +
'0.1, 0.996078756986491, 0.0000358045528206596, 0.0038854384606878' + sLineBreak +
'1.1, 0.963667686673703, 0.0000303041118084737, 0.0363020092144877' + sLineBreak +
'2.1, 0.939451332352495, 0.0000267159108393531, 0.0605219517366647' + sLineBreak +
'3.1, 0.920128504616787, 0.0000241619275342562, 0.0798473334556778' + sLineBreak +
'4.1, 0.9040393701884, 0.0000222348852857176, 0.0959383949263137' + sLineBreak +
'5.1, 0.890239360560074, 0.000020717254031991, 0.109739922185893' + sLineBreak +
'6.1, 0.878145088345054, 0.000019484000518161, 0.121835427654427' + sLineBreak +
'7.1, 0.867367296444266, 0.0000184568567637937, 0.13261424669897' + sLineBreak +
'8.1, 0.857634487634622, 0.0000175841320725818, 0.142347928233304' + sLineBreak +
'9.1, 0.848756410617394, 0.0000168306845055204, 0.1512267586981' + sLineBreak +
'10.1, 0.840592242489962, 0.0000161726143140577, 0.159391584895723' + sLineBreak +
'11.1, 0.833030399583051, 0.0000155908095479817, 0.166954009607401' + sLineBreak +
'12.1, 0.825982898811784, 0.0000150715570203041, 0.174002029631195' + sLineBreak +
'13.1, 0.81938071946259, 0.0000146047546183286, 0.180604675782791' + sLineBreak +
'14.1, 0.81316859731682, 0.0000141811393247042, 0.186817221543854' + sLineBreak +
'15.1, 0.807300408201681, 0.0000137960276702675, 0.192685795770648' + sLineBreak +
'16.1, 0.801737684626298, 0.000013442560820945, 0.19824887281288' + sLineBreak +
'17.1, 0.796448971206893, 0.0000131163303560489, 0.20353791246275' + sLineBreak +
'18.1, 0.791407051520662, 0.0000128150919881847, 0.208580133387348' + sLineBreak +
'19.1, 0.786589778579988, 0.0000125353573955033, 0.213397686062615';
end;

end.