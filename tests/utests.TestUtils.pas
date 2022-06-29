unit utests.TestUtils;

interface
 uses
 System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
 WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
 System.Generics.Collections;

 {type
 TStringTestCase = record  // Used for tests that are represented by a string.
      testName : string;
      strSerializedTest : string;
  end;            }

 function compareStrResults(testResult: string; refResult: string): TList<string>;

implementation
 function compareStrResults(testResult: string; refResult: string): TList<string>;
 var testResultList: TStringList;
     refList: TStringList;
     i: integer;
 begin
   Result := TList<string>.create;
   testResultList := TStringList.Create;
   testResultList.Text := testResult;// break up into array of strings based on newLine char
   refList := TStringList.Create;
   refList.Text := refResult;
   for i := 0 to refList.Count -1 do
     begin
     if i < testResultList.Count then  // Make sure index not out of bounds
       begin
       if trim(testResultList[i]) <> trim(refList[i]) then
         begin
         Result.Add(testResultList[i] + ', *** Ref: ' +refList[i]);
         end;
       end
     else Result.Add('Test line blank, Ref: ' +refList[i]);
     end;

 end;


end.
