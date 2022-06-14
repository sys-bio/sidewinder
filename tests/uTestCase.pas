unit uTestCase;

interface
uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections;

type
 TTestCase = class
   private
   intTestId: integer;
   testResult: boolean;
   strTestName: string;

   public
   sTestInfoList: TList<string>; // contains any error info
   constructor create(newTestId: integer; newTestName: string) overload;
   constructor create(cpTestCase: TTestCase) overload;
   procedure testPass;
   procedure testFail;
   function getBooleanTestResult(): boolean;
   function getTestId(): integer;
   procedure setTestId( newId: integer);
   function getTestName(): string;

 end;


implementation

   constructor  TTestCase.create(newTestId: integer; newTestName: string) overload;
   begin
     self.intTestId := newTestId;
     self.strTestName := newTestName;
     self.testResult := false;
     self.sTestInfoList := TList<string>.create;
   end;

   constructor TTestCase.create(cpTestCase: TTestCase) overload;
   var i: integer;
   begin
     self.intTestId := cpTestCase.getTestId;
     self.strTestName := cpTestCase.getTestName;
     self.testResult := cpTestCase.getBooleanTestResult;
     self.sTestInfoList := TList<string>.create;
     for i := 0 to cpTestCase.sTestInfoList.Count -1 do
       self.sTestInfoList.Add(cpTestCase.sTestInfoList[i]);
   end;

   procedure  TTestCase.testPass;
   begin
     self.testResult := true;
   end;

   procedure  TTestCase.testFail;
   begin
     self.testResult := false;
   end;

   function TTestCase.getBooleanTestResult: boolean;
   begin
     Result := self.testResult;
   end;

   function TTestCase.getTestId(): integer;
   begin
     Result := self.intTestId;
   end;
   procedure TTestCase.setTestId( newId: integer);
   begin
     if newId >0 then self.intTestId := newId;

   end;

   function TTestCase.getTestName(): string;
   begin
     Result := self.strTestName;
   end;

end.
