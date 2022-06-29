unit uTestCase;

interface
uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections;

type
 TTestCaseResult = class
   private
   intTestId: integer;
   testResult: boolean;
   strTestName: string;

   public
   sTestInfoList: TList<string>; // contains any error info
   constructor create(newTestId: integer; newTestName: string) overload;
   constructor create(cpTestCase: TTestCaseResult) overload;
   procedure testPass;
   procedure testFail;
   function getBooleanTestResult(): boolean;
   function getTestId(): integer;
   procedure setTestId( newId: integer);
   function getTestName(): string;

 end;


implementation

   constructor  TTestCaseResult.create(newTestId: integer; newTestName: string) overload;
   begin
     self.intTestId := newTestId;
     self.strTestName := newTestName;
     self.testResult := false;
     self.sTestInfoList := TList<string>.create;
   end;

   constructor TTestCaseResult.create(cpTestCase: TTestCaseResult) overload;
   var i: integer;
   begin
     self.intTestId := cpTestCase.getTestId;
     self.strTestName := cpTestCase.getTestName;
     self.testResult := cpTestCase.getBooleanTestResult;
     self.sTestInfoList := TList<string>.create;
     for i := 0 to cpTestCase.sTestInfoList.Count -1 do
       self.sTestInfoList.Add(cpTestCase.sTestInfoList[i]);
   end;

   procedure  TTestCaseResult.testPass;
   begin
     self.testResult := true;
   end;

   procedure  TTestCaseResult.testFail;
   begin
     self.testResult := false;
   end;

   function TTestCaseResult.getBooleanTestResult: boolean;
   begin
     Result := self.testResult;
   end;

   function TTestCaseResult.getTestId(): integer;
   begin
     Result := self.intTestId;
   end;
   procedure TTestCaseResult.setTestId( newId: integer);
   begin
     if newId >0 then self.intTestId := newId;

   end;

   function TTestCaseResult.getTestName(): string;
   begin
     Result := self.strTestName;
   end;

end.
