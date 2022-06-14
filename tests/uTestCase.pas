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
   constructor create(newTestId: integer; newTestName: string);
   procedure testPass;
   procedure testFail;
   function getBooleanTestResult(): boolean;
   function getTestId(): integer;
   function getTestName(): string;

 end;


implementation

   constructor  TTestCase.create(newTestId: integer; newTestName: string);
   begin
     self.intTestId := newTestId;
     self.strTestName := newTestName;
     self.testResult := false;
     self.sTestInfoList := TList<string>.create;
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
   function TTestCase.getTestName(): string;
   begin
     Result := self.strTestName;
   end;

end.
