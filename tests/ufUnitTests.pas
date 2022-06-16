unit ufUnitTests;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections, LSODA.test, uTestLSODA_JS, uTestCase, uTestSBMLReadWrite;

const SIMULATOR = 0;
      TESTGROUPS: array [0..1] of string = ('Simulation group', 'Reading/Writing SBML models');

type
  TfUnitTests = class(TWebForm)
    btnRunall: TWebButton;
    lbTestGroups: TWebListBox;
    btnRunSpecifiedTests: TWebButton;
    lblPickTests: TWebLabel;
    lbTestResults: TWebListBox;
    lblResults: TWebLabel;
    btnSaveFile: TWebButton;

    procedure WebFormCreate(Sender: TObject);
    procedure btnRunSpecifiedTestsClick(Sender: TObject);
    procedure btnRunallClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
  private
    procedure runSimulationTests();
    procedure runSBMLReadWriteTests();
    procedure populateTestResultsListBox();
    procedure addFinishedTestCases( newTestCases: TList<TTestCase> );
  public
    testCases: TList<TTestCase>;
    procedure SBMLReadWriteTestsDone( testCaseResults : TList<TTestCase> );
  end;

var
  fUnitTests: TfUnitTests;

implementation

{$R *.dfm}

procedure TfUnitTests.btnRunallClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to length(TESTGROUPS) -1 do
    begin
      if i = 0 then self.runSimulationTests;
      if i = 1 then self.runSBMLReadWriteTests;
    end;
end;

procedure TfUnitTests.btnRunSpecifiedTestsClick(Sender: TObject);
var i: integer;
begin
  //for i := 0 to self.lbTestGroups.Count -1 do
  //  begin
      if self.lbTestGroups.Selected[0] then self.runSimulationTests;
      if self.lbTestGroups.Selected[1] then self.runSBMLReadWriteTests;
  //  end;
end;

procedure TfUnitTests.btnSaveFileClick(Sender: TObject);
begin
// TODO
end;

procedure TfUnitTests.WebFormCreate(Sender: TObject);
var i: integer;
    testList: TStringList;
begin
  self.testCases := TList<TTestCase>.create;
  testList := TStringList.create();
  for i := 0 to length(TESTGROUPS) -1 do
    begin
      testList.Add(TESTGROUPS[i]);
    end;
  self.lbTestGroups.Items := testList;  // Populate listbox
  self.lbTestGroups.MultiSelect := true;// Allow user to pick more than one test group
end;

procedure TfUnitTests.runSimulationTests;
var lsoda_JSTestRun: TLSODA_JSTests;
    curTestList: TList<TTestCase>;
begin
  self.testCases := LSODA_Test(self.testCases); // Pascal only test
  lsoda_JSTestRun := TLSODA_JSTests.create;
  curTestList := lsoda_JSTestRun.LSODATests; // Javascript/pascal mix
  self.addFinishedTestCases(curTestList);
  curTestList := nil;
  self.populateTestResultsListBox;
end;

procedure TfUnitTests.runSBMLReadWriteTests();
var sbmlReadWrite: TTestSBMLReadWrite;
    curTestList: TList<TTestCase>;
begin
  console.log('Running SBML Read-Write tests');
  sbmlReadWrite := TTestSBMLReadWrite.create;
  sbmlReadWrite.OnNotify := self.SBMLReadWriteTestsDone;  // asyncronous calls
  sbmlReadWrite.runTests;

end;

procedure TfUnitTests.SBMLReadWriteTestsDone( testCaseResults : TList<TTestCase> );
begin
  console.log('Got SBML read-write test results');
  self.addFinishedTestCases(testCaseResults);
  self.populateTestResultsListBox;
end;

procedure TfUnitTests.populateTestResultsListBox();
var i: integer;
    testStr: string;
    strBool: string;
begin
  self.lbTestResults.clear;
  for i := 0 to self.testCases.Count -1 do
    begin
    testStr := '';
    if self.testCases[i].getBooleanTestResult then strBool := 'Pass' else strBool := '** Fail';
    testStr := strBool + ': ' + inttostr(self.testCases[i].getTestId) + ': ' + self.testCases[i].getTestName;
    if self.testCases[i].sTestInfoList.Count > 0 then
      testStr := testStr + ': ' + self.testCases[i].sTestInfoList[0];

    self.lbTestResults.AddItem(testStr, nil);

    end;

end;

procedure TfUnitTests.addFinishedTestCases( newTestCases: TList<TTestCase> );
var i: integer;
begin
  for i := 0 to newTestCases.Count -1 do
    begin
    self.testCases.add(newTestCases[i]);
    self.testCases[self.testCases.Count -1].setTestId(self.testCases.Count);
    end;
end;

end.