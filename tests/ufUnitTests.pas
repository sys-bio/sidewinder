unit ufUnitTests;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections, LSODA.test, uTestLSODA_JS, uTestCase, uTestSBMLReadWrite,
  uTestSBMLReadGenerateEqs, uSidewinderTypes;

const TESTGROUPS: array [0..2] of string = ('Simulation tests', 'Reading/Writing SBML models',
      'Read SBML Model and generate equations list for Sim');

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
    strTestResults: string; // used to write to file.
    runAllTests: boolean;
    groupTestIndex: integer;
    procedure runSimulationTests();
    procedure runSBMLReadWriteTests();
    procedure runReadSBMLGenerateEqsTests(); // read SBML model generate Eqs for Sim.
    procedure populateTestResultsListBox();
    procedure addFinishedTestCases( newTestCases: TList<TTestCaseResult> );
    procedure saveTestResults(fName: string);
  public
    testCases: TList<TTestCaseResult>;
    procedure testsDone( testCaseResults : TList<TTestCaseResult> );  // callback
  end;

var
  fUnitTests: TfUnitTests;

implementation

{$R *.dfm}

procedure TfUnitTests.btnRunallClick(Sender: TObject);
var
  i: Integer;
begin
  self.groupTestIndex := 0;
  self.runAllTests := true;
  self.runSimulationTests;
end;

procedure TfUnitTests.btnRunSpecifiedTestsClick(Sender: TObject);
begin
    if self.lbTestGroups.Selected[0] then self.runSimulationTests;
    if self.lbTestGroups.Selected[1] then self.runSBMLReadWriteTests;
    if self.lbTestGroups.Selected[2] then self.runReadSBMLGenerateEqsTests;

end;

procedure TfUnitTests.btnSaveFileClick(Sender: TObject);
var fileName: string;
begin
  fileName := InputBox('Save Test results to the downloads directory',
    'Enter File Name:', 'newtest.txt');
  if fileName <> '' then
    begin
      if self.strTestResults <> '' then
        self.saveTestResults(fileName)
      else notifyUser('No results to save.');
    end
  else
    notifyUser('Save cancelled');
end;

procedure TfUnitTests.saveTestResults(fName: string);
 begin
   Application.DownloadTextFile(self.strTestResults, fName);
 end;


procedure TfUnitTests.WebFormCreate(Sender: TObject);
var i: integer;
    testList: TStringList;
begin
  self.testCases := TList<TTestCaseResult>.create;
  self.groupTestIndex := 0;
  self.strTestResults := '';
  self.runAllTests := false;
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
    curTestList: TList<TTestCaseResult>;
begin
  self.testCases := LSODA_Test(self.testCases); // Pascal only test
  lsoda_JSTestRun := TLSODA_JSTests.create;
  curTestList := lsoda_JSTestRun.LSODATests; // Javascript/pascal mix
  self.testsDone(curTestList);
end;

procedure TfUnitTests.runSBMLReadWriteTests();
var sbmlReadWrite: TTestSBMLReadWrite;
    curTestList: TList<TTestCaseResult>;
begin
  console.log('Running SBML Read-Write tests');
  sbmlReadWrite := TTestSBMLReadWrite.create;
  sbmlReadWrite.OnNotify := self.testsDone;  // asyncronous calls
  sbmlReadWrite.runTests;
end;

procedure TfUnitTests.runReadSBMLGenerateEqsTests();
var curTestList: TList<TTestCaseResult>;
    sbmlReadGenerateEqs: TTestSBMLReadGenerateEqs;
begin
  console.log(' Running SBML read, generate Eq list tests');
  sbmlReadGenerateEqs := TTestSBMLReadGenerateEqs.create;
  sbmlReadGenerateEqs.OnNotify := self.testsDone;
  sbmlReadGenerateEqs.runTest;
end;

procedure TfUnitTests.testsDone( testCaseResults : TList<TTestCaseResult> );
begin
  console.log('Got group test results');
  self.addFinishedTestCases(testCaseResults);
  self.populateTestResultsListBox;
  if self.runAllTests then
  begin
    inc(self.groupTestIndex);
   // Call next group of tests....
    if self.groupTestIndex = 1 then self.runSBMLReadWriteTests;
    if self.groupTestIndex = 2 then self.runReadSBMLGenerateEqsTests;
  end;

end;

procedure TfUnitTests.populateTestResultsListBox();
var i: integer;
    testStr: string;
    strBool: string;
begin
  self.lbTestResults.clear;
  self.strTestResults := '';
  for i := 0 to self.testCases.Count -1 do
    begin
    testStr := '';
    if self.testCases[i].getBooleanTestResult then strBool := 'Pass' else strBool := '** Fail';
    testStr := strBool + ': ' + inttostr(self.testCases[i].getTestId) + ': ' + self.testCases[i].getTestName;
    if self.testCases[i].sTestInfoList.Count > 0 then
      testStr := testStr + ': ' + self.testCases[i].sTestInfoList[0];
    self.strTestResults := self.strTestResults + testStr + sLineBreak;
    self.lbTestResults.AddItem(testStr, nil);
    end;

end;

procedure TfUnitTests.addFinishedTestCases( newTestCases: TList<TTestCaseResult> );
var i: integer;
begin
  for i := 0 to newTestCases.Count -1 do
    begin
    self.testCases.add(newTestCases[i]);
    self.testCases[self.testCases.Count -1].setTestId(self.testCases.Count);
    end;
end;

end.