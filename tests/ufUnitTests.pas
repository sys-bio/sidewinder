unit ufUnitTests;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections, LSODA.test, uTestLSODA_JS, uTestCase;

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
  public
    testCases: TList<TTestCase>;
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
  for i := 0 to self.lbTestGroups.Count -1 do
    begin
      if self.lbTestGroups.Selected[0] then self.runSimulationTests;
      if self.lbTestGroups.Selected[1] then self.runSBMLReadWriteTests;
    end;
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
begin
  self.testCases := LSODA_Test(self.testCases); // Pascal only test
//  lsoda_JSTestRun := TLSODA_JSTests.create;
 // lsoda_JSTestRun.LSODATests; // Javascript/pascal mix
  self.populateTestResultsListBox;
end;
procedure TfUnitTests.runSBMLReadWriteTests();
begin
  console.log('Running SBML Read-Write tests');
end;

procedure TfUnitTests.populateTestResultsListBox();
var i: integer;
    testStr: string;
    strBool: string;
begin
  for i := 0 to self.testCases.Count -1 do
    begin
    testStr := '';
    if self.testCases[i].getBooleanTestResult then strBool := 'Pass' else strBool := 'Fail';
    testStr := strBool + ': ' + inttostr(self.testCases[i].getTestId) + ': ' + self.testCases[i].getTestName;
    if self.testCases[i].sTestInfoList.Count > 0 then
      testStr := testStr + ': ' + self.testCases[i].sTestInfoList[0];
    //self.lbTestResults.ItemIndex := i;
    self.lbTestResults.AddItem(testStr, nil);

    end;

end;

end.