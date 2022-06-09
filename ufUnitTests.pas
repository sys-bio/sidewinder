unit ufUnitTests;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections, LSODA.test;

const SIMULATOR = 0;
      TESTGROUPS: array [0..1] of string = ('Simulation group', 'Reading/Writing SBML models');

type
  TfUnitTests = class(TWebForm)
    btnRunall: TWebButton;
    lbTestGroups: TWebListBox;
    btnRunSpecifiedTests: TWebButton;
    lblPickTests: TWebLabel;
    procedure lbTestGroupsClick(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure btnRunSpecifiedTestsClick(Sender: TObject);
  private
    procedure runSimulationTests();
    procedure runSBMLReadWriteTests();
  public
    { Public declarations }
  end;

var
  fUnitTests: TfUnitTests;

implementation

{$R *.dfm}

procedure TfUnitTests.btnRunSpecifiedTestsClick(Sender: TObject);
var i: integer;
begin
// TODO
  for i := 0 to self.lbTestGroups.Count -1 do
    begin
      if self.lbTestGroups.Selected[0] then self.runSimulationTests;
      if self.lbTestGroups.Selected[1] then self.runSBMLReadWriteTests;
    end;
end;

procedure TfUnitTests.lbTestGroupsClick(Sender: TObject);
begin
// TODO
end;

procedure TfUnitTests.WebFormCreate(Sender: TObject);
var i: integer;
    testList: TStringList;
begin

  testList := TStringList.create();
  for i := 0 to length(TESTGROUPS) -1 do
    begin
      testList.Add(TESTGROUPS[i]);
    end;
  self.lbTestGroups.Items := testList;
  self.lbTestGroups.MultiSelect := true;
end;

procedure TfUnitTests.runSimulationTests;
begin
  LSODA_Test; // Need to integrate ufTestLSODA.pas into this. LSODA_TEST is OLD.
end;
procedure TfUnitTests.runSBMLReadWriteTests();
begin
  console.log('Running SBML Read-Write tests');
end;

end.