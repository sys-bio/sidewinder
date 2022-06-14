unit uTestSBMLReadWrite;

// Test reading a SBML file and storing in a TModel using TSBML Classes
// Test writing a SBML file from TModel

interface
uses   System.SysUtils, System.Classes, JS, Web, System.Generics.Collections,
 uTestCase, uModel, uSBMLClasses;

type
 TTestSBMLReadWrite = class
  private
    refResultsList: TList<string>;
    testModel: TModel;
    procedure generateModel();
  public
    resultInfo: TList<string>;
    testResultList: TList<TTestCase>;

    constructor create();
    function run writeSBMLTests(): TList<TTestCase>;
    function run readSBMLTests(): TList<TTestCase>;
 end;


implementation
constructor TTestSBMLReadWrite.create();
begin
  self.testResultList := TList<TTestCase>.create;
  self.testModel := TModel.create;
end;

procedure TTestSBMLReadWrite.generateModel();
var i: integer;
    testSpecies: TSBMLSpecies;
begin
  //TODO
  testSpecies := TSBMLSpecies.create('S1');

  self.testModel.addSBMLspecies(testSpecies);
  testSpecies.free;
  testSpecies := TSBMLSpecies.create('S2');

end;

end.
