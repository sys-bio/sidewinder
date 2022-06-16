unit uTestSBMLReadWrite;

// Test reading a SBML file and storing in a TModel using TSBML Classes
// Test writing a SBML file from TModel

interface
uses   System.SysUtils, System.Classes, JS, Web, System.Generics.Collections,
 uTestCase, utests.TestUtils, uSBMLClasses, uSBMLWriter, uModel;

const NUM_WRITE_TESTS = 1;
      NUM_READ_TESTS = 0;

type
 TReadWriteTestsFinished = procedure(readWriteTestCase: TList<TTestCase>) of object;  // Notify when done testing
 TTestSBMLReadWrite = class
  private
    FNotify: TReadWriteTestsFinished; // send sbml info to listener once asynchronous read done.
    testModel: TModel;
    currentWriteTestIndex: integer; // Due to asynchronous calls to libsbml.js, need to keep track of this?
    currentReadTestIndex: integer;
    function generateWriteTestModel(testIndex: integer): TModel;
    function getTestReferenceString(testIndex: integer): string;
  public
    resultInfo: TList<string>;
    testResultList: TList<TTestCase>;

    constructor create();
    procedure runTests; // run Write, then read tests
    procedure runWriteSBMLTests();
    procedure runReadSBMLTests();
    procedure testsFinished(); // Notify listener
    procedure modelWritten(modelStr: String);  // callback: notified when SBML string created.
    procedure modelRead(testModel: TModel);  // callback: notified when SBML string created.
    property OnNotify: TReadWriteTestsFinished read FNotify write FNotify;
 end;


implementation
constructor TTestSBMLReadWrite.create();
begin
  self.testResultList := TList<TTestCase>.create;
  self.currentWriteTestIndex := -1;
  self.currentReadTestIndex := -1;
end;

procedure TTestSBMLReadWrite.modelWritten(modelStr: String);
var i: integer; refStr: string;
    refResultsList: TList<string>;
begin
if self.currentWriteTestIndex > -1 then
  begin
  refResultsList := TList<string>.create;
  refStr := self.getTestReferenceString(self.currentWriteTestIndex);
  if refStr <> '' then
    begin
    refResultsList := compareStrResults(modelStr, refStr);
    if self.testResultList.count > self.currentWriteTestIndex then
      begin
      if refResultsList.Count <1 then self.testResultList[self.currentWriteTestIndex].testPass
      else
        begin
        self.testResultList[self.currentWriteTestIndex].testFail;
        for i := 0 to refResultsList.count -1 do
          self.testResultList[self.currentWriteTestIndex].sTestInfoList.Add(refResultsList[i]);
        end;
      end;



    end;
  end;
console.log('MODEL: ', modelStr);
end;

procedure TTestSBMLReadWrite.modelRead(testModel: TModel);
begin
  console.log('MODEL read');
end;

procedure TTEstSBMLReadWrite.runTests;
begin
  self.runWriteSBMLTests;
  self.runReadSBMLTests;
  self.testsFinished;

end;

procedure TTestSBMLReadWrite.runWriteSBMLTests();
var i: integer;
    testModel: TModel;
    sbmlTestWriter: TSBMLWriter;
begin
  for i := 0 to NUM_WRITE_TESTS -1 do
    begin
    self.testResultList.Add(TTestCase.create(i+1, 'SBML write test ' + inttostr(i)));
    testModel := self.generateWriteTestModel(i);
    self.currentWriteTestIndex := i;
    sbmlTestWriter := TSBMLWriter.create();
    sbmlTestWriter.OnNotify := self.modelWritten;
    sbmlTestWriter.buildSBMLString(testModel);
    end;
end;

procedure TTestSBMLReadWrite.runReadSBMLTests();
var i: integer;
begin

end;

procedure TTestSBMLReadWrite.testsFinished;
begin
    if Assigned(FNotify) then
     FNotify(self.testResultList);
 end;

function TTestSBMLReadWrite.generateWriteTestModel(testIndex: integer): TModel;
var i: integer;
    curModel: TModel;
    testSpeciesAr: array of TSBMLSpecies; testComp: TSBMLCompartment; testParamAr: array of TSBMLparameter;
    testRxn: SBMLReaction;
    testProdSpAr: array of TSBMLSpeciesReference; testReactSpAr: array of TSBMLSpeciesReference;
begin
  case testIndex of
    0: begin curModel := TModel.create;
       setLength(testSpeciesAr, 2);
       testSpeciesAr[0] := TSBMLSpecies.create('S1');
       testSpeciesAr[0].setInitialConcentration(13);
       curModel.addSBMLspecies(testSpeciesAr[0]);
       testSpeciesAr[1] := TSBMLSpecies.create('S2');
       curModel.addSBMLspecies(testSpeciesAr[1]);
       testComp := TSBMLCompartment.create('Compartment01');
       testComp.setVolume(1.5);
       curModel.addSBMLCompartment(testComp);
       setLength(testParamAr, 1);
       testParamAr[0] := TSBMLparameter.create('k1');
       testParamAr[0].setValue(0.15);
       testParamAr[0].setConstant(true);
       curModel.addSBMLParameter(testParamAr[0]);
       setLength(testProdSpAr, 1); setLength(testReactSpAr, 1);
       testReactSpAr[0] := TSBMLSpeciesReference.create('S1', 1);
       testProdSpAr[0] := TSBMLSpeciesReference.create('S2', 2);
       testRxn := SBMLReaction.create('Reaction_1', testProdSpAr, testReactSpAr);
       testRxn.setKineticLaw( SBMLKineticLaw.create('reaction1_kinlaw','k1*S1', testParamAr) );
       curModel.addSBMLReaction(testRxn);
       Result := curModel;
    end;
    // ****************************************

    else Result := nil;
  end;


end;
  function TTestSBMLReadWrite.getTestReferenceString(testIndex: Integer): string;
  var refTestStrList: TList<string>;
  begin
    refTestStrList := TList<string>.create;
    refTestStrList.Add('<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="2" layout:required="false" render:required="false">' + sLineBreak +
  '<model>' + sLineBreak +
    '<listOfCompartments>' + sLineBreak +
      '<compartment id="Compartment01" constant="false"/>' + sLineBreak +
    '</listOfCompartments>' + sLineBreak +
    '<listOfSpecies>' + sLineBreak +
      '<species id="S1" initialConcentration="13" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S2" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
    '</listOfSpecies>' + sLineBreak +
    '<listOfParameters>' + sLineBreak +
      '<parameter id="k1" value="0.15" constant="true"/>' + sLineBreak +
    '</listOfParameters>' + sLineBreak +
    '<listOfReactions>' + sLineBreak +
      '<reaction id="Reaction_1" reversible="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference id="S1" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference id="S2" stoichiometry="2" constant="false"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw id="reaction1_kinlaw">' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> k1 </ci>' + sLineBreak +
              '<ci> S1 </ci>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
    '</listOfReactions>' + sLineBreak +
    '<layout:listOfLayouts xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1">' + sLineBreak +
      '<layout:layout>' + sLineBreak +
        '<layout:dimensions layout:width="0" layout:height="0"/>' + sLineBreak +
        '<render:listOfRenderInformation xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1">' + sLineBreak +
          '<render:renderInformation/>' + sLineBreak +
        '</render:listOfRenderInformation>' + sLineBreak +
      '</layout:layout>' + sLineBreak +
    '</layout:listOfLayouts>' + sLineBreak +
  '</model>' + sLineBreak +
'</sbml>' );



  if refTestStrList.count > testIndex then
    Result := refTestStrList[testIndex]
  else Result := '';

  end;


end.
