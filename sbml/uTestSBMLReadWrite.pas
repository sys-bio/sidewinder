unit uTestSBMLReadWrite;

// Test reading a SBML file and storing in a TModel using TSBML Classes and libsbml.js
// Test writing a SBML file from TModel using TSBML classes and libsbml.js.
//
// Add tests:
// 1a. If Read test then add sbml model to uTestSBML_ReadModels.pas
//     Update SBML_TEST_MODELS (in uTestSBML_ReadModels) by one.
// 1b. If Write test then add TModel test to generateWriteTestModel() method.
// 2. Run 'Reading/Writing SBML models' test group from testing harness
// 3. Open up browser console window and grab pertinent output from newly added test.
//    Ouptut string should be after 'MODEL for SBML write test' or 'MODEL for SBML read test'
// 4a. Read test: add output to getWriteTestReferenceString() method
// 4b. Write test: add output to getReadTestReferenceString() method
// 5. TestSBML Read Generate Eqs uses the same test models so you need to update uTestSBMLGenerateEqs
//    with expected results.


interface
uses   System.SysUtils, System.Classes, JS, Web, System.Generics.Collections,
 uTestCase, utests.TestUtils, uSBMLClasses, uSBMLWriter, uSBMLReader, uModel, uTestModel,
 uTestSBML_ReadModels, uSBMLClasses.rule;

type
 TReadWriteTestsFinished = procedure(readWriteTestCase: TList<TTestCaseResult>) of object;  // Notify when done testing
 TTestSBMLReadWrite = class
  private
    FNotify: TReadWriteTestsFinished; // send sbml info to listener once asynchronous read done.
    testModel: TModel;
    writeRefResults: TList<string>;
    readRefResults: TList<string>;
    currentWriteTestIndex: integer; // Due to asynchronous calls to libsbml.js, need to keep track of this?
    currentReadTestIndex: integer;
    function  generateWriteTestModel(testIndex: integer): TModel;
    function  generateReadTestModel(testIndex: integer): string;
    procedure generateWriteTestReferences();
    procedure generateReadTestReferences();
    function  getWriteTestReferenceString(testIndex: integer): string;
    function  getReadTestReferenceString(testIndex: integer): string;
    function  getNumReadTests(): integer;
    function  getNumWriteTests(): integer;
    function  getNumReadWriteTests(): integer;
  public
    resultInfo: TList<string>;
    testResultList: TList<TTestCaseResult>;

    constructor create();
    procedure runTests; // run Write, then read tests
    procedure runWriteSBMLTest();
    procedure runReadSBMLTest();
    procedure testsFinished(); // Notify listener
    procedure modelWritten(modelStr: String);  // callback: notified when SBML string created.
    procedure modelRead();  // callback: notified when SBML string created.
    property OnNotify: TReadWriteTestsFinished read FNotify write FNotify;
 end;


implementation

constructor TTestSBMLReadWrite.create();
begin
  self.generateWriteTestReferences;
  self.generateReadTestReferences;
  self.testResultList := TList<TTestCaseResult>.create;
  self.currentWriteTestIndex := 0;
  self.currentReadTestIndex := 0;
end;

function  TTestSBMLReadWrite.getNumReadTests(): integer;
begin
  Result := self.readRefResults.count;
end;
function TTestSBMLReadWrite.getNumWriteTests(): integer;
begin
  Result := self.writeRefResults.count;
end;
function TTestSBMLReadWrite.getNumReadWriteTests(): integer;
begin
  Result := self.readRefResults.count + self.writeRefResults.count;
end;

procedure TTestSBMLReadWrite.modelWritten(modelStr: String);
var i: integer; refStr: string;
    refResultsList: TList<string>;
begin
if self.currentWriteTestIndex > -1 then
  begin
  refResultsList := TList<string>.create;
  refStr := self.getWriteTestReferenceString(self.currentWriteTestIndex);
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
 console.log('MODEL for SBML write test: ', self.currentWriteTestIndex + 1);
 console.log(modelStr);
 inc(self.currentWriteTestIndex); // next test...
 if self.currentWriteTestIndex < self.writeRefResults.count then
   runWriteSBMLTest( )
 else
   begin
   if self.currentReadTestIndex < self.readRefResults.count then
     runReadSBMLTest()
   else self.testsFinished;
   end
end;

procedure TTestSBMLReadWrite.modelRead();
var i: integer; refStr, testModelStr: string;
    refResultsList: TList<string>;
// self.testResultList index is assumed to be at self.currentWriteTestIndex + self.currentReadTestIndex
begin
  console.log('MODEL for SBML read test: ', self.currentReadTestIndex +1 );
  if self.currentReadTestIndex > -1 then
    begin
    testModelStr := self.testModel.printStr;
    console.log(testModelStr); // <== if need to update reference just copy from console and add ' ' and sLineBreak to each line.
            // Example line: '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
    refResultsList := TList<string>.create;
    refStr := self.getReadTestReferenceString(self.currentReadTestIndex);
    if refStr <> '' then
      begin
      refResultsList := compareStrResults(testModelStr, refStr);
      if self.testResultList.count > (self.currentWriteTestIndex + self.currentReadTestIndex) then
        begin
        if refResultsList.Count <1 then self.testResultList[self.currentWriteTestIndex + self.currentReadTestIndex].testPass
        else
          begin
          self.testResultList[self.currentWriteTestIndex + self.currentReadTestIndex].testFail;
          for i := 0 to refResultsList.count -1 do
            self.testResultList[self.currentWriteTestIndex + self.currentReadTestIndex].sTestInfoList.Add(refResultsList[i]);
          end;
        end;
      end;
    end;

  inc(self.currentReadTestIndex);
  //if self.currentReadTestIndex < self.readRefResults.count then    // add one if adding new test
  if self.currentReadTestIndex < 2 + SBML_TEST_MODELS then
    begin
    self.testModel.Free;
    runReadSBMLTest( )
    end
  else self.testsFinished;
end;

procedure TTEstSBMLReadWrite.runTests;
begin
  if self.writeRefResults.count > 0 then
    self.runWriteSBMLTest
  else if self.readRefResults.count > 0 then self.runReadSBMLTest;


end;

procedure TTestSBMLReadWrite.runWriteSBMLTest();
var testWriteModel: TModel;
    sbmlTestWriter: TSBMLWriter;
begin
  self.testResultList.Add(TTestCaseResult.create(self.currentWriteTestIndex +1,
                   'SBML write test ' + inttostr(self.currentWriteTestIndex +1)));
  testWriteModel := self.generateWriteTestModel(self.currentWriteTestIndex);
  sbmlTestWriter := TSBMLWriter.create();
  sbmlTestWriter.OnNotify := self.modelWritten;
  sbmlTestWriter.buildSBMLString(testWriteModel);
end;

procedure TTestSBMLReadWrite.runReadSBMLTest();
var i: integer;
    sbmlTestReader: TSBMLRead;
    testSBMLStr: string;
begin
  self.testResultList.Add(TTestCaseResult.create(self.currentReadTestIndex +1,
                   'SBML read test ' + inttostr(self.currentReadTestIndex +1)));
  testSBMLStr := self.generateReadTestModel(self.currentReadTestIndex);
  console.log(' *** SBML string: ');
  console.log(testSBMLStr);
  self.testModel := TModel.create;
  self.testModel.OnPing := self.modelRead;
  sbmlTestReader := TSBMLRead.create(self.testModel, testSBMLStr );

end;

procedure TTestSBMLReadWrite.testsFinished;
begin
    if Assigned(FNotify) then
     FNotify(self.testResultList);
 end;

function TTestSBMLReadWrite.generateWriteTestModel(testIndex: integer): TModel;
var i: integer;
    curModel: TModel; testInitAssignAr: array of TSBMLInitialAssignment;
    testAssignRuleAr: array of TSBMLRule;
    testSpeciesAr: array of TSBMLSpecies; testComp: TSBMLCompartment; testParamAr: array of TSBMLparameter;
    testRxn: SBMLReaction; testRxnAr: array of SBMLReaction;
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
    // Model with 2 init assignments, 1 assignment rule and 5 reactions
    1: begin curModel := TModel.create;
       curModel.setModelId('Feedback_with_InitAssignments');
       setLength(testSpeciesAr, 6);
       testSpeciesAr[0] := TSBMLSpecies.create('X0');
       testSpeciesAr[0].setInitialConcentration(10);
       testSpeciesAr[0].setBoundaryCondition(true);
       curModel.addSBMLspecies(testSpeciesAr[0]);
       testSpeciesAr[1] := TSBMLSpecies.create('S1');
       testSpeciesAr[1].setInitialConcentration(0);
       curModel.addSBMLspecies(testSpeciesAr[1]);
       testSpeciesAr[2] := TSBMLSpecies.create('S2');
       testSpeciesAr[2].setInitialConcentration(0);
       curModel.addSBMLspecies(testSpeciesAr[2]);
       testSpeciesAr[3] := TSBMLSpecies.create('S3');
       testSpeciesAr[3].setInitialConcentration(0);
       curModel.addSBMLspecies(testSpeciesAr[3]);
       testSpeciesAr[4] := TSBMLSpecies.create('X1');
       testSpeciesAr[4].setInitialConcentration(0);
       testSpeciesAr[4].setBoundaryCondition(true);
       curModel.addSBMLspecies(testSpeciesAr[4]);
       testSpeciesAr[5] := TSBMLSpecies.create('S4');
       testSpeciesAr[5].setInitialConcentration(0);
       curModel.addSBMLspecies(testSpeciesAr[5]);
       testComp := TSBMLCompartment.create('Compartment01');
       testComp.setVolume(1.1);
       curModel.addSBMLCompartment(testComp);
       setLength(testParamAr, 5);
       testParamAr[0] := TSBMLparameter.create('VM1');
       //testParamAr[0].setValue(0.15);
       testParamAr[0].setConstant(false);
       curModel.addSBMLParameter(testParamAr[0]);
       testParamAr[1] := TSBMLparameter.create('Keq1');
       //testParamAr[0].setValue(0.15);
       testParamAr[1].setConstant(false);
       curModel.addSBMLParameter(testParamAr[1]);
       testParamAr[2] := TSBMLparameter.create('h');
       testParamAr[2].setConstant(false);
       curModel.addSBMLParameter(testParamAr[2]);
       testParamAr[3] := TSBMLparameter.create('V4');
       testParamAr[0].setValue(2.5);
       testParamAr[3].setConstant(false);
       curModel.addSBMLParameter(testParamAr[3]);
       testParamAr[4] := TSBMLparameter.create('KS4');
       testParamAr[0].setValue(0.5);
       testParamAr[4].setConstant(false);
       curModel.addSBMLParameter(testParamAr[4]);
       setLength(testInitAssignAr, 2);
       testInitAssignAr[0] := TSBMLInitialAssignment.create();
       testInitAssignAr[0].setId('initAssign_0');
       testInitAssignAr[0].setSymbol('h');
       testInitAssignAr[0].setFormula('Keq1 + 2');
       curModel.addInitialAssignment(testInitAssignAr[0]);
       testInitAssignAr[1] := TSBMLInitialAssignment.create();
       testInitAssignAr[1].setId('initAssign_1');
       testInitAssignAr[1].setSymbol('VM1');
       testInitAssignAr[1].setFormula('10 + S2');
       curModel.addInitialAssignment(testInitAssignAr[1]);
       setLength(testAssignRuleAr, 1);
       testAssignRuleAr[0] := TSBMLRule.create();
       testAssignRuleAr[0].setId('AssignRule_0');
       testAssignRuleAr[0].setAssignment(true);
       testAssignRuleAr[0].setParameter(true);
       testAssignRuleAr[0].setVariable('Keq1');
       testAssignRuleAr[0].setFormula('S1*10+10');
       curModel.addSBMLrule(testAssignRuleAr[0]);

       setLength(testProdSpAr, 1); setLength(testReactSpAr, 1);
       testReactSpAr[0] := TSBMLSpeciesReference.create('X0', 1);
       testProdSpAr[0] := TSBMLSpeciesReference.create('S1', 1);
       setLength(testRxnAr, 5);
       testRxnAr[0] := SBMLReaction.create('J0', testProdSpAr, testReactSpAr);
       testRxnAr[0].setReversible(true);
       testRxnAr[0].setKineticLaw( SBMLKineticLaw.create('reaction0_kinlaw','(VM1 * (X0 - S1/Keq1))/(1 + X0 + S1 + S4^h)', testParamAr) );
       curModel.addSBMLReaction(testRxnAr[0]);

       setLength(testProdSpAr, 1); setLength(testReactSpAr, 1);
       testReactSpAr[0] := TSBMLSpeciesReference.create('S1', 1);
       testProdSpAr[0] := TSBMLSpeciesReference.create('S2', 1);
       testRxnAr[1] := SBMLReaction.create('J1', testProdSpAr, testReactSpAr);
       testRxnAr[1].setReversible(true);
       testRxnAr[1].setKineticLaw( SBMLKineticLaw.create('reaction1_kinlaw','(10 * S1 - 2 * S2) / (1 + S1 + S2)', testParamAr) );
       curModel.addSBMLReaction(testRxnAr[1]);

       setLength(testProdSpAr, 1); setLength(testReactSpAr, 1);
       testReactSpAr[0] := TSBMLSpeciesReference.create('S2', 1);
       testProdSpAr[0] := TSBMLSpeciesReference.create('S3', 1);
       testRxnAr[2] := SBMLReaction.create('J2', testProdSpAr, testReactSpAr);
       testRxnAr[2].setReversible(true);
       testRxnAr[2].setKineticLaw( SBMLKineticLaw.create('reaction2_kinlaw','(10 * S2 - 2 * S3) / (1 + S2 + S3)', testParamAr) );
       curModel.addSBMLReaction(testRxnAr[2]);

       setLength(testProdSpAr, 1); setLength(testReactSpAr, 1);
       testReactSpAr[0] := TSBMLSpeciesReference.create('S3', 1);
       testProdSpAr[0] := TSBMLSpeciesReference.create('S4', 1);
       testRxnAr[3] := SBMLReaction.create('J3', testProdSpAr, testReactSpAr);
       testRxnAr[3].setReversible(true);
       testRxnAr[3].setKineticLaw( SBMLKineticLaw.create('reaction3_kinlaw','(10 * S3 - 2 * S4) / (1 + S3 + S4)', testParamAr) );
       curModel.addSBMLReaction(testRxnAr[3]);

       setLength(testProdSpAr, 1); setLength(testReactSpAr, 1);
       testReactSpAr[0] := TSBMLSpeciesReference.create('S4', 1);
       testProdSpAr[0] := TSBMLSpeciesReference.create('X1', 1);
       testRxnAr[4] := SBMLReaction.create('J4', testProdSpAr, testReactSpAr);
       testRxnAr[4].setReversible(true);
       testRxnAr[4].setKineticLaw( SBMLKineticLaw.create('reaction3_kinlaw',' (V4 * S4) / (KS4 + S4)', testParamAr) );
       curModel.addSBMLReaction(testRxnAr[4]);
       Result := curModel;
    end

    else Result := nil;
  end;


end;

function TTestSBMLReadWrite.generateReadTestModel(testIndex: integer): string;
begin
  if testIndex < 2 then  //  change from 2 to SBML_EXAMPLE_MODELS when all examples added
 // if testIndex < SBML_EXAMPLE_MODELS then
    Result := getTestModel(testIndex) // --> from uTestModel
  else
    begin
    if testIndex < 2 + SBML_TEST_MODELS then
      Result := getSBMLReadTestModel (testIndex - 2) // --> from uTestSBML_ReadModels
    else Result := '';
    end;

end;

// ********************************************************************************************
// ********************************************************************************************
// TEST Write REFERENCE results for comparison:

procedure TTestSBMLReadWrite.generateWriteTestReferences();
begin
  self.writeRefResults := TList<string>.create;
  // Test 1 ***********************************************************
  self.writeRefResults.Add('<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
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

  // Test 2 *********************************************
  self.writeRefResults.Add( '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="2" layout:required="false" render:required="false">' + sLineBreak +
  '<model id="Feedback_with_InitAssignments">' + sLineBreak +
    '<listOfCompartments>' + sLineBreak +
      '<compartment id="Compartment01" constant="false"/>' + sLineBreak +
    '</listOfCompartments>' + sLineBreak +
    '<listOfSpecies>' + sLineBreak +
      '<species id="X0" initialConcentration="10" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' + sLineBreak +
      '<species id="S1" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S2" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S3" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="X1" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' + sLineBreak +
      '<species id="S4" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
    '</listOfSpecies>' + sLineBreak +
    '<listOfParameters>' + sLineBreak +
      '<parameter id="VM1" constant="false"/>' + sLineBreak +
      '<parameter id="Keq1" constant="false"/>' + sLineBreak +
      '<parameter id="h" constant="false"/>' + sLineBreak +
      '<parameter id="V4" constant="false"/>' + sLineBreak +
      '<parameter id="KS4" constant="false"/>' + sLineBreak +
    '</listOfParameters>' + sLineBreak +
    '<listOfInitialAssignments>' + sLineBreak +
      '<initialAssignment symbol="h">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<ci> Keq1 </ci>' + sLineBreak +
            '<cn type="integer"> 2 </cn>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</initialAssignment>' + sLineBreak +
      '<initialAssignment symbol="VM1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<cn type="integer"> 10 </cn>' + sLineBreak +
            '<ci> S2 </ci>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</initialAssignment>' + sLineBreak +
    '</listOfInitialAssignments>' + sLineBreak +
    '<listOfRules>' + sLineBreak +
      '<assignmentRule variable="Keq1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> S1 </ci>' + sLineBreak +
              '<cn type="integer"> 10 </cn>' + sLineBreak +
            '</apply>' + sLineBreak +
            '<cn type="integer"> 10 </cn>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</assignmentRule>' + sLineBreak +
    '</listOfRules>' + sLineBreak +
    '<listOfReactions>' + sLineBreak +
      '<reaction id="J0" reversible="true">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference id="X0" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference id="S1" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw id="reaction0_kinlaw">' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<times/>' + sLineBreak +
                '<ci> VM1 </ci>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<minus/>' + sLineBreak +
                  '<ci> X0 </ci>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<divide/>' + sLineBreak +
                    '<ci> S1 </ci>' + sLineBreak +
                    '<ci> Keq1 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> X0 </ci>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<power/>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                  '<ci> h </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J1" reversible="true">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference id="S1" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference id="S2" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw id="reaction1_kinlaw">' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S1 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J2" reversible="true">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference id="S2" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference id="S3" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw id="reaction2_kinlaw">' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J3" reversible="true">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference id="S3" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference id="S4" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw id="reaction3_kinlaw">' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J4" reversible="true">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference id="S4" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference id="X1" stoichiometry="1" constant="false"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw id="reaction3_kinlaw">' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<times/>' + sLineBreak +
                '<ci> V4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<ci> KS4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
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

end;

 function TTestSBMLReadWrite.getWriteTestReferenceString(testIndex: Integer): string;
 var refTestStrList: TList<string>;
 begin
   if self.writeRefResults.count > testIndex then
     Result := self.writeRefResults[testIndex]
   else Result := '';

 end;


// ********************************************************************************************
// ********************************************************************************************
// TEST Read REFERENCE results for comparieson:
 procedure TTestSBMLReadWrite.generateReadTestReferences();
 begin
   self.readRefResults := TList<string>.create;

   self.readRefResults.Add('Model id: __main, Species:  Species ID: S1, Boundary sp: false, Init Conc: 20, Comp: default_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, No Comp name, Comp size: 1, Comp constant, ' + sLineBreak +
 'Model params:  Param ID: k1, No Param name, Value: 0.1, Param Const Param ID: k2, No Param name, Value: 0.2, Param Const Param ID: k3, No Param name, Value: 0.1, Param Const Param ID: k4, No Param name, Value: 0.2, Param Const ' + sLineBreak +
 'Model Rxns: , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k1 * S1, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k2 * S2, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k3 * S3, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J4, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k4 * S4, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. ' + sLineBreak +
 'Model Initial Assignments: ' + sLineBreak +
 'Model Rules: ' + sLineBreak +
 'Model events: 0 ' + sLineBreak +
 'Model Func definitions: ' + sLineBreak +
 'Model Layout: NO layout' + sLineBreak +
 'Model Render Info: NO Render Info' );

    self.readRefResults.Add( 'Model id: , Species:  Species ID: X0, Boundary sp: true, Init Conc: 10, Comp: unit_compartment Species ID: S1, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: X1, Boundary sp: true, Init Conc: 0, Comp: unit_compartment' + sLineBreak +
 'Model compartments:  Comp ID: unit_compartment, No Comp name, Comp size: 1, Comp constant,' + sLineBreak +
 'Model params:  Param ID: VM1, No Param name, Value: 10, Param Const Param ID: Keq1, No Param name, Value: 10, Param Const Param ID: h, No Param name, Value: 10, Param Const Param ID: default_compartment, No Param name, Value: 1, Param can vary Param ID: V4, No Param name, Value: 2.5, Param Const Param ID: KS4, No Param name, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: VM1 * (X0 - S1 / Keq1) / (1 + X0 + S1 + pow(S4, h)), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S1 - 2 * S2) / (1 + S1 + S2), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S2 - 2 * S3) / (1 + S2 + S3), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S3 - 2 * S4) / (1 + S3 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: V4 * S4 / (KS4 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. ' + sLineBreak +
 'Model Initial Assignments:' + sLineBreak +
 'Model Rules: ' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions: ' + sLineBreak +
 'Model Layout:  Layout ID: , Layout dims:  Layout Dims ID:  Layout w, h: 863, 636' + sLineBreak +
 'Additional Graphical Glyphs:' + sLineBreak +
 'Additional General Glyphs:' + sLineBreak +
 'Compartment Glyphs:' + sLineBreak +
 'Species Glyphs:  Layout species glyph: , Layout Graph Object ID:speciesGlyphX0 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 204.808629732497, 192.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: X0' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:speciesGlyphS1 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 170.563342861622, 336.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S1' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:speciesGlyphS4 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 659.865504755851, 253.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S4' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:speciesGlyphS2 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 334.903954537822, 388.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S2' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:speciesGlyphS3 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 515.817311698465, 331.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S3' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:speciesGlyphX1 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 703.041256413743, 405.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: X1' + sLineBreak +
 'Reaction and Sp Ref Glyphs: , Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ0 No bounding box, Reaction Glyph Rxn Id: J0, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphX0J0 No bounding box, Sp Glyph ID: speciesGlyphX0, Sp Ref ID: X0J0, Role: substrate Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 228.331, 239.1, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 226.247308014778, 248.5, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS1J0 No bounding box, Sp Glyph ID: speciesGlyphS1, Sp Ref ID: S1J0, Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 207.05, 327.837, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 209.124664579341, 320.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 212.417480624617, 306.653846153846, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ1 No bounding box, Reaction Glyph Rxn Id: J1, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 207.05, 327.837, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 209.124664579341, 320.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 212.417480624617, 306.653846153846, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS1J1 No bounding box, Sp Glyph ID: speciesGlyphS1, Sp Ref ID: S1J1, Role: substrate Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 237.914, 367.704, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 241.648495780672, 369.5, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS2J1 No bounding box, Sp Glyph ID: speciesGlyphS2, Sp Ref ID: S2J1, Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 325.989, 395.205, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 323.818801618772, 395.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 308.016819726829, 390.5, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ2 No bounding box, Reaction Glyph Rxn Id: J2, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 325.989, 395.205, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 323.818801618772, 395.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 308.016819726829, 390.5, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS2J2 No bounding box, Sp Glyph ID: speciesGlyphS2, Sp Ref ID: S2J2, Role: substrate Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 401.096, 396.902, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 410.132293827982, 394.25, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS3J2 No bounding box, Sp Glyph ID: speciesGlyphS3, Sp Ref ID: S3J2, Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 506.887, 363.511, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 500.588972408304, 365.75, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 483.193457296704, 371.230769230769, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ3 No bounding box, Reaction Glyph Rxn Id: J3, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 506.887, 363.511, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 500.588972408304, 365.75, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 483.193457296704, 371.230769230769, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS3J3 No bounding box, Sp Glyph ID: speciesGlyphS3, Sp Ref ID: S3J3, Role: substrate Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 582.79, 330.677, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 581.829359962812, 332, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS4J3 No bounding box, Sp Glyph ID: speciesGlyphS4, Sp Ref ID: S4J3, Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 650.479, 294.216, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 653.853456491505, 293, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 640.002668697525, 300.5, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ4 No bounding box, Reaction Glyph Rxn Id: J4, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 650.479, 294.216, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 653.853456491505, 293, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 640.002668697525, 300.5, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS4J4 No bounding box, Sp Glyph ID: speciesGlyphS4, Sp Ref ID: S4J4, Role: substrate Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 696.362, 300.437, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 700.659442670324, 311.5, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphX1J4 No bounding box, Sp Glyph ID: speciesGlyphX1, Sp Ref ID: X1J4, Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 724.979, 396.803, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 722.24731849927, 387.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 718.09580391678, 372.884615384615, Layout Pt Z: 0' + sLineBreak +
 sLineBreak +
 'Text Glyphs: , Layout TextGlyph: , Layout Graph Object ID:txtGlyphX0 No bounding box, TextGlyph text: X0, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphX0, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS1 No bounding box, TextGlyph text: S1, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS1, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS4 No bounding box, TextGlyph text: S4, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS4, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS2 No bounding box, TextGlyph text: S2, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS2, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS3 No bounding box, TextGlyph text: S3, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS3, Layout TextGlyph: , Layout Graph Object ID:txtGlyphX1 No bounding box, TextGlyph text: X1, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphX1' + sLineBreak +
 sLineBreak + 'Model Render Info:' + sLineBreak +
 'Render Information - id: renderInfotestNetwork, Local Render Info: true' + sLineBreak +
 'Color Definitions:  Color id: fillColorSpecies_speciesGlyphX0, value: A6CAF0FF,  Color id: outlineColorSpecies_speciesGlyphX0, value: FF6600FF,  Color id: fillColorSpecies_speciesGlyphS1, value: FFCC99FF,  Color id: outlineColorSpecies_speciesGlyphS1, value: FF6600FF,  Color id: fillColorSpecies_speciesGlyphS4, value: FFCC99FF,  Color id: outlineColorSpecies_speciesGlyphS4, value: FF6600FF,  Color id: fillColorSpecies_speciesGlyphS2, value: FFCC99FF,  Color id: outlineColorSpecies_speciesGlyphS2, value: FF6600FF,  Color id: fillColorSpecies_speciesGlyphS3, value: FFCC99FF,  Color id: outlineColorSpecies_speciesGlyphS3, value: FF6600FF,  Color id: fillColorSpecies_speciesGlyphX1, value: A6CAF0FF,  Color id: outlineColorSpecies_speciesGlyphX1, value: FF6600FF,  Color id: fillColorReaction_specRefGlyphX0J0, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS1J0, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS1J1, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS2J1, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS2J2, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS3J2, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS3J3, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS4J3, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphS4J4, value: B0C4DEFF,  Color id: fillColorReaction_specRefGlyphX1J4, value: B0C4DEFF,' + sLineBreak +
 'Style list: --  Render style id:speciesStyle_X0, RG:  Render group: Stoke width: 3, Stroke color: outlineColorSpecies_speciesGlyphX0, fill color: fillColorSpecies_speciesGlyphX0, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: speciesGlyphX0' + sLineBreak +
'--  Render style id:textStyle_X0, RG:  Render group: Stoke width: -1, Stroke color: , fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:speciesStyle_S1, RG:  Render group: Stoke width: 3, Stroke color: outlineColorSpecies_speciesGlyphS1, fill color: fillColorSpecies_speciesGlyphS1, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: speciesGlyphS1' + sLineBreak +
'--  Render style id:textStyle_S1, RG:  Render group: Stoke width: -1, Stroke color: , fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:speciesStyle_S4, RG:  Render group: Stoke width: 3, Stroke color: outlineColorSpecies_speciesGlyphS4, fill color: fillColorSpecies_speciesGlyphS4, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: speciesGlyphS4' + sLineBreak +
'--  Render style id:textStyle_S4, RG:  Render group: Stoke width: -1, Stroke color: , fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:speciesStyle_S2, RG:  Render group: Stoke width: 3, Stroke color: outlineColorSpecies_speciesGlyphS2, fill color: fillColorSpecies_speciesGlyphS2, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: speciesGlyphS2' + sLineBreak +
'--  Render style id:textStyle_S2, RG:  Render group: Stoke width: -1, Stroke color: , fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:speciesStyle_S3, RG:  Render group: Stoke width: 3, Stroke color: outlineColorSpecies_speciesGlyphS3, fill color: fillColorSpecies_speciesGlyphS3, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: speciesGlyphS3' + sLineBreak +
'--  Render style id:textStyle_S3, RG:  Render group: Stoke width: -1, Stroke color: , fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:speciesStyle_X1, RG:  Render group: Stoke width: 3, Stroke color: outlineColorSpecies_speciesGlyphX1, fill color: fillColorSpecies_speciesGlyphX1, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: speciesGlyphX1' + sLineBreak +
'--  Render style id:textStyle_X1, RG:  Render group: Stoke width: -1, Stroke color: , fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:reaction_reactant_Style_J0, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphX0J0, fill color: fillColorReaction_specRefGlyphX0J0, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: specRefGlyphX0J0' + sLineBreak +
'--  Render style id:reaction_product_Style_J0, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS1J0, fill color: fillColorReaction_specRefGlyphS1J0, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: arrowHead_productJ0, types: , roles: product, , Graphical Obj Ids: specRefGlyphS1J0' + sLineBreak +
'--  Render style id:reaction_reactant_Style_J1, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS1J1, fill color: fillColorReaction_specRefGlyphS1J1, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: specRefGlyphS1J1' + sLineBreak +
'--  Render style id:reaction_product_Style_J1, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS2J1, fill color: fillColorReaction_specRefGlyphS2J1, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: arrowHead_productJ1, types: , roles: product, , Graphical Obj Ids: specRefGlyphS2J1' + sLineBreak +
'--  Render style id:reaction_reactant_Style_J2, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS2J2, fill color: fillColorReaction_specRefGlyphS2J2, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: specRefGlyphS2J2' + sLineBreak +
'--  Render style id:reaction_product_Style_J2, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS3J2, fill color: fillColorReaction_specRefGlyphS3J2, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: arrowHead_productJ2, types: , roles: product, , Graphical Obj Ids: specRefGlyphS3J2' + sLineBreak +
'--  Render style id:reaction_reactant_Style_J3, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS3J3, fill color: fillColorReaction_specRefGlyphS3J3, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: specRefGlyphS3J3' + sLineBreak +
'--  Render style id:reaction_product_Style_J3, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS4J3, fill color: fillColorReaction_specRefGlyphS4J3, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: arrowHead_productJ3, types: , roles: product, , Graphical Obj Ids: specRefGlyphS4J3' + sLineBreak +
'--  Render style id:reaction_reactant_Style_J4, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS4J4, fill color: fillColorReaction_specRefGlyphS4J4, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids: specRefGlyphS4J4' + sLineBreak +
'--  Render style id:reaction_product_Style_J4, RG:  Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphX1J4, fill color: fillColorReaction_specRefGlyphX1J4, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: arrowHead_productJ4, types: , roles: product, , Graphical Obj Ids: specRefGlyphX1J4' + sLineBreak +
 'Line endings: --  Render line ending, id: arrowHead_productJ0, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: -4.5, -7, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 9, 14 Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS1J0, fill color: fillColorReaction_specRefGlyphS1J0, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 14, Abs Coord, Render pt ID: , x,y: 3, 7, Abs Coord, Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 9, 7, Abs Coord,' + sLineBreak +
'--  Render line ending, id: arrowHead_productJ1, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: -4.5, -7, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 9, 14 Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS2J1, fill color: fillColorReaction_specRefGlyphS2J1, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 14, Abs Coord, Render pt ID: , x,y: 3, 7, Abs Coord, Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 9, 7, Abs Coord,' + sLineBreak +
'--  Render line ending, id: arrowHead_productJ2, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: -4.5, -7, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 9, 14 Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS3J2, fill color: fillColorReaction_specRefGlyphS3J2, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 14, Abs Coord, Render pt ID: , x,y: 3, 7, Abs Coord, Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 9, 7, Abs Coord,' + sLineBreak +
'--  Render line ending, id: arrowHead_productJ3, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: -4.5, -7, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 9, 14 Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphS4J3, fill color: fillColorReaction_specRefGlyphS4J3, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 14, Abs Coord, Render pt ID: , x,y: 3, 7, Abs Coord, Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 9, 7, Abs Coord,' + sLineBreak +
'--  Render line ending, id: arrowHead_productJ4, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: -4.5, -7, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 9, 14 Render group: Stoke width: 3, Stroke color: fillColorReaction_specRefGlyphX1J4, fill color: fillColorReaction_specRefGlyphX1J4, font size: -1, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 14, Abs Coord, Render pt ID: , x,y: 3, 7, Abs Coord, Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 9, 7, Abs Coord,'
);
     // getSBMLReadTestModel (0)
     self.readRefResults.Add('Model id: feedback, Species:  Species ID: X0, Boundary sp: true, Init Conc: 10, Comp: default_compartment Species ID: S1, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: X1, Boundary sp: true, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, No Comp name, Comp size: 1, Comp constant, ' + sLineBreak +
 'Model params:  Param ID: VM1, No Param name, Value: 0, Param can vary Param ID: Keq1, No Param name, Value: 0, Param can vary Param ID: h, No Param name, Value: 0, Param can vary Param ID: V4, No Param name, Value: 2.5, Param Const Param ID: KS4, No Param name, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: VM1 * (X0 - S1 / Keq1) / (1 + X0 + S1 + pow(S4, h)), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S1 - 2 * S2) / (1 + S1 + S2), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S2 - 2 * S3) / (1 + S2 + S3), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S3 - 2 * S4) / (1 + S3 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: V4 * S4 / (KS4 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list.' + sLineBreak +
 'Model Initial Assignments:  InitAssign Id: InitAssign_h, Symbol: h, Formula: Keq1 + 2' + sLineBreak +
 'Model Rules:  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: VM1, Rule formula: 10 + S2, Rule Does not use piecewise.  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: Keq1, Rule formula: S1 * 10 + 10, Rule Does not use piecewise. ' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions: ' + sLineBreak +
 'Model Layout: NO layout' + sLineBreak +
 'Model Render Info: NO Render Info' );
  // getSBMLReadTestModel (1)
    self.readRefResults.Add('Model id: feedback, Species:  Species ID: X0, Boundary sp: true, Init Conc: 10, Comp: default_compartment Species ID: S1, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: X1, Boundary sp: true, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, No Comp name, Comp size: 1, Comp constant,' + sLineBreak +
 'Model params:  Param ID: VM1, No Param name, Value: 10, Param Const Param ID: Keq1, No Param name, Value: 10, Param Const Param ID: h, No Param name, Value: 10, Param Const Param ID: V4, No Param name, Value: 2.5, Param Const Param ID: KS4, No Param name, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: VM1 * (X0 - S1 / Keq1) / (1 + X0 + S1 + pow(S4, h)), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S1 - 2 * S2) / (1 + S1 + S2), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S2 - 2 * S3) / (1 + S2 + S3), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S3 - 2 * S4) / (1 + S3 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: V4 * S4 / (KS4 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list.' + sLineBreak +
 'Model Initial Assignments:  InitAssign Id: InitAssign_S1, Symbol: S1, Formula: Keq1 + h' + sLineBreak +
 'Model Rules:' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions:' + sLineBreak +
 'Model Layout: NO layout' + sLineBreak +
 'Model Render Info: NO Render Info' );

  // getSBMLReadTestModel(2)
    self.readRefResults.Add('Model id: feedback, Species:  Species ID: X0, Name: X0, Boundary sp: true, Init Conc: 10, Comp: default_compartment Species ID: S1, Name: S1, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Name: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S2, Name: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Name: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: X1, Name: X1, Boundary sp: true, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, Comp name: default_compartment, Comp size: 1, Comp constant,' + sLineBreak +
 'Model params:  Param ID: VM1, Param name: VM1, Value: 10, Param Const Param ID: Keq1, Param name: Keq1, Value: 5, Param can vary Param ID: h, Param name: h, Value: 10, Param Const Param ID: V4, Param name: V4, Value: 2.5, Param Const Param ID: KS4, Param name: KS4, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J0(Keq1, S1, S4, VM1, X0, default_compartment, h), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J1(S1, S2, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J2(S2, S3, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J3(S3, S4, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J4(KS4, S4, V4, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list.' + sLineBreak +
 'Model Initial Assignments:' + sLineBreak +
 'Model Rules:  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: Keq1, Rule formula: S2 * 5 + 5, Rule Does not use piecewise. ' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions: , Func Def ID: Function_for_J0, Name: Function for J0, Func Def vars and formula: Function_for_J0(Keq1, S1, S4, VM1, X0, default_compartment, h), Func Def ID: Function_for_J1, Name: Function for J1, Func Def vars and formula: Function_for_J1(S1, S2, default_compartment), Func Def ID: Function_for_J2, Name: Function for J2, Func Def vars and formula: Function_for_J2(S2, S3, default_compartment), Func Def ID: Function_for_J3, Name: Function for J3, Func Def vars and formula: Function_for_J3(S3, S4, default_compartment), Func Def ID: Function_for_J4, Name: Function for J4, Func Def vars and formula: Function_for_J4(KS4, S4, V4, default_compartment)' + sLineBreak +
 'Model Layout: NO layout' + sLineBreak +
 'Model Render Info: NO Render Info' );

 // getSBMLReadTestModel(3)
   self.readRefResults.Add('Model id: feedback_GlobalRender, Species:  Species ID: X0, Name: X0, Boundary sp: true, Init Conc: 10, Comp: default_compartment Species ID: S1, Name: S1, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Name: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S2, Name: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Name: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: X1, Name: X1, Boundary sp: true, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, Comp name: default_compartment, Comp size: 1, Comp constant,' + sLineBreak +
 'Model params:  Param ID: VM1, Param name: VM1, Value: 10, Param Const Param ID: Keq1, Param name: Keq1, Value: 5, Param can vary Param ID: h, Param name: h, Value: 10, Param Const Param ID: V4, Param name: V4, Value: 2.5, Param Const Param ID: KS4, Param name: KS4, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J0(Keq1, S1, S4, VM1, X0, default_compartment, h), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J1(S1, S2, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J2(S2, S3, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J3(S3, S4, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: default_compartment * Function_for_J4(KS4, S4, V4, default_compartment), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list.' + sLineBreak +
 'Model Initial Assignments:' + sLineBreak +
 'Model Rules:  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: Keq1, Rule formula: S2 * 5 + 5, Rule Does not use piecewise.' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions: , Func Def ID: Function_for_J0, Name: Function for J0, Func Def vars and formula: Function_for_J0(Keq1, S1, S4, VM1, X0, default_compartment, h), Func Def ID: Function_for_J1, Name: Function for J1, Func Def vars and formula: Function_for_J1(S1, S2, default_compartment), Func Def ID: Function_for_J2, Name: Function for J2, Func Def vars and formula: Function_for_J2(S2, S3, default_compartment), Func Def ID: Function_for_J3, Name: Function for J3, Func Def vars and formula: Function_for_J3(S3, S4, default_compartment), Func Def ID: Function_for_J4, Name: Function for J4, Func Def vars and formula: Function_for_J4(KS4, S4, V4, default_compartment)' + sLineBreak +
 'Model Layout:  Layout ID: , Layout dims:  Layout Dims ID:  Layout w, h: 96.1373438530456, 83.7041550748293' + sLineBreak +
 'Additional Graphical Glyphs:' + sLineBreak +
 'Additional General Glyphs:' + sLineBreak +
 'Compartment Glyphs:' + sLineBreak +
 'Species Glyphs:  Layout species glyph: , Layout Graph Object ID:layout_glyph_0 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 283.272798346204, 59.7323682144198, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 36, 28, species Glyph: species id: S1' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:layout_glyph_1 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 158.718043857074, 0, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 36, 28, species Glyph: species id: X0' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:layout_glyph_2 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 272.05458204873, 270.37091562488, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 36, 28, species Glyph: species id: S3' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:layout_glyph_3 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 0, 154.162571271122, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 36, 28, species Glyph: species id: X1' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:layout_glyph_4 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 155.125452446903, 171.241585003035, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 36, 28, species Glyph: species id: S4' + sLineBreak +
 'Layout species glyph: , Layout Graph Object ID:layout_glyph_5 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 381.175818391212, 169.519917085474, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 36, 28, species Glyph: species id: S2' + sLineBreak +
 'Reaction and Sp Ref Glyphs: , Layout Reaction Glyph: , Layout Graph Object ID:layout_glyph_6 No bounding box, Reaction Glyph Rxn Id: J3, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 219.457504199267, 262.122456144769, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 207.764591239084, 252.209523082585, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_7 No bounding box, Sp Glyph ID: layout_glyph_2, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 219.457504199267, 262.122456144769, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 267.05458204873, 279.554327389641, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 231.150417159449, 272.035389206954, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 252.025727844135, 278.273091563844, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_8 No bounding box, Sp Glyph ID: layout_glyph_4, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 207.764591239084, 252.209523082585, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 180.766820468434, 204.241585003035, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 196.071678278901, 242.2965900204, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 185.496021133622, 220.790854246172, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:layout_glyph_9 No bounding box, Reaction Glyph Rxn Id: J0, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 212.36614508771, 81.1716212614926, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 224.821620536623, 87.1448580829346, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 207.764591239084, 252.209523082585, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 180.766820468434, 204.241585003035, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 196.071678278901, 242.2965900204, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 185.496021133622, 220.790854246172, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_10 No bounding box, Sp Glyph ID: layout_glyph_1, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 212.36614508771, 81.1716212614926, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 183.918558934141, 33, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 199.910669638797, 75.1983844400506, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 188.800745424241, 52.6058830146648, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_11 No bounding box, Sp Glyph ID: layout_glyph_0, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 224.821620536623, 87.1448580829346, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 278.272798346204, 80.6995815962833, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 237.277095985536, 93.1180949043766, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 260.888816028098, 88.4021474556904, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_12 No bounding box, Sp Glyph ID: layout_glyph_4, Sp Ref ID: , Role: modifier Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 181.616497088903, 166.241585003035, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 214.269744487579, 93.1749922888621, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 197.943120788241, 129.708288645949, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 209.945606162992, 102.191744905511, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:layout_glyph_13 No bounding box, Reaction Glyph Rxn Id: J4, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 99.5025281600643, 195.839768592242, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 83.989982915374, 194.131867219051, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 181.616497088903, 166.241585003035, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 214.269744487579, 93.1749922888621, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 197.943120788241, 129.708288645949, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 209.945606162992, 102.191744905511, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_14 No bounding box, Sp Glyph ID: layout_glyph_4, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 99.5025281600643, 195.839768592242, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 150.125452446903, 190.112314790047, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 115.015073404755, 197.547669965433, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 136.448399237001, 194.256967721038, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_15 No bounding box, Sp Glyph ID: layout_glyph_3, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 83.989982915374, 194.131867219051, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 41, 179.217254332878, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 68.4774376706837, 192.423965845859, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 50.8605825241693, 185.393634746071, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:layout_glyph_16 No bounding box, Reaction Glyph Rxn Id: J1, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 371.970776811952, 99.1471782282335, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 381.761078816453, 110.125933115339, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 83.989982915374, 194.131867219051, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 41, 179.217254332878, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 68.4774376706837, 192.423965845859, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 50.8605825241693, 185.393634746071, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_17 No bounding box, Sp Glyph ID: layout_glyph_0, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 371.970776811952, 99.1471782282335, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 324.272798346204, 79.1837215939162, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 362.180474807451, 88.1684233411281, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 340.779061075703, 80.9313837457458, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_18 No bounding box, Sp Glyph ID: layout_glyph_5, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 381.761078816453, 110.125933115339, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 396.854841348218, 164.519917085474, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 391.551380820954, 121.104688002444, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 396.650686585711, 145.556991265735, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:layout_glyph_19 No bounding box, Reaction Glyph Rxn Id: J2, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 373.712745814801, 254.863478138621, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 362.800622180553, 264.948577992561, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 381.761078816453, 110.125933115339, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 396.854841348218, 164.519917085474, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 391.551380820954, 121.104688002444, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 396.650686585711, 145.556991265735, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_20 No bounding box, Sp Glyph ID: layout_glyph_5, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 373.712745814801, 254.863478138621, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 394.662678092078, 202.519917085474, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 384.624869449049, 244.77837828468, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 392.371804679126, 221.127872721592, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:layout_glyph_21 No bounding box, Sp Glyph ID: layout_glyph_2, Sp Ref ID: , Role: product Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 362.800622180553, 264.948577992561, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 313.05458204873, 280.897797940169, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 351.888498546305, 275.033677846502, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 329.743509388955, 280.487012856821, Layout Pt Z: 0' + sLineBreak +
  sLineBreak +
 'Text Glyphs: , Layout TextGlyph: , Layout Graph Object ID:layout_glyph_22 No bounding box, TextGlyph text: , TextGlyph text origin: S1, TextGlyph GraphObj ID: layout_glyph_0, Layout TextGlyph: , Layout Graph Object ID:layout_glyph_23 No bounding box, TextGlyph text: , TextGlyph text origin: X0, TextGlyph GraphObj ID: layout_glyph_1, Layout TextGlyph: , Layout Graph Object ID:layout_glyph_24 No bounding box, TextGlyph text: , TextGlyph text origin: S3, TextGlyph GraphObj ID: layout_glyph_2, Layout TextGlyph: , Layout Graph Object ID:layout_glyph_25 No bounding box, TextGlyph text: , TextGlyph text origin: X1, TextGlyph GraphObj ID: layout_glyph_3, Layout TextGlyph: , Layout Graph Object ID:layout_glyph_26 No bounding box, TextGlyph text: , TextGlyph text origin: S4, TextGlyph GraphObj ID: layout_glyph_4, Layout TextGlyph: , Layout Graph Object ID:layout_glyph_27 No bounding box, TextGlyph text: , TextGlyph text origin: S2, TextGlyph GraphObj ID: layout_glyph_5' + sLineBreak +
  sLineBreak +
 'Model Render Info:' + sLineBreak +
 'Render Information - id: GlobalRenderInformation_0, Local Render Info: false' + sLineBreak +
 'Color Definitions:  Color id: black, value: 000000FF,  Color id: white, value: FFFFFFFF,  Color id: transparent, value: FFFFFF00,  Color id: EmptySetOutline, value: 808080FF,  Color id: EmptySetGradientStart, value: FFFFFFFF,  Color id: EmptySetGradientEnd, value: D3D3D3FF,  Color id: CompartmentBorder, value: E69600B0,  Color id: CloneMarkerColor, value: FFA500FF,  Color id: CurveColor, value: 000000A0,  Color id: ModulationCurveColor, value: 0000A0A0,' + sLineBreak +
 'Style list: --  Render style id:, RG:  Render group: Stoke width: 0, Stroke color: #ffffff00, fill color: #ffffff00, font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 1, Stroke color: black, fill color: , font size: 12, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: TEXTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 3, Stroke color: CurveColor, fill color: , font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: REACTIONGLYPH, , roles: substrate, sidesubstrate, , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 3, Stroke color: CurveColor, fill color: , font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: InhibitionHead, types: , roles: inhibitor, , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 3, Stroke color: ModulationCurveColor, fill color: white, font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: ModulationHead, types: , roles: modifier, , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 3, Stroke color: CurveColor, fill color: white, font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: ActivationHead, types: , roles: activator, , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 3, Stroke color: CurveColor, fill color: , font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: TransitionHead, types: , roles: product, sideproduct, , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 0, Stroke color: , fill color: #a0e0a030, font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: SPECIESGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 7, Stroke color: CompartmentBorder, fill color: , font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: COMPARTMENTGLYPH, , roles: , Graphical Obj Ids:' + sLineBreak +
'--  Render style id:, RG:  Render group: Stoke width: 0, Stroke color: black, fill color: #f0707070, font size: 0, font style: normal, vTextAnchor: V_MIDDLE, hTextAnchor: H_MIDDLE, start head: , end head: , types: ANY, , roles: , Graphical Obj Ids:' + sLineBreak +
 'Line endings: --  Render line ending, id: ActivationHead, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: bb, BB pt:  Layout Pt ID:  Layout Pt XY: -12, -6, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 12, 12 Render group: Stoke width: 1, Stroke color: CurveColor, fill color: white, font size: -1, font style: normal, vTextAnchor: V_TOP, hTextAnchor: H_START, start head: , end head:' + sLineBreak +
'--  Render line ending, id: TransitionHead, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: bb, BB pt:  Layout Pt ID:  Layout Pt XY: -8, -6, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 12, 12 Render group: Stoke width: 0.001, Stroke color: CurveColor, fill color: CurveColor, font size: -1, font style: normal, vTextAnchor: V_TOP, hTextAnchor: H_START, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 100, 50, Rel Coord, Render pt ID: , x,y: 0, 100, Rel Coord, Render pt ID: , x,y: 33, 50, Rel Coord, Render pt ID: , x,y: 0, 0, Rel Coord,' + sLineBreak +
'--  Render line ending, id: ModulationHead, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: bb, BB pt:  Layout Pt ID:  Layout Pt XY: -5, -5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 10, 10 Render group: Stoke width: 1, Stroke color: ModulationCurveColor, fill color: ModulationCurveColor, font size: -1, font style: normal, vTextAnchor: V_TOP, hTextAnchor: H_START, start head: , end head:' + sLineBreak +
'--  Render line ending, id: InhibitionHead, stroke color: , stroke width: -1, Rotational mapping: true BoundingBox ID: bb, BB pt:  Layout Pt ID:  Layout Pt XY: -0.5, -4, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 0.6, 8 Render group: Stoke width: 2, Stroke color: black, fill color: black, font size: -1, font style: normal, vTextAnchor: V_TOP, hTextAnchor: H_START, start head: , end head:  Render Polygon:  Primative 1D id: , stroke: , stroke width: -1, fill:  Render pt ID: , x,y: 0, 0, Rel Coord, Render pt ID: , x,y: 0.3, 0, Abs Coord, Render pt ID: , x,y: 0.3, 8, Abs Coord, Render pt ID: , x,y: 0, 8, Abs Coord,'
 );
   // getSBMLReadTestModel(4)
   self.readRefResults.Add('Model id: feedback, Species:  Species ID: X0, Boundary sp: true, Init Conc: 10, Comp: default_compartment Species ID: S1, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: X1, Boundary sp: true, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, No Comp name, Comp size: 1, Comp constant,' + sLineBreak +
 'Model params:  Param ID: VM1, No Param name, Value: 0, Param can vary Param ID: Keq1, No Param name, Value: 0, Param can vary Param ID: h, No Param name, Value: 0, Param can vary Param ID: V4, No Param name, Value: 2.5, Param Const Param ID: KS4, No Param name, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: VM1 * (X0 - S1 / Keq1) / (1 + X0 + S1 + pow(S4, h)), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S1 - 2 * S2) / (1 + S1 + S2), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S2 - 2 * S3) / (1 + S2 + S3), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S3 - 2 * S4) / (1 + S3 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: V4 * S4 / (KS4 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list.' + sLineBreak +
 'Model Initial Assignments:  InitAssign Id: InitAssign_h, Symbol: h, Formula: Keq1 + 2 InitAssign Id: InitAssign_VM1, Symbol: VM1, Formula: 10 + S2' + sLineBreak +
 'Model Rules:  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: Keq1, Rule formula: S1 * 10 + 10, Rule Does not use piecewise.' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions:' + sLineBreak +
 'Model Layout: NO layout' + sLineBreak +
 'Model Render Info: NO Render Info' );

 end;


  function TTestSBMLReadWrite.getReadTestReferenceString(testIndex: integer): string;
  begin
    if self.readRefResults.count > testIndex then
      Result := self.readRefResults[testIndex]
    else Result := '';
  end;

end.
