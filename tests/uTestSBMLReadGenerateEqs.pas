unit uTestSBMLReadGenerateEqs;
// Read SBML model, generate System of eqs to be used by the simulator. Compare to reference.
{
  Add/modify a test case:
    1. Convert SBML model to string and add it to ../sbml/uTestSBML_ReadModels.pas. See earlier
       test for an example format.
    2. Add one to SBML_TEST_MODELS = # at top of uTestSBML_ReadModels.pas (if new test)
    3. Run test group 'Read SBML model and generate Eqs list'
    4. Open browser console window and grab string after 'EqList:' Use previous test for reference.
    5. Format as string for insertion into TTestSBMLReadGenerateEqs.generateRefEqsList()
    6. Since the SBML read tests use the same model, update uTestSBMLReadWrite.pas with the expected output
 }

interface
uses   System.SysUtils, System.Classes, JS, Web, System.Generics.Collections,
 uTestCase, utests.TestUtils, uSBMLClasses, uSBMLWriter, uSBMLReader, uModel, uTestModel,
 uTestSBML_ReadModels, uSimulation;

type
 TReadGenerateEqsFinished = procedure(readGenerateEqsTestCases: TList<TTestCaseResult>) of object;  // Notify when done testing

 TTestSBMLReadGenerateEqs = class
  private
    FNotify: TReadGenerateEqsFinished; // send results to listener once asynchronous read done.
    testModel: TModel;
    equationsRefList: TList<string>;
    currentTestIndex: integer;
    numTests: integer;
    procedure generateRefEqsList();

  public
    resultInfo: TList<string>;
    testResultList: TList<TTestCaseResult>;
    constructor create;
    procedure runTest;
    procedure testsFinished(); // Notify listener
    procedure modelRead();  // callback: notified when TModel created from SBML model.
    function  getTestEqsRef( testIndex: integer): string;
    property  OnNotify: TReadGenerateEqsFinished read FNotify write FNotify;
 end;

implementation
 constructor TTestSBMLReadGenerateEqs.create;
 begin
  self.numTests := 0;
  self.testResultList := TList<TTestCaseResult>.create;
  self.currentTestIndex := 0;
  self.generateRefEqsList();
  //self.runTest;
 end;

 procedure TTestSBMLReadGenerateEqs.runTest();
 var i: integer;
    sbmlTestReader: TSBMLRead;
    testSBMLStr: string;
 begin
  self.testResultList.Add(TTestCaseResult.create(self.currentTestIndex +1,
                   'SBML Read and generate Eqs test ' + inttostr(self.currentTestIndex +1)));
  testSBMLStr := getSBMLReadTestModel(self.currentTestIndex);
  if testSBMLStr <> '' then
    begin
    self.testModel := TModel.create;
    self.testModel.OnPing := self.modelRead;
    sbmlTestReader := TSBMLRead.create(self.testModel, testSBMLStr );
    end;
 end;

 procedure TTestSBMLReadGenerateEqs.modelRead(); // callback, register with uSBMLReader
 var i: integer;
     time, step: double;
     sTestEqList, sRefEqList: string;
     refResultsList: TList<string>;
     curSim: TSimulationJS;
 begin
   refResultsList := TList<string>.create;
   time := 10;
   step := 100;
   sTestEqList := '';
   sRefEqList := '';
   sRefEqList := self.getTestEqsRef(self.currentTestIndex);
   curSim := TSimulationJS.create(time, step, self.testModel, LSODAS);
   sTestEqList := curSim.getParamInitAssignEqs;
   sTestEqList := sTestEqList + curSim.getSpeciesInitAssignEqs;
   sTestEqList := sTestEqList + curSim.getLSODAeqs;
   console.log(' EqList: ', sTestEqList); // Use this to update reference when needed.
   if sRefEqList <> '' then
      begin
      refResultsList := compareStrResults(sTestEqList, sRefEqList);
      if refResultsList.Count <1 then self.testResultList[self.currentTestIndex].testPass
      else
        begin
        self.testResultList[self.currentTestIndex].testFail;
        for i := 0 to refResultsList.count -1 do
          self.testResultList[self.currentTestIndex].sTestInfoList.Add(refResultsList[i]);
        end;

      end;


   inc(self.currentTestIndex);
   self.testModel.Free;
   if self.currentTestIndex < SBML_TEST_MODELS then
     begin
     runTest( )
     end
   else self.testsFinished;
 end;

 procedure TTestSBMLReadGenerateEqs.testsFinished();
 begin
   if Assigned(FNotify) then
     FNotify(self.testResultList);
 end;

 procedure TTestSBMLReadGenerateEqs.generateRefEqsList();
 begin
   self.equationsRefList := TList<string>.create;
   // Expected results:
   self.equationsRefList.Add('p[0] = 10 + s[2]' + sLineBreak +
'p[1] = s[0] * 10 + 10' + sLineBreak +
'p[2] = p[1] + 2' + sLineBreak +
'return p;' + sLineBreak +
'let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]); p[0] = 10 + s[2];p[1] = s[0] * 10 + 10;dydt_s.setVal(1, (1)*(1)* (p[0] * (p[6] - s[0] / p[1]) / (1 + p[6] + s[0] + Math.pow(s[1], p[2])))+  (-1)*(1)* ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2])));dydt_s.setVal(3, (1)*(1)* ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2]))+  (-1)*(1)* ((10 * s[2] - 2 * s[3]) / (1 + s[2] + s[3])));dydt_s.setVal(4, (1)*(1)* ((10 * s[2] - 2 * s[3]) / (1 + s[2] + s[3]))+  (-1)*(1)* ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1])));dydt_s.setVal(2, (1)*(1)* ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1]))+  (-1)*(1)* (p[3] * s[1] / (p[4] + s[1]))); return dydt_s;' ); // Expected result

   self.equationsRefList.Add('s[0] = p[1] + p[2]' + sLineBreak +
'return s;' + sLineBreak +
'let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]); dydt_s.setVal(1, (1)*(1)* (p[0] * (p[6] - s[0] / p[1]) / (1 + p[6] + s[0] + Math.pow(s[1], p[2])))+  (-1)*(1)* ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2])));dydt_s.setVal(3, (1)*(1)* ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2]))+  (-1)*(1)* ((10 * s[2] - 2 * s[3]) / (1 + s[2] + s[3])));dydt_s.setVal(4, (1)*(1)* ((10 * s[2] - 2 * s[3]) / (1 + s[2] + s[3]))+  (-1)*(1)* ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1])));dydt_s.setVal(2, (1)*(1)* ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1]))+  (-1)*(1)* (p[3] * s[1] / (p[4] + s[1]))); return dydt_s;' );

   //Test #3:
   self.equationsRefList.Add('p[1] = s[2] * 5 + 5' + sLineBreak +
'return p;' + sLineBreak +
'let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]); p[1] = s[2] * 5 + 5;dydt_s.setVal(1, (1)*(1)* (p[5] * (p[0] * (p[6] - s[0] / p[1]) / (1 + p[6] + s[0] + Math.pow(s[1], p[2])) / p[5]))+  (-1)*(1)* (p[5] * ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2]) / p[5])));dydt_s.setVal(3, (1)*(1)* (p[5] * ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2]) / p[5]))+  (-1)*(1)* (p[5] * ((5 * s[2] - 2 * s[3]) / (1 + s[2] + s[3]) / p[5])));dydt_s.setVal(4, (1)*(1)* (p[5] * ((5 * s[2] - 2 * s[3]) / (1 + s[2] + s[3]) / p[5]))+  (-1)*(1)* (p[5] * ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1]) / p[5])));dydt_s.setVal(2, (1)*(1)* (p[5] * ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1]) / p[5]))+  (-1)*(1)* (p[5] * (p[3] * s[1] / (p[4] + s[1]) / p[5]))); return dydt_s;');

   // Test #4 (results should be the same as #3:
   self.equationsRefList.Add('p[1] = s[2] * 5 + 5' + sLineBreak +
'return p;' + sLineBreak +
'let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]); p[1] = s[2] * 5 + 5;dydt_s.setVal(1, (1)*(1)* (p[5] * (p[0] * (p[6] - s[0] / p[1]) / (1 + p[6] + s[0] + Math.pow(s[1], p[2])) / p[5]))+  (-1)*(1)* (p[5] * ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2]) / p[5])));dydt_s.setVal(3, (1)*(1)* (p[5] * ((10 * s[0] - 2 * s[2]) / (1 + s[0] + s[2]) / p[5]))+  (-1)*(1)* (p[5] * ((5 * s[2] - 2 * s[3]) / (1 + s[2] + s[3]) / p[5])));dydt_s.setVal(4, (1)*(1)* (p[5] * ((5 * s[2] - 2 * s[3]) / (1 + s[2] + s[3]) / p[5]))+  (-1)*(1)* (p[5] * ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1]) / p[5])));dydt_s.setVal(2, (1)*(1)* (p[5] * ((10 * s[3] - 2 * s[1]) / (1 + s[3] + s[1]) / p[5]))+  (-1)*(1)* (p[5] * (p[3] * s[1] / (p[4] + s[1]) / p[5]))); return dydt_s;');

   self.numTests := self.equationsRefList.Count;
 end;

 function TTestSBMLReadGenerateEqs.getTestEqsRef( testIndex: integer): string;
 begin
   if testIndex < self.equationsRefList.Count then
     Result := self.equationsRefList[testIndex]
   else Result := '';
 end;

end.
