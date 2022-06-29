unit uTestSBMLReadWrite;

// Test reading a SBML file and storing in a TModel using TSBML Classes
// Test writing a SBML file from TModel

interface
uses   System.SysUtils, System.Classes, JS, Web, System.Generics.Collections,
 uTestCase, utests.TestUtils, uSBMLClasses, uSBMLWriter, uSBMLReader, uModel, uTestModel,
 uTestSBML_ReadModels;

const NUM_WRITE_TESTS = 1;
      NUM_READ_TESTS = 3;

type
 TReadWriteTestsFinished = procedure(readWriteTestCase: TList<TTestCaseResult>) of object;  // Notify when done testing
 TTestSBMLReadWrite = class
  private
    FNotify: TReadWriteTestsFinished; // send sbml info to listener once asynchronous read done.
    testModel: TModel;
    currentWriteTestIndex: integer; // Due to asynchronous calls to libsbml.js, need to keep track of this?
    currentReadTestIndex: integer;
    function generateWriteTestModel(testIndex: integer): TModel;
    function generateReadTestModel(testIndex: integer): string;
    function getWriteTestReferenceString(testIndex: integer): string;
    function getReadTestReferenceString(testIndex: integer): string;
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
  self.testResultList := TList<TTestCaseResult>.create;
  self.currentWriteTestIndex := 0;
  self.currentReadTestIndex := 0;
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
 console.log('MODEL: ', modelStr);
 inc(self.currentWriteTestIndex); // next test...
 if self.currentWriteTestIndex < NUM_WRITE_TESTS then
   runWriteSBMLTest( )
 else
   begin
   if self.currentReadTestIndex < NUM_READ_TESTS then
     runReadSBMLTest()
   else self.testsFinished;
   end
end;

procedure TTestSBMLReadWrite.modelRead();
var i: integer; refStr, testModelStr: string;
    refResultsList: TList<string>;
// self.testResultList index is assumed to be at self.currentWriteTestIndex + self.currentReadTestIndex
begin
  console.log('MODEL read: ');
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
  if self.currentReadTestIndex < NUM_READ_TESTS then
    begin
    self.testModel.Free;
    runReadSBMLTest( )
    end
  else self.testsFinished;
end;

procedure TTEstSBMLReadWrite.runTests;
begin
  if NUM_WRITE_TESTS > 0 then
    self.runWriteSBMLTest
  else if NUM_READ_TESTS > 0 then self.runReadSBMLTest;


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

function TTestSBMLReadWrite.generateReadTestModel(testIndex: integer): string;
begin
  if testIndex < 2 then  //  change from 2 to SBML_EXAMPLE_MODELS when all examples added
 // if testIndex < SBML_EXAMPLE_MODELS then
    Result := getTestModel(testIndex)
  else
    begin
    if testIndex < 2 + SBML_TEST_MODELS then
      Result := getSBMLReadTestModel (testIndex - 2)
    else Result := '';
    end;

end;

 function TTestSBMLReadWrite.getWriteTestReferenceString(testIndex: Integer): string;
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


  function TTestSBMLReadWrite.getReadTestReferenceString(testIndex: integer): string;
  var refTestStrList: TList<string>;
  begin
    refTestStrList := TList<string>.create;
    refTestStrList.Add('Model id: , Species:  Species ID: S1, Boundary sp: false, Init Conc: 20, Comp: default_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, No Comp name, Comp size: 1, Comp constant, ' + sLineBreak +
 'Model params:  Param ID: k1, No Param name, Value: 0.1, Param Const Param ID: k2, No Param name, Value: 0.2, Param Const Param ID: k3, No Param name, Value: 0.1, Param Const Param ID: k4, No Param name, Value: 0.2, Param Const ' + sLineBreak +
 'Model Rxns: , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k1 * S1, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k2 * S2, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k3 * S3, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J4, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: k4 * S4, Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. ' + sLineBreak +
 'Model Initial Assignments: ' + sLineBreak +
 'Model Rules: ' + sLineBreak +
 'Model events: 0 ' + sLineBreak +
 'Model Func definitions: ' + sLineBreak +
 'Model Layout: NO layout');

    refTestStrList.Add( 'Model id: , Species:  Species ID: X0, Boundary sp: true, Init Conc: 10, Comp: unit_compartment Species ID: S1, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: unit_compartment Species ID: X1, Boundary sp: true, Init Conc: 0, Comp: unit_compartment' + sLineBreak +
 'Model compartments:  Comp ID: unit_compartment, No Comp name, Comp size: 1, Comp constant,' + sLineBreak +
 'Model params:  Param ID: VM1, No Param name, Value: 10, Param Const Param ID: Keq1, No Param name, Value: 10, Param Const Param ID: h, No Param name, Value: 10, Param Const Param ID: default_compartment, No Param name, Value: 1, Param can vary Param ID: V4, No Param name, Value: 2.5, Param Const Param ID: KS4, No Param name, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: VM1 * (X0 - S1 / Keq1) / (1 + X0 + S1 + pow(S4, h)), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S1 - 2 * S2) / (1 + S1 + S2), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S2 - 2 * S3) / (1 + S2 + S3), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S3 - 2 * S4) / (1 + S3 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn Not reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: V4 * S4 / (KS4 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. ' + sLineBreak +
 'Model Initial Assignments:' + sLineBreak +
 'Model Rules: ' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions: ' + sLineBreak +
 'Model Layout:  Layout ID: , Layout dims:  Layout Dims ID:  Layout w, h: 863, 636' + sLineBreak +
 'Additional Graphical Glyphs: ' + sLineBreak +
 'Additional General Glyphs: ' + sLineBreak +
 'Compartment Glyphs: ' + sLineBreak +
 'Species Glyphs:  Layout species glyph: , Layout Graph Object ID:speciesGlyphX0 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 204.808629732497, 192.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: X0 Layout species glyph: , Layout Graph Object ID:speciesGlyphS1 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 170.563342861622, 336.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S1 Layout species glyph: , Layout Graph Object ID:speciesGlyphS4 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 659.865504755851, 253.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S4 Layout species glyph: , Layout Graph Object ID:speciesGlyphS2 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 334.903954537822, 388.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S2 Layout species glyph: , Layout Graph Object ID:speciesGlyphS3 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 515.817311698465, 331.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: S3 Layout species glyph: , Layout Graph Object ID:speciesGlyphX1 BoundingBox ID: , BB pt:  Layout Pt ID:  Layout Pt XY: 703.041256413743, 405.5, Layout Pt Z: 0 Layout Dims ID:  Layout w, h: 60, 40, species Glyph: species id: X1' + sLineBreak +
 'Reaction and Sp Ref Glyphs: , Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ0 No bounding box, Reaction Glyph Rxn Id: J0, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphX0J0 No bounding box, Sp Glyph ID: speciesGlyphX0, Sp Ref ID: X0J0, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 228.331, 239.1, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 226.247308014778, 248.5, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS1J0 No bounding box, Sp Glyph ID: speciesGlyphS1, Sp Ref ID: S1J0, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 207.05, 327.837, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 209.124664579341, 320.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 212.417480624617, 306.653846153846, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ1 No bounding box, Reaction Glyph Rxn Id: J1, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 217.685986297059, 284.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 207.05, 327.837, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 209.124664579341, 320.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 212.417480624617, 306.653846153846, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS1J1 No bounding box, Sp Glyph ID: speciesGlyphS1, Sp Ref ID: S1J1, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 237.914, 367.704, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 241.648495780672, 369.5, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS2J1 No bounding box, Sp Glyph ID: speciesGlyphS2, Sp Ref ID: S2J1, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 325.989, 395.205, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 323.818801618772, 395.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 308.016819726829, 390.5, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ2 No bounding box, Reaction Glyph Rxn Id: J2, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 282.733648699722, 382.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 325.989, 395.205, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 323.818801618772, 395.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 308.016819726829, 390.5, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS2J2 No bounding box, Sp Glyph ID: speciesGlyphS2, Sp Ref ID: S2J2, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 401.096, 396.902, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 410.132293827982, 394.25, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS3J2 No bounding box, Sp Glyph ID: speciesGlyphS3, Sp Ref ID: S3J2, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 506.887, 363.511, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 500.588972408304, 365.75, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 483.193457296704, 371.230769230769, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ3 No bounding box, Reaction Glyph Rxn Id: J3, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 455.360633118143, 380, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 506.887, 363.511, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 500.588972408304, 365.75, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 483.193457296704, 371.230769230769, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS3J3 No bounding box, Sp Glyph ID: speciesGlyphS3, Sp Ref ID: S3J3, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 582.79, 330.677, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 581.829359962812, 332, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS4J3 No bounding box, Sp Glyph ID: speciesGlyphS4, Sp Ref ID: S4J3, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 650.479, 294.216, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 653.853456491505, 293, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 640.002668697525, 300.5, Layout Pt Z: 0' + sLineBreak +
', Layout Reaction Glyph: , Layout Graph Object ID:reactionGlyphJ4 No bounding box, Reaction Glyph Rxn Id: J4, Reaction Glyph Curve: , Layout Curve ID: Layout Curve lineSeg:, Layout LineSeg ID: , Start:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0, End:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 617.841408227158, 312.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 650.479, 294.216, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 653.853456491505, 293, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 640.002668697525, 300.5, Layout Pt Z: 0, Reaction Glyph SpRefGlyphs: , Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphS4J4 No bounding box, Sp Glyph ID: speciesGlyphS4, Sp Ref ID: S4J4, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 696.362, 300.437, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 700.659442670324, 311.5, Layout Pt Z: 0' + sLineBreak +
', Layout Species Ref Glyph: , Layout Graph Object ID:specRefGlyphX1J4 No bounding box, Sp Glyph ID: speciesGlyphX1, Sp Ref ID: X1J4, Role: undefined Sp Ref Glyph Curve: , Layout Curve ID: Layout Curve Bezier:, Layout Bezier ID: , Bezier Start:  Layout Pt ID:  Layout Pt XY: 711.453380584797, 349.5, Layout Pt Z: 0, Bezier End:  Layout Pt ID:  Layout Pt XY: 724.979, 396.803, Layout Pt Z: 0Bezier BPt 1:  Layout Pt ID:  Layout Pt XY: 722.24731849927, 387.5, Layout Pt Z: 0, Bezier BPt 2:  Layout Pt ID:  Layout Pt XY: 718.09580391678, 372.884615384615, Layout Pt Z: 0' + sLineBreak +
'' + sLineBreak +
 'Text Glyphs: , Layout TextGlyph: , Layout Graph Object ID:txtGlyphX0 No bounding box, TextGlyph text: X0, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphX0, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS1 No bounding box, TextGlyph text: S1, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS1, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS4 No bounding box, TextGlyph text: S4, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS4, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS2 No bounding box, TextGlyph text: S2, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS2, Layout TextGlyph: , Layout Graph Object ID:txtGlyphS3 No bounding box, TextGlyph text: S3, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphS3, Layout TextGlyph: , Layout Graph Object ID:txtGlyphX1 No bounding box, TextGlyph text: X1, TextGlyph text origin: , TextGlyph GraphObj ID: speciesGlyphX1'
     );

     refTestStrList.Add('Model id: , Species:  Species ID: X0, Boundary sp: true, Init Conc: 10, Comp: default_compartment Species ID: S1, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S4, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S2, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: S3, Boundary sp: false, Init Conc: 0, Comp: default_compartment Species ID: X1, Boundary sp: true, Init Conc: 0, Comp: default_compartment' + sLineBreak +
 'Model compartments:  Comp ID: default_compartment, No Comp name, Comp size: 1, Comp constant, ' + sLineBreak +
 'Model params:  Param ID: VM1, No Param name, Value: 0, Param can vary Param ID: Keq1, No Param name, Value: 0, Param can vary Param ID: h, No Param name, Value: 0, Param can vary Param ID: V4, No Param name, Value: 2.5, Param Const Param ID: KS4, No Param name, Value: 0.5, Param Const' + sLineBreak +
 'Model Rxns: , Rxn ID: J0, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S1J0, SpRef species: S1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: X0J0, SpRef species: X0, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: VM1 * (X0 - S1 / Keq1) / (1 + X0 + S1 + pow(S4, h)), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J1, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S2J1, SpRef species: S2, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S1J1, SpRef species: S1, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S1 - 2 * S2) / (1 + S1 + S2), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J2, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S3J2, SpRef species: S3, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S2J2, SpRef species: S2, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S2 - 2 * S3) / (1 + S2 + S3), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J3, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: S4J3, SpRef species: S4, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S3J3, SpRef species: S3, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: (10 * S3 - 2 * S4) / (1 + S3 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list. , Rxn ID: J4, No Rxn Name, No Rxn Comp , Rxn reversible, Rxn products:  SpRef ID: X1J4, SpRef species: X1, Stoich Coeff: 1, Rxn reactants:  SpRef ID: S4J4, SpRef species: S4, Stoich Coeff: 1, Rxn KinLaw:  Kinetic Law id: dummy, Kinetic Law No name, Kinetic Law formula: V4 * S4 / (KS4 + S4), Kinetic Law params: self.paramIds[i], self.paramIds[i],  End of Kinetic Law param list.' + sLineBreak +
 'Model Initial Assignments:  InitAssign Id: InitAssign_h, Symbol: h, Formula: Keq1 + 2' + sLineBreak +
 'Model Rules:  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: VM1, Rule formula: 10 + S2, Rule Does not use piecewise.  Assignment Rule ID: , No Rule name, Rule is for parameter, Rule variable: Keq1, Rule formula: S1 * 10 + 10, Rule Does not use piecewise. ' + sLineBreak +
 'Model events: 0' + sLineBreak +
 'Model Func definitions: ' + sLineBreak +
 'Model Layout: NO layout');

    if refTestStrList.count > testIndex then
      Result := refTestStrList[testIndex]
    else Result := '';
  end;

end.
