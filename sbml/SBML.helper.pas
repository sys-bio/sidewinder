unit SBML.helper;

interface
uses Web, JS, SBML.model, SBML.model.rule;

type

{ Define a procedural type used for event handling }
    TPingEvent = procedure() of object;

//  **************************************************************
 // Reads SBML model parsed by libsbml.js module
 SBMLhelpClass = class
   private
    errors, numbRxns: integer;  // SBML reading errors, number of rxns in model.
    numbPlugins: integer;
    numSpecies, numParams, numCompartments: integer; // number of species,params,comps in model
    numEvents, numRules, numFuncDefs: integer;
    annotationStr: String;
    sbmlreactions: array of SBMLReaction; // list of reactions
    modelSpecies: array of SBMLspecies;
    modelComps: array of SBMLcompartment;
    modelParams: array of SBMLparameter;
    modelRules: array of TSBMLrule;

    FPing: TPingEvent; // Used to send sbml info listener once asynchronous read done.

   public
   constructor create();
   procedure readSBML(textSBML: string);

   procedure setAnnotationStr(annotate:String);
   function getAnnotationStr():String;
   procedure setRxnNumb(rnxNumb:integer);
   procedure addSBMLReaction(rxnid:String; prods: array of String; prodStoich: array of double;
        reactants: array of String; reactantsStoich: array of double; kinetic: String);
   function getRxnsNumb():integer;

   function getParamNumb(): integer;
   function getSBMLparameter(i:integer): SBMLparameter;
   function getSBMLparameterArr(): array of SBMLparameter;
   procedure addSBMLparameter(newParam: SBMLparameter);
   function getSBMLmodelRules():array of TSBMLrule;

   procedure addSBMLspecies(newSpecies: SBMLspecies);
   procedure addSBMLrule( newR: TSBMLrule);
   function getSpeciesNumb(): integer;
   function getSBMLspecies(i:integer): SBMLspecies; overload;
   function getSBMLspecies(spId:string): SBMLspecies; overload;
   function getSBMLspeciesArr(): array of SBMLspecies;

   function getCompNumb(): integer;
   function getSBMLcompartmentsArr(): array of SBMLcompartment;
   function getSBMLcompartment(i:integer): SBMLcompartment;
   procedure addSBMLcompartment(newComp: SBMLcompartment);

   function getReactions(): array of SBMLReaction;


   property OnPing: TPingEvent read FPing write FPing;
    { Triggers the event if anything is registered }
   procedure TriggerEvent();

 end;

implementation

constructor SBMLhelpClass.create();
begin
    errors:=0;
    numbRxns:= -1;
    numSpecies:= 0; numParams:= 0; numCompartments:= 0;
    numEvents:= 0; numRules:= 0; numFuncDefs:=0;
    modelRules:= nil;
end;


procedure SBMLhelpClass.readSBML(textSBML:string);
 var numb:integer;

 nSpecies: SBMLspecies;
 jsSpecies: TJSObject;
 newComp: SBMLcompartment;
 jsComp: TJSObject;
 newParam: SBMLparameter;
 jsParam: TJSObject;
 newRule: TSBMLrule;
 jsRule: TJSObject;

begin
 nSpecies:= SBMLspecies.create(); // a new species
 jsSpecies:= JS.ToObject(nSpecies);
 newComp:= SBMLcompartment.create(); // a new compartment
 jsComp:= JS.ToObject(newComp);
 newParam:= SBMLparameter.create(); // a new parameter
 jsParam:= JS.ToObject(newParam);
 newRule:= TSBMLrule.create;
 jsRule:= JS.ToObject(newRule);

 // TODO: need to check if libsbml already loaded ??
 asm;   // javascript

   libsbml().then((libsbml) => {
  // now it is safe to use the module
  const reader = new libsbml.SBMLReader();
  const doc = reader.readSBMLFromString(textSBML);
  // read with no errors .. TODO: Chk for errors

  const model = doc.getModel();
  numb = model.getNumReactions();
  this.numbRxns = numb;
  this.numSpecies = model.getNumSpecies();
  this.numParams = model.getNumParameters();
  this.numCompartments = model.getNumCompartments();
  this.numEvents = model.getNumEvents();
  this.numRules = model.getNumRules();
  this.numbPlugins = model.getNumPlugins();
  //this.numFuncDefs = model.numFunctionDefinitions(); call wrong?? need to chk if exists first?
  this.annotationStr = model.getNotesString();
  var i;

  if(this.numRules>0) {
    for(i=0;i<this.numRules; i++) {
       //console.log('Rule: ', model.getRule(i).getFormula());
       var rule = model.getRule(i);
       jsRule.setScaler(false);
       // process rule here....
       jsRule.setAssignment(rule.isAssignment());
       jsRule.setAlgebraic(rule.isAlgebraic());
       //jsRule.setScaler(rule.isScaler());
       jsRule.setRate(rule.isRate());
       if(rule.isSetFormula()) {
         jsRule.setFormula(rule.getFormula());
      //   console.log('Formula for Rule: ', jsRule.getFormula());
       }
       if (rule.isParameter()) {
         jsRule.setParameter(true);
       }
       else {jsRule.setParameter(false); }
       if(rule.isSpeciesConcentration()) {
         jsRule.setSpeciesConcentration(true);
       }
       else {jsRule.setSpeciesConcentration(false);}

       if (rule.isSetVariable) {
      //   console.log('Parameter variable for rule: ',rule.getVariable());
         jsRule.setVariable(rule.getVariable());
       }
       if (rule.isSetId) {
       //  console.log('Parameter id for rule: ',rule.getId() );
         jsRule.setId(rule.getId());
         }
       this.addSBMLrule(jsRule);
    }
  }

  for(i=0;i<this.numSpecies; i++) {
   // generate array of SBMLspecies for model:
   const newSpecies = model.getSpecies(i);
   jsSpecies.setID(newSpecies.getId());
   if(newSpecies.isSetInitialAmount())
        { jsSpecies.setInitialAmount(newSpecies.getInitialAmount());}
   else if(newSpecies.isSetInitialConcentration())
        { jsSpecies.setInitialConcentration(newSpecies.getInitialConcentration());}
        else {jsSpecies.setInitialConcentration(0);} // default is 0.
   if(newSpecies.isSetCompartment()) {
      jsSpecies.setCompartment(newSpecies.getCompartment()); }
   if(newSpecies.isSetBoundaryCondition()) {
      jsSpecies.setBoundaryCondition(newSpecies.getBoundaryCondition());
     // console.log( ' Setting boundary condition to true') }
   if (newSpecies.isSetName()) {
      jsSpecies.setName(newSpecies.getName()); }
   this.addSBMLspecies(jsSpecies); // Add new species to array of all species used in model.
  // console.log('SBMLSpecies ID: ', jsSpecies.getID());
  }
  for(i=0;i<this.numCompartments;i++) {
    const newComp = model.getCompartment(i);
    jsComp.setID(newComp.getId());
  //  console.log('SBMLcompartment ID: ', jsComp.getID());
    if(newComp.isSetSize()) {
      jsComp.setSize(newComp.getSize()); }
    else if(newComp.isSetVolume()) {
      jsComp.setSize(newComp.getSize()); } // Save as size
    if(newComp.isSetName()) {
      jsComp.setName(newComp.getName()); }
    if(newComp.isSetConstant()) {
      jsComp.setConstant(newComp.getConstant()); }
    this.addSBMLcompartment(jsComp);
  }

   for(i=0;i<this.numParams;i++) {
    jsParam.unSetValueFlag();
    jsParam.setValue(0);
    const newParam = model.getParameter(i);
    jsParam.setId(newParam.getId());
   // console.log('SBMLparameter ID: ', jsParam.getId());
    if(newParam.isSetValue()) {
      jsParam.setValue(newParam.getValue());}
    if(newParam.isSetName()) {
      jsParam.setName(newParam.getName()); }
    if(newParam.isSetConstant()) {
      jsParam.setConstant(newParam.getConstant()); }
    //console.log('*** jsParam.value: ', jsParam.getValue());
    this.addSBMLparameter(jsParam);
  }


  for(i=0; i< numb; i++) {
    var numbTot;
    var j;
    var k;
    var reactants = [];  // an array of strings for each rxn
    var reactStoich = []; // number for each reactant per rxn
    var products = [];   // an array of strings
    var prodStoich = [];
    numbTot = model.getReaction(i).getNumReactants();

    for(j=0; j<numbTot; j++) {
    // Get info for SpeciesReference object:
     reactants[j] = model.getReaction(i).getReactant(j).getSpecies();
     if( model.getReaction(i).getReactant(j).isSetStoichiometry() ) {
       reactStoich[j] = model.getReaction(i).getReactant(j).getStoichiometry();
     }
     else { reactStoich[j] =1; }  // default
    }

    numbTot=0;
    numbTot = model.getReaction(i).getNumProducts();
    for(j=0; j<numbTot; j++) {
     products[j] = model.getReaction(i).getProduct(j).getSpecies();
     prodStoich[j] = model.getReaction(i).getProduct(j).getStoichiometry();
    }
    var kineticForm = model.getReaction(i).getKineticLaw().getFormula();
    const newKineticL = model.getReaction(i).getKineticLaw();
    //jsKineticLaw.setNumParameters(newKineticL.getNumParameters());

  //  console.log('model.getReaction... numb of params: ',model.getReaction(i).getKineticLaw().getNumParameters());
    for(k=0;k < model.getReaction(i).getKineticLaw().getNumParameters(); k++) {
      //jsKineticLaw.addParameter(model.getReaction(i).getKineticLaw().getParameter(k).getId());
    }

  //  console.log('--> kinetic law: ',model.getReaction(i).getKineticLaw().getFormula());
    this.addSBMLReaction(model.getReaction(i).getId(),products,prodStoich, reactants,reactStoich,kineticForm);

  }
  this.TriggerEvent();  // libsbml loaded and model processed.
  libsbml.destroy(doc);
  libsbml.destroy(reader);
  });

 end;

end;


procedure SBMLhelpClass.setRxnNumb(rnxNumb:integer);
var numbRxns: integer;
begin
  numbRxns:= rnxNumb;
end;

function SBMLhelpClass.getRxnsNumb():integer;
begin
   Result:= numbRxns;
end;

procedure SBMLhelpClass.TriggerEvent();
 begin
   { Call the registerd event only if there is a listener }
   if Assigned(FPing) then
     FPing();
 end;

 procedure SBMLhelpClass.setAnnotationStr(annotate:String);
 begin
  annotationStr:=annotate;
 end;

 function SBMLhelpClass.getAnnotationStr():String;
 begin
   Result:= annotationStr;
 end;

 procedure SBMLhelpClass.addSBMLReaction(rxnid:String; prods: array of String; prodStoich: array of double;
        reactants: array of String; reactantsStoich: array of double; kinetic: String);

 var
 newRxn: SBMLReaction;
 len: integer;
 p, r: array of SBMLspeciesreference;
  I: Integer;
  paramArray: array of String;
 begin
 //  console.log('In add Reaction', rxnid, reactants[0], prods[0]);
 setlength(p, Length(prods));
 paramArray:= ['nothing', 'empty'];
 for I := 0 to Length(prods)-1 do
  begin
     p[I]:= SBMLspeciesReference.create(prods[I],prodStoich[I]);
  end;

  setlength(r, Length(reactants));

 for I := 0 to Length(reactants)-1 do
  begin
     r[I]:= SBMLspeciesReference.create(reactants[I],reactantsStoich[I]);
  end;

   newRxn:= SBMLReaction.create(rxnid, p, r);
   newRxn.kineticLawStr:= kinetic;   // Get rid of.....
   //newId: String; newFormula: String; paramArr: array of String
   newRxn.setKineticLaw(SBMLkineticLaw.create('dummy', kinetic,paramArray));
   len:= Length(SBMLreactions);
   SetLength(SBMLreactions,len+1);    // Add new reaction to array
   sbmlreactions[len]:= newRxn;
 end;

 function SBMLhelpClass.getReactions(): array of SBMLReaction;
 begin
    Result:= SBMLreactions;
 end;

 function SBMLhelpClass.getSpeciesNumb(): integer;
 begin
   Result:= numSpecies;
 end;

 procedure SBMLhelpClass.addSBMLspecies(newSpecies: SBMLspecies);
 var len:integer;
 begin
  len:= Length(modelSpecies);
  SetLength(modelSpecies,len+1);  // add new species to array
  modelSpecies[len]:= SBMLspecies.create(newSpecies);
 end;

 function SBMLhelpClass.getSBMLspeciesArr(): array of SBMLspecies;
 begin
   Result:= modelSpecies;
 end;

 function SBMLhelpClass.getSBMLspecies(i:integer): SBMLspecies; overload;
 begin
  //console.log('get name of species:',modelSpecies[i].getID() );
   Result:= modelSpecies[i];
 end;

 function SBMLhelpClass.getSBMLspecies(spId:string): SBMLspecies; overload;
 var i: integer;
 begin
   for i := 0 to Length(self.modelSpecies)-1 do
     begin
       if self.modelSpecies[i].getId = spId then
         Result:= self.modelSpecies[i]
       else Result:= nil;

     end;
 end;

 function SBMLhelpClass.getSBMLcompartmentsArr(): array of SBMLcompartment;
 begin
   Result:= modelComps;
 end;
 function SBMLhelpClass.getSBMLcompartment(i:integer): SBMLcompartment;
 begin
   Result:= modelComps[i];
 end;

 function SBMLhelpClass.getCompNumb(): integer;
 begin
   Result:= numCompartments;
 end;

 procedure SBMLhelpClass.addSBMLcompartment(newComp: SBMLcompartment);
 var len:integer;
 begin
   len:= Length(modelComps);
   modelComps[len]:= SBMLcompartment.create(newComp);
 end;

 function SBMLhelpClass.getParamNumb(): integer;
 begin
   Result:= numParams;
 end;
 function SBMLhelpClass.getSBMLparameter(i:integer): SBMLparameter;
 begin
    Result:= modelParams[i];
 end;

 function SBMLhelpClass.getSBMLparameterArr(): array of SBMLparameter;
 begin
   Result:= modelParams;
 end;
 procedure SBMLhelpClass.addSBMLparameter(newParam: SBMLparameter);
 var len:integer;
 begin
  len:= Length(modelParams);
  modelParams[len]:= SBMLparameter.create(newParam);
 // console.log('addSBMLparameter: ',modelParams[len].getID());
 // console.log(' ... addSBMLparameter: Value: ',modelParams[len].getValue());
  if newParam.isSetName() then console.log('addSBMLparameter: ',newParam.getname());

 end;

 procedure SBMLhelpClass.addSBMLrule( newR: TSBMLrule);
 var len:integer;
 begin
   len:= Length(self.modelRules);
   self.modelRules[len]:= TSBMLrule.create(newR);
 end;

 function SBMLhelpClass.getSBMLmodelRules():array of TSBMLrule;
 begin
   Result:= self.modelRules;
 end;
end.   // unit
