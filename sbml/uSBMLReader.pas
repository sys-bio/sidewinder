unit uSBMLReader;

interface
uses Web, JS,  uSBMLClasses, uSBMLClasses.rule, uModel, uControllerNetwork;

type
  TSBMLLoadedEvent = procedure(model: TModel) of object;  // Notify when done loading

  TSBMLRead = class
  private
    FNotify: TSBMLLoadedEvent; // send sbml info to listener once asynchronous read done.

  public
    constructor create(newModel: TModel; SBMLtext: String);
    property OnPing: TSBMLLoadedEvent read FNotify write FNotify;
    procedure modelLoaded(model:TModel);
  end;


implementation
  constructor TSBMLRead.create(newModel: TModel; SBMLtext: String);
  var
    newSpecies: TSBMLSpecies;
    jsSpecies: TJSObject;
    newComp: SBMLcompartment;
    jsComp: TJSObject;
    newParam: SBMLparameter;
    jsParam: TJSObject;
    newRule: TSBMLRule;
    jsRule: TJSObject;
  begin
    newSpecies:= TSBMLSpecies.create(); // a new species
    jsSpecies:= JS.ToObject(newSpecies);
    newComp:= SBMLcompartment.create(); // a new compartment
    jsComp:= JS.ToObject(newComp);
    newParam:= SBMLparameter.create(); // a new parameter
    jsParam:= JS.ToObject(newParam);
    newRule:= TSBMLRule.create;
    jsRule:= JS.ToObject(newRule);

    // TODO: need to check if libsbml already loaded ??
 asm;   // javascript

  libsbml().then((libsbml) => {

  // now it is safe to use the module
  const reader = new libsbml.SBMLReader();
  const doc = reader.readSBMLFromString(SBMLtext);
  // read with no errors .. TODO: Chk for errors

  const model = doc.getModel();
  newModel.numReactions = model.getNumReactions();
  newModel.numSpecies = model.getNumSpecies();
  newModel.numParams = model.getNumParameters();
  newModel.numCompartments = model.getNumCompartments();
  newModel.numEvents = model.getNumEvents();
  newModel.numRules = model.getNumRules();
  newModel.numbPlugins = model.getNumPlugins();
  //newModel.numFuncDefs = model.numFunctionDefinitions(); call wrong?? need to chk if exists first?
  newModel.annotationStr = model.getNotesString();
  var i;

  if (newModel.numRules > 0) {
    for (i=0; i < newModel.numRules; i++) {
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
         jsRule.setVariable(rule.getVariable());
       }
       if (rule.isSetId) {
         jsRule.setId(rule.getId());
         }
       newModel.addSBMLrule(jsRule);
    }
  }

  for (i=0; i < newModel.numSpecies; i++) {
   // generate array of TSBMLSpecies for model:
   const newSpecies = model.getSpecies(i);
   jsSpecies.setID(newSpecies.getId());
   if (newSpecies.isSetInitialAmount())
        { jsSpecies.setInitialAmount(newSpecies.getInitialAmount());}
   else if (newSpecies.isSetInitialConcentration())
        { jsSpecies.setInitialConcentration(newSpecies.getInitialConcentration());}
        else {jsSpecies.setInitialConcentration(0);} // default is 0.
   if (newSpecies.isSetCompartment()) {
      jsSpecies.setCompartment(newSpecies.getCompartment()); }
   if (newSpecies.isSetBoundaryCondition()) {
      jsSpecies.setBoundaryCondition(newSpecies.getBoundaryCondition());  }
      // console.log( ' Setting boundary condition to true') }
   if (newSpecies.isSetName()) {
      jsSpecies.setName(newSpecies.getName()); }
   newModel.addSBMLspecies(jsSpecies); // Add new species to array of all species used in model.
  }
  for (i=0; i < newModel.numCompartments; i++) {
    const newComp = model.getCompartment(i);
    jsComp.setID(newComp.getId());
    if (newComp.isSetSize()) {
      jsComp.setSize(newComp.getSize()); }
    else if(newComp.isSetVolume()) {
      jsComp.setSize(newComp.getSize()); } // Save as size
    if (newComp.isSetName()) {
      jsComp.setName(newComp.getName()); }
    if (newComp.isSetConstant()) {
      jsComp.setConstant(newComp.getConstant()); }
    newModel.addSBMLcompartment(jsComp);
  }

   for(i=0; i < newModel.numParams; i++) {
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
    newModel.addSBMLparameter(jsParam);
  }

  for( i=0; i< newModel.numReactions; i++) {
    var numTotalSpecies;
    var j;
    var k;
    var reactants = [];  // an array of strings for each rxn
    var reactStoich = []; // number for each reactant per rxn
    var products = [];   // an array of strings
    var prodStoich = [];
    numTotalSpecies = model.getReaction(i).getNumReactants();

    for (j=0; j < numTotalSpecies; j++) {
    // Get info for SpeciesReference object:
     reactants[j] = model.getReaction(i).getReactant(j).getSpecies();
     if( model.getReaction(i).getReactant(j).isSetStoichiometry() ) {
       reactStoich[j] = model.getReaction(i).getReactant(j).getStoichiometry();
     }
     else { reactStoich[j] =1; }  // default
    }

    numTotalSpecies = 0;
    numTotalSpecies = model.getReaction(i).getNumProducts();
    for (j=0; j < numTotalSpecies; j++) {
      products[j] = model.getReaction(i).getProduct(j).getSpecies();
      prodStoich[j] = model.getReaction(i).getProduct(j).getStoichiometry();
    }
    var kineticForm = model.getReaction(i).getKineticLaw().getFormula();
    const newKineticLaw = model.getReaction(i).getKineticLaw();

  //  console.log('--> kinetic law: ',model.getReaction(i).getKineticLaw().getFormula());
    newModel.addSBMLReaction(model.getReaction(i).getId(), products, prodStoich, reactants, reactStoich, kineticForm);

  }
  newModel.SBML_LoadedEvent();  // libsbml loaded and model processed.
  this.modelLoaded(newModel);
  libsbml.destroy(doc);
  libsbml.destroy(reader);
  });

   end;


  end;

  procedure TSBMLRead.modelLoaded(model: TModel);
  begin
      { Call the registerd event only if there is a listener }
   if Assigned(FNotify) then
     FNotify(model);
  end;
end.
