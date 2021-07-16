


 // Class to convert libSBML model to TModel format.
 //   libSBML is loaded module from libsbml.js

class ProcessSBML {
 constructor( libSBMLModel ){
   this.model = libSBMLModel; // Model from libSBML doc.getModel()
 }
 getNumbers(tModela) { //  class TModel
   tModela.numSpecies = this.model.getNumSpecies();
   tModela.numParams = this.model.getNumParameters();
   tModela.numCompartments = this.model.getNumCompartments();
   tModela.numEvents = this.model.getNumEvents();
   tModela.numRules = this.model.getNumRules();
   tModela.numbPlugins = this.model.getNumPlugins();
  //tModela.numFuncDefs = model.numFunctionDefinitions(); call wrong?? need to chk if exists first?
   tModela.annotationStr = this.model.getNotesString();
   return tModela;
  }

getRules(tModela, tRule) {
   var i;
    if (tModela.numRules > 0) {
      for (i=0; i < tModela.numRules; i++) {
        //console.log('Rule: ', model.getRule(i).getFormula());
         var rule = this.model.getRule(i);
         tRule.setScaler(false);
       // process rule here....
         tRule.setAssignment(rule.isAssignment());
         tRule.setAlgebraic(rule.isAlgebraic());
       //tRule.setScaler(rule.isScaler());
         tRule.setRate(rule.isRate());
         if(rule.isSetFormula()) {
           tRule.setFormula(rule.getFormula());
       //  console.log('Formula for Rule: ', tRule.getFormula());
         }
         if (rule.isParameter()) {
           tRule.setParameter(true);
         }
         else {tRule.setParameter(false); }
         if(rule.isSpeciesConcentration()) {
           tRule.setSpeciesConcentration(true);
         }
         else {tRule.setSpeciesConcentration(false);}
         if (rule.isSetVariable) {
           tRule.setVariable(rule.getVariable());
         }
         if (rule.isSetId) {
           tRule.setId(rule.getId());
         }
         tModela.addSBMLrule(tRule);
      }
   }
   return tModela;
  }

  getSpecies(tModela, tSpecies ) {
    var i;
    for (i=0; i < tModela.numSpecies; i++) {
   // generate array of TSBMLSpecies for model:
      const newSpecies = this.model.getSpecies(i);
      tSpecies.setID(newSpecies.getId());
      if (newSpecies.isSetInitialAmount())
        { tSpecies.setInitialAmount(newSpecies.getInitialAmount());}
      else if (newSpecies.isSetInitialConcentration())
        { tSpecies.setInitialConcentration(newSpecies.getInitialConcentration());}
        else {tSpecies.setInitialConcentration(0);} // default is 0.
      if (newSpecies.isSetCompartment()) {
        tSpecies.setCompartment(newSpecies.getCompartment()); }
      if (newSpecies.isSetBoundaryCondition()) {
        tSpecies.setBoundaryCondition(newSpecies.getBoundaryCondition());}
      // console.log( ' Setting boundary condition to true') }
      if (newSpecies.isSetName()) {
        tSpecies.setName(newSpecies.getName()); }
      tModela.addSBMLspecies(tSpecies); // Add new species to array of all species used in model.
    }
    return tModela;
  }

  getCompartments(tModela, tComp ) {
    var i;
    for (i=0; i < tModela.numCompartments; i++) {
      const newComp = this.model.getCompartment(i);
      tComp.setID(newComp.getId());
      if (newComp.isSetSize()) {
        tComp.setSize(newComp.getSize()); }
      else if(newComp.isSetVolume()) {
        tComp.setSize(newComp.getSize()); } // Save as size
      if (newComp.isSetName()) {
        tComp.setName(newComp.getName()); }
      if (newComp.isSetConstant()) {
        tComp.setConstant(newComp.getConstant()); }
      tModela.addSBMLcompartment(tComp);
    }
    return tModela;
  }

  getParameters(tModela, tParam ) {
    var i;
    for(i=0; i < tModela.numParams; i++) {
      tParam.unSetValueFlag();
      tParam.setValue(0);
      const newParam = this.model.getParameter(i);
      tParam.setId(newParam.getId());
    //console.log('SBMLparameter ID: ', tParam.getId());
      if(newParam.isSetValue()) {
        tParam.setValue(newParam.getValue());}
      if(newParam.isSetName()) {
        tParam.setName(newParam.getName()); }
      if(newParam.isSetConstant()) {
        tParam.setConstant(newParam.getConstant()); }
      tModela.addSBMLparameter(tParam);
    }
    return tModela;
  }

  getReactions(tModela){
    var i;
    for( i=0; i< this.model.getNumReactions(); i++) {
      var numTotalSpecies;
      var j;
      var k;
      var reactants = [];  // an array of strings for each rxn
      var reactStoich = []; // number for each reactant per rxn
      var products = [];   // an array of strings
      var prodStoich = [];
      numTotalSpecies = this.model.getReaction(i).getNumReactants();

      for (j=0; j < numTotalSpecies; j++) {
    // Get info for SpeciesReference object:
        reactants[j] = this.model.getReaction(i).getReactant(j).getSpecies();
        if( this.model.getReaction(i).getReactant(j).isSetStoichiometry() ) {
          reactStoich[j] = this.model.getReaction(i).getReactant(j).getStoichiometry();
        }
        else { reactStoich[j] =1; }  // default
      }
      numTotalSpecies = 0;
      numTotalSpecies = this.model.getReaction(i).getNumProducts();
      for (j=0; j < numTotalSpecies; j++) {
        products[j] = this.model.getReaction(i).getProduct(j).getSpecies();
        prodStoich[j] = this.model.getReaction(i).getProduct(j).getStoichiometry();
      }
      var kineticForm = this.model.getReaction(i).getKineticLaw().getFormula();
      const newKineticLaw = this.model.getReaction(i).getKineticLaw();
   //console.log('--> kinetic law: ',this.model.getReaction(i).getKineticLaw().getFormula());
      tModela.addSBMLReaction(this.model.getReaction(i).getId(), products, prodStoich, reactants, reactStoich, kineticForm);
    }
    return tModela;
  }

 }