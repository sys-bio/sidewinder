
 // Class to convert TModel to libSBML format for writing to file.
 //   libSBML is loaded module from libsbml.js
class GenerateSBMLModel {
 constructor( libSBML, newModel ){
   this.writer = new libSBML.SBMLWriter();
   this.sbmlDoc = new libsbml.SBMLDocument();
   this.modelStr = "";
   this.rxnModel = newModel; // TModel
   this.libSBMLmodel = null;
 }
 buildModel() { // TModel
   const modelName = "Chemical Reaction"; // Not saved ?
   this.libSBMLmodel = this.sbmlDoc.createModel();
   this.libSBMLmodel.setId(modelName);
   this.createCompartments();
   this.createParameters();
   this.createSpecies();
   this.createRules();
   this.createReactions();
 }
 getSBMLString() {
   var sbmlStr = "";
   sbmlStr = this.writer.writeSBMLToString(this.sbmlDoc);
   return sbmlStr;
 }
 createCompartments() {
   var i;
   const rxnComps = this.rxnModel.getSBMLcompartmentsArr();
   for( i=0; i< rxnComps.length; i++) {
     const newComp = this.libSBMLmodel.createCompartment();
     newComp.setId(rxnComps[i].getID());
     if(rxnComps[i].isSetName()){newComp.setName(rxnComps[i].getName());}
     newComp.setConstant(rxnComps[i].getConstant());
     if(rxnComps[i].isSetSize()){newComp.setSize(rxnComps[i].getSize());}
     if(rxnComps[i].isSetVolume()){newComp.setVolume(rxnComps[i].getVolume());}
   }
 }

 createParameters() {
   var i;
   const rxnParams = this.rxnModel.getSBMLparameterAr();
   for(i=0; i< rxnParams.length; i++) {
     const newParam = this.libSBMLmodel.createParameter();
     newParam.setId(rxnParams[i].getId());
     if(rxnParams[i].isSetName()){newParam.setName(rxnParams[i].getName());}
     newParam.setConstant(rxnParams[i].isSetConstant());
     if(rxnParams[i].isSetValue()){newParam.setValue(rxnParams[i].getValue());}
   }
 }

 createSpecies() {
   var i;
   const rxnSpecies = this.rxnModel.getSBMLspeciesAr();
   for(i=0; i< rxnSpecies.length; i++) {
     const newSpecies = this.libSBMLmodel.createSpecies();
     newSpecies.setId(rxnSpecies[i].getID());
     if(rxnSpecies[i].isSetName()){ newSpecies.setName(rxnSpecies[i].getName()) ;}
     if(rxnSpecies[i].isSetInitialAmount())
       { newSpecies.setInitialAmount(rxnSpecies[i].getInitialAmount()) ;}
     if(rxnSpecies[i].isSetInitialConcentration())
       { newSpecies.setInitialConcentration(rxnSpecies[i].getInitialConcentration()) ;}
     newSpecies.setBoundaryCondition(rxnSpecies[i].getBoundaryCondition());
     if(rxnSpecies[i].isSetCompartment())
       {newSpecies.setCompartment(rxnSpecies[i].getCompartment());}
     newSpecies.setConstant(rxnSpecies[i].getConstant());
     newSpecies.setHasOnlySubstanceUnits(rxnSpecies[i].getHasOnlySubstanceUnits());
   }
 }

 createRules() {
   var i;
   const rxnRules = this.rxnModel.getSBMLmodelRules();
   for( i=0; i< rxnRules.length; i++) {
     var newRule;
     if(rxnRules[i].isRate()){
       newRule = this.libSBMLmodel.createRateRule();}
     else if(rxnRules[i].isAssignment()){
            newRule = this.libSBMLmodel.createAssignmentRule();}
     else if(rxnRules[i].isAlgebraic()){
            newRule = this.libSBMLmodel.createAlgebraicRule();}
     newRule.setId(rxnRules[i].getId());
     if(rxnRules[i].isSetName()){newRule.setName(rxnRules[i].getName());}
     if(rxnRules[i].isScaler()){newRule.setScaler(true); }
     if(rxnRules[i].isSpeciesConcentration()){ newRule.setSpeciesConcentration(true);}
     if(rxnRules[i].isParameter()){ newRule.setParameter(true);}
     if(rxnRules[i].isSetFormula()){ newRule.setFormula(rxnRules[i].getFormula());}
     if(rxnRules[i].isSetVariable()){ newRule.setVariable(rxnRules[i].getVariable());}
   }
 }

 createReactions() {
   var i,j;
   var rxnReactions = this.rxnModel.getReactions();
   for(i=0; i< rxnReactions.length; i++) {
     const newReaction = this.libSBMLmodel.createReaction();
     newReaction.setId(rxnReactions[i].getID());
     if(rxnReactions[i].isSetName()){ newReaction.setName(rxnReactions[i].getName());}
     if(rxnReactions[i].isSetCompartment()){ newReaction.setCompartment(rxnReactions[i].getCompartment());}
     newReaction.setReversible(rxnReactions[i].getReversible());
     // Reactants:
     var rxnSpeciesRef = rxnReactions[i].getrxnReactants();
     for(j=0; j< rxnSpeciesRef.length; j++) {
       const newReactant = newReaction.createReactant();
       newReactant.setId(rxnSpeciesRef[j].getId());
       if(rxnSpeciesRef[j].isSetSpecies()){ newReactant.setSpecies(rxnSpeciesRef[j].getSpecies());
         newReactant.setName(rxnSpeciesRef[j].getSpecies()); } // name same as species
       if(rxnSpeciesRef[j].isSetStoichiometry())    // Stoich is double, convert to int?
         {newReactant.setStoichiometry(rxnSpeciesRef[j].getStoichiometry()); }
       newReactant.setConstant(rxnSpeciesRef[j].getConstant()); // stoich constant?
     }
     // Products:
     rxnSpeciesRef = rxnReactions[i].getrxnProducts();
     for(j=0; j< rxnSpeciesRef.length; j++) {
       const newProduct = newReaction.createProduct();
       newProduct.setId(rxnSpeciesRef[j].getId());
       if(rxnSpeciesRef[j].isSetSpecies()){ newProduct.setSpecies(rxnSpeciesRef[j].getSpecies());
         newProduct.setName(rxnSpeciesRef[j].getSpecies()); } // name same as species
       if(rxnSpeciesRef[j].isSetStoichiometry())     // Stoich is double, convert to int?
         {newProduct.setStoichiometry(rxnSpeciesRef[j].getStoichiometry()); }
       newProduct.setConstant(rxnSpeciesRef[j].getConstant()); // stoich constant?
     }
     // Kinetic Law:  Note: local param not implimented.
     if( rxnReactions[i].isSetKineticLaw()) {
       const rxnKineticLaw = rxnReactions[i].getKineticLaw();
       const newKineticLaw = newReaction.createKineticLaw();
       newKineticLaw.setId(rxnKineticLaw.getId());
       if(rxnKineticLaw.isNameFlagSet()){newKineticLaw.setName(rxnKineticLaw.getName());}
       //for(j=0; j<rxnKineticLaw.getNumParameters(); j++) // Do not add params for kinetic law
       newKineticLaw.setFormula(rxnKineticLaw.getFormula());


     }

   }
 }

}