
 // Class to convert TModel to libSBML format for writing to file.
 //   libSBML is loaded module from libsbml.js
class GenerateSBMLModel {
 constructor( curLibSBML, newModel ){
   this.libSBML = curLibSBML;
   this.writer = new this.libSBML.SBMLWriter();
   this.sbmlDoc = new this.libSBML.SBMLDocument(3,2);
  // this.sbmlDoc.enablePackage('layout', false);   // trying htis.
   this.modelStr = "";
   this.rxnModel = newModel; // TModel
   this.libSBMLmodel = null;
 }
 buildModel() { // TModel
   const modelName = this.rxnModel.getModelId();
   this.libSBMLmodel = this.sbmlDoc.createModel();
   this.libSBMLmodel.setId(modelName);  // Not saved ?

  // if(this.sbmlDoc.getLevel() == 3) {
   this.sbmlDoc.enablePackage(this.libSBML.LayoutExtension.prototype.getXmlnsL3V1V1(), 'layout', true);
   this.sbmlDoc.enablePackage(this.libSBML.RenderExtension.prototype.getXmlnsL3V1V1(), 'render', true);
  // }

   const lPlugin = this.libSBML.castObject(this.libSBMLmodel.findPlugin("layout"), libsbml.LayoutModelPlugin);
   this.libSBMLlayout = lPlugin.createLayout();// create new libsbml layout to be populated by this.rxnModel.

   const rPlugin = this.libSBML.castObject(this.libSBMLlayout.getPlugin("render"), libsbml.RenderLayoutPlugin);
   this.libSBMLrInfo = rPlugin.createLocalRenderInformation();
   // *************************************************************
   this.createCompartments();
   this.createParameters();
   this.createSpecies();
   this.createRules();
   this.createReactions();
   this.modelLayout = this.rxnModel.getSBMLLayout();
   if( this.modelLayout != undefined) {
        // Build SBML layout, add to SBML model
     this.libSBMLlayout.setId(modelName + this.modelLayout.getId());
     this.createLayout();
   }

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
 }   // createReactions()

 createLayout() {
   this.createCompartmentGlyphs();
   this.createSpeciesGlyphs();
   this.createReactionGlyphs();
   this.createTextGlyphs();
   this.createAdditionalGraphicalObjects(); // Not used
   this.createGeneralGlyphs();
 }

 processBoundingBox(bBox, sbmlBBox) {
   const sbmlBBDims = sbmlBBox.getDimensions();   // pointer to speciesGlyph bounding box dims
   sbmlBBDims.setWidth(bBox.getDims().getWidth());
   sbmlBBDims.setHeight(bBox.getDims().getHeight());
console.log('process BBox, width x height: ', sbmlBBox.getDimensions().getWidth(),sbmlBBox.getDimensions().getHeight() );
   sbmlBBox.setX(bBox.getPoint().getX()); // sets x coord in speciesglyph bbox center.
   sbmlBBox.setY(bBox.getPoint().getY()); // assume no Z

 }

  // cubBez: TSBMLLayoutCubicBezier, sbmlCubBez: libSBMLjs CubicBezier
 processCubicBezier( cubBez, sbmlCubBez ) {
   // assume no Z coord
   const sbmlBP1 = sbmlCubBez.getBasePoint1();
   sbmlBP1.setX( cubBez.getBasePoint1().getX() );
   sbmlBP1.setY( cubBez.getBasePoint1().getY() );
   const sbmlBP2 = sbmlCubBez.getBasePoint2();
   sbmlBP2.setX( cubBez.getBasePoint2().getX() );
   sbmlBP2.setY( cubBez.getBasePoint2().getY() );
   const sbmlStart = sbmlCubBez.getStart();
   sbmlStart.setX( cubBez.getStart().getX() );
   sbmlStart.setY( cubBez.getStart().getY() );
   const sbmlEnd = sbmlCubBez.getEnd();
   sbmlEnd.setX( cubBez.getEnd().getX() );
   sbmlEnd.setY( cubBez.getEnd().getY() );
   sbmlCubBez.setId( cubBez.getId() );
 }

 // lnSeg: TSBMLLayoutLineSegment, sbmlLnSeg: libSBMLjs LineSegment
 processLineSeg( lnSeg, sbmlLnSeg ) {
   const sbmlStart = sbmlLnSeg.getStart();
   sbmlStart.setX( lnSeg.getStartPt().getX() );
   sbmlStart.setY( lnSeg.getStartPt().getY() );
   const sbmlEnd = sbmlLnSeg.getEnd();
   sbmlEnd.setX( lnSeg.getEndPt().getX() );
   sbmlEnd.setY( lnSeg.getEndPt().getY() );
   sbmlLnSeg.setId( lnSeg.getId() );
 }

 createCompartmentGlyphs() {
   var i;
   for( i=0; i< this.modelLayout.getNumCompGlyphs(); i++) {
     const curCompGlyph = this.modelLayout.getCompGlyph(i);
     const sbmlCompGlyph = this.libSBMLlayout.createCompartmentGlyph();
     sbmlCompGlyph.setCompartmentId( curCompGlyph.getCompId() );
     sbmlCompGlyph.setOrder( curCompGlyph.getOrder() );
     sbmlCompGlyph.setId( curCompGlyph.getId() );
     const curBBox = curCompGlyph.getBoundingBox();
     const sbmlBBox = sbmlCompGlyph.getBoundingBox();
     this.processBoundingBox( curBBox, sbmlBBox );
   }
 }

 createSpeciesGlyphs() {
   var i;
   for( i=0; i < this.modelLayout.getNumSpGlyphs(); i++ ) {
     const curSpGlyph = this.modelLayout.getSpGlyph(i);
     const sbmlSpGlyph = this.libSBMLlayout.createSpeciesGlyph();
     sbmlSpGlyph.setId( curSpGlyph.getId() );
     sbmlSpGlyph.setSpeciesId( curSpGlyph.getSpeciesId() );
     const curBBox = curSpGlyph.getBoundingBox();
     const sbmlBBox = sbmlSpGlyph.getBoundingBox(); // pointer to speciesGlyph bounding box
     const sbmlBBDims = sbmlBBox.getDimensions();   // pointer to speciesGlyph bounding box dims
     sbmlBBDims.setWidth(curBBox.getDims().getWidth());
     sbmlBBDims.setHeight(curBBox.getDims().getHeight());
     console.log('BBox, width x height: ', sbmlBBox.getDimensions().getWidth(),sbmlBBox.getDimensions().getHeight() );
  //   sbmlBBox.setDimensions(sbmlBBDims);   // not needed
     sbmlBBox.setX(curBBox.getPoint().getX()); // sets x coord in speciesglyph bbox center.
     sbmlBBox.setY(curBBox.getPoint().getY()); // assume no Z
  //   sbmlSpGlyph.setBoundingBox(sbmlBBox);   // <-- Not needed

   }
 }

 createReactionGlyphs() {
   var i;
   for( i=0; i < this.modelLayout.getNumRxnGlyphs(); i++ ) {
     const curRxnGlyph = this.modelLayout.getRxnGlyph(i);
     const sbmlRxnGlyph = this.libSBMLlayout.createReactionGlyph();
     sbmlRxnGlyph.setReactionId( curRxnGlyph.getReactionId() );
     var j;
     for( j=0; j < curRxnGlyph.getNumSpeciesRefGlyphs(); j++ ) {
       const curSpRefGlyph = curRxnGlyph.getSpeciesRefGlyph(j);
       const sbmlSpRefGlyph = this.libSBMLlayout.createSpeciesReferenceGlyph();
       sbmlSpRefGlyph.setSpeciesGlyphId( curSpRefGlyph.getSpeciesGlyphId() );
       sbmlSpRefGlyph.setSpeciesReferenceId( curSpRefGlyph.getSpeciesRefId() );
       sbmlSpRefGlyph.setId( curSpRefGlyph.getId() );
       // TODO: set role

       if( curSpRefGlyph.isCurveSet() ) {
         const curCurve = curSpRefGlyph.getCurve;
         if( curCurve.getNumCubicBeziers() >0 ) {
           var k;
           for( k=0; k < curCurve.getNumCubicBeziers(); k++ ) {
             const sbmlCubicBez = sbmlSpRefGlyph.createCubicBezier();
             this.processCubicBezier( curCurve.getCubicBezier(k), sbmlCubicBez );
           }
         }
         if( curCurve.getNumCurveSegments() >0 ) {
           var k;
           for( k=0; k < curve.getNumCurveSegments(); k++ ) {
             const sbmlLineSeg = sbmlSpRefGlyph.createLineSegment();
             this.processLineSeg( curCurve.getLineSegment(k), sbmlLineSeg );
           }
         }
       }
       // Set bounding box for sbmlSpRefGlyph:
       // chk if bounding box
       if( curSpRefGlyph.boundingBoxIsSet() )  {
         const curBBox = curSpRefGlyph.getBoundingBox();
         const sbmlBBox = sbmlSpRefGlyph.getBoundingBox(); // pointer to speciesRefGlyph bounding box
         this.processBoundingBox( curBBox, sbmlBBox );
       }
     } // getNumSpeciesRefGlyphs
     if( curRxnGlyph.isCurveSet() ) {
       const curCurve = curRxnGlyph.getCurve();
       if( curCurve.getNumCubicBeziers() >0 ) {
         var k;
         for( k=0; k < curCurve.getNumCubicBeziers(); k++ ) {
           const sbmlCubicBez = sbmlRxnGlyph.createCubicBezier();
           this.processCubicBezier( curCurve.getCubicBezier(k), sbmlCubicBez );
         }
       }
       if( curCurve.getNumCurveSegments() >0 ) {
         var k;
         for( k=0; k < curve.getNumCurveSegments(); k++ ) {
           const sbmlLineSeg = sbmlRxnGlyph.createLineSegment();
           this.processLineSeg( curCurve.getLineSegment(k), sbmlLineSeg );
         }
       }
     }

   }  //getNumRxnGlyphs()
 } // createReactionGlyphs()

 createTextGlyphs() {
   var i;
   for( i=0; i < this.modelLayout.getNumTextGlyphs(); i++ ) {
     const curTextGlyph = this.modelLayout.getTextGlyph(i);
     const sbmlTextGlyph = this.libSBMLlayout.createTextGlyph();
     sbmlTextGlyph.setText( curTextGlyph.getText() );
     sbmlTextGlyph.setOriginOfTextId( curTextGlyph.getOriginOfText() );
     sbmlTextGlyph.setGraphicalObjectId( curTextGlyph.getOriginOfText() ); // same as OriginOfText for now.
     sbmlTextGlyph.setId( curTextGlyph.getId() );
     const curBBox = curTextGlyph.getBoundingBox();
     const sbmlBBox = sbmlTextGlyph.getBoundingBox();
     this.processBoundingBox( curBBox, sbmlBBox );
   }
 }

 createAdditionalGraphicalObjects() {
   var i;
   for( i=0; i < this.modelLayout.getNumAddionalGraphObjs(); i++ ) {
     const curAddGraphObj = this.modelLayout.getAdditionalGraphObj(i);
     const sbmlAddGraphObj = this.libSBMLlayout.createAdditionalGraphicalObject();
     sbmlAddGraphObj.setId( curAddGraphObj.getId() );
     const curBBox = curAddGraphObj.getBoundingBox();
     const sbmlBBox = sbmlAddGraphObj.getBoundingBox();
     this.processBoundingBox( curBBox, sbmlBBox );
   }
 }

 createGeneralGlyphs() {
   var i;
   for( i=0; i < this.modelLayout.getNumGenGlyphs(); i++ ) {
     const curGenGlyph = this.modelLayout.getGenGlyph(i);
     const sbmlGenGlyph = this.libSBMLlayout.createGeneralGlyph();
     sbmlGenGlyph.setId( curGenGlyph.getId() );
     const curBBox = curGenGlyph.getBoundingBox();
     const sbmlBBox = sbmlGenGlyph.getBoundingBox();
     this.processBoundingBox( curBBox, sbmlBBox );
   }
 }

}