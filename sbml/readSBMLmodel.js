


 // Class to convert libSBML model to TModel format.
 //   libSBML is loaded module from libsbml.js

class ProcessSBML {
 constructor( libSBMLModel, curLibsbml ){
   this.libSBML = curLibsbml;
   this.model = libSBMLModel; // Model from libSBML doc.getModel()
   this.isLocalRenderSet = false;
   this.isGlobalRenderSet = false;
   this.glyphIds = new Array(); // needed for render styles
   this.specIds = new Array();  // needed for render styles, coyote uses this
   this.spRefRoles = ['substrate', 'product', 'sidesubstrate', 'sideproduct', 'modifier',
                      'activator', 'inhibitor', 'undefined'];
   this.glyphTypes = ['COMPARTMENTGLYPH', 'SPECIESGLYPH', 'REACTIONGLYPH',
     'SPECIESREFERENCEGLYPH', 'TEXTGLYPH', 'GENERALGLYPH', 'GRAPHICALOBJECT', 'ANY'];
   console.log(' # ofplugins: ', this.model.getNumPlugins() );
   if(this.model.getNumPlugins() >0) {
     this.SBMLLayOut = this.model.findPlugin('layout');
     this.SBMLGlobalRender = libsbml.RenderExtension.prototype.getXmlnsL3V1V1()
    // if(this.model.hasPlugin('render')) {
    //   console.log(' model has render plugin' );
    // }
     if(this.model.hasPlugin('layout')) {
       console.log(' model has layout plugin' );
     }
     if(this.SBMLLayOut == undefined) {
       console.log('No layout plugin defined');
     }
     else {
       this.newLayoutPlug = this.SBMLLayOut.asLayout();
       this.numLayouts = this.newLayoutPlug.getNumLayouts(); // why does it report 2 if only one?

     }
   }
 }

 getNumbers(tModela) { //  class TModel
   tModela.numSpecies = this.model.getNumSpecies();
   tModela.numParams = this.model.getNumParameters();
   tModela.numCompartments = this.model.getNumCompartments();
   const numInitAssignments = this.model.getNumInitialAssignments(); // TODO: Need to get these
   console.log(' Number of Init assignments: ',numInitAssignments);
   tModela.numEvents = this.model.getNumEvents();
   console.log( 'Number of events: ', this.model.getNumEvents() );
   tModela.numRules = this.model.getNumRules();
   tModela.numbPlugins = this.model.getNumPlugins();
   console.log('Number of plugins: ', this.model.getNumPlugins());

   tModela.numFuncDefs = this.model.getNumFunctionDefinitions(); // TODO: store these?
   tModela.annotationStr = this.model.getNotesString();
   return tModela;
  }

getInitialAssignments(tModela, tInitAssign) {
  for( var i=0; i < this.model.getNumInitialAssignments(); i++ ) {
    const newAssign = this.model.getInitialAssignment(i);
    if (newAssign.isSetIdAttribute()) {
      tInitAssign.setId(newAssign.getId()); }
    else { tInitAssign.setId('InitAssign_' + newAssign.getSymbol()); }
    if( newAssign.isSetMath()) {
      const astMath = newAssign.getMath();
      tInitAssign.setFormula( new this.libSBML.SBMLFormulaParser().formulaToL3String(astMath));}
    else { tInitAssign.setFormula(''); }
    tInitAssign.setSymbol( newAssign.getSymbol() );
    tModela.addInitialAssignment( tInitAssign );
  }
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
         //console.log('getRules:  Formula for Rule: ', tRule.getFormula());
           if(rule.getFormula().includes("piecewise")) {
             tRule.setPiecewise(true);
           }
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
      this.specIds.push(newSpecies.getId()); // for render styles
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
      if (newSpecies.isSetHasOnlySubstanceUnits()) {
        tSpecies.setHasOnlySubstanceUnits(newSpecies.getHasOnlySubstanceUnits()); }
      tModela.addSBMLspecies(tSpecies); // Add new species to array of all species used in model.
    }
    return tModela;
  }

  getFuncDefs(tModela, tFuncDef) {
    var i;
    const numFuncs = this.model.getNumFunctionDefinitions();
    if( numFuncs >0 ){
      for( i=0; i< numFuncs; i++) {
        tFuncDef.clear();
        const newFuncDef = this.model.getFunctionDefinition(i);
        if( newFuncDef.isSetIdAttribute()) {
        //  console.log('func Def: ', newFuncDef.getId());
          tFuncDef.setId(newFuncDef.getId());
        }
        if( newFuncDef.isSetName() ) {
          tFuncDef.setName(newFuncDef.getName());
        }
        if( newFuncDef.isSetBody() ) {
          const newBody = newFuncDef.getBody();
       //   const strBody = new this.libSBML.SBMLFormulaParser().formulaToL3String(newBody);  // exponent is represented as x^2
          const strBody = new this.libSBML.SBMLFormulaParser().formulaToString(newBody); // exponent is represented as pow(x,2)
          tFuncDef.setFuncFormula(strBody.trim());
        }
        const newMath = newFuncDef.getMath();
        //console.log(' func Math: ', newMath.getType() );
        const strMath = new this.libSBML.SBMLFormulaParser().formulaToL3String(newMath);
      //  console.log('** Func def Math: ', strMath.trim());
        var fullFuncLabel = tFuncDef.getId() + '('; // func name with comma sep vars in parenthesis
        let varArr = strMath.split(',');
        const numVars = varArr.length -1; // last element is formula
        const elementZeroArr = varArr[0].split('lambda('); // assume format lambda(var1, var2, ...
        if( elementZeroArr.length > 1) {
         varArr[0] = elementZeroArr[1]; // discard first element: 'lambda('
        }
        for( var j=0; j< numVars; j++) {
          tFuncDef.addVar(varArr[j].trim());
          if( j == 0 ) { fullFuncLabel = fullFuncLabel + varArr[j].trim(); }
          else { fullFuncLabel = fullFuncLabel + ', ' + varArr[j].trim(); }
         // console.log(' var ',j,': ',varArr[j].trim());
        }
        fullFuncLabel = fullFuncLabel + ')';
        tFuncDef.setFullFuncLabel( fullFuncLabel );
        tModela.addFuncDef( tFuncDef );
       // console.log(' Func eq: ', strMath);
      }
    }
    //else { console.log('No SBML Func Defs!');}
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
        else { reactStoich[j] = 1; }  // default for reactant
      }
      numTotalSpecies = 0;
      numTotalSpecies = this.model.getReaction(i).getNumProducts();
      for (j=0; j < numTotalSpecies; j++) {
        products[j] = this.model.getReaction(i).getProduct(j).getSpecies();
        if( this.model.getReaction(i).getProduct(j).isSetStoichiometry() ) {
          prodStoich[j] = this.model.getReaction(i).getProduct(j).getStoichiometry();
        }
        else { prodStoich[j] = 1; }
      }
      var kineticForm = this.model.getReaction(i).getKineticLaw().getFormula();
      var rxnReversible;
      if (this.model.getReaction(i).isSetReversible()) {
        rxnReversible = this.model.getReaction(i).getReversible(); }
      else { rxnReversible = true; } // level 2 default is true
      // Note: we do not check for localParameters to add to kinetic law.
      // Assume reaction only uses global parameters.
      // this.model.getReaction(i).getKineticLaw().getNumLocalParameters();
   //console.log('--> kinetic law: ',this.model.getReaction(i).getKineticLaw().getFormula());
      tModela.addSBMLReaction(this.model.getReaction(i).getId(), products, prodStoich,
                              reactants, reactStoich, kineticForm, rxnReversible);
    }
    return tModela;
  }

    // tLayout: TSBMLLayout,   assume only one layout for now.
    // tDims: TSBMLLayoutDimensions, tPt: TSBMLLayoutPoint, etc...
    // create these blank objects ahead of time, so do not have to create them here.
  getLayout(tLayout, tDims, tPt, tBBox,
            tLineSeg, tCubicB, tCurve, tCompGlyph, tGraphObj, tGenObj,
            tSpGlyph, tSpRefGlyph, tRxnGlyph, tTextGlyph) {
    var i;
  
    for(i=0; i< this.numLayouts; i++) {
   // for(i=0; i< 1; i++) { // cap at one for now, numLayouts reports 2 when there is one?
    //  console.log(' Getting next layout #: ', i);
      const aLayout = this.newLayoutPlug.getLayout(i);
      const rPlugin = this.libSBML.castObject(aLayout.getPlugin("render"), this.libSBML.RenderLayoutPlugin);


      const numLocalRenderPlug = rPlugin.getNumLocalRenderInformationObjects();
      if( numLocalRenderPlug > 0 ) {
        this.localRenderInfo = rPlugin.getRenderInformation(numLocalRenderPlug-1); // works for localInfo, not GlobalInfo
      this.isLocalRenderSet = true;
      }
    // global?: NO ...
//    const rGlobalPluginList = this.libSBML.castObject(aLayout.getPlugin("render"), this.libSBML.RenderListOfLayoutsPlugin);   // none found
//    const numGlobalInfo = rGlobalRenderList.getRenderInformation();
//    const numGlobalObj = rGlobalRenderList.getNumGlobalRenderInformationObjects();
 //   if( numGlobalObj > 0 ) {
 //     this.globalRenderInfo = rGlobalRenderList.getRenderGlobalInformation(numGlobalObj -1 );
 //   }

      const dims = aLayout.getDimensions()
      tDims = this.assignDims(tDims, dims);
      tLayout.setDims(tDims); // assume NO depth.
      tLayout = this.getCompGlyphs(tLayout, aLayout, tDims, tPt, tBBox, tCompGlyph );
      tLayout = this.getSpeciesGlyphs(tLayout, aLayout, tDims, tPt, tBBox, tSpGlyph);
      tLayout = this.getRxnGlyphs(tLayout, aLayout, tDims, tPt, tBBox, tLineSeg,
                    tCubicB, tCurve, tSpRefGlyph, tRxnGlyph);
      tLayout = this.getTextGlyphs(tLayout, aLayout, tDims, tPt, tBBox, tGraphObj, tTextGlyph )
    }

    return tLayout;
  }

   assignDims(tDims, readDims)
   {
     if(readDims.isSetIdAttribute()) {tDims.setId(readDims.getId());}
     else {tDims.setId('');}
     tDims.setWidth(readDims.getWidth());
     tDims.setHeight(readDims.getHeight());
     if(readDims.getDepth() == undefined){ tDims.setDepth(0);}
     else { tDims.setDepth(readDims.getDepth()); }
     return tDims;
   }

   assignPoint( tPt, readPt)
   {
     if(readPt.isSetIdAttribute()){ tPt.setId(readPt.getId());}
     else {tPt.setId('');}
     tPt.setX(readPt.x());
     tPt.setY(readPt.y());
     if(readPt.z() == undefined) {
       tPt.unSetZFlag(); tPt.setZ(-1);
     }
     else {tPt.setZ(readPt.z());}
     tPt.setZ(readPt.z());
     return tPt;
   }

   assignBBox(tBBox, readBBox, tDims, tPt)
   {
   //  console.log('ProcessSBML: assignBBox(), tBBox: ', tBBox );
     tBBox.setId(readBBox.getId());
     tDims.setWidth(0);
     tDims.setHeight(0);
     tDims.setDepth(0);
     tDims = this.assignDims(tDims, readBBox.getDimensions()); //readBBox is libsbmljs object
     tBBox.setDims(tDims);
     tBBox.setPoint(this.assignPoint(tPt, readBBox.getPosition()));
     return tBBox;
   }

   assignLineSegment(tLineSeg, readLineSeg, tPt)
   {
     if(readLineSeg.isSetIdAttribute()) { tLineSeg.setId(readLineSeg.getId()); }
     else { tLineSeg.setId(''); }
     tPt = this.assignPoint(tPt, readLineSeg.getStart());
     tLineSeg.setStartPt(tPt);
     tPt = this.assignPoint(tPt, readLineSeg.getEnd());
     tLineSeg.setEndPt(tPt);
     return tLineSeg;
   }

   assignCubicB(tCubicB, readCubicB, tPt)
   {
     if(readCubicB.isSetIdAttribute()) { tCubicB.setId(readCubicB.getId()); }
     else { tCubicB.setId(''); }
     tPt = this.assignPoint(tPt, readCubicB.getBasePoint1());
     tCubicB.setBasePt1(this.assignPoint(tPt, readCubicB.getBasePoint1()));
     tPt = this.assignPoint(tPt, readCubicB.getBasePoint2());
     tCubicB.setBasePt2(this.assignPoint(tPt, readCubicB.getBasePoint2()));
     tPt = this.assignPoint(tPt, readCubicB.getStart());
     tCubicB.setStart(tPt);
     tPt = this.assignPoint(tPt, readCubicB.getEnd());
     tCubicB.setEnd(this.assignPoint(tPt, readCubicB.getEnd()));
     return tCubicB;
   }

   getCompGlyphs(nLayout, sbmlLayout, sbmlDims, sbmlPt, sbmlBBox, sbmlCompGlyph )
   {
     var i;
     for(i=0; i< sbmlLayout.getNumCompartmentGlyphs(); i++) {
       const curComp = sbmlLayout.getCompartmentGlyph(i);
       sbmlCompGlyph.setId(curComp.getId());
       this.glyphIds.push(curComp.getId());
       if( curComp.isSetCompartmentId()) {
         sbmlCompGlyph.setCompId(curComp.getCompartmentId());
       }
       else { sbmlCompGlyph.setCompId('');}
       if( curComp.isSetOrder()) {
         sbmlCompGlyph.setOrder(curComp.getOrder());
       }
    //   console.log('getCompGlyphs, assignBBox() call.');
       sbmlBBox = this.assignBBox(sbmlBBox, curComp.getBoundingBox(),sbmlDims, sbmlPt);
       sbmlCompGlyph.setBoundingBox(sbmlBBox);
       nLayout.addCompGlyph(sbmlCompGlyph);
     }
     return nLayout;
   }

   getSpeciesGlyphs(nLayout, sbmlLayout, sbmlDims, sbmlPt, sbmlBBox, sbmlSpGlyph)
   {
     var i;
     for(i = 0; i< sbmlLayout.getNumSpeciesGlyphs(); i++) {
       const curSpGlyph = sbmlLayout.getSpeciesGlyph(i);
       sbmlSpGlyph.setId(curSpGlyph.getId());
       this.glyphIds.push(curSpGlyph.getId());
       if(curSpGlyph.isSetSpeciesId()) {
         sbmlSpGlyph.setSpeciesId(curSpGlyph.getSpeciesId());
       }
       else { sbmlSpGlyph.setSpeciesId('');}
     //  console.log('getSpeciesGlyphs, assignBBox() call.');
       sbmlBBox = this.assignBBox(sbmlBBox, curSpGlyph.getBoundingBox(),sbmlDims, sbmlPt);
       sbmlSpGlyph.setBoundingBox(sbmlBBox);
       nLayout.addSpGlyph(sbmlSpGlyph);
     }
     return nLayout;
   }

   getRxnGlyphs(nLayout, readLayout, sbmlDims, sbmlPt, sbmlBBox, sbmlLineSeg,
                    sbmlCubicB, sbmlCurve, sbmlSpRefGlyph, sbmlRxnGlyph)
   {
     var i, j, k;
     for(i=0; i< readLayout.getNumReactionGlyphs(); i++)
     {
       const curRxnGlyph = readLayout.getReactionGlyph(i);
       sbmlRxnGlyph.clear();
       if( curRxnGlyph.isSetIdAttribute() ) {
         sbmlRxnGlyph.setId(curRxnGlyph.getId());
         this.glyphIds.push(curRxnGlyph.getId());
       }
       if(curRxnGlyph.isSetReactionId()) {sbmlRxnGlyph.setReactionId(curRxnGlyph.getReactionId()); }
       else { sbmlRxnGlyph.setReactionId(''); }
       // Grab reaction curve if exists.....
       if(curRxnGlyph.isSetCurve()) {
         const curCurve = curRxnGlyph.getCurve();
         if(curCurve.isSetIdAttribute()) { sbmlCurve.setCurveId(curCurve.getId()); }
         else { sbmlCurve.setCurveId(''); }
     //    console.log(' curve segments: ', curCurve.getNumCurveSegments());
         for(k=0; k< curCurve.getNumCurveSegments(); k++) {
           const curCurveSeg = curCurve.getCurveSegment(k);
           if(curCurveSeg.isCubicBezier()){
       //      console.log('Bezier curve ** ');
             sbmlCubicB = this.assignCubicB(sbmlCubicB, curCurveSeg.asCubicBezier(), sbmlPt);
             sbmlCurve.addCubicBezier(sbmlCubicB);
           }
           else {  // line segment:
             sbmlLineSeg = this.assignLineSegment(sbmlLineSeg, curCurveSeg, sbmlPt);
             sbmlCurve.addLineSegment(sbmlLineSeg);
           }
         }
         sbmlRxnGlyph.setCurve(sbmlCurve);
       }

       for(j=0; j< curRxnGlyph.getNumSpeciesReferenceGlyphs(); j++)
       {
         sbmlSpRefGlyph.clear();
         sbmlCurve.clear();
         const curSpRefGlyph = curRxnGlyph.getSpeciesReferenceGlyph(j);
         if( curSpRefGlyph.isSetIdAttribute() ) {
           sbmlSpRefGlyph.setId(curSpRefGlyph.getId());
           this.glyphIds.push(curSpRefGlyph.getId());
         }
         if(curSpRefGlyph.isSetSpeciesGlyphId()) {
           sbmlSpRefGlyph.setSpeciesGlyphId(curSpRefGlyph.getSpeciesGlyphId()); }
         else { curSpRefGlyph.setSpeciesGlyphId(''); }
         if(curSpRefGlyph.isSetSpeciesReferenceId()) {
           sbmlSpRefGlyph.setSpeciesRefId(curSpRefGlyph.getSpeciesReferenceId()); }
         else { sbmlSpRefGlyph.setSpeciesRefId('');}
         if(curSpRefGlyph.isSetRole() ){
           sbmlSpRefGlyph.setRole(curSpRefGlyph.getRoleString()); }
         else {sbmlSpRefGlyph.setRole('undefined'); }
         // If the curve is specified, it overrides the inherited bounding box:
         if(curSpRefGlyph.isSetCurve()) {
           const curCurve = curSpRefGlyph.getCurve();
           if(curCurve.isSetIdAttribute()) { sbmlCurve.setCurveId(curCurve.getId()); }
           else { sbmlCurve.setCurveId(''); }
     //    console.log(' curve segments: ', curCurve.getNumCurveSegments());
           for(k=0; k< curCurve.getNumCurveSegments(); k++) {
             const curCurveSeg = curCurve.getCurveSegment(k);
             if(curCurveSeg.isCubicBezier()){
        //     console.log('Bezier curve ** ');
               sbmlCubicB = this.assignCubicB(sbmlCubicB, curCurveSeg.asCubicBezier(), sbmlPt);
               sbmlCurve.addCubicBezier(sbmlCubicB);
             }
             else {  // line segment:
               sbmlLineSeg = this.assignLineSegment(sbmlLineSeg, curCurveSeg, sbmlPt);
               sbmlCurve.addLineSegment(sbmlLineSeg);
             }
           }
           sbmlSpRefGlyph.setCurve(sbmlCurve);
         }
         //else { sbmlSpRefGlyph.
         if(curSpRefGlyph.getBoundingBox() != null ) {
    //     console.log('spRefGlyphs, assignBBox() call.');
           sbmlBBox = this.assignBBox(sbmlBBox,curSpRefGlyph.getBoundingBox(),sbmlDims, sbmlPt);
           sbmlSpRefGlyph.setBoundingBox(sbmlBBox);
         }
         sbmlRxnGlyph.addSpeciesRefGlyph(sbmlSpRefGlyph);
       }

       nLayout.addRxnGlyph(sbmlRxnGlyph);
      // sbmlRxnGlyph.clear();
      console.log('end of getRxnGlyph');
     }

     return nLayout;
   }

   getTextGlyphs(nLayout, readLayout, sbmlDims, sbmlPt, sbmlBBox, sbmlGraphObj, sbmlTextGlyph )
   {
     var i;

     for( i=0; i< readLayout.getNumTextGlyphs(); i++)
     {
       const curTextGlyph = readLayout.getTextGlyph(i);
       if(curTextGlyph.isSetIdAttribute()) {
         sbmlTextGlyph.setId(curTextGlyph.getId());
         this.glyphIds.push(curTextGlyph.getId());
       }
       else { sbmlTextGlyph.setId(''); }
       if(curTextGlyph.isSetText()) {
         sbmlTextGlyph.setText(curTextGlyph.getText());
       }
       else { sbmlTextGlyph.setText(''); }
       if( curTextGlyph.isSetOriginOfTextId() ) {
         sbmlTextGlyph.setOriginOfText(curTextGlyph.getOriginOfTextId());
       }
       else { sbmlTextGlyph.setOriginOfText(''); }
       if(curTextGlyph.isSetGraphicalObjectId()) {
         sbmlTextGlyph.setGraphicalObjId( curTextGlyph.getGraphicalObjectId() );
    //   console.log('getTextGlyphs, assignBBox() call.');
         sbmlBBox = this.assignBBox(sbmlBBox, curTextGlyph.getBoundingBox(), sbmlDims, sbmlPt);
         sbmlTextGlyph.setBoundingBox(sbmlBBox);
       }
       nLayout.addTextGlyph(sbmlTextGlyph);
       sbmlTextGlyph.clear();

     }

     return nLayout;
   }
    // TODO: deal with global render info..
   getSBMLRenderInformation( tRenderInfo, tRenderStyle, tLineEnding, tRenderGroup,
         tEllipse, tRectangle, tPolygon, tRenderPt, tRender1D, tColorDef, tBBox,
         tDims, tPt )
   {
     if( this.localRenderInfo.isSetId() ) {
       tRenderInfo.setId( this.localRenderInfo.getId() );
     }
     tRenderInfo = this.getColorDefs( tRenderInfo, tColorDef );
     tRenderInfo = this.getLineEndings(tRenderInfo, tLineEnding, tRenderGroup, tBBox,
                                  tPolygon, tRectangle, tRenderPt, tDims, tPt );
     for( var i=0; i< this.localRenderInfo.getNumStyles(); i++ ) {
       const newRenderStyle = this.localRenderInfo.getStyle(i);
       tRenderStyle = this.getStyle( newRenderStyle,tRenderStyle, tRenderGroup,
                    tPolygon, tRectangle, tEllipse, tBBox, tRenderPt, tDims, tPt );
       tRenderInfo.addStyle(tRenderStyle);

     }

     return tRenderInfo;
   }

   getColorDefs( nRenderInfo, tColorDef ) {
     var i;
     var numbObjs = 0;
     if( this.isLocalRenderSet ) {

       numbObjs = this.localRenderInfo.getNumColorDefinitions();
     } //else this.global
     for( i=0; i< numbObjs; i++ ) {
       const newColorDef = this.localRenderInfo.getColorDefinition(i);
       if( newColorDef.isSetId() ) { tColorDef.setId( newColorDef.getId() ); }
       const red =  newColorDef.getRed(); // int
       const blue = newColorDef.getBlue();
       const green = newColorDef.getGreen();
       const alpha = newColorDef.getAlpha();
       tColorDef.setValueRGBAInts( red, green, blue, alpha );
       nRenderInfo.addColorDef( tColorDef );
       tColorDef.clear();
     }

     return nRenderInfo;
   }

   assignRenderGroup(nRenderGroup, sbmlRenderGroup, tPolygon, tRectangle, tRenderPt )
   {
     nRenderGroup.clear();
     if( sbmlRenderGroup.isSetStrokeWidth() ) {
       nRenderGroup.setStrokeWidth( sbmlRenderGroup.getStrokeWidth() );
     }
     if( sbmlRenderGroup.isSetStroke() ) {
       nRenderGroup.setStrokeColor( sbmlRenderGroup.getStroke() );
     }
     if( sbmlRenderGroup.isSetFillColor() ) {
       nRenderGroup.setFillColor( sbmlRenderGroup.getFillColor() );
     }
  
     const sbmlFontSize = sbmlRenderGroup.getFontSize(); // RelAbsVector
     var fontSz = sbmlFontSize.getAbsoluteValue();
     if( fontSz == 0 ){
       fontSz = sbmlFontSize.getRelativeValue(); }
     nRenderGroup.setFontSize( fontSz );
     const fontStyle = sbmlRenderGroup.getFontStyle();// integer : 1='normal', 2='italic'
     if( fontStyle == 2 ){
       nRenderGroup.setFontStyle('italic'); }
     else { nRenderGroup.setFontStyle('normal');}

     if( sbmlRenderGroup.isSetTextAnchor() )
     { nRenderGroup.setHTextAnchor( sbmlRenderGroup.getTextAnchor() ) ; }
     if( sbmlRenderGroup.isSetVTextAnchor() ) {
       nRenderGroup.setVTextAnchor( sbmlRenderGroup.getVTextAnchor() ) ; }

     if( sbmlRenderGroup.isSetEndHead() ) {
       nRenderGroup.setEndHead( sbmlRenderGroup.getEndHead() ); }
     if( sbmlRenderGroup.isSetStartHead() ) {
       nRenderGroup.setStartHead( sbmlRenderGroup.getStartHead() ); }

      const numElements = sbmlRenderGroup.getNumElements(); // not necessary/used?
      const newPoly = sbmlRenderGroup.polygons;
      //if(newPoly.length > 0) {
      for( var i = 0; i < newPoly.length ; i++ ){  // assume only one polygon ber render group.
        tPolygon = this.getPolygon(tPolygon, newPoly[i], tRenderPt);
        console.log('Polygons present in Render group');
        nRenderGroup.setPolygon(tPolygon);
      }
      const newRects = sbmlRenderGroup.rectangles;
      if(newRects > 0) {
        console.log(' Rectangles present in Render group' );
      }
      const newEllipses = sbmlRenderGroup.ellipses;
      if(newEllipses >0) {
        console.log(' Ellipses present in Render group' );
      }
      const newCurves = sbmlRenderGroup.curves;
      if(newCurves >0) {
        console.log(' Curves present in Render group' );
      }
      const newTexts = sbmlRenderGroup.texts;
      if(newTexts >0) {
        console.log(' Text present in Render group' );
      }


     return nRenderGroup;
   }

   getLineEndings(nRenderInfo, tLineE, tRenderGroup, tBBox, tPolygon,
                  tRectangle, tRenderPt, tDims, tPt )
   {
     var numObjs;
     if( this.isLocalRenderSet ) {
       numObjs = this.localRenderInfo.getNumLineEndings();
     }
     else { numObjs = this.globalRenderInfo.getNumLineEndings(); }
     for( let i=0; i< numObjs; i++ ) {
       var newLineEnd;// = this.localRenderInfo.getLineEnding(i);
       newLineEnd = this.localRenderInfo.getLineEnding(i);
       tLineE.setId(newLineEnd.getId());
       tLineE.setRotationalMapping(newLineEnd.getIsEnabledRotationalMapping());
       tBBox = this.assignBBox(tBBox, newLineEnd.getBoundingBox(), tDims, tPt);
       tLineE.setBoundingBox( tBBox );
       const newRenderGroup = newLineEnd.getGroup();
       tRenderGroup = this.assignRenderGroup(tRenderGroup, newRenderGroup,
                          tPolygon, tRectangle, tRenderPt );
       tLineE.setRenderGroup( tRenderGroup );
       console.log('tLineE- id: ',tLineE.getId() );
     //   nRenderInfo.setRenderGroup( tlineEnding );
    //   nRenderInfo.addLineEnding( tlineE ); //ReferenceError: tlineE is not defined
      // tRenderGroup.clear(); // needed?
     }
     return nRenderInfo;
   }

   getStyle( sbmlRenderStyle,nRenderStyle, tRenderGroup, tPolygon, tRectangle, tEllipse,
    tBBox, tRenderPt, tDims, tPt )
   {
     nRenderStyle.clear();
     if( sbmlRenderStyle.isSetId() ) { nRenderStyle.setId(sbmlRenderStyle.getId()); }
     for( let i =0; i < sbmlRenderStyle.getNumTypes(); i++ ) {
       for( let j=0; j < this.glyphTypes.length; j ++) {
         if(sbmlRenderStyle.isInTypeList(this.glyphTypes[j])) {
           nRenderStyle.addType(this.glyphTypes[j]);
         }
       }
     }
     for( let i=0; i < sbmlRenderStyle.getNumRoles(); i++ ) {
       for( let j=0; j < this.spRefRoles.length; j ++) {
         if(sbmlRenderStyle.isInRoleList(this.spRefRoles[j])) {
           nRenderStyle.addRole(this.spRefRoles[j]);
         }
       }
     }      // check glyph Ids:
     for( let i=0; i< sbmlRenderStyle.getNumIds(); i++ ) {
       for( let j=0; j < this.glyphIds.length; j++ ) {
         if(sbmlRenderStyle.isInIdList( this.glyphIds[j] )) {
           nRenderStyle.addGoId( this.glyphIds[j] );
         }
       }
     }
          // check if IdList contains species node Id as well:
     for( let i=0; i< sbmlRenderStyle.getNumIds(); i++ ) {
       for( let j=0; j < this.specIds.length; j++ ) {
         if(sbmlRenderStyle.isInIdList( this.specIds[j] )) {
           nRenderStyle.addGoId( this.specIds[j] );
         }
       }
     }

     const sbmlRGroup = sbmlRenderStyle.getGroup();
     tRenderGroup = this.assignRenderGroup(tRenderGroup, sbmlRGroup,tPolygon, tRectangle, tRenderPt );
     nRenderStyle.setRenderGroup( tRenderGroup );
     return nRenderStyle;
   }

   getGradientDefs(nRenderInfo ) {
      // TODO
     return nRenderInfo;
   }

   getEllipse( tEllipse, sbmlEllipse )
   {

     return tEllipse;
   }

   getRectangle( tRect, sbmlRect )
   {

     return tRect;
   }

   getPolygon( tPoly, sbmlPoly, tPt )
   {
     tPoly.clear();

     for( let i=0; i < sbmlPoly.getNumElements(); i++ ) {
       const newRPt = sbmlPoly.getElement(i);
       const elementName = newRPt.getElementName();
       const rAVectX = newRPt.x();
       const rAVectY = newRPt.y();
       // Assume no Z coord
       // if( newRPt.isSetZ() ) { }

       if( rAVectX.getAbsoluteValue() > 0 ) {
         tPt.setX( rAVectX.getAbsoluteValue() );
         tPt.setRelCoordinate(false);
       }
       else {
         tPt.setX( rAVectX.getRelativeValue() );
         tPt.setRelCoordinate(true);
       }

       if( rAVectY.getAbsoluteValue() > 0 ) {
         tPt.setY( rAVectY.getAbsoluteValue() );
         tPt.setRelCoordinate(false);
       }
       else {
         tPt.setY( rAVectY.getRelativeValue() );
         //tPt.setRelCoordinate(true);
       }
       tPoly.addPt(tPt);
     }

     return tPoly;
   }

 }