unit uSBMLReader;

interface
uses Web, JS,  uSBMLClasses, uSBMLClasses.rule, uModel, uSBMLClasses.Layout,
 uSBMLClasses.Render;

type
  TSBMLLoadedEvent = procedure(model: TModel) of object;  // Notify when done loading

  TSBMLRead = class
  private
    FNotify: TSBMLLoadedEvent; // send sbml info to listener once asynchronous read done.

  public
    constructor create(newModel: TModel; SBMLtext: String);
    property OnSBMLLoaded: TSBMLLoadedEvent read FNotify write FNotify;
  end;


implementation
  constructor TSBMLRead.create(newModel: TModel; SBMLtext: String);
  var
    newSpecies: TSBMLSpecies;
    jsSpecies: TJSObject;
    newComp: TSBMLcompartment;
    jsComp: TJSObject;
    newParam: TSBMLparameter;
    jsParam: TJSObject;
    newRule: TSBMLRule;
    jsRule: TJSObject;
    newLayout: TSBMLLayout; jsLayout: TJSObject;
    newDims: TSBMLLayoutDims; jsDims: TJSObject;
    newPt: TSBMLLayoutPoint; jsPt: TJSObject;
    newBBox: TSBMLLayoutBoundingBox; jsBBox: TJSObject;
    newLineSeg: TSBMLLayoutLineSegment;  jsLineSeg: TJSObject;
    newCubicB: TSBMLLayoutCubicBezier; jsCubicB: TJSObject;
    newCurve: TSBMLLayoutCurve; jsCurve: TJSObject;
    newGraphObj: TSBMLLayoutGraphicalObject; jsGraphObj: TJSObject;
    newGenObj: TSBMLLayoutGeneralGlyph; jsGenObj: TJSObject;
    newCompGlyph: TSBMLLayoutCompartmentGlyph; jsCompGlyph: TJSObject;
    newSpGlyph: TSBMLLayoutSpeciesGlyph; jsSpGlyph: TJSObject;
    newSpRefGlyph: TSBMLLayoutSpeciesReferenceGlyph; jsSpRefGlyph: TJSObject;
    newRxnGlyph: TSBMLLayoutReactionGlyph; jsRxnGlyph: TJSObject;
    newTextGlyph: TSBMLLayoutTextGlyph; jsTextGlyph: TJSObject;
     // Render info:
    newRenderInfo: TSBMLRenderInformation; jsRenderInfo: TJSObject;
    newStyle: TSBMLRenderStyle; jsRenderStyle: TJSObject;
    newLineEnding: TSBMLRenderLineEnding; jsLineEnding: TJSObject;
    newRenderGroup: TSBMLRenderGroup; jsRenderGroup: TJSObject;
    newEllipse: TSBMLRenderEllipse; jsEllipse: TJSObject;
    newRectangle: TSBMLRenderRectangle; jsRectangle: TJSObject;
    newPolygon: TSBMLRenderPolygon; jsPolygon: TJSObject;
    newRenderPt: TSBMLRenderPoint; jsRenderPt: TJSObject;
    new1D: TSBMLRenderPrimitive1D; jsRender1D: TJSObject;
    newColorDef: TSBMLRenderColorDefinition; jsColorDef: TJSObject;
  begin
    newSpecies:= TSBMLSpecies.create(); // a new species
    jsSpecies:= JS.ToObject(newSpecies);
    newComp:= TSBMLcompartment.create(); // a new compartment
    jsComp:= JS.ToObject(newComp);
    newParam:= TSBMLparameter.create(); // a new parameter
    jsParam:= JS.ToObject(newParam);
    newRule:= TSBMLRule.create;
    jsRule:= JS.ToObject(newRule);
    // Layout variables:
    newLayout := TSBMLLayout.create;
    jsLayout := JS.toObject(newLayout);
    newDims := TSBMLLayoutDims.create; jsDims := JS.toObject(newDims);
    newPt := TSBMLLayoutPoint.create; jsPt := JS.toObject(newPt);
    newBBox := TSBMLLayoutBoundingBox.create; jsBBox := JS.toObject(newBBox);
    newLineSeg := TSBMLLayoutLineSegment.create; jsLineSeg := JS.toObject(newLineSeg);
    newCubicB := TSBMLLayoutCubicBezier.create; jsCubicB := JS.toObject(newCubicB);
    newCurve := TSBMLLayoutCurve.create; jsCurve := JS.toObject(newCurve);
    newGraphObj := TSBMLLayoutGraphicalObject.create; jsGraphObj := JS.toObject(newGraphObj);
    newGenObj := TSBMLLayoutGeneralGlyph.create; jsGenObj := JS.toObject(newGenObj);
    newCompGlyph := TSBMLLayoutCompartmentGlyph.create; jsCompGlyph := JS.toObject(newCompGlyph);
    newSpGlyph := TSBMLLayoutSpeciesGlyph.create; jsSpGlyph := JS.toObject(newSpGlyph);
    newSpRefGlyph := TSBMLLayoutSpeciesReferenceGlyph.create;
    jsSpRefGlyph := JS.toObject(newSpRefGlyph);
    newRxnGlyph := TSBMLLayoutReactionGlyph.create; jsRxnGlyph := JS.toObject(newRxnGlyph);
    newTextGlyph := TSBMLLayoutTextGlyph.create; jsTextGlyph := JS.toObject(newTextGlyph);
    // Render variables:
    newRenderInfo := TSBMLRenderInformation.create; jsRenderInfo := JS.toObject( newRenderInfo );
    newStyle := TSBMLRenderStyle.create; jsRenderStyle := JS.toObject(newStyle);
    newLineEnding := TSBMLRenderLineEnding.create; jsLineEnding := JS.toObject(newLineEnding);
    newRenderGroup := TSBMLRenderGroup.create; jsRenderGroup := JS.toObject(newRenderGroup);
    newEllipse := TSBMLRenderEllipse.create; jsEllipse := JS.toObject(newEllipse);
    newRectangle := TSBMLRenderRectangle.create; jsRectangle := JS.toObject(newRectangle);
    newPolygon := TSBMLRenderPolygon.create; jsPolygon := JS.toObject(newPolygon);
    newRenderPt := TSBMLRenderPoint.create; jsRenderPt := JS.toObject(newRenderPt);
    new1D := TSBMLRenderPrimitive1D.create; jsRender1D := JS.toObject(new1D);
    newColorDef := TSBMLRenderColorDefinition.create; jsColorDef := JS.toObject(newColorDef);

  asm;   // javascript

  try {
    libsbml().then((libsbml) => {

  // now it is safe to use the module
      const reader = new libsbml.SBMLReader();
      const doc = reader.readSBMLFromString(SBMLtext);

  //  Error check:
  //   These are not only libSBML warnings/errors but also assumptions made by Sidewinder.
  //   Lets user know what assumptions, issues are present once the model is loaded.
      var i;
      const sbmlErrors = doc.getNumErrors();
      for( i=0; i < sbmlErrors; i++ ) {
        //console.log( 'Read Sbml error: ', doc.getError(i).getMessage() );
        newModel.addSBMLErrorStr( doc.getError(i).getMessage() );
      }


      doc.enablePackage(libsbml.LayoutExtension.prototype.getXmlnsL3V1V1(), 'layout', true);
      doc.enablePackage(libsbml.RenderExtension.prototype.getXmlnsL3V1V1(), 'render', true);

      const model = doc.getModel();
   //  const lplugin = libsbml.castObject(model.findPlugin("layout"), libsbml.LayoutModelPlugin); // not used for getting layout.

      const moreReading = new ProcessSBML(model, libsbml);

      if(model.getNumPlugins() >0) {
        if(model.findPlugin('layout') != undefined) {
        // Load layout information for model:
          if( moreReading.numLayouts >0) {
            jsLayout = moreReading.getLayout(jsLayout, jsDims, jsPt, jsBBox,
              jsLineSeg, jsCubicB, jsCurve, jsCompGlyph, jsGraphObj, jsGenObj,
              jsSpGlyph, jsSpRefGlyph, jsRxnGlyph, jsTextGlyph );
            if( jsLayout != undefined) {  // another chk
              newModel.setSBMLLayout(jsLayout);
            }
          }
          if( moreReading.isLocalRenderSet ) {
            jsRenderInfo = moreReading.getSBMLRenderInformation( jsRenderInfo, jsRenderStyle,
                jsLineEnding, jsRenderGroup, jsEllipse, jsRectangle, jsPolygon,
                jsRenderPt, jsRender1D, jsColorDef, jsBBox, jsDims, jsPt );
            if( jsRenderInfo != undefined ) {
              newModel.setSBMLRenderInfo(jsRenderInfo);
            }
          }
        }

      }

      newModel = moreReading.getNumbers(newModel);
      newModel = moreReading.getRules(newModel,jsRule);
      newModel = moreReading.getSpecies(newModel, jsSpecies );
      newModel = moreReading.getCompartments(newModel, jsComp);
      newModel = moreReading.getParameters(newModel, jsParam);
      newModel = moreReading.getReactions(newModel);
      newModel.SBML_UpdateEvent();  // libsbml loaded and model processed.
     libsbml.destroy(doc);
     libsbml.destroy(reader);
    });
   } catch(e) {
       if (e instanceof Error ) {   // Need better logic, do not use catch(e) mechanism.
         console.log(' SBMLReader: Error loading libsbml: ', e.code);

         const reader = new libsbml.SBMLReader();
         const doc = reader.readSBMLFromString(SBMLtext);
         const model = doc.getModel();
         const moreReading = new ProcessSBML(model, libsbml);

         if(model.getNumPlugins() >0) {
           if(model.findPlugin('layout') != undefined) {
        // Load layout information for model:
             if( moreReading.numLayouts >0) {
               jsLayout = moreReading.getLayout(jsLayout, jsDims, jsPt, jsBBox,
                 jsLineSeg, jsCubicB, jsCurve, jsCompGlyph, jsGraphObj, jsGenObj,
                 jsSpGlyph, jsSpRefGlyph, jsRxnGlyph, jsTextGlyph );
               if( jsLayout != undefined) {  // another chk
               newModel.setSBMLLayout(jsLayout);
               }
             }
           }
         }
         newModel = moreReading.getNumbers(newModel);
         newModel = moreReading.getRules(newModel,jsRule);
         newModel = moreReading.getSpecies(newModel, jsSpecies );
         newModel = moreReading.getCompartments(newModel, jsComp);
         newModel = moreReading.getParameters(newModel, jsParam);
         newModel = moreReading.getReactions(newModel);
         newModel.SBML_UpdateEvent();  // libsbml loaded and model processed.
         libsbml.destroy(doc);
         libsbml.destroy(reader);

         }
       else throw e;
     }
  end; // asm block

  end;

  
end.
