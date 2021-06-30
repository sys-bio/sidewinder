unit uSBMLReader;

interface
uses Web, JS,  uSBMLClasses, uSBMLClasses.rule, uModel;

type
  TSBMLLoadedEvent = procedure(model: TModel) of object;  // Notify when done loading

  TSBMLRead = class
  private
    FNotify: TSBMLLoadedEvent; // send sbml info to listener once asynchronous read done.

  public
    constructor create(newModel: TModel; SBMLtext: String);
    property OnSBMLLoaded: TSBMLLoadedEvent read FNotify write FNotify;
    procedure modelLoaded(model:TModel);
  end;


implementation
  constructor TSBMLRead.create(newModel: TModel; SBMLtext: String);
  var
    newSpecies: TSBMLSpecies;
    jsSpecies: TJSObject;
    newComp: SBMLcompartment;
    jsComp: TJSObject;
    newParam: TSBMLparameter;
    jsParam: TJSObject;
    newRule: TSBMLRule;
    jsRule: TJSObject;
  begin
    newSpecies:= TSBMLSpecies.create(); // a new species
    jsSpecies:= JS.ToObject(newSpecies);
    newComp:= SBMLcompartment.create(); // a new compartment
    jsComp:= JS.ToObject(newComp);
    newParam:= TSBMLparameter.create(); // a new parameter
    jsParam:= JS.ToObject(newParam);
    newRule:= TSBMLRule.create;
    jsRule:= JS.ToObject(newRule);

  asm;   // javascript

  try {
   // if(typeof libsbml() === 'undefined') {console.log(' libsbml is undefined');}
    libsbml().then((libsbml) => {

  // now it is safe to use the module
      const reader = new libsbml.SBMLReader();
      const doc = reader.readSBMLFromString(SBMLtext);
  // read with no errors .. TODO: Chk for errors
      const model = doc.getModel();
      const moreReading = new ProcessSBML(model);

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
        // console.log(' SBMLReader: Error loading libsbml: ', e.code);

         const reader = new libsbml.SBMLReader();
         const doc = reader.readSBMLFromString(SBMLtext);
         const model = doc.getModel();
         const moreReading = new ProcessSBML(model);
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

  procedure TSBMLRead.modelLoaded(model: TModel);
  begin
      { Call the registerd event only if there is a listener }
   if Assigned(FNotify) then
     FNotify(model);
  end;
end.
