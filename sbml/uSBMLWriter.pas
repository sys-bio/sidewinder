unit uSBMLWriter;
interface
uses Web, JS, uSBMLClasses, uSBMLClasses.rule, uModel;

type
  TSBMLWriteLoaded = procedure(SBMLStr: String) of object;  // Notify when done loading

  TSBMLWriter = class
  private
    FNotify: TSBMLWriteLoaded; // send sbml info to listener once asynchronous read done.
    sbmlModel: TModel;
    sbmlStr: String;
  public
    constructor create();
    procedure buildSBMLString(model: TModel);
    property OnNotify: TSBMLWriteLoaded read FNotify write FNotify;
    procedure SBMLStrCreated(modelStr: String);

  end;

implementation

 constructor TSBMLWriter.create();
 begin
   self.sbmlStr := '';
   self.sbmlModel := nil;
 end;
 procedure TSBMLWriter.buildSBMLString(model: TModel);
 begin
   self.sbmlStr := '';
   self.sbmlModel := model;
  // console.log('In TSBMLWriter.create ');
  asm;   // javascript
   try  {

       libsbml().then((libsbml) => {
   // now it is safe to use the module
       const writeModel = new GenerateSBMLModel(libsbml, model);
       writeModel.buildModel();
       this.sbmlStr = writeModel.getSBMLString();
       this.SBMLStrCreated(this.sbmlStr);
       //libsbml.destroy(sbmlDoc);
       //libsbml.destroy(writer);
      });

      } catch(e) {
        if (e instanceof Error ) {
        //  console.log(' loading libsbml: ', e.code);
          const writeModel = new GenerateSBMLModel(libsbml, model);
          writeModel.buildModel();
          this.sbmlStr = writeModel.getSBMLString();
          this.SBMLStrCreated(this.sbmlStr);
       }
        else throw e;
      }

  end;  // asm block

 end;

 procedure TSBMLWriter.SBMLStrCreated(modelStr: String);
 begin
    if Assigned(FNotify) then
     FNotify(modelStr);
 end;


end.
