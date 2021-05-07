unit uSBMLWriter;
interface
uses Web, JS, uSBMLClasses, uSBMLClasses.rule, uModel;

type
  TSBMLWriteLoaded = procedure() of object;  // Notify when done loading

  TSBMLWriter = class
  private
    FNotify: TSBMLWriteLoaded; // send sbml info to listener once asynchronous read done.
    sbmlModel: TModel;
    sbmlStr: String;
  public
    constructor create(model: TModel );
    property OnNotify: TSBMLWriteLoaded read FNotify write FNotify;

    function getSBMLStr(): String;

  end;

implementation

 constructor TSBMLWriter.create(model: TModel);
 begin
   self.sbmlStr := '';
   self.sbmlModel := model;
   console.log('In TSBMLWriter.create ');
  asm;   // javascript
   // TODO
   // libsbml().then((libsbml) => {

  // now it is safe to use the module
  //  const writer = new libsbml.SBMLWriter();   // declare later ?
  //  const sbmlDoc = new libsbml.SBMLDocument();
  //  const model = sbmlDoc.createModel();

  //  libsbml.destroy(doc);
  //  libsbml.destroy(writer);

   // });
  end;



 end;

 function TSBMLWriter.getSBMLStr(): String;
 begin
   Result := self.sbmlStr;
 end;

end.
