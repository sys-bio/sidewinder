unit uSBMLClasses.FuncDefinition;

interface
 uses System.Generics.Collections, Web, JS;
type

 TSBMLFuncDefinition = class
 private
  id, name: string;
  varList: TList<string>; // Holds species, params, compartment ids used in function.
  strFuncFormula: string; // String holding function equation (function body).
  strFullFuncLabel: string;  //Full func label ex: func01(x, y, x), used to find
                   // and replace function in kinetic law or other assignment as needed.

 public
  constructor create() overload;
  constructor create( funcCopy: TSBMLFuncDefinition ) overload;
  procedure clear();
  procedure setId( newId: string );
  function  getId(): string;
  procedure setName( newName: string );
  function  getName(): string;
  procedure addVar( newVar: string );
  function  getVar( index: integer ): string;
  function  getNumVars(): integer;
  function  getFuncFormula(): string;
  procedure setFuncFormula( newFormula: string );
  function  getFullFuncLabel(): string;
  procedure setFullFuncLabel( newLabel: string );
  function printStr(): string;

 end;

implementation

 constructor TSBMLFuncDefinition.create();
 begin
   self.id := '';
   self.name := '';
   self.strFuncFormula := '';
   self.strFullFuncLabel := '';
   self.varList := TList<string>.create;

 end;

 constructor TSBMLFuncDefinition.create( funcCopy: TSBMLFuncDefinition ) overload;
 var i: integer;
 begin
   self.id := funcCopy.getId;
   self.name := funcCopy.getName;
   self.strFuncFormula := funcCopy.getFuncFormula;
   self.strFullFuncLabel := funcCopy.getFullFuncLabel;
   self.varList := TList<string>.create;
   for i := 0 to funcCopy.getNumVars -1 do
     self.addVar(funcCopy.getVar(i));

 end;

 function TSBMLFuncDefinition.printStr(): string;
 begin
   Result := '';
   Result := ', Func Def ID: ' + self.getId + ', Name: ' + self.getName;
   Result := Result + ', Func Def vars and formula: '  + self.getFullFuncLabel;

 end;

 procedure TSBMLFuncDefinition.clear();
 begin
   self.id := '';
   self.name := '';
   self.strFuncFormula := '';
   self.strFullFuncLabel := '';
   self.varList.Free;
   self.varList := TList<string>.create;
 end;

 procedure TSBMLFuncDefinition.setId( newId: string );
 begin
   self.id := newId;
 end;
 function  TSBMLFuncDefinition.getId(): string;
 begin
   Result := self.id;
 end;
 procedure TSBMLFuncDefinition.setName( newName: string );
 begin
   self.name := newName;
 end;
 function  TSBMLFuncDefinition.getName(): string;
 begin
   Result := self.name;
 end;

 procedure TSBMLFuncDefinition.addVar( newVar: string );
 begin
   self.varList.Add(newVar);
 end;
 function  TSBMLFuncDefinition.getVar( index: integer ): string;
 begin
   if index < self.varList.Count then
     Result := self.varList[index]
   else Result := '';
 end;

 function TSBMLFuncDefinition.getNumVars(): integer;
 begin
   Result := self.varList.Count;
 end;

 function TSBMLFuncDefinition.getFuncFormula(): string;
 begin
   Result:= self.strFuncFormula;
 end;
 procedure TSBMLFuncDefinition.setFuncFormula( newFormula: string );
 begin
   self.strFuncFormula := newFormula;
 end;

 function  TSBMLFuncDefinition.getFullFuncLabel(): string;
 begin
   Result := self.strFullFuncLabel;
 end;
 procedure TSBMLFuncDefinition.setFullFuncLabel( newLabel: string );
 begin
   self.strFullFuncLabel := newLabel;
 end;

end.
