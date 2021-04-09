unit SBML.model.rule;

interface
uses Web, JS;
type
 //  SBML Rule
 TSBMLrule = class
 private
   formula:String;
   SBMLvar: String;
   id, name: String;
   RateR, AssignmentR, AlgebraicR, ScalerR: boolean;
   isSpeciesConc, isParam: boolean;

 public
   constructor Create(); overload;
   constructor Create(cpyRule: TSBMLrule); overload;
   function isRate(): boolean; //virtual; abstract; // rate rule
   procedure setRate(isR: boolean);
   function isAssignment(): boolean; //virtual; abstract; // assignment rule
   procedure setAssignment(isAssign: boolean);
   function isAlgebraic(): boolean;//virtual; abstract; // algebraic rule
   procedure setAlgebraic(isAlg: boolean);
   function isScaler(): boolean; //virtual; abstract; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   procedure setScaler(isS: boolean);
   function isSpeciesConcentration(): boolean; //virtual; abstract; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   procedure setSpeciesConcentration(isSp: boolean);
   function isParameter(): boolean; //virtual; abstract;// parameter rule
   procedure setParameter(isP: boolean);
   //function getParameter(): boolean; // get rid of. use isParameter ....
   function getFormula(): String;
   procedure setFormula(eq: String);
   function isSetFormula(): boolean;
   function getVariable(): String;
   procedure setVariable(sid: String);// sid the identifier of a Compartment, Species or Parameter.
   function isSetVariable():boolean;
   procedure unsetVariable();  // Unsets the variable .. set to nil
   function getId(): String;
   procedure setId(id: String);
   function isSetIdAttribute(): boolean;
   function getName(): String;
   procedure setName( name: String);
   function isSetName(): boolean;

 end;

 {SBMLalgebraicrule = class(SBMLrule)
 private

 public
   function isRate(): boolean; override; // rate rule
   function isAssignment(): boolean; override; // assignment rule
   function isAlgebraic(): boolean;override; // algebraic rule
   function isScaler(): boolean; override; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean; override; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; override;// parameter rule
 end;  }

{ TSBMLassignmentrule = class(TSBMLrule)
 private

 public
   function isRate(): boolean; override; // rate rule
   function isAssignment(): boolean; override; // assignment rule
   function isAlgebraic(): boolean;override; // algebraic rule
   function isScaler(): boolean; override; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean; override; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; override;// parameter rule
 end;

 TSBMLraterule = class(TSBMLrule)
 private

 public
   function isRate(): boolean; override; // rate rule
   function isAssignment(): boolean; override; // assignment rule
   function isAlgebraic(): boolean;override; // algebraic rule
   function isScaler(): boolean; override; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean;override; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; override;// parameter rule
 end;
   }

implementation

constructor TSBMLrule.Create(); overload;
begin
  self.formula:= '';
  self.SBMLvar:= '';
  self.id:= ''; name:= '';
  self.RateR:= false; self.AssignmentR:= false; self.AlgebraicR:= false; self.ScalerR:= false;
  self.isSpeciesConc:= false; self.isParam:= false;
end;

constructor TSBMLrule.Create(cpyRule: TSBMLrule); overload;
begin
   self.formula:= cpyRule.getFormula;
   self.SBMLvar:= cpyRule.getVariable;
   if cpyRule.isSetVariable then
   begin
     self.SBMLvar:= cpyRule.getVariable;
   end;
   if cpyRule.isParameter then
   begin
     self.isParam:= cpyRule.isParameter;
   end;
   self.id:= cpyRule.getId;
   self.name:= cpyRule.getName;
   self.RateR:= cpyRule.isRate;
   self.AssignmentR:= cpyRule.isAssignment;
   self.AlgebraicR:= cpyRule.isAlgebraic;
   self.ScalerR:= cpyRule.isScaler;
   self.isSpeciesConc:= cpyRule.isSpeciesConcentration;
   self.isParam:= cpyRule.isParameter;
end;

function TSBMLrule.isRate(): boolean; //virtual; abstract; // rate rule
begin
  Result:= self.RateR;
end;
function TSBMLrule.isAssignment(): boolean; //virtual; abstract; // assignment rule
begin
  Result:= self.AssignmentR;
end;
function TSBMLrule.isAlgebraic(): boolean;//virtual; abstract; // algebraic rule
begin
  Result:= self.AlgebraicR;
end;
function TSBMLrule.isScaler(): boolean; //virtual; abstract;
begin
  Result:= self.ScalerR;
end;

procedure TSBMLrule.setRate(isR: boolean);
begin
  self.RateR:= isR;
end;
procedure TSBMLrule.setAssignment(isAssign: boolean);
begin
  self.AssignmentR:= isAssign;
end;

procedure TSBMLrule.setAlgebraic(isAlg: boolean);
begin
  self.AlgebraicR:= isAlg;
end;

procedure TSBMLrule.setScaler(isS: boolean);
begin
  self.ScalerR:= isS;
end;

function TSBMLrule.isSpeciesConcentration(): boolean; //virtual; abstract;
begin
  Result:= self.isSpeciesConc;
end;
procedure TSBMLrule.setSpeciesConcentration(isSp: boolean);
begin
  self.isSpeciesConc:= isSp;
end;

function TSBMLrule.isParameter(): boolean; //virtual; abstract;// parameter rule
begin
  Result:= self.isParam;
end;
procedure TSBMLrule.setParameter(isP: boolean);
begin
  self.isParam:= isP;
  self.isParam:= true;
end;
//function TSBMLrule.getParameter(): boolean;
//begin
//  Result:= self.isParam;
//end;


function TSBMLrule.getFormula(): String;
begin
  Result:= self.formula;
end;
procedure TSBMLrule.setFormula(eq: String);
begin
  self.formula:= eq;
end;
function TSBMLrule.isSetFormula(): boolean;
begin
  if self.formula= '' then Result:= false
  else Result:= true;
end;

function TSBMLrule.getVariable(): String;
begin
  Result:= self.SBMLvar;
end;
procedure TSBMLrule.setVariable(sid: String);// sid the identifier of a Compartment, Species or Parameter.
begin
  self.SBMLvar:= sid;
end;
function TSBMLrule.isSetVariable():boolean;
begin
  if self.SBMLvar = '' then Result:= false
  else Result:= true;
end;
procedure TSBMLrule.unsetVariable();  // Unsets the variable .. set to nil
begin
  self.SBMLvar:= '';
end;

function TSBMLrule.getId(): String;
begin
  Result:= self.id;
end;
procedure TSBMLrule.setId(id: String);
begin
  self.id:= id;
end;
function TSBMLrule.isSetIdAttribute(): boolean;
begin
  if self.id = '' then Result:= false
  else Result:= true;
end;
function TSBMLrule.getName(): String;
begin
  Result:= self.name;
end;
procedure TSBMLrule.setName( name: String);
begin
  self.name:= name;
end;
function TSBMLrule.isSetName(): boolean;
begin
  if self.name = '' then Result:= false
  else Result:= true;
end;

end.
