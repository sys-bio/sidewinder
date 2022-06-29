unit uSBMLClasses.rule;

interface
uses Web, JS;
type
 //  SBML Rule
 TSBMLRule = class
 private
   formula:String;
   SBMLvar: String; // for Assignment, the variable id that is being assigned the math (formula).
   id, name: String;
   RateR, AssignmentR, AlgebraicR, ScalerR: boolean;
   isSpeciesConc, isParam: boolean;
   isSpeciesRef: boolean;  // species reference: formula is stoichiometry
   piecewiseUsed: boolean; // law contains piecewise in formula

 public
   constructor Create(); overload;
   constructor Create(cpyRule: TSBMLrule); overload;
   function isRate(): boolean;       // rate rule
   procedure setRate(isR: boolean);
   function isAssignment(): boolean; // assignment rule
   procedure setAssignment(isAssign: boolean);
   function isAlgebraic(): boolean;  // algebraic rule
   procedure setAlgebraic(isAlg: boolean);
   function isScaler(): boolean; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   procedure setScaler(isS: boolean);
   function isSpeciesConcentration(): boolean; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   procedure setSpeciesConcentration(isSp: boolean);
   function isParameter(): boolean;        // parameter rule
   procedure setParameter(isP: boolean);
   function isSpeciesReference(): boolean; // speciesReference rule
   procedure setSpeciesReference(isSpR: boolean);
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
   procedure setPiecewise(piecewiseVal: boolean);
   function  containsPiecewise(): boolean;
   function printStr(): string;
 end;

 {TSBMLalgebraicrule = class(TSBMLrule)
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

constructor TSBMLRule.Create(); overload;
begin
  self.formula:= '';
  self.SBMLvar:= '';
  self.id:= ''; name:= '';
  self.RateR:= false; self.AssignmentR:= false; self.AlgebraicR:= false; self.ScalerR:= false;
  self.isSpeciesConc:= false; self.isParam:= false; self.isSpeciesRef := false;
end;

constructor TSBMLRule.Create(cpyRule: TSBMLrule); overload;
begin
   self.formula:= cpyRule.getFormula;
   self.SBMLvar:= cpyRule.getVariable;
   if cpyRule.isSetVariable then
   begin
     self.SBMLvar:= cpyRule.getVariable;
   end;
   {if cpyRule.isParameter then
   begin
     self.isParam:= cpyRule.isParameter;
   end;}
   self.id:= cpyRule.getId;
   self.name:= cpyRule.getName;
   self.RateR:= cpyRule.isRate;
   self.AssignmentR:= cpyRule.isAssignment;
   self.AlgebraicR:= cpyRule.isAlgebraic;
   self.ScalerR:= cpyRule.isScaler;
   self.isSpeciesConc:= cpyRule.isSpeciesConcentration;
   self.isParam:= cpyRule.isParameter;
   self.isSpeciesRef := cpyRule.isSpeciesRef;
   self.piecewiseUsed := cpyRule.containsPiecewise;
end;

function TSbmlRule.printStr(): string;
begin
  Result := '';
  //Result := ' Rule ID: ' + self.id;
  if self.isRate then Result := ' Rate Rule ID: ' + self.id
  else
    begin
    if self.isAssignment then Result := ' Assignment Rule ID: ' + self.id;
    if self.isAlgebraic then Result := ' Algebraic Rule ID: ' + self.id;
    if self.isScaler then Result := Result + ', Rule is scaler';

    end;
  if self.isSetName then Result := Result + ', Rule name: ' + self.name
  else Result := Result + ', No Rule name';
  if self.isSpeciesConc then Result := Result + ', Rule is for species conc';
  if self.isSpeciesRef then Result := Result + ', Rule is for species reference';
  if self.isParam then Result := Result + ', Rule is for parameter';
  Result := Result + ', Rule variable: ' + self.SBMLvar;
  Result := Result + ', Rule formula: ' + self.formula;
  if self.piecewiseUsed then Result := Result + ', Rule uses piecewise. '
  else Result := Result + ', Rule Does not use piecewise. ';






end;

function TSBMLRule.isRate(): boolean; //virtual; abstract; // rate rule
begin
  Result:= self.RateR;
end;


function TSBMLRule.isAssignment(): boolean; //virtual; abstract; // assignment rule
begin
  Result:= self.AssignmentR;
end;


function TSBMLRule.isAlgebraic(): boolean;//virtual; abstract; // algebraic rule
begin
  Result:= self.AlgebraicR;
end;


function TSBMLRule.isScaler(): boolean; //virtual; abstract;
begin
  Result:= self.ScalerR;
end;

procedure TSBMLRule.setRate(isR: boolean);
begin
  self.RateR:= isR;
end;


procedure TSBMLRule.setAssignment(isAssign: boolean);
begin
  self.AssignmentR:= isAssign;
end;


procedure TSBMLRule.setAlgebraic(isAlg: boolean);
begin
  self.AlgebraicR:= isAlg;
end;


procedure TSBMLRule.setScaler(isS: boolean);
begin
  self.ScalerR:= isS;
end;


function TSBMLRule.isSpeciesConcentration(): boolean;
begin
  Result:= self.isSpeciesConc;
end;


procedure TSBMLRule.setSpeciesConcentration(isSp: boolean);
begin
  self.isSpeciesConc:= isSp;
end;

function TSBMLRule.isSpeciesReference(): boolean; // speciesReference rule
begin
  Result := self.isSpeciesRef;
end;
procedure TSBMLRule.setSpeciesReference(isSpR: boolean);
begin
  self.isSpeciesRef := isSpR;
end;

function TSBMLRule.isParameter(): boolean; // parameter rule
begin
  Result:= self.isParam;
end;


procedure TSBMLRule.setParameter(isP: boolean);
begin
  self.isParam:= isP;
  self.isParam:= true;
end;

function TSBMLRule.getFormula(): String;
begin
  Result:= self.formula;
end;


procedure TSBMLRule.setFormula(eq: String);
begin
  self.formula:= eq;
end;


function TSBMLRule.isSetFormula(): boolean;
begin
  if self.formula= '' then Result:= false
  else Result:= true;
end;


function TSBMLRule.getVariable(): String;
begin
  Result:= self.SBMLvar;
end;


procedure TSBMLRule.setVariable(sid: String);// sid the identifier of a Compartment, Species or Parameter.
begin
  self.SBMLvar:= sid;
end;


function TSBMLRule.isSetVariable():boolean;
begin
  if self.SBMLvar = '' then Result:= false
  else Result:= true;
end;


procedure TSBMLRule.unsetVariable();  // Unsets the variable .. set to nil
begin
  self.SBMLvar:= '';
end;


function TSBMLRule.getId(): String;
begin
  Result:= self.id;
end;


procedure TSBMLRule.setId(id: String);
begin
  self.id:= id;
end;


function TSBMLRule.isSetIdAttribute(): boolean;
begin
  if self.id = '' then Result:= false
  else Result:= true;
end;


function TSBMLRule.getName(): String;
begin
  Result:= self.name;
end;


procedure TSBMLRule.setName( name: String);
begin
  self.name:= name;
end;

function TSBMLRule.isSetName(): boolean;
begin
  if self.name = '' then Result:= false
  else Result:= true;
end;

procedure TSBMLRule.setPiecewise(piecewiseVal: boolean);
begin
  self.piecewiseUsed := piecewiseVal;
end;

function  TSBMLRule.containsPiecewise(): boolean;
begin
  Result := self.piecewiseUsed;
end;

end.
