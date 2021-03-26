unit SBML.model;
interface
 uses Web, JS;
type

 SBMLspecies = class
   private
   initAmt, initConc: double;
   sid, name: String;
   compSid: String; // Compartment sid
   boundCond: boolean;
   amtConst: boolean; // if amt constant.
   initAmtFlag, initConcFlag: boolean;
   compFlag, boundCondFlag, constFlag: boolean;
   idFlag, nameFlag: boolean;

   public
   constructor create(); Overload;
   constructor create(id:String); Overload;
   constructor create(copySp: SBMLspecies); Overload;   // copy
   function getInitialAmount(): double;
   procedure setInitialAmount(amt:double);
   function isSetInitialAmount(): boolean;
   function getInitialConcentration(): double;
   procedure setInitialConcentration(amt:double);
   function isSetInitialConcentration():boolean;
   function getCompartment(): String;
   procedure setCompartment(sid:String);// sid identifier of a Compartment object defined elsewhere in this Model
   function isSetCompartment():boolean;
   function getBoundaryCondition():boolean; // true if this Species' "boundaryCondition" attribute value is true, false otherwise
   procedure setBoundaryCondition(val:boolean);
  // function isSetBoundaryCondition(): boolean; // true if this Species object's "boundaryCondition" attribute is set.
   function getConstant():boolean; // true if this Species's "constant" attribute value is true, false otherwise
   procedure setConstant(val:boolean);
  // function isSetConstant():boolean;
   function getID(): String;
   procedure setID(id:String);
   function isSetIdAttribute(): boolean;
   function getName(): String;
   procedure setName(val: String);
   function isSetName(): boolean;
 end;

 SBMLcompartment = class
 private
    id, name: String;
    size: double; // vol returns size.
    idSetFlag, nameSetFlag: boolean;
    sizeSetFlag, volSetFlag: boolean;
    constSetFlag: boolean;
 public
    constructor create(); Overload;
    constructor create(val:String); Overload;
    constructor create(copyComp:SBMLcompartment); Overload;

    function getConstant(): boolean; // true if compartment size constant
    procedure setConstant(val: boolean);
    function isSetConstant(): boolean;
    function getSize(): double;
    procedure setSize(val:double);
    function isSetSize(): boolean;
    function getVolume(): double; // level 1 only, otherwise returns 'size'
    procedure setVolume(val:double); // level 1 only
    function isSetVolume():boolean;//  true if the "volume" attribute ("size" in Level 2 and above) of this Compartment object is set.
    function getID(): String;
    procedure setID(val:String);
    function isSetIdAttribute(): boolean;
    function getName(): String;
    procedure setName(val:String);
    function isSetName(): boolean;
 end;


 SBMLparameter = class
 private
    value: double;
    valSetFlag, constSetFlag: boolean;
    id, name: String;
    idSetFlag, nameSetFlag: boolean;

 public
    constructor create(); Overload;
    constructor create(paramId: String); Overload;
    constructor create(copyParam:SBMLparameter); Overload;
    function getValue():double;
    procedure setValue(val: double);
    function isSetValue(): boolean;
    function getConstant(): boolean; // true if param constant.
    procedure setConstant(val: boolean);
    function isSetConstant(): boolean;
    function getId(): String;
    procedure setId(val: String);
    function isSetIDAttribute(): boolean;
    function getName(): String;
    procedure setName(val: String);
    function isSetName(): boolean;

 end;

// SBMLlocalParameter not implimented, same methods as SBMLParameter.

 SBMLspeciesreference = class
 private
    stoichValue: double;
  //  constSetFlag: boolean;
    id, species: String;
   // name: String;
    idSetFlag, speciesSetFlag: boolean;
    //nameSetFlag: boolean;
    stoichSetFlag: boolean;
 public
    constructor create(); Overload;
    constructor create(id:String); Overload;
    constructor create(id:String; stoich: double); Overload;
    function getSpecies():String;
    procedure setSpecies(val:String);
    function isSetSpecies(): boolean;
    function getId(): String;
    procedure setId( val: String);
    function isSetIdAttribute(): boolean;
//    function getConstant(): boolean; // check if stoich is constant.
//    procedure setConstant(val: boolean);
//    function isSetConstant(): boolean;
    function getStoichiometry(): double;
    procedure setStoichiometry(val: double);
 //   function getStoichiometry(): double;
 //   procedure setStoichiometry(val: double);
    function isSetStoichiometry(): boolean;
 //   function getName(): String;
 //   procedure setName(val: String);
 //   function isSetName(): boolean;

 end;
 {
 // abstract class for all the rules
 SBMLrule = class
 private
   formula:String;
   SBMLvar: String;
   id, name: String;

 public
   function isRate(): boolean; virtual; abstract; // rate rule
   function isAssignment(): boolean; virtual; abstract; // assignment rule
   function isAlgebraic(): boolean;virtual; abstract; // algebraic rule
   function isScaler(): boolean; virtual; abstract; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean; virtual; abstract; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; virtual; abstract;// parameter rule
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

 SBMLalgebraicrule = class(SBMLrule)
 private

 public
   function isRate(): boolean; override; // rate rule
   function isAssignment(): boolean; override; // assignment rule
   function isAlgebraic(): boolean;override; // algebraic rule
   function isScaler(): boolean; override; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean; override; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; override;// parameter rule
 end;

 SBMLassignmentrule = class(SBMLrule)
 private

 public
   function isRate(): boolean; override; // rate rule
   function isAssignment(): boolean; override; // assignment rule
   function isAlgebraic(): boolean;override; // algebraic rule
   function isScaler(): boolean; override; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean; override; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; override;// parameter rule
 end;

 SBMLraterule = class(SBMLrule)
 private

 public
   function isRate(): boolean; override; // rate rule
   function isAssignment(): boolean; override; // assignment rule
   function isAlgebraic(): boolean;override; // algebraic rule
   function isScaler(): boolean; override; // true if this Rule is an AssignmentRule (Level 2) or has type "scalar" (Level 1), false otherwise.
   function isSpeciesConcentration(): boolean;override; //returning true if this Rule is a SpeciesConcentrationRule or equivalent
   function isParameter(): boolean; override;// parameter rule
 end;

 SBMLkineticlaw = class
 // Note: no localParameters looked for/used.
 private
    parameters: array of SBMLparameter;
    formula: String;
    id, name: String;

 public
    function getNumParameters(): integer;
    function createParameter(): SBMLparameter;
    function getParameter(n: integer): SBMLparameter;
    function removeParameter(n: integer): SBMLparameter;
    function getFormula(): String;
    procedure setFormula(formula: String);// the mathematical expression to use, represented in text-string form
    function getId(): String;
    procedure setID(id: String);
    function isSetIdAttribute(): boolean;
    function getName(): String;
    procedure setName(name: String);
    function isSetName(): boolean;

 end;
   }

   // Local Parameters not implimented...
 SBMLkineticlaw = class
  private
   id: String;
   name: String;
   formula: String;  // math expression converted to String by libSBML
   paramIds: array of String; // Store parameter ids used in kinetic law.
   numParams: integer;
   nameFlagSet: boolean;

  public
   constructor create(); Overload;
   constructor create(newId: String; newFormula: String; paramArr: array of String); Overload;
   function getNumParameters(): integer;
   procedure setNumParameters(i:integer);
   function addParameter(param: String): String;
   function getParameter(n: integer): String;
  // function removeParameter(n: integer): String;

   function getId(): String;
   procedure setId(val: String);
   function getName():String;
   procedure setName(val: String);
   function isNameFlagSet():boolean;
   function getFormula(): String;
   procedure setFormula(newFormula: String);

 end;
     // reaction Modifiers not implimented
 SBMLReaction = class
 private
  rxnID, name: String;
  rxnIDFlagSet: boolean;
  rxnName: String;
  rxnNameFlagSet: boolean;
  compartment: String;  // Compartment sid
  compartmentFlagSet: boolean;
  kineticlaw: SBMLkineticlaw;
  kineticLawFlagSet: boolean;
  products: array of String;   // remove at some point
  reactants: array of String;  // remove at "

  numbProducts, numbReactants: integer;
  rxnProducts: array of SBMLspeciesreference;
  rxnReactants: array of SBMLspeciesreference;

 public
  kineticLawStr: String;  // temporary, use SBMLkineticlaw class later
  constructor create(id:String; prod: array of String; reactant: array of String); Overload; // remove at some point
  constructor create(id:String; prod: array of SBMLspeciesreference; reactant: array of SBMLspeciesreference); Overload;
  constructor create(id:String); Overload;
  function getID():String;   // from model.getReaction(i).getId()
  function getProducts(): array of String;   // remove at some pointGet products (species) for reaction
  function getReactants(): array of String;  // remove at some pointGet reactants (species) for reaction
  function getProduct(index: integer):SBMLspeciesreference;
  function getCompartment(): String;
  procedure setCompartment(val:String);
  function isSetCompartment(): boolean;
  procedure setProducts(prod: array of String);  // remove at some point
  procedure setReactants(reactant: array of String);  // remove at some point
  function getrxnProducts(): array of SBMLspeciesreference;   // Get products (species) for reaction
  function getrxnReactants(): array of SBMLspeciesreference;  // Get reactants (species) for reaction
  procedure setrxnProducts(prod: array of SBMLspeciesreference);
  procedure setrxnReactants(reactant: array of SBMLspeciesreference);

  function getNumReactants(): integer;
  function getNumProducts(): integer;
 { function getReactant(n: integer): SBMLspeciesreference;
  procedure addReactant(spec: SBMLspeciesreference);
  function getProduct(n: integer): SBMLspeciesreference;
  procedure addProduct(spec: SBMLspeciesreference);
  function createReactant(): SBMLspeciesreference;
  function createProduct(): SBMLspeciesreference;
  function removeReactant(n:integer): SBMLspeciesreference;
  function removeProduct(n:integer): SBMLspeciesreference;
  function createKineticLaw():SBMLkineticlaw;}
  function getKineticLaw(): SBMLkineticlaw;
  procedure setKineticLaw(law: SBMLkineticlaw);
  function isSetKineticLaw(): boolean;
//  function getReversible(): boolean;
//  procedure setReversible();
//  function isSetReversible(): boolean;
//  procedure unsetReversible();
  procedure setId(id: String);
  function isSetIdAttribute(): boolean;
  function getName(): String;
  procedure setName(name: String);
  function isSetName(): boolean;

 end;

implementation
// *******************************************************
  constructor SBMLspecies.create(); Overload;
  begin
    sid:= '';
    initAmt:= 0;
    initConc:= 0;
    compSid:= '';
    name:= '';
    idFlag:= false;
    boundCond:= false; amtConst:= false; initAmtFlag:= false; initConcFlag:= false;
    compFlag:= false; boundCondFlag:= false; constFlag:= false;
    nameFlag:= false;
  end;

  constructor SBMLspecies.create(id:String); Overload;
  begin
    sid:= id;
    initAmt:= 0;
    initConc:= 0;
    compSid:= '';
    name:= '';
    idFlag:= true;
    boundCond:= false; amtConst:= false; initAmtFlag:= false; initConcFlag:= false;
    compFlag:= false; boundCondFlag:= false; constFlag:= false;
    nameFlag:= false;

  end;
  constructor SBMLspecies.create(copySp: SBMLspecies); Overload;
  begin
    sid:= copySp.getID();
    initAmt:= copySp.getInitialAmount();
    initConc:= copySp.getInitialConcentration();
    compSid:=copySp.getCompartment();
    name:= copySp.getName();
    nameFlag:= copySp.isSetName();
    idFlag:= copySp.isSetIdAttribute();
    boundCond:= copySp.getBoundaryCondition();
    //boundCondFlag:= nsp.boundCondFlag;
    amtConst:= copySp.getConstant();
    initAmtFlag:= copySp.isSetInitialAmount();
    initConcFlag:= copySp.isSetInitialConcentration();
    compFlag:= copySp.isSetCompartment();



  end;
   function SBMLspecies.getInitialAmount(): double;
   begin
     Result:= initAmt;
   end;
   procedure SBMLspecies.setInitialAmount(amt:double);
   begin
     initAmt:=amt;
     initAmtFlag:= true;
   end;
   function SBMLspecies.isSetInitialAmount(): boolean;
   begin
     Result:=initAmtFlag;
   end;
   function SBMLspecies.getInitialConcentration(): double;
   begin
     Result:= initConc;
   end;
   procedure SBMLspecies.setInitialConcentration(amt:double);
   begin
     initConc:= amt;
     initConcFlag:= true;
   end;
   function SBMLspecies.isSetInitialConcentration():boolean;
   begin
     Result:= initConcFlag;
   end;
   function SBMLspecies.getCompartment(): String;
   begin
     Result:= compSid;
   end;
   procedure SBMLspecies.setCompartment(sid:String);// sid identifier of a Compartment object defined elsewhere in this Model
   begin
     compSid:= sid;
     compFlag:= true;
   end;
   function SBMLspecies.isSetCompartment():boolean;
   begin
     Result:= compFlag;
   end;
   function SBMLspecies.getBoundaryCondition():boolean; // true if this Species' "boundaryCondition" attribute value is true, false otherwise
   begin
     Result:= boundCond;
   end;
    procedure SBMLspecies.setBoundaryCondition(val:boolean);
    begin
      boundCond:= val;
    end;
   //function SBMLspecies.isSetBoundaryCondition(): boolean; // true if this Species object's "boundaryCondition" attribute is set.
   function SBMLspecies.getConstant():boolean; // true if this Species's "constant" attribute value is true, false otherwise
   begin
     Result:= constFlag;
   end;
   procedure SBMLspecies.setConstant(val:boolean);
   begin
     constFlag:= val;
   end;
  // function SBMLspecies.isSetConstant():boolean;
   function SBMLspecies.getID(): String;
   begin
     Result:= sid;
   end;
   procedure SBMLspecies.setID(id:String);
   begin
     sid:= id;
     idFlag:= true;
   end;
   function SBMLspecies.isSetIdAttribute(): boolean;
   begin
     Result:= idFlag;
   end;
   function SBMLspecies.getName(): String;
   begin
     Result:= name;
   end;
   procedure SBMLspecies.setName(val: String);
   begin
     name:= val;
     nameFlag:= true;
   end;
   function SBMLspecies.isSetName(): boolean;
   begin
     Result:= nameFlag;
   end;


  // ************************************************

  constructor SBMLSpeciesReference.create(); Overload;
  begin
     id:= '';
     species:= '' ;
     stoichValue:= 1;  // Default is '1' do not set stoich flag.
     idSetFlag:= false;
     speciesSetFlag:= false;
     stoichSetFlag:= false;
  end;
  constructor SBMLSpeciesReference.create(id:String); Overload;
  begin
     id:= id;
     species:= '' ;
     stoichValue:= 1;  // Default is '1' do not set stoich flag.
     idSetFlag:= true;
     speciesSetFlag:= false;
     stoichSetFlag:= false;
  end;
  constructor SBMLSpeciesReference.create(id:String; stoich: double); Overload;
  begin
     id:= id;
     species:= id ; // for now, may change
     stoichValue:= stoich;
     idSetFlag:= true;
     speciesSetFlag:= true;
     stoichSetFlag:= false;
  end;
  function SBMLSpeciesReference.getSpecies():String;
  begin
    Result:= species;
  end;
  procedure SBMLSpeciesReference.setSpecies(val:String);
  begin
    species:= val;
    speciesSetFlag:= true;
  end;
  function SBMLSpeciesReference.isSetSpecies(): boolean;
  begin
    Result:= speciesSetFlag;
  end;
//    function getConstant(): boolean; // check if stoich is constant.
//    procedure setConstant(val: boolean);
//    function isSetConstant(): boolean;
  function SBMLSpeciesReference.getStoichiometry(): double;
  begin
    Result:= stoichValue;
  end;
  procedure SBMLSpeciesReference.setStoichiometry(val: double);
  begin
    stoichValue:= val;
    stoichSetFlag:= true;
  end;
  function SBMLSpeciesReference.isSetStoichiometry(): boolean;
  begin
    Result:= stoichSetFlag;
  end;
  function SBMLSpeciesReference.getId(): String;
  begin
    Result:= id;
  end;
  procedure SBMLSpeciesReference.setId( val: String);
  begin
    id:= val;
    idSetFlag:= true;
  end;
  function SBMLSpeciesReference.isSetIdAttribute(): boolean;
  begin
    Result:= idSetFlag;
  end;

  //   ******************************************
  constructor SBMLparameter.create(); Overload;
  begin
    id:= '';
    value:= 0;
    idSetFlag:= true;
    valSetFlag:= false;
    constSetFlag:= false;
    nameSetFlag:= false;
  end;
  constructor SBMLparameter.create(paramId:String); Overload;
  begin
    id:= paramId;
    value:= 0;
    idSetFlag:= true;
    valSetFlag:= false;
    constSetFlag:= false;
    nameSetFlag:= false;
  end;

  constructor SBMLparameter.create(copyParam:SBMLparameter); Overload;
  begin
    id:= copyParam.getId;
    idSetFlag:= copyParam.isSetIDAttribute;
    valSetFlag:= copyParam.isSetValue;
    constSetFlag:= copyParam.isSetConstant;
    nameSetFlag:= copyParam.isSetName;
    name:= copyParam.getName;
    value:= copyParam.getValue;
  end;
  function SBMLparameter.getValue():double;
  begin
    Result:= value;
  end;
  procedure SBMLparameter.setValue(val: double);
  begin
    value:= val;
    valSetFlag:= true;
  end;
  function SBMLparameter.isSetValue(): boolean;
  begin
    Result:= valSetFlag;
  end;
  function SBMLparameter.getConstant(): boolean; // true if param constant.
  begin
    Result:= constSetFlag;
  end;
  procedure SBMLparameter.setConstant(val: boolean);
  begin
    constSetFlag:= val;
  end;
  function SBMLparameter.isSetConstant(): boolean;
  begin
    Result:= constSetFlag;
  end;
  function SBMLparameter.getId(): String;
  begin
    Result:= id;
  end;
  procedure SBMLparameter.setId(val: String);
  begin
    id:= val;
    idSetFlag:= true;
  end;
  function SBMLparameter.isSetIDAttribute(): boolean;
  begin
    Result:= idSetFlag;
  end;
  function SBMLparameter.getName(): String;
  begin
    Result:= name;
  end;
  procedure SBMLparameter.setName(val: String);
  begin
    name:= val;
    nameSetFlag:= true;
  end;
  function SBMLparameter.isSetName(): boolean;
  begin
    Result:= nameSetFlag;
  end;

  // ****************************************
  constructor SBMLcompartment.create(); Overload;
    begin
      id:= '';
      size:= -1;
      idSetFlag:= false;
      nameSetFlag:= false;
      sizeSetFlag:= false;
      volSetFlag:= false;
      constSetFlag:= false;
    end;
  constructor SBMLcompartment.create(val:String); Overload;
    begin
      id:= val;
      size:= -1;
      idSetFlag:= true;
      nameSetFlag:= false;
      sizeSetFlag:= false;
      volSetFlag:= false;
      constSetFlag:= false;
    end;

  constructor SBMLcompartment.create(copyComp:SBMLcompartment); Overload;
  begin
    id:= copyComp.getID();
    size:= copyComp.getSize();
    name:= copyComp.getName();
    idSetFlag:= true;
    nameSetFlag:= copyComp.isSetName();
    sizeSetFlag:= copyComp.isSetSize();
    volSetFlag:= copyComp.isSetVolume();
    constSetFlag:= copyComp.isSetConstant();

  end;
  function SBMLcompartment.getConstant(): boolean; // true if compartment size constant
  begin
      Result:= constSetFlag;
  end;
  procedure SBMLcompartment.setConstant(val: boolean);
  begin
    constSetFlag:= val;
  end;
  function SBMLcompartment.isSetConstant(): boolean;
  begin
    Result:= constSetFlag;
  end;

  function SBMLcompartment.getSize(): double;
  begin
    Result:= size;
  end;
  procedure SBMLcompartment.setSize(val:double);
  begin
    size:= val;
    sizeSetFlag:= true;
  end;
  function SBMLcompartment.isSetSize(): boolean;
  begin
    Result:= sizeSetFlag;
  end;
  function SBMLcompartment.getVolume(): double; // level 1 only, otherwise returns 'size'
  begin
    Result:= size;
  end;
  procedure SBMLcompartment.setVolume(val:double); // level 1 only
  begin
    size:= val;
  end;
  function SBMLcompartment.isSetVolume():boolean;//  true if the "volume" attribute ("size" in Level 2 and above) of this Compartment object is set.
  begin
    // ??? necessary??
    Result:= sizeSetFlag;
  end;
  function SBMLcompartment.getID(): String;
  begin
    Result:= id;
  end;
  procedure SBMLcompartment.setID(val:String);
  begin
    id:= val;
    idSetFLag:= true;
  end;
  function SBMLcompartment.isSetIdAttribute(): boolean;
  begin
    Result:= idSetFlag;
  end;
  function SBMLcompartment.getName(): String;
  begin
    Result:= name;
  end;
  procedure SBMLcompartment.setName(val:String);
  begin
    name:= val;
    nameSetFlag:= true;
  end;
  function SBMLcompartment.isSetName(): boolean;
  begin
    Result:= nameSetFlag;
  end;


  // **********************************************************************
  constructor SBMLkineticlaw.create(); Overload;
  begin
    id:= '';
    formula:= '';
    name:= '';
    nameFlagSet:= false;
    SetLength(paramIds,0);
    numParams:= 0;
  end;
   constructor SBMLkineticlaw.create(newId: String; newFormula: String; paramArr: array of String); Overload;
   begin
     id:= newId;
     formula:= newFormula;
     name:= '';
     nameFlagSet:= false;
     paramIds:= Copy(paramArr, 0, Length(paramArr));
     numParams:= Length(paramArr);
   end;
   function SBMLkineticlaw.getId(): String;
   begin
     Result:= id;
   end;
   procedure SBMLkineticlaw.setId(val: String);
   begin
     id:= val;
   end;
   function SBMLkineticlaw.getName():String;
   begin
     Result:= name;
   end;
   procedure SBMLkineticlaw.setName(val: String);
   begin
     name:= val;
   end;
   function SBMLkineticlaw.isNameFlagSet():boolean;
   begin
     Result:= nameFlagSet;
   end;
   function SBMLkineticlaw.getFormula(): String;
   begin
     Result:= formula;
   end;
   procedure SBMLkineticlaw.setFormula(newFormula: String);
   begin
     formula:= newFormula;
   end;
   function SBMLkineticlaw.getNumParameters(): integer;
   begin
     Result:= numParams;
   end;
   procedure SBMLkineticlaw.setNumParameters(i:integer);
   begin
     numParams:= i;
   end;

   function SBMLkineticlaw.addParameter(param:String): String;  // SBMLparameter;
    var len: integer;
   begin
    len:= Length(paramIds);
    SetLength(paramIds,len+1);  // add new paramID to array
    paramIds[len]:= param;
    console.log('kineticLaw.addParameter: ',param);
   end;
   function SBMLkineticlaw.getParameter(n: integer): String; // SBMLparameter;
   begin
     Result:= paramIds[n];
   end;
  // function removeParameter(n: integer): String; // SBMLparameter;

 // **************************************************************
 constructor SBMLReaction.create(id:String; prod: array of String; reactant: array of String); // remove at some point
 begin
  rxnID:= id;
  rxnIdFlagSet:= true;
  rxnName:= '';
  rxnNameFlagSet:= false;
  compartment:= '';
  compartmentFlagSet:= false;
  products:= Copy(prod, 0, Length(prod));  // list of species
  reactants:= Copy(reactant, 0, Length(reactant)); // list of species
  numbProducts:= Length(prod);
  numbReactants:= Length(reactant);
  kineticlaw:= nil;
  kineticLawFlagSet:= false;
  kineticLawStr:= '';
 end;

 constructor SBMLReaction.create(id:String; prod: array of SBMLspeciesreference; reactant: array of SBMLspeciesreference);
 begin
  rxnID:= id;
  rxnIdFlagSet:= true;
  rxnName:= '';
  rxnNameFlagSet:= false;
  compartment:= '';
  compartmentFlagSet:= false;
  console.log('Create rxn: number of products:',Length(prod));
  rxnproducts:= Copy(prod, 0, Length(prod));  // list of species
  rxnreactants:= Copy(reactant, 0, Length(reactant)); // list of species
  numbProducts:= Length(prod);
  numbReactants:= Length(reactant);
  kineticlaw:= nil;
  kineticLawFlagSet:= false;
  kineticLawStr:= '';
 end;

 constructor SBMLReaction.create(id:String);
 begin
  rxnID:= id;
  rxnIdFlagSet:= true;
  rxnName:= '';
  rxnNameFlagSet:= false;
  compartment:= '';
  compartmentFlagSet:= false;
  products:= nil;
  reactants:= nil;
  numbProducts:= 0;
  numbReactants:= 0;
  kineticLawStr:= '';
  kineticlaw:= nil;
  kineticLawFlagSet:= false;
 end;

 function SBMLReaction.getCompartment(): String;
 begin
   Result:= compartment;
 end;
 procedure SBMLReaction.setCompartment(val:String);
 begin
   compartment:= val;
   compartmentFlagSet:= true;
 end;
 function SBMLReaction.isSetCompartment(): boolean;
 begin
   Result:= compartmentFlagSet;
 end;

 procedure SBMLReaction.setProducts(prod: array of string);
 begin
  products:= Copy(prod, 0, Length(prod));  // list of species
 end;

 procedure SBMLReaction.setReactants(reactant: array of string);
 begin
  reactants:= Copy(reactant, 0, Length(reactant)); // list of species
 end;

 function SBMLReaction.getID(): String;
 begin
  Result:=rxnID;
 end;
 function SBMLReaction.getProducts(): array of String;
 begin
  Result:= products;
 end;
 function SBMLReaction.getReactants(): array of String;
 begin
  Result:= reactants;
 end;

 function SBMLReaction.getrxnProducts(): array of SBMLspeciesreference;   // Get products (species) for reaction
 begin
 //console.log('GetrxnProducts: number of products:',Length(rxnproducts));
  Result:= rxnproducts;
 end;
 function SBMLReaction.getrxnReactants(): array of SBMLspeciesreference;  // Get reactants (species) for reaction
 begin
 //console.log('GetrxnReactants: number of reactants:',Length(rxnreactants));
  Result:= rxnreactants;
 end;
 function SBMLReaction.getProduct(index: integer):SBMLspeciesreference;
 begin
   Result:= rxnproducts[index];
 end;
 procedure SBMLReaction.setrxnProducts(prod: array of SBMLspeciesreference);
 begin
 //console.log('SetrxnProducts: number of products:',Length(prod));
  rxnproducts:= Copy(prod, 0, Length(prod));
 // console.log('SetrxnProducts: After copy:number of products:',Length(rxnproducts));
 end;
 procedure SBMLReaction.setrxnReactants(reactant: array of SBMLspeciesreference);
 begin
  rxnreactants:= Copy(reactant, 0, Length(reactant));
 end;

 function SBMLReaction.getNumReactants(): integer;
 begin
  Result:= numbReactants;
 end;
 function SBMLReaction.getNumProducts(): integer;
 begin
  Result:= numbProducts;
 end;

 function SBMLReaction.getKineticLaw(): SBMLkineticlaw;
 begin
   Result:= kineticlaw;
 end;
 procedure SBMLReaction.setKineticLaw(law: SBMLkineticlaw);
 begin
   kineticlaw:= law;
   kineticLawFlagSet:= true;
 end;
 function SBMLReaction.isSetKineticLaw(): boolean;
 begin
   Result:= kineticLawFlagSet;
 end;
//  function getReversible(): boolean;
//  procedure setReversible();
//  function isSetReversible(): boolean;
//  procedure unsetReversible();
 procedure SBMLReaction.setId(id: String);
 begin
    rxnId:= id;
    rxnIdFlagSet:= true;
 end;
 function SBMLReaction.isSetIdAttribute(): boolean;
 begin
   Result:= rxnIdFlagSet;
 end;
 function SBMLReaction.getName(): String;
  begin
    Result:= rxnName;
  end;
 procedure SBMLReaction.setName(name: String);
 begin
   rxnName:= name;
   rxnNameFlagSet:= true;
 end;
 function SBMLReaction.isSetName(): boolean;
 begin
   Result:= rxnNameFlagSet;
 end;

end.
