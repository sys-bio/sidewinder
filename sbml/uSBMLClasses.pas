unit uSBMLClasses;
interface
 uses Web, JS;
type

 TSBMLSpecies = class
   private
   initAmt, initConc: double;
   sid, name: String;
   compSid: String; // Compartment sid
   boundCond: boolean;
   amtConst: boolean; // if amt constant.
   hasOnlySubstanceUnits: boolean;   // 4.6.5 The hasOnlySubstanceUnits attribute
   initAmtFlag, initConcFlag: boolean;
   compFlag, boundCondFlag, constFlag: boolean;
   idFlag, nameFlag: boolean;

   public
   constructor create(); Overload;
   constructor create(id:String); Overload;
   constructor create(copySp: TSBMLSpecies); Overload;   // copy
   function getInitialAmount(): double;
   procedure setInitialAmount(amt:double);
   function isSetInitialAmount(): boolean;
   function getInitialConcentration(): double;
   procedure setInitialConcentration(amt:double);
   function isSetInitialConcentration():boolean;
   function getCompartment(): String;  // get compartment species is in, if set
   procedure setCompartment(sid:String);// sid identifier of a Compartment object defined elsewhere in this Model
   function isSetCompartment():boolean;
   function getBoundaryCondition():boolean; // true if this Species' "boundaryCondition" attribute value is true, false otherwise
   procedure setBoundaryCondition(val:boolean);
  // function isSetBoundaryCondition(): boolean; // true if this Species object's "boundaryCondition" attribute is set.
   function getConstant(): boolean; // true if this Species's "constant" attribute value is true, false otherwise
   procedure setConstant(val:boolean);
   function getHasOnlySubstanceUnits(): boolean;
   procedure setHasOnlySubstanceUnits(newHasOnly: boolean);
   function getID(): String;
   procedure setID(id:String);
   function isSetIdAttribute(): boolean;
   function getName(): String;
   procedure setName(val: String);
   function isSetName(): boolean;
 end;

 TSBMLcompartment = class
 private
    id, name: String;
    size: double; // vol returns size.
    idSetFlag, nameSetFlag: boolean;
    sizeSetFlag, volSetFlag: boolean;
    constSetFlag: boolean;
 public
    constructor create(); Overload;
    constructor create(val:String); Overload;
    constructor create(copyComp:TSBMLcompartment); Overload;

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


 TSBMLparameter = class
 private
    value: double;
    valSetFlag, constSetFlag: boolean;
    id, name: String;
    idSetFlag, nameSetFlag: boolean;

 public
    constructor create(); Overload;
    constructor create(paramId: String); Overload;
    constructor create(copyParam:TSBMLparameter); Overload;
    function getValue():double;
    procedure setValue(val: double);
    procedure unSetValueFlag();
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

// SBMLlocalParameter not implimented, same methods as TSBMLParameter.

 TSBMLSpeciesReference = class
 private
    stoichValue: double;  // A positive value indicates the species is effectively a
    // product and a negative value indicates the species is effectively a reactant
    spConst: boolean;  // Stoich constant ?
    id, species: String;
   // name: String;
    idSetFlag, speciesSetFlag: boolean;
    //nameSetFlag: boolean;
    stoichSetFlag: boolean;
 public
    constructor create(); Overload;
    constructor create(newId:String); Overload;
    constructor create(newId:String; stoich: double); Overload;
    function getSpecies():String;
    procedure setSpecies(val:String);
    function isSetSpecies(): boolean;
    function getId(): String;
    procedure setId( val: String);
    function isSetIdAttribute(): boolean;
    function getConstant(): boolean; // check if stoich is constant.
    procedure setConstant(val: boolean);
//    function isSetConstant(): boolean;
    function getStoichiometry(): double;
    procedure setStoichiometry(val: double);
    function isSetStoichiometry(): boolean;
 //   function getName(): String;
 //   procedure setName(val: String);
 //   function isSetName(): boolean;

 end;


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
  reversible: boolean;
  kineticlaw: SBMLkineticlaw;
  kineticLawFlagSet: boolean;
  products: array of String;   // remove at some point
  reactants: array of String;  // remove at "

  numbProducts, numbReactants: integer;
  rxnProducts: array of TSBMLSpeciesReference;
  rxnReactants: array of TSBMLSpeciesReference;

 public
  kineticLawStr: String;  // temporary, use SBMLkineticlaw class later
  constructor create(id:String; prod: array of String; reactant: array of String); Overload; // remove at some point
  constructor create(id:String; prod: array of TSBMLSpeciesReference; reactant: array of TSBMLSpeciesReference); Overload;
  constructor create(id:String); Overload;
  function getID():String;   // from model.getReaction(i).getId()
  function getProducts(): array of String;   // remove at some pointGet products (species) for reaction
  function getReactants(): array of String;  // remove at some pointGet reactants (species) for reaction
  function getProduct(index: integer):TSBMLSpeciesReference;
  function getReactant(index: integer): TSBMLSpeciesReference;
  function getCompartment(): String;
  procedure setCompartment(val:String);
  function isSetCompartment(): boolean;
  function getReversible(): boolean;
  procedure setReversible(r: boolean);
  procedure setProducts(prod: array of String);  // remove at some point
  procedure setReactants(reactant: array of String);  // remove at some point
  function getrxnProducts(): array of TSBMLSpeciesReference;   // Get products (species) for reaction
  function getrxnReactants(): array of TSBMLSpeciesReference;  // Get reactants (species) for reaction
  procedure setrxnProducts(prod: array of TSBMLSpeciesReference);
  procedure setrxnReactants(reactant: array of TSBMLSpeciesReference);

  function getNumReactants(): integer;
  function getNumProducts(): integer;
 
  function getKineticLaw(): SBMLkineticlaw;
  procedure setKineticLaw(law: SBMLkineticlaw);
  function isSetKineticLaw(): boolean;
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
  constructor TSBMLSpecies.create(); Overload;
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
    self.hasOnlySubstanceUnits := false;
  end;

  constructor TSBMLSpecies.create(id:String); Overload;
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
    self.hasOnlySubstanceUnits := false;

  end;
  constructor TSBMLSpecies.create(copySp: TSBMLSpecies); Overload;
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
    self.hasOnlySubstanceUnits := copySp.getHasOnlySubstanceUnits();



  end;
   function TSBMLSpecies.getInitialAmount(): double;
   begin
     Result:= initAmt;
   end;
   procedure TSBMLSpecies.setInitialAmount(amt:double);
   begin
     initAmt:=amt;
     initAmtFlag:= true;
   end;
   function TSBMLSpecies.isSetInitialAmount(): boolean;
   begin
     Result:=initAmtFlag;
   end;
   function TSBMLSpecies.getInitialConcentration(): double;
   begin
     Result:= initConc;
   end;
   procedure TSBMLSpecies.setInitialConcentration(amt:double);
   begin
     initConc:= amt;
     initConcFlag:= true;
   end;
   function TSBMLSpecies.isSetInitialConcentration():boolean;
   begin
     Result:= initConcFlag;
   end;
   function TSBMLSpecies.getCompartment(): String;
   begin
     Result:= compSid;
   end;
   procedure TSBMLSpecies.setCompartment(sid:String);// sid identifier of a Compartment object defined elsewhere in this Model
   begin
     compSid:= sid;
     compFlag:= true;
   end;
   function TSBMLSpecies.isSetCompartment():boolean;
   begin
     Result:= compFlag;
   end;
   function TSBMLSpecies.getBoundaryCondition():boolean; // true if this Species' "boundaryCondition" attribute value is true, false otherwise
   begin
     Result:= boundCond;
   end;
    procedure TSBMLSpecies.setBoundaryCondition(val:boolean);
    begin
      boundCond:= val;
    end;
   //function TSBMLSpecies.isSetBoundaryCondition(): boolean; // true if this Species object's "boundaryCondition" attribute is set.
   function TSBMLSpecies.getConstant():boolean; // true if this Species's "constant" attribute value is true, false otherwise
   begin
     Result:= constFlag;
   end;
   procedure TSBMLSpecies.setConstant(val:boolean);
   begin
     constFlag:= val;
   end;
  // function TSBMLSpecies.isSetConstant():boolean;
   function TSBMLSpecies.getHasOnlySubstanceUnits(): boolean;
   begin
     Result := self.hasOnlySubstanceUnits;
   end;
   procedure TSBMLSpecies.setHasOnlySubstanceUnits(newHasOnly: boolean);
   begin
     self.hasOnlySubstanceUnits := newHasOnly;
   end;

   function TSBMLSpecies.getID(): String;
   begin
     Result:= sid;
   end;
   procedure TSBMLSpecies.setID(id:String);
   begin
     sid:= id;
     idFlag:= true;
   end;
   function TSBMLSpecies.isSetIdAttribute(): boolean;
   begin
     Result:= idFlag;
   end;
   function TSBMLSpecies.getName(): String;
   begin
     Result:= name;
   end;
   procedure TSBMLSpecies.setName(val: String);
   begin
     name:= val;
     nameFlag:= true;
   end;
   function TSBMLSpecies.isSetName(): boolean;
   begin
     Result:= nameFlag;
   end;


  // ************************************************

  constructor TSBMLSpeciesReference.create(); Overload;
  begin
     id:= '';
     species:= '' ;
     stoichValue:= 1;  // Default Stoich coefficient is '1'
     idSetFlag:= false;
     speciesSetFlag:= false;
     stoichSetFlag:= false;
     self.spConst := true;  // default
  end;
  constructor TSBMLSpeciesReference.create(newId:String); Overload;
  begin
     id:= newId;
     species:= '' ;
     stoichValue:= 1;  // Default is '1'
     idSetFlag:= true;
     speciesSetFlag:= false;
     stoichSetFlag:= false;
  end;
  constructor TSBMLSpeciesReference.create(newId:String; stoich: double); Overload;
  begin
     id:= newId;
     species:= '' ; // Cannot be the same as id
     stoichValue:= stoich;
     idSetFlag:= true;
     speciesSetFlag:= true;
     stoichSetFlag:= true;
  end;
  function TSBMLSpeciesReference.getSpecies():String;
  begin
    Result:= species;
  end;
  procedure TSBMLSpeciesReference.setSpecies(val:String);
  begin
    species:= val;
    speciesSetFlag:= true;
  end;
  function TSBMLSpeciesReference.isSetSpecies(): boolean;
  begin
    Result:= speciesSetFlag;
  end;

  function TSBMLSpeciesReference.getConstant(): boolean; // check if stoich is constant.
  begin
    Result := self.spConst;
  end;
  procedure TSBMLSpeciesReference.setConstant(val: boolean);
  begin
    self.spConst := val;
  end;

  function TSBMLSpeciesReference.getStoichiometry(): double;
  begin
    Result:= stoichValue;
  end;
  procedure TSBMLSpeciesReference.setStoichiometry(val: double);
  begin
    stoichValue:= val;
    stoichSetFlag:= true;
  end;
  function TSBMLSpeciesReference.isSetStoichiometry(): boolean;
  begin
    Result:= stoichSetFlag;
  end;
  function TSBMLSpeciesReference.getId(): String;
  begin
    Result:= id;
  end;
  procedure TSBMLSpeciesReference.setId( val: String);
  begin
    id:= val;
    idSetFlag:= true;
  end;
  function TSBMLSpeciesReference.isSetIdAttribute(): boolean;
  begin
    Result:= idSetFlag;
  end;

  //   ******************************************
  constructor TSBMLparameter.create(); Overload;
  begin
    id:= '';
    value:= 0;
    idSetFlag:= true;
    valSetFlag:= false;
    constSetFlag:= false;
    nameSetFlag:= false;
  end;
  constructor TSBMLparameter.create(paramId:String); Overload;
  begin
    id:= paramId;
    value:= 0;
    idSetFlag:= true;
    valSetFlag:= false;
    constSetFlag:= false;
    nameSetFlag:= false;
  end;

  constructor TSBMLparameter.create(copyParam:TSBMLparameter); Overload;
  begin
    id:= copyParam.getId;
    idSetFlag:= copyParam.isSetIDAttribute;
    valSetFlag:= copyParam.isSetValue;
    constSetFlag:= copyParam.isSetConstant;
    nameSetFlag:= copyParam.isSetName;
    name:= copyParam.getName;
    value:= copyParam.getValue;
  end;
  function TSBMLparameter.getValue():double;
  begin
    Result:= self.value;
  end;
  procedure TSBMLparameter.setValue(val: double);
  begin
    self.value:= val;
    self.valSetFlag:= true;
  end;
  function TSBMLparameter.isSetValue(): boolean;
  begin
    Result:= self.valSetFlag;
  end;
  procedure TSBMLparameter.unSetValueFlag();
  begin
    self.valSetFlag:= false;
  end;
  function TSBMLparameter.getConstant(): boolean; // true if param constant.
  begin
    Result:= self.constSetFlag;
  end;
  procedure TSBMLparameter.setConstant(val: boolean);
  begin
    self.constSetFlag:= val;
  end;
  function TSBMLparameter.isSetConstant(): boolean;
  begin
    Result:= self.constSetFlag;
  end;
  function TSBMLparameter.getId(): String;
  begin
    Result:= self.id;
  end;
  procedure TSBMLparameter.setId(val: String);
  begin
    self.id:= val;
    self.idSetFlag:= true;
  end;
  function TSBMLparameter.isSetIDAttribute(): boolean;
  begin
    Result:= self.idSetFlag;
  end;
  function TSBMLparameter.getName(): String;
  begin
    Result:= self.name;
  end;
  procedure TSBMLparameter.setName(val: String);
  begin
    self.name:= val;
    self.nameSetFlag:= true;
  end;
  function TSBMLparameter.isSetName(): boolean;
  begin
    Result:= self.nameSetFlag;
  end;

  // ****************************************
  constructor TSBMLcompartment.create(); Overload;
    begin
      id:= '';
      size:= -1;
      idSetFlag:= false;
      nameSetFlag:= false;
      sizeSetFlag:= false;
      volSetFlag:= false;
      constSetFlag:= false;
    end;
  constructor TSBMLcompartment.create(val:String); Overload;
    begin
      id:= val;
      size:= -1;
      idSetFlag:= true;
      nameSetFlag:= false;
      sizeSetFlag:= false;
      volSetFlag:= false;
      constSetFlag:= false;
    end;

  constructor TSBMLcompartment.create(copyComp:TSBMLcompartment); Overload;
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
  function TSBMLcompartment.getConstant(): boolean; // true if compartment size constant
  begin
      Result:= constSetFlag;
  end;
  procedure TSBMLcompartment.setConstant(val: boolean);
  begin
    constSetFlag:= val;
  end;
  function TSBMLcompartment.isSetConstant(): boolean;
  begin
    Result:= constSetFlag;
  end;

  function TSBMLcompartment.getSize(): double;
  begin
    Result:= size;
  end;
  procedure TSBMLcompartment.setSize(val:double);
  begin
    size:= val;
    sizeSetFlag:= true;
  end;
  function TSBMLcompartment.isSetSize(): boolean;
  begin
    Result:= sizeSetFlag;
  end;
  function TSBMLcompartment.getVolume(): double; // level 1 only, otherwise returns 'size'
  begin
    Result:= size;
  end;
  procedure TSBMLcompartment.setVolume(val:double); // level 1 only
  begin
    size:= val;
  end;
  function TSBMLcompartment.isSetVolume():boolean;//  true if the "volume" attribute ("size" in Level 2 and above) of this Compartment object is set.
  begin
    // ??? necessary??
    Result:= sizeSetFlag;
  end;
  function TSBMLcompartment.getID(): String;
  begin
    Result:= id;
  end;
  procedure TSBMLcompartment.setID(val:String);
  begin
    id:= val;
    idSetFLag:= true;
  end;
  function TSBMLcompartment.isSetIdAttribute(): boolean;
  begin
    Result:= idSetFlag;
  end;
  function TSBMLcompartment.getName(): String;
  begin
    Result:= name;
  end;
  procedure TSBMLcompartment.setName(val:String);
  begin
    name:= val;
    nameSetFlag:= true;
  end;
  function TSBMLcompartment.isSetName(): boolean;
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

   function SBMLkineticlaw.addParameter(param:String): String;  // TSBMLparameter;
    var len: integer;
   begin
    len:= Length(paramIds);
    SetLength(paramIds,len+1);  // add new paramID to array
    paramIds[len]:= param;
    console.log('kineticLaw.addParameter: ',param);
   end;
   function SBMLkineticlaw.getParameter(n: integer): String; // TSBMLparameter;
   begin
     Result:= paramIds[n];
   end;
  // function removeParameter(n: integer): String; // TSBMLparameter;

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
  reversible := false;
 end;

 constructor SBMLReaction.create(id:String; prod: array of TSBMLSpeciesReference; reactant: array of TSBMLSpeciesReference);
 begin
  rxnID:= id;
  rxnIdFlagSet:= true;
  rxnName:= '';
  rxnNameFlagSet:= false;
  compartment:= '';
  compartmentFlagSet:= false;
 // console.log('Create rxn: number of products:',Length(prod));
  rxnproducts:= Copy(prod, 0, Length(prod));  // list of species
  rxnreactants:= Copy(reactant, 0, Length(reactant)); // list of species
  numbProducts:= Length(prod);
  numbReactants:= Length(reactant);
  kineticlaw:= nil;
  kineticLawFlagSet:= false;
  kineticLawStr:= '';
  self.reversible := false;  // default
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
  self.reversible := false;
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

 function SBMLReaction.getrxnProducts(): array of TSBMLSpeciesReference;   // Get products (species) for reaction
 begin
 //console.log('GetrxnProducts: number of products:',Length(rxnproducts));
  Result:= rxnproducts;
 end;
 function SBMLReaction.getrxnReactants(): array of TSBMLSpeciesReference;  // Get reactants (species) for reaction
 begin
 //console.log('GetrxnReactants: number of reactants:',Length(rxnreactants));
  Result:= rxnreactants;
 end;
 function SBMLReaction.getProduct(index: integer):TSBMLSpeciesReference;
 begin
   Result:= rxnproducts[index];
 end;
 function SBMLReaction.getReactant(index: integer):TSBMLSpeciesReference;
 begin
   Result:= rxnreactants[index];
 end;
 procedure SBMLReaction.setrxnProducts(prod: array of TSBMLSpeciesReference);
 begin
 //console.log('SetrxnProducts: number of products:',Length(prod));
  rxnproducts:= Copy(prod, 0, Length(prod));
 // console.log('SetrxnProducts: After copy:number of products:',Length(rxnproducts));
 end;
 procedure SBMLReaction.setrxnReactants(reactant: array of TSBMLSpeciesReference);
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

  function SBMLReaction.getReversible(): boolean;
  begin
    Result := self.reversible;
  end;
  procedure SBMLReaction.setReversible(r: boolean);
  begin
    self.reversible := r;
  end;

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
