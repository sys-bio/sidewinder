unit uModel;

interface
uses Web, JS, uSBMLClasses, uSBMLClasses.rule;

type

{ Define a procedural type used for event handling }
    TPingEvent = procedure() of object;

//  **************************************************************
 // Reads SBML model parsed by libsbml.js module
 TModel = class
   private
    errors, numReactions: integer;  // SBML reading errors, number of rxns in model.
    numbPlugins: integer; // Not used
    numSpecies, numParams, numCompartments: integer; // number of species,params,comps in model
    numEvents, numRules, numFuncDefs: integer;  // Currently not used.
    annotationStr: String;    // optional
    sbmlreactions: array of SBMLReaction; // list of reactions
    modelSpecies: array of TSBMLSpecies;
    modelComps: array of SBMLcompartment;
    modelParams: array of SBMLparameter;
    modelRules: array of TSBMLRule;  // optional

    FPing: TPingEvent; // Used to send sbml info listener once asynchronous read done.

  public
   constructor create();

   procedure setAnnotationStr(annotate:String);
   function  getAnnotationStr():String;
   procedure setNumReactions (rnxNumb : integer); // TODO, remove, just increment as rxn added.
   function  getNumReactions : integer;
   procedure addSBMLReaction(rxnid:String; prods: array of String; prodStoich: array of double;
        reactants: array of String; reactantsStoich: array of double; kinetic: String);

   function  getParamNumb(): integer;
   function  getSBMLparameter(i:integer): SBMLparameter;
   function  getSBMLparameterAr(): array of SBMLparameter;
   procedure addSBMLparameter(newParam: SBMLparameter);
   function  getSBMLmodelRules():array of TSBMLrule;
   procedure addSBMLrule( newR: TSBMLrule);
   function  getSpeciesNumb(): integer;
   procedure addSBMLspecies(newSpecies: TSBMLSpecies);
   function  getSBMLspecies(i:integer): TSBMLSpecies; overload;
   function  getSBMLspecies(spId:string): TSBMLSpecies; overload;
   function  getSBMLspeciesAr(): array of TSBMLSpecies;  // All species
   function  getSBML_BC_SpeciesAr(): array of TSBMLSpecies; // Just species listed as boundary condition for a species
   function  getSBMLdynamicSpeciesAr(): array of TSBMLSpecies;  // return species that are not a bc or amt (not conc) is constant.
   function  getCompNumb(): integer;
   function  getSBMLcompartmentsArr(): array of SBMLcompartment;
   function  getSBMLcompartment(i:integer): SBMLcompartment;
   procedure addSBMLcompartment(newComp: SBMLcompartment);

   function  getReactions(): array of SBMLReaction;

   property OnPing: TPingEvent read FPing write FPing;
    { Triggers the event if anything is registered }
   procedure TriggerEvent();

 end;

implementation

constructor TModel.create();
begin
    errors:=0;
    numReactions:= -1;
    numSpecies:= 0; numParams:= 0; numCompartments:= 0;
    numEvents:= 0; numRules:= 0; numFuncDefs:=0;
    modelRules:= nil;
end;


procedure TModel.setNumReactions (rnxNumb : integer);
begin
  numReactions:= rnxNumb;
end;


function TModel.getNumReactions () : integer;
begin
   result:= numReactions;
end;


procedure TModel.TriggerEvent();
 begin
   { Call the registerd event only if there is a listener }
   if Assigned(FPing) then
     FPing();
 end;

 procedure TModel.setAnnotationStr(annotate:String);
 begin
  annotationStr:=annotate;
 end;

 function TModel.getAnnotationStr():String;
 begin
   Result:= annotationStr;
 end;

 procedure TModel.addSBMLReaction(rxnid:String; prods: array of String; prodStoich: array of double;
        reactants: array of String; reactantsStoich: array of double; kinetic: String);

 var
 newRxn: SBMLReaction;
 len: integer;
 p, r: array of TSBMLSpeciesReference;
  I: Integer;
  paramArray: array of String;
 begin
 //  console.log('In add Reaction', rxnid, reactants[0], prods[0]);
 setlength(p, Length(prods));
 paramArray:= ['nothing', 'empty'];
 for I := 0 to Length(prods)-1 do
  begin
     p[I]:= TSBMLSpeciesReference.create(prods[I],prodStoich[I]);
  end;

  setlength(r, Length(reactants));

 for I := 0 to Length(reactants)-1 do
  begin
     r[I]:= TSBMLSpeciesReference.create(reactants[I],reactantsStoich[I]);
  end;

   newRxn:= SBMLReaction.create(rxnid, p, r);
   newRxn.kineticLawStr:= kinetic;   // Get rid of.....
   //newId: String; newFormula: String; paramArr: array of String
   newRxn.setKineticLaw(SBMLkineticLaw.create('dummy', kinetic,paramArray));
   len:= Length(SBMLreactions);
   SetLength(SBMLreactions,len+1);    // Add new reaction to array
   sbmlreactions[len]:= newRxn;
 end;

 function TModel.getReactions(): array of SBMLReaction;
 begin
    Result:= SBMLreactions;
 end;

 function TModel.getSpeciesNumb(): integer;
 begin
   Result:= numSpecies;
 end;

 procedure TModel.addSBMLspecies(newSpecies: TSBMLSpecies);
 var len:integer;
 begin
  len:= Length(modelSpecies);
  SetLength(modelSpecies,len+1);  // add new species to array
  modelSpecies[len]:= TSBMLSpecies.create(newSpecies);
 end;

 function TModel.getSBMLspeciesAr(): array of TSBMLSpecies;
 begin
   Result:= modelSpecies;
 end;

 function TModel.getSBMLspecies(i:integer): TSBMLSpecies; overload;
 begin
  //console.log('get name of species:',modelSpecies[i].getID() );
   Result:= modelSpecies[i];
 end;

 function TModel.getSBMLspecies(spId:string): TSBMLSpecies; overload;
 var i: integer;
 begin
   for i := 0 to Length(self.modelSpecies)-1 do
     begin
       if self.modelSpecies[i].getId = spId then
         Result := self.modelSpecies[i]
       else Result:= nil;

     end;
 end;

 // Just species listed as boundary condition for a species
 function TModel.getSBML_BC_SpeciesAr(): array of TSBMLSpecies;
 var i, j: integer;
 begin
  j := 0;
  for i := 0 to Length(self.modelSpecies)-1 do
     begin
       if self.modelSpecies[i].getBoundaryCondition() then
       begin
         Result[j] := TSBMLSpecies.create(self.modelSpecies[i]);
         inc(j);
       end;

     end;
 end;

 // return species that are not a bc or amt (not conc) is constant.
 function TModel.getSBMLdynamicSpeciesAr(): array of TSBMLSpecies;
 var i, j: integer;
 begin
  j := 0;
  for i := 0 to Length(self.modelSpecies)-1 do
     begin
       if Not(self.modelSpecies[i].getBoundaryCondition) and Not( self.modelSpecies[i].getConstant) then
       begin
         Result[j] := TSBMLSpecies.create(self.modelSpecies[i]);
         inc(j);
       end;

     end;
 end;


 function TModel.getSBMLcompartmentsArr(): array of SBMLcompartment;
 begin
   Result := self.modelComps;
 end;
 function TModel.getSBMLcompartment(i:integer): SBMLcompartment;
 begin
   Result:= modelComps[i];
 end;

 function TModel.getCompNumb(): integer;
 begin
   Result:= numCompartments;
 end;

 procedure TModel.addSBMLcompartment(newComp: SBMLcompartment);
 var len:integer;
 begin
   len:= Length(modelComps);
   modelComps[len]:= SBMLcompartment.create(newComp);
 end;

 function TModel.getParamNumb(): integer;
 begin
   Result:= numParams;
 end;
 function TModel.getSBMLparameter(i:integer): SBMLparameter;
 begin
    Result:= modelParams[i];
 end;

 function TModel.getSBMLparameterAr(): array of SBMLparameter;
 begin
   Result:= modelParams;
 end;
 procedure TModel.addSBMLparameter(newParam: SBMLparameter);
 var len:integer;
 begin
  len:= Length(modelParams);
  modelParams[len]:= SBMLparameter.create(newParam);
 // console.log('addSBMLparameter: ',modelParams[len].getID());
 // console.log(' ... addSBMLparameter: Value: ',modelParams[len].getValue());
  if newParam.isSetName() then console.log('addSBMLparameter: ',newParam.getname());

 end;

 procedure TModel.addSBMLrule( newR: TSBMLrule);
 var len:integer;
 begin
   len:= Length(self.modelRules);
   self.modelRules[len]:= TSBMLrule.create(newR);
 end;

 function TModel.getSBMLmodelRules():array of TSBMLrule;
 begin
   Result:= self.modelRules;
 end;
end.   // unit
