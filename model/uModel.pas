unit uModel;

interface
uses Web, JS, uSBMLClasses, uSBMLClasses.rule, uSidewinderTypes;

type

{ SBML model has finished loading }
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

    // Arrays of Double used by ODE integrator, keep array of strings for mapping:
    s_Vals: array of Double; // Changes, one to one correlation: s_Vals[n] <=> s_Names[n]
    s_Names: array of String; // Use species ID as name
    s_NameValAr: TVarNameValAr; // List of species, init val pairs.
    p_Vals: array of Double; // Holds current value, Changes, Includes compartments and boundary species.
    p_Names: array of String;//Same size as p_Vals,
    p_NameValAr: TVarNameValAr;// Holds current param name/value, changes, Includes compartments and boundary species.
    FPing: TPingEvent;// Used to send sbml info to listener once asynchronous read done.
  //  FPingAgain: TModelUpdateEvent;// Used to send sbml info to another listener once model updated.
    procedure fillSpeciesArray();
    procedure fillParameterArray();

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

   function getS_Names(): array of String;
   function getS_Vals(): array of Double;
   function getP_Names(): array of String;
   function getP_Vals(): array of Double;
   function getP_Val(pos: integer): Double;
   function setP_Val(pos: integer; newVal: Double): Boolean;
   procedure changeParamVal(pos: Integer; newVal: Double);
   function  getReactions(): array of SBMLReaction;

   property OnPing: TPingEvent read FPing write FPing;
    { Triggers the event if anything is registered }
   procedure SBML_LoadedEvent(); // SBML model loaded.


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


procedure TModel.SBML_LoadedEvent();
 begin
   self.fillSpeciesArray;
   self.fillParameterArray;
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
   newRxn.kineticLawStr:= kinetic;   // Get rid of.....?
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

 // Get initial vals for Species from SBML model
procedure TModel.fillSpeciesArray();
var
  i: Integer;
  spAr: array of TSBMLSpecies;
begin
  if self.getSpeciesNumb() > 0 then
    begin
      spAr := self.getSBMLdynamicSpeciesAr; // Do not include BCs and consts
      self.s_NameValAr := TVarNameValAr.create();
      for i := 0 to Length(spAr) - 1 do
        begin
          if spAr[i].isSetInitialAmount() then
          begin
            self.s_NameValAr.add( TVarNameVal.create(spAr[i].getID(),spAr[i].getInitialAmount));
          end
          else if spAr[i].isSetInitialConcentration() then
          begin
            self.s_NameValAr.add(TVarNameVal.create(spAr[i].getID(),spAr[i].getInitialConcentration));
          end;

        end;
      self.s_Names := self.s_NameValAr.getNameAr();
      self.s_Vals := self.s_NameValAr.getValAr();
    end;
end;

procedure TModel.fillParameterArray();
 var i, paramLen : integer;
     currNameVal: TVarNameVal;

begin
   // Get parameter and compartment names and combine into one array, pVals[]:
  self.p_NameValAr := TVarNameValAr.create();
  for i := 0 to Length(self.getSBMLparameterAr())-1 do
      begin
      currNameVal := TVarNameVal.create;
      if self.getSBMLparameter(i).isSetIDAttribute() then
         begin
         currNameVal.setId(self.getSBMLparameter(i).getId());
         // Get initial value for each param:
         if self.getSBMLparameter(i).isSetValue() then
           begin
           currNameVal.setVal(self.getSBMLparameter(i).getValue());
           end
         else currNameVal.setVal(0);

         end
      else
       begin
         currNameVal.setId('');
         currNameVal.setVal(0);
       end;
      self.p_NameValAr.add(currNameVal);
      end;
    // Get compartments:
  paramLen := Length(self.p_Names);
  for i := 0 to self.getCompNumb()-1 do
       begin
       currNameVal := TVarNameVal.create();
       if self.getSBMLcompartment(i).isSetIdAttribute() then
         currNameVal.setId(self.getSBMLcompartment(i).getID())
       else currNameVal.setId(self.getSBMLcompartment(i).getName());
       // Get Size/Vols of compartments:
       if self.getSBMLcompartment(i).isSetSize() then
         currNameVal.setVal(self.getSBMLcompartment(i).getSize())
       else if self.getSBMLcompartment(i).isSetVolume() then
            currNameVal.setVal(self.getSBMLcompartment(i).getVolume())
          else currNameVal.setVal(1);  // Default value

       self.p_NameValAr.Add(currNameVal);
       end;

     // now look at species, put species boundary conditions and species constants in parameter list.
  for i := 0 to Length(self.getSBMLspeciesAr())-1 do
      begin

      if self.getSBMLspeciesAr()[i].getBoundaryCondition or self.getSBMLspeciesAr()[i].getConstant then
        begin
          currNameVal := TVarNameVal.create();
          paramLen := Length(p_Names);
          currNameVal.setId(self.getSBMLspeciesAr()[i].getId());
          if self.getSBMLspeciesAr()[i].isSetInitialAmount() then
          begin
            currNameVal.setVal(self.getSBMLspeciesAr()[i].getInitialAmount());
          end
          else if self.getSBMLspeciesAr()[i].isSetInitialConcentration() then
            begin
              currNameVal.setVal(self.getSBMLspeciesAr()[i].getInitialConcentration());
            end
            else currNameVal.setVal(0);
          self.p_NameValAr.Add(currNameVal);
        end;

      end;
  self.p_Names := self.p_NameValAr.getNameAr();
  self.p_Vals := self.p_NameValAr.getValAr();

end;


function TModel.getS_Names(): array of String;
begin
  Result := self.s_Names;
end;

function TModel.getS_Vals(): array of Double;
begin
  Result := self.s_Vals;
end;

function TModel.getP_Names(): array of String;
begin
  Result := self.p_Names;
end;

function TModel.getP_Vals(): array of Double;
begin
  Result := self.p_Vals;
end;

function TModel.getP_Val(pos: integer): Double;
begin
  Result := self.p_Vals[pos];
end;

function TModel.setP_Val(pos: integer; newVal: Double): Boolean;
begin
  if (Length(self.p_Vals) > (pos + 1)) and (pos >= 0) then
  begin
    self.p_Vals[pos] := newVal;
    Result := true;
  end
  else  Result := false;
end;

procedure TModel.changeParamVal(pos: Integer; newVal: Double);
begin
  self.p_Vals[pos] := newVal;

  // Other viewers update ?
end;

end.   // unit