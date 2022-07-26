unit uModel;

interface
uses System.SysUtils, Web, JS, System.Generics.Collections, uSBMLClasses, uSBMLClasses.rule,
     uSBMLClasses.Layout, uSBMLClasses.Render, uSidewinderTypes, uSBMLClasses.FuncDefinition;

type

{ SBML model has finished loading }
    TPingEvent = procedure() of object;

//  **************************************************************
 // Reads SBML model parsed by libsbml.js module
 TModel = class
   private
    numReactions: integer;  // number of rxns in model.
    numbPlugins: integer; // Not used
    numSpecies,{ numParams,} numCompartments: integer; // number of species,comps in model
    numEvents, numRules: integer;  // Currently not used.
    annotationStr: String;    // optional
    sbmlreactions: array of SBMLReaction; // list of reactions
    modelSpecies: array of TSBMLSpecies;
    modelComps: array of TSBMLcompartment;
    modelParams: array of TSBMLparameter;
    modelInitialAssignments: TList<TSBMLInitialAssignment>;
    modelRules: array of TSBMLRule;  // optional
    modelFuncDefList: TList<TSBMLFuncDefinition>; // optional list of functions
    modelId: String; // optional model name
    modelLayout: TSBMLLayout;
    modelRendering: TSBMLRenderInformation;
    strSBMLErrors: TList<string>; // SBML document errors

    // Arrays of Double used by ODE integrator, keep array of strings for mapping:
    s_Vals: array of Double; // Changes, one to one correlation: s_Vals[n] <=> s_Names[n]
    s_Names: array of String; // Use species ID as name
    s_NameValAr: TVarNameValList; // List of species, init val pairs.
    p_Vals: array of Double; // Holds current value, Changes, Includes compartments and boundary species.
    p_Names: array of String;//Same size as p_Vals,
    p_NameValAr: TVarNameValList;// Holds current param name/value, changes, Includes compartments and boundary species.
    FPing: TPingEvent;// Used to send sbml info to listener once asynchronous read done.
    FPing2: TPingEvent;// Used to send sbml info to listener once asynchronous read done.

    procedure fillSpeciesArray();
    procedure fillParameterArray();

  public

   constructor create();
   procedure setAnnotationStr(annotate:String);
   function  getAnnotationStr():String;
   procedure setNumReactions (rnxNumb : integer); // TODO, remove, just increment as rxn added.
   function  getNumReactions : integer;
   procedure addSBMLReaction(rxnid:String; prods: array of String; prodStoich: array of double;
             reactants: array of String; reactantsStoich: array of double;
             kineticLaw: String; newReverse: boolean); overload;
   procedure addSBMLReaction(newReaction: SBMLReaction); overload;
   function  getModelId(): string;
   procedure setModelId(newId: string);
   function  getParamNumb(): integer;  // Number of parameters in model.
   function  getSBMLparameter(i:integer): TSBMLparameter;
   function  getSBMLparameterAr(): array of TSBMLparameter;
   procedure addSBMLparameter(newParam: TSBMLparameter);
   function  isParameterIdinList(id: string): boolean;
   procedure addInitialAssignment( newInitAssign: TSBMLInitialAssignment );
   function  getNumInitialAssignments(): integer;
   function  getInitialAssignment( index: integer): TSBMLInitialAssignment;
   function  getInitialAssignmentWithSymbolId( symbolId: string ): TSBMLInitialAssignment;
   function  getSBMLmodelRules():array of TSBMLrule;
   function  getSBMLRule( index: integer ): TSBMLRule;
   function  getSBMLRuleWithVarId( varId: string ): TSBMLRule;
   procedure addSBMLrule( newR: TSBMLrule);
   //procedure setNumFuncDefs( newNum: integer );
   function  getNumFuncDefs(): integer;
   procedure addFuncDef( newFuncDef: TSBMLFuncDefinition );
   function  getFuncDef( index: integer ): TSBMLFuncDefinition;
   function  getFuncDefList(): TList<TSBMLFuncDefinition>;
   function  convertFuncDefToKineticLaw(sKineticLaw: string): string;
   function  getSpeciesNumb(): integer;
   procedure addSBMLspecies(newSpecies: TSBMLSpecies);
   function  getSBMLspecies(i:integer): TSBMLSpecies; overload;
   function  getSBMLspecies(spId:string): TSBMLSpecies; overload;
   function  getSBMLspeciesAr(): array of TSBMLSpecies;  // All species
   function  getSBML_BC_SpeciesAr(): array of TSBMLSpecies; // Just species listed as boundary condition for a species
   function  getSBMLdynamicSpeciesAr(): array of TSBMLSpecies;  // return species that are not a bc or amt (not conc) is constant.
   function  getCompNumb(): integer;
   function  getSBMLcompartmentsArr(): array of TSBMLcompartment;
   function  getSBMLcompartment(i:integer): TSBMLcompartment; overload;
   function  getSBMLcompartment(compId: string): TSBMLcompartment; overload;
   procedure addSBMLcompartment(newComp: TSBMLcompartment);
   procedure setSBMLLayout( newLayout: TSBMLLayout );
   function  getSBMLLayout(): TSBMLLayout;
   procedure setSBMLRenderInfo( newRender: TSBMLRenderInformation );
   function  getSBMLRenderInfo(): TSBMLRenderInformation;
   function  getRenderStyle(newGlyphId: string; newSpeciesId: string;
                 newGlyphType: string; newGlyphRole: string ): TSBMLRenderStyle;

   function  getS_Names(): array of String;
   function  getS_Vals(): array of Double; // remove at some point, only getS_initVals needed ?
   function  getS_initVals(): array of double;
   procedure resetS_Vals();   // reset with species initial vals. Move this to Simulation class.
   function  getP_NameValAr(): TVarNameValList;
   function  getP_Names(): array of String;
   function  getP_Vals(): array of Double;
   function  getP_Val(pos: integer): Double;
   function  setP_Val(pos: integer; newVal: Double): Boolean;
   function  getNumModelEvents(): integer;  // events not currently supported
   function  getNumPiecewiseFuncs(): integer;
   procedure changeParamVal(pos: Integer; newVal: Double);
   function  getReactions(): array of SBMLReaction;
   function  getReaction(i: integer): SBMLReaction;
   procedure addSBMLErrorStr( newErr: string );
   function  getSBMLErrorStrs(): TList<string>;
   function  getNumSBMLErrors(): integer;

   property OnPing: TPingEvent read FPing write FPing;
   property OnPing2: TPingEvent read FPing2 write FPing2;
    { Triggers the event if anything is registered }
   procedure SBML_UpdateEvent(); // SBML model updated.
   procedure testModelUpdate(); // check model update mechanism
   function printStr(): string; // serialize as string, use for testing
 end;

implementation

constructor TModel.create();
begin
    self.strSBMLErrors := TList<string>.create;
    self.modelInitialAssignments := TList<TSBMLInitialAssignment>.create;
    self.modelFuncDefList := TList<TSBMLFuncDefinition>.create;
    numReactions:= 0;
    numSpecies:= 0; numCompartments:= 0;
    numEvents:= 0; numRules:= 0;
    modelRules:= nil;
    modelId := '';
    self.modelLayout := nil;
    self.modelRendering := nil;
end;

function TModel.printStr(): string;
var
  i: Integer;
begin
  Result := '';
  Result := ' Model id: ' + self.modelId + ', Species: ';
  for i := 0 to self.getSpeciesNumb - 1 do
    Result := Result + self.getSBMLspecies(i).printStr;
  Result := Result + sLineBreak;
  Result := Result + ' Model compartments: ';
  for i := 0 to self.getCompNumb -1 do
    Result := Result + self.getSBMLcompartment(i).printStr;
  Result := Result + sLineBreak;
  Result := Result + ' Model params: ';
  for i := 0 to self.getParamNumb -1 do
    Result := Result + self.getSBMLparameter(i).printStr;
  Result := Result + sLineBreak;
  Result := Result + ' Model Rxns: ';
  for i := 0 to self.getNumReactions -1 do
    Result := Result + self.getReaction(i).printStr;
  Result := Result + sLineBreak;
  Result := Result + ' Model Initial Assignments: ';
  for i := 0 to self.getNumInitialAssignments -1 do
    Result := Result + self.getInitialAssignment(i).printStr;
  Result := Result + sLineBreak;
  Result := Result + ' Model Rules: ';
  for i := 0 to self.numRules -1 do
    Result := Result + self.getSBMLRule(i).printStr;
  Result := Result + sLineBreak;
  Result := Result + ' Model events: ' + inttostr(self.numEvents);
  Result := Result + sLineBreak;
  Result := Result + ' Model Func definitions: ';
  for i := 0 to self.getNumFuncDefs -1 do
    Result := Result + self.modelFuncDefList[i].printStr;
  Result := Result + sLineBreak + ' Model Layout: ';
  if self.modelLayout <> nil then Result := Result + self.getSBMLLayout.printStr
  else Result := Result + 'NO layout';
  Result := Result + sLineBreak + ' Model Render Info: ';
  if self.modelRendering <> nil then Result := Result + self.getSBMLRenderInfo.printStr
  else Result := Result + 'NO Render Info';

end;

procedure TModel.setNumReactions (rnxNumb : integer); // remove, use addSBMLReaction
begin
  numReactions:= rnxNumb;
end;

procedure TModel.addSBMLErrorStr( newErr: string );
begin
  self.strSBMLErrors.Add(newErr);
end;
function  TModel.getSBMLErrorStrs(): TList<string>;
begin
  Result := self.strSBMLErrors;
end;
function  TModel.getNumSBMLErrors(): integer;
begin
  Result := self.strSBMLErrors.Count;
end;

function TModel.getNumReactions () : integer;
begin
   //result:= numReactions;
   Result := length(self.sbmlreactions);
end;

// Notify others that model has been loaded/changed
procedure TModel.SBML_UpdateEvent();
 begin
   self.fillSpeciesArray;
   self.fillParameterArray;
   { Call the registerd event only if there is a listener }
   if Assigned(FPing) then
     FPing();
   if Assigned(FPing2) then
     FPing2();
   //  console.log('TModel.SBML_UpdateEvent: SBML_UpdateEvent: Model has been updated....');
 end;

 procedure TModel.setAnnotationStr(annotate:String);
 begin
  annotationStr:=annotate;
  self.SBML_UpdateEvent;
 end;

 function TModel.getAnnotationStr():String;
 begin
   Result:= annotationStr;
 end;

 procedure TModel.addSBMLReaction(rxnid:String; prods: array of String; prodStoich: array of double;
         reactants: array of String; reactantsStoich: array of double;
         kineticLaw: String; newReverse: boolean); overload;
 var
   newRxn: SBMLReaction;
   len: integer;
   p, r: array of TSBMLSpeciesReference;
   I: Integer;
   paramArray: array of String;
 begin
   setlength(p, Length(prods));
   paramArray:= ['nothing', 'empty']; // Local Parameter values, not used for now.
   for I := 0 to Length(prods)-1 do
   begin
     p[I]:= TSBMLSpeciesReference.create(prods[I] + rxnid,prodStoich[I]);
     p[I].setSpecies(prods[I]);
   end;

   setlength(r, Length(reactants));

   for I := 0 to Length(reactants)-1 do
   begin
     r[I]:= TSBMLSpeciesReference.create(reactants[I] + rxnid,reactantsStoich[I]);
     r[I].setSpecies(reactants[I]);
   end;

   newRxn:= SBMLReaction.create(rxnid, p, r);
   newRxn.setReversible(newReverse);
   newRxn.kineticLawStr:= kineticLaw;   // Get rid of.....?
   //newId: String; newFormula: String; paramArr: array of String
   newRxn.setKineticLaw(SBMLkineticLaw.create('dummy', kineticLaw ,paramArray));
   len:= Length(SBMLreactions);
   SetLength(SBMLreactions,len+1);    // Add new reaction to array
   sbmlreactions[len]:= newRxn;
   inc(self.numReactions);
 end;

 procedure TModel.addSBMLReaction(newReaction: SBMLReaction); overload;
 var len: integer;
 begin
   len:= Length(self.sbmlreactions);
   SetLength(self.sbmlreactions, len+1);
   self.sbmlreactions[len] := newReaction; // Need to create instead?
   inc(self.numReactions);
 end;

 function TModel.getReactions(): array of SBMLReaction;
 begin
    Result:= SBMLreactions;
 end;

 function TModel.getReaction(i: integer): SBMLReaction;
 begin
   if self.getNumReactions >i then
   begin
     Result := SBMLreactions[i];
   end
   else Result := nil;

 end;

 function  TModel.getModelId(): string;
 begin
   Result := self.modelId;
 end;

 procedure TModel.setModelId(newId: string);
 begin
   self.modelId := newId;
 end;

 function TModel.getNumFuncDefs: Integer;
 begin
   Result := self.modelFuncDefList.Count;
 end;

 procedure TModel.addFuncDef( newFuncDef: TSBMLFuncDefinition );
 begin
   self.modelFuncDefList.Add(TSBMLFuncDefinition.create(newFuncDef));
 end;

 function TModel.getFuncDef( index: integer ): TSBMLFuncDefinition;
 begin
   if index < self.modelFuncDefList.count then
     Result:= self.modelFuncDefList[index]
   else Result := nil;
 end;

 function  TModel.getFuncDefList(): TList<TSBMLFuncDefinition>;
 begin
   Result:= self.modelFuncDefList;
 end;


 // Convert any SBML func defs that are used for reaction kinetic laws:
 // Used to generate list of ODE equations to integrate.
function TModel.convertFuncDefToKineticLaw(sKineticLaw: string): string;
var i: integer;
   strNewKLaw: string;
   formula: string;
begin
  strNewKLaw := '';
  strNewKLaw := sKineticLaw;
 // console.log(' Initial NewFormula:', strNewKLaw);
  if self.modelFuncDefList <> nil then
    begin
    for i := 0 to self.modelFuncDefList.count -1 do
      begin
      formula := '';
      if strNewKLaw.Contains(self.modelFuncDefList[i].getId) then
        begin
        formula := '(' + self.modelFuncDefList[i].getFuncFormula + ')';
        strNewKLaw := strNewKLaw.Replace(self.modelFuncDefList[i].getFullFuncLabel, formula);
        end;

      end;
    end;
  //console.log(' Final NewFormula:', strNewKLaw);
  Result := strNewKLaw;
end;


 function TModel.getSpeciesNumb(): integer;
 begin
   self.numSpecies := Length(self.modelSpecies);
   Result:= self.numSpecies;
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
       begin
         Result := self.modelSpecies[i];
         exit;
       end;
     end;
   Result:= nil;
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


 function TModel.getSBMLcompartmentsArr(): array of TSBMLcompartment;
 begin
   Result := self.modelComps;
 end;
 function TModel.getSBMLcompartment(i:integer): TSBMLcompartment overload;
 begin
   Result:= modelComps[i];
 end;

 function TModel.getSBMLcompartment(compId: string): TSBMLcompartment overload;
 var i: integer;
     foundComp: TSBMLCompartment;
 begin
   foundComp := nil;
   for i := 0 to Length(self.modelComps) -1 do
     begin
       if self.modelComps[i].getID = compId then
       foundComp := self.modelComps[i];
     end;
   Result := foundComp;
 end;

 function TModel.getCompNumb(): integer;
 begin
   Result:= numCompartments;
 end;

 procedure TModel.addSBMLcompartment(newComp: TSBMLcompartment);
 var len:integer;
 begin
   len:= Length(modelComps);
   modelComps[len]:= TSBMLcompartment.create(newComp);
 end;

 function TModel.getParamNumb(): integer;
 begin
   Result:= length(self.modelParams);
 end;
 function TModel.getSBMLparameter(i:integer): TSBMLparameter;
 begin
    Result:= modelParams[i];
 end;

 function TModel.getSBMLparameterAr(): array of TSBMLparameter;
 begin
   Result:= modelParams;
 end;
 procedure TModel.addSBMLparameter(newParam: TSBMLparameter);
 var len:integer;
 begin
  len:= Length(modelParams);
  modelParams[len]:= TSBMLparameter.create(newParam);
 // if newParam.isSetName() then console.log('addSBMLparameter: ',newParam.getname());

 end;

 function  TModel.getNumModelEvents(): integer;
 begin
   Result := self.numEvents;
 end;

 function  TModel.getNumPiecewiseFuncs(): integer;
 var i: integer;
 begin
   Result := 0;
   for i := 0 to length(self.modelRules) -1 do
     begin
      if self.modelRules[i].containsPiecewise then inc(Result);
     end;
   // TODO: Need to check if piecewise in Reaction kineticLaw
 end;

 function  TModel.isParameterIdinList(id: string): boolean;
 var i: integer;
     found: boolean;
 begin
   found := false;
   for i := 0 to self.getParamNumb - 1 do
      begin
         if self.getSBMLparameter(i).getId = id then
           found := true;
      end;
   Result := found;
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

 function  TModel.getSBMLRule( index: integer ): TSBMLRule;
 begin
   if index < length(self.modelRules) then
     Result := self.modelRules[index]
   else Result := nil;
 end;

 function  TModel.getSBMLRuleWithVarId( varId: string ): TSBMLRule;
 var i: integer;
 begin
   Result := nil;
   for i := 0 to length(self.modelRules) -1 do
     begin
     if self.modelRules[i].getVariable = varId then
       Result:= self.modelRules[i];
     end;
 end;

 procedure TModel.addInitialAssignment( newInitAssign: TSBMLInitialAssignment );
 begin
   if self.modelInitialAssignments = nil then
     self.modelInitialAssignments := TList<TSBMLInitialAssignment>.create;
   self.modelInitialAssignments.Add(newInitAssign);
 end;

 function  TModel.getNumInitialAssignments(): integer;
 begin
   Result := self.modelInitialAssignments.Count;
 end;
 function  TModel.getInitialAssignment( index: integer): TSBMLInitialAssignment;
 begin
   if index < self.modelInitialAssignments.Count then
     Result := self.modelInitialAssignments[index]
   else Result := nil;
 end;

 function  TModel.getInitialAssignmentWithSymbolId(symbolId: string): TSBMLInitialAssignment;
 var i: integer;
 begin
   Result := nil;
   for i := 0 to self.modelInitialAssignments.Count -1 do
     begin
     if self.modelInitialAssignments[i].getSymbol = symbolId then
       Result := self.modelInitialAssignments[i];
     end;

 end;

 procedure TModel.setSBMLLayout(newLayout: TSBMLLayout);
 begin
   self.modelLayout := TSBMLLayout.create(newLayout);
 end;
 function  TModel.getSBMLLayout(): TSBMLLayout;
 begin
   Result := self.modelLayout;
 end;

 procedure TModel.setSBMLRenderInfo( newRender: TSBMLRenderInformation );
 begin
   self.modelRendering := TSBMLRenderInformation.create( newRender );
 end;
 function  TModel.getSBMLRenderInfo(): TSBMLRenderInformation;
 begin
   Result := self.modelRendering;
 end;

 function  TModel.getRenderStyle(newGlyphId: string; newSpeciesId: string;
             newGlyphType: string; newGlyphRole: string ): TSBMLRenderStyle;
 var i,j: integer;
 begin
  // See SBML Render Spec: C.2 Style Resolution for details.
  Result := nil;    // does not work if idList is actual node species name and not speciesGlyph
  for i := 0 to self.modelRendering.getNumberStyles -1 do
    begin
      for j := 0 to self.modelRendering.getStyle(i).getNumbGoIds -1 do
        begin
        if self.modelRendering.getStyle(i).getGoId(j) = newGlyphId then
          begin
          Result := self.modelRendering.getStyle(i);
          exit;
          end
        else
          begin
          if self.modelRendering.getStyle(i).getGoId(j) = newSpeciesId then
          begin
          Result := self.modelRendering.getStyle(i);
          exit;
          end
          end;

        end;
    end;

    if newGlyphRole <> '' then
      begin
        for i := 0 to self.modelRendering.getNumberStyles -1 do
        begin
          for j := 0 to self.modelRendering.getStyle(j).getNumbRoles -1 do
            begin
            if self.modelRendering.getStyle(j).getRole(j) = newGlyphRole then
              begin
              Result := self.modelRendering.getStyle(i);
              exit;
              end;

            end;
        end;
      end;

    if newGlyphType <> '' then
      begin
      for i := 0 to self.modelRendering.getNumberStyles -1 do
        begin
        for j := 0 to self.modelRendering.getStyle(i).getNumbTypes -1 do
          begin
          if self.modelRendering.getStyle(i).getType(j) = newGlyphType then
            begin
            Result := self.modelRendering.getStyle(i);
            exit;
            end;
          end;
        end;
      end;





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
      self.s_NameValAr := TVarNameValList.create();
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

procedure TModel.resetS_Vals();
begin
  self.fillSpeciesArray(); // bj added
  self.s_Vals := self.s_NameValAr.getValAr();
end;

function TModel.getS_initVals(): array of double;
begin
  Result := self.s_NameValAr.getValAr();
end;

procedure TModel.fillParameterArray();
 var i, paramLen : integer;
     currNameVal: TVarNameVal;

begin
   // Get parameter and compartment names and combine into one array, pVals[]:
  self.p_NameValAr := TVarNameValList.create();
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

function  TModel.getP_NameValAr(): TVarNameValList;
begin
  Result := self.p_NameValAr;
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
 // console.log('TModel.changeParamVal, pos: ',pos,', Value: ',newVal);
  // Other viewers update ?

end;

procedure TModel.testModelUpdate();
var i: integer;
   rxn: SBMLReaction;
   ids: array of String;
begin
  // TODO  make changes ...
  // Works with uTestModel
  rxn := nil;
  if self.getNumReactions()>1 then
  begin
    if self.getReaction(0).isSetKineticLaw then
    begin
      self.getReaction(0).getKineticLaw.setFormula('k1 * G1');
      self.getReaction(0).getReactant(0).setId('G1');
   //   self.getReaction(0).getReactants()[0] := 'G1';
      self.getSBMLSpecies(0).setId('G1');
    end;

    console.log('Formula: ',self.getReaction(0).getKineticLaw.getFormula());
    console.log('species: ',self.getReaction(0).getReactant(0).getId());
    self.SBML_UpdateEvent();
  end;


end;

end.   // unit
