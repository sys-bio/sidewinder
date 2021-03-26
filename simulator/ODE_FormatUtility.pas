unit ODE_FormatUtility;

// Takes a list of SBML reactions and Lists of Species and parameters and returns
// An array of equation strings with the names replaced with simple array names.
// array lists is in same order ans size as lists of species and parameters that were passed in.
// Convert basic math functions to javascript notation (ie pow() -> Math.pow() )

interface
uses System.SysUtils, System.StrUtils, web, SBML.model, SBML.helper;

type
FormatODEs = class
  private
  const odeStart: String='dydt_s[';  // Used for building up ODE eqs.
  const odeLSODAstart: String = 'dydt_s.setval('; // Used for LSODA ODE eqs.
  //const odeStart2: String='Result['; // Used for building up ODE eqs for LSODA
  var odeEqs: array of String; // list of ODE eqs using std notation
    odeEqs2: array of String; // LSODA list of eqs.
    odeEqSet:String;  // Contains all eqs as one String
    odeEqSet2:String; // LSODA specific
    rxns: array of SBMLreaction;
    prods: array of SBMLspeciesreference;
    reactants: array of SBMLspeciesreference;
    speciesStrAr: array of String;   // keep to convert short name to long name.
    paramsStr: array of String;    // keep to convert short name to long name.
    sVals: array of double;   // init val of species, same size as speciesStrAr
    pVals: array of double;   // init val of parameters, same size as paramsStr.

  function buildODE_LHS( rxn: SBMLreaction ): array of String; // build the 'dydt_s[]=' or '-dydt_s[]=', store in rhsSymbols
  function getODEeqLoc(Speciesdydt : String): Integer; // return index of Species in odeEqs.

  public
  constructor create( model: SBMLhelpClass);
  function replaceStrNames(names: array of String; stringtoedit: String; prefixStr: String):String;
  function StrInArray(const Value : String;const ArrayOfString : Array of String) : Integer;
  function testStrReplace( ): String;  // testing....
  function getODEs(): array of String;
  function getODEs2(): array of String;
  function getODEeqSet(): String;
  function getODEeqSet2(): String; // LSODA eq set.
  function get_sVals(): array of double; // get init vals for species used in eqs.
  function get_pVals(): array of double; // get init vals for params, including comp vols used in eqs.
  function get_speciesStrAr(): array of String;  // SBML Species id
  function get_paramsStr(): array of String;   // SBML parameter and compartment ids.
  procedure buildLSODAeqs();  // build LSODA eqs (odeEqs2)
  procedure buildFinalEqSet(); // Build up final ODE eqs list as one string for use by solver.

end;
// Currently no support for user defined functions in kinetic law formula.

function JSMathConvert(eqStr: String): String;

implementation

constructor FormatODEs.create( model: SBMLhelpClass);
var i, j,k, paramLen, count: Integer;
    curODE: String;
    odeStrs: array of String;
    lhsSymbols: array of String; // contains the 'dydt_s[]=' argument.
    found_dydt: Integer;

begin
  setLength(speciesStrAr,Length(model.getSBMLspeciesArr()));
  setLength(paramsStr, Length(model.getSBMLparameterArr()));
  setLength(odeStrs,model.getrxnsnumb());
  setLength(sVals,model.getSpeciesNumb());
  count:=0;
  for i := 0 to Length(speciesStrAr)-1 do
  begin
    speciesStrAr[i]:= model.getSBMLspecies(i).getId();
    console.log('---> Species: ', speciesStrAr[i]);
   // Get initial conc/amounts for each one:
    if model.getSBMLspecies(i).isSetInitialAmount() then
      sVals[i]:= model.getSBMLspecies(i).getInitialAmount()
      else if model.getSBMLspecies(i).isSetInitialConcentration() then
        sVals[i]:= model.getSBMLspecies(i).getInitialConcentration()
        else sVals[i]:=0;

  end;
  // Get parameter and compartment names and combine into one array, pVals[]:
  SetLength(pVals,Length(paramsStr));
  for i := 0 to Length(paramsStr)-1 do
  begin
   if model.getSBMLparameter(i).isSetIDAttribute() then
    begin
     //  paramsStr[i]:= params[i].getId() ;
      paramsStr[i]:= model.getSBMLparameter(i).getId();
       console.log('---> Param: ', paramsStr[i]);
      // Get initial value for each param:
      if model.getSBMLparameter(i).isSetValue() then
       pVals[i]:= model.getSBMLparameter(i).getValue()
       else pVals[i]:= 0;
    end
    else begin
      paramsStr[i]:= '';
      pVals[i]:= 0;
    end;
    console.log('Param Val: ',pVals[i],' for: ',paramsStr[i]);
  end;
    // Get compartments:
  paramLen:=Length(paramsStr);
  SetLength(paramsStr, paramLen+model.getCompNumb());
  SetLength(pVals, paramLen + model.getCompNumb());
  for i := 0 to model.getCompNumb()-1 do
    begin

      if model.getSBMLcompartment(i).isSetIdAttribute() then
      begin
        paramsStr[paramLen+i]:= model.getSBMLcompartment(i).getID();
        console.log('Adding compartment id');
      end
      else paramsStr[paramLen+i]:= model.getSBMLcompartment(i).getName();
      console.log('Adding compartment id: ', paramsStr[paramLen+i]);
      // Get Size/Vols of compartments:
      if model.getSBMLcompartment(i).isSetSize() then
        pVals[paramLen+i]:= model.getSBMLcompartment(i).getSize()
        else if model.getSBMLcompartment(i).isSetVolume() then
          pVals[paramLen+i]:= model.getSBMLcompartment(i).getVolume()
          else pVals[paramLen+i]:= 1; // default
    end;
  // *******************************************************************
  // go through each reaction eq and replace all species and params in arrays:
   rxns:= Copy(model.getReactions(),0,model.getrxnsnumb());
  for j := 0 to Length(rxns)-1 do
  begin
    if rxns[j].isSetKineticLaw() then
    begin
      lhsSymbols:= buildODE_LHS(rxns[j]);   // check if existing LHS, then just
                  // add eq to it.
      curODE:= rxns[j].getKineticLaw().getFormula()
    end
    else odeStrs[j]:= '';
    odeStrs[j]:= replaceStrNames(speciesStrAr, curODE,'s');
    //console.log('j = ',j,'Species. --> New Eq: ',odeStrs[j]);
    odeStrs[j]:= replaceStrNames(paramsStr, odeStrs[j],'p');
    setLength(odeEqs,length(lhsSymbols)+count);
    for k := 0 to length(lhsSymbols)-1 do
    begin
      found_dydt:=  getODEeqLoc(lhsSymbols[k] );
      console.log(' found_dydt: ', found_dydt);
      if found_dydt <0 then // not found
      begin
        self.odeEqs[count]:= lhsSymbols[k] + odeStrs[j] + ')';  // rhs inclosed in perenthesis.
        console.log('... New Eq with params: ',odeEqs[count]);
        count:= count+1;  // count: Total number of eqs: Sum(prod+reactants per rxn)
      end
      else begin
        if ContainsText(lhsSymbols[k],'(-1)*') then
          self.odeEqs[found_dydt]:= self.odeEqs[found_dydt] + '+ ' + '(-1)*('+ odeStrs[j] + ')'
        else self.odeEqs[found_dydt]:= self.odeEqs[found_dydt] + '+ ' + '('+ odeStrs[j] + ')';

      end ;

      console.log('... New Eq with MathOps: ',odeEqs[count]);
      //count:= count+1;  // count: Total number of eqs: Sum(prod+reactants per rxn)
    end;

  end;
   // Now replace math operators:
  for i := 0 to Length(odeEqs)-1 do
    begin
       odeEqs[i]:= JSMathConvert(odeEqs[i]);
    end;

  buildLSODAeqs();

end;

// ************************************************************
// Replace names in string with array names ('species1' -> 's[0]')
 function FormatODEs.replaceStrNames(names: array of String; stringtoedit: String; prefixStr: String):String;
 var tmpStoE,tmpCatStr, currEqStr: String; // temp holder for string
 //var tmpCatStr2: String; // test...
 var i,j,k: integer;
// var strR: String;  // Replacement prefix
 var beginC, endC: String;
 const eqdelims: array of String = [' ', '(', ')', '+', '-', '*', '/', ','];

 begin
   currEqStr:= Copy(stringtoedit,1,MaxInt);  // use instead of stringtoedit
    console.log('Value of currEqStr:', currEqStr);
   tmpStoE:= '';
   tmpCatStr:= '';
   for i := 0 to Length(names)-1 do
     begin
           tmpStoE:= '';
           tmpCatStr:= '';
           j:=1; // Position to check in string where name may be.    was 0.
           while j < length(currEqStr) do
       //   while j < length(currEqStr)+1 do
             begin
             k:= 0; // position in string where name found.
             beginC:= ''; endC:= '';
             k:= PosEx(names[i],currEqStr,j);  // returns 1 if name found at currEqStr[0]
              if ( k>0) then    // found name
                begin
                  console.log('Found match: ',names[i]);  // Found at correct position, k
                  if k =1 then beginC:= ' ' // First char of formula string is in names[i]
                  else beginC:= Copy(currEqStr,(k-1),1);
                  console.log('Char before match ...:',beginC,':...');
                    endC:= Copy(currEqStr,(k+length(names[i])),1);
                  if StrInArray(beginC,eqdelims) > -1  then
                  begin
                    tmpCatStr:= Copy(currEqStr,j,(k-j));
                    if (StrInArray(endC,eqdelims) > -1) or (endC= '') then   // end of string chk
                    begin
                      console.log('A second delimeter found! j:',j,', k: ',k);
                   // Put new eq str together. Append prefix[i] to it.
                   // if end of eq str need to put endC at end ?? (NO), missing ')' sometimes.
                     // tmpStoE:= tmpStoE + tmpCatStr + PrefixStr+ '['+inttostr(i)+']';
                     tmpStoE:= tmpStoE + tmpCatStr + PrefixStr+ '['+inttostr(i)+']';
                    end
                    else
                    begin
                    // leave str alone:
                      tmpStoE:= tmpStoE + tmpCatStr + names[i];
                      console.log('No match, string: ',tmpStoE);
                    end;

                  end
                  else
                  begin
                        tmpCatStr:= Copy(currEqStr,j,(k-j));
                        console.log('Check first part of string: ',tmpCatStr);
                       tmpStoE:= tmpStoE + tmpCatStr + names[i];
                       console.log('No match a part of another name, string: ',tmpStoE);
                  end;    // end new
                  console.log('Char after match: ',endC);  // Issue here ?? (ending parenthesis)
                  if (length(names[i])<1) then j:= j+1
                  else
                    begin
                     j:= k+ length(names[i]);
                     if j=length(currEqStr) then tmpStoE:= tmpStoE + endC; // added

                    end;
                end
              else
              begin
                tmpCatStr:= Copy(currEqStr,j,(Length(currEqStr)-(j-1)));
                tmpStoE:= tmpStoE + tmpCatStr;
                j:= length(currEqStr);  // name Not found.
              end;
             end;
             // Now copy tmpStoE to stringtoedit and start over for next name.
             currEqStr:= Copy(tmpStoE,1,Length(tmpStoE));
             console.log('.. Current value currEqStr: ', currEqStr);
     end;
     Result:= currEqStr;
 end;

// ****************************************************************
 function FormatODEs.StrInArray(const Value : String; const ArrayOfString : Array of String) : Integer;
var
 Loop : String;
 i: Integer;
begin
  i:= -1; // Not found
  for Loop in ArrayOfString do
  begin
  i:=i+1;
  //console.log('Loop= ',Loop,', Value: ',Value);
    if Value = Loop then
    begin

       Exit(i);
    end;
  end;
  result := -1;
end;

// ********************************************************
function FormatODEs.testStrReplace( ): String;
var testEq, spList, parList: array of String;
    finalEq: String;
    i: integer;
begin
//
  SetLength(testEq,2);
  testEq[0]:= 'store * (Vm3 * pow(Y, m) * pow(Z, p) / ((pow(Kr, m) + pow(Y, m)) * (pow(Ka, p) + pow(Z, p))))';
  testEq[1]:= 'v1 * ((1 -m) / m * Z - Y)';
  spList:= ['Z', 'Y'];
  parList:= ['v1','Vm3','m', 'n', 'Kr','Ka','p'];
  for i := 0 to Length(testEq)-1 do
    begin
    console.log('TEST - Init Eq: ', testEq[i]);
    finalEq:= replaceStrNames(spList, testEq[i],'s');
    finalEq:= replaceStrNames(parList, finalEq,'p');
    console.log('TEST -> Final Eq: ', finalEq);
    end;
end;

// **********************************************
function FormatODEs.getODEs(): array of String;
begin
  result:= self.odeEqs;
end;

function FormatODEs.getODEs2(): array of String;
begin
  Result:= self.odeEqs2;
end;

// **********************************************
function FormatODEs.getODEeqSet(): String;
begin
  Result:= self.odeEqSet;
end;

// **********************************************
function FormatODEs.getODEeqSet2(): String;
begin
  Result:= self.odeEqSet2;
end;

// **********************************************
// get init vals for species used in eqs.
function FormatODEs.get_sVals(): array of double;
begin
  result:= sVals;
end;

// **********************************************
// get init vals for params, including comp vols used in eqs.
function FormatODEs.get_pVals(): array of double;
begin
  result:= pVals;
end;

// *************************************************
function FormatODEs.get_speciesStrAr(): array of String;
begin
  result:= self.speciesStrAr;
end;

// **************************************************
function FormatODEs.get_paramsStr(): array of String;
begin
  result:= self.paramsStr;
end;

// ***********************************************
// build the 'dydt_s[]=' or 'dydt_s[]= (-1)*', store in lhsSymbols[]
function FormatODEs.buildODE_LHS(rxn: SBMLreaction):array of String;
var i, nr, np: Integer;
var lhs: array of String;
begin
  nr:= rxn.getNumReactants();
  np:= rxn.getNumProducts();
  // check previous reaction strings and see if LHS for species already exists.
  // If so, then flag it.
  // When checking if product or reactant just add eq to previous one.
  SetLength(lhs,nr+np);
  if np>0 then
  begin
      self.prods:= rxn.getRxnProducts();
    // Works with only one product/reactant, need to generalize for many products.
    // Need one ODE for product (+);
  if length(self.prods)>0 then
     begin
      for i := 0 to np-1 do
       if prods[i].isSetSpecies() then
         lhs[i]:= odeStart + IntToStr(strInArray(self.prods[0].getSpecies(),speciesStrAr))+']'+'= (';    // dydt_name

     console.log('prods lhs: ', lhs[i]);
     end;
  end;

  if nr>0 then
  begin
    self.reactants:= rxn.getRxnReactants();
    // Works with only one product/reactant, need to generalize for many products.
    // Need one ODE one for Reactant (-);
  if length(self.reactants)>0 then
     begin
      for i := 0 to nr-1 do
       if self.reactants[i].isSetSpecies() then
         lhs[np+i]:= odeStart + IntToStr(strInArray(reactants[0].getSpecies(),speciesStrAr))+']'+'= (-1)* (';    // dydt_name

       console.log('reactants lhs: ', lhs[np+i]);
     end;
   end;
  Result:= lhs;
end;
    // Speciesdydt: dydt_s[n]
function FormatODEs.getODEeqLoc(Speciesdydt : String): Integer; // return index of Species in odeEqs.
var i,found: Integer;
  spStr, b_str: String;

begin
console.log( ' --> Looking for Speciesdydt: ', Speciesdydt );
  spStr:= '';
  b_str:= '';
  found:= -1;
  for i := 0 to Length(self.speciesStrAr) -1 do
    begin
      b_str:= '['+IntToStr(i)+']';
      if ContainsText(Speciesdydt,b_str) then
      begin
        spStr:= odeStart + IntToStr(i)+ ']'; // 'dydt_s[i]'
        console.log('Found Speciesdydt: ', Speciesdydt, ', spStr: ', spStr);
      end;

    end;
  for i := 0 to Length(Self.odeEqs)-1 do
  begin
    if Not (spStr = '') then
    begin
      if ContainsText(odeEqs[i], spStr) then
      begin
      console.log('odeEq: ',odeEqs[i], 'spStr: ',spStr);
      found:= i;
      console.log('**** Found existing ODE eq!');
      end;
    end;


  end;

  Result:= found;

end;

 procedure FormatODEs.buildLSODAeqs();
 var i,j:integer;
    replStr:String;
    editStr: String;
   begin
   for i := 0 to Length(self.odeEqs)-1 do
     begin
     for j := 0 to Length(self.svals)-1 do
       begin
       editStr:= 'dydt_s['+intToStr(j)+']=';
       replStr:= 'dydt_s.setVal('+intToStr(j+1)+',';   // Uses TVector which start at 1, not 0.
       if ContainsText(self.odeEqs[i],editStr) then
         begin
         self.odeEqs2[i]:= StringReplace(self.odeEqs[i],editStr,replStr,[])+')';

         end;

       end;
     console.log(' -> ODE eq: ',self.odeEqs[i]);
     console.log(' --> LSODA eq: ',self.odeEqs2[i]);
     end;

   end;

   // Build up final ODE eqs list as one string for use by solver
 procedure FormatODEs.buildFinalEqSet();
 var i:Integer;
 begin
  self.odeEqSet:= '';
  self.odeEqSet2:= 'let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]); ';  // Eq set for LSODA


  //SetLength(odeStrs,Length(rxnsArray));
   // Generate array of SBML params plus volumes and their inital values.
   //odeStrs:= odeFormat.getODEs();
   //odeStrs2:= odeFormat.getODEs2();
   //editStr:= '';
   for i := 0 to Length(self.ODEeqs)-1 do
    begin
      if Length(self.ODEeqs[i])>1 then  // Do not add empty statements
      begin
        odeEqSet:= odeEqSet+self.ODEeqs[i]+ ';'  ;
        odeEqSet2:= odeEqSet2 + self.ODEeqs2[i]+ ';';
      end;
    end;


    // Run Simulation using info from odeFormat:
   odeEqSet:= odeEqSet + ' return dydt_s ;' ;
   odeEqSet2:= odeEqSet2 + ' return dydt_s;';  // Add eqs LSODA list.
   console.log('*** Final odeEqSetVal: *****');
   console.log(odeEqSet);
   console.log(' ** LSODA eqs: **');
   console.log(odeEqSet2);


 end;

// SBML math operators:
// arithmetic operators:  power, root, abs, exp, ln, log, floor, ceiling,
// factorial, quotient, max, min, rem
//  trigonometric operators: sin, cos, tan, sec, csc, cot, sinh, cosh, tanh, sech, csch, coth, arcsin, arccos,
// arctan, arcsec, arccsc, arccot, arcsinh, arccosh, arctanh, arcsech, arccsch, arccoth
// constants:  notanumber, pi, infinity, exponentiale

// Assumes left parenthesis follows operator: 'pow(a,b)', not 'pow (a,b)' or 'pow a,b'
function JSMathConvert(eqStr: String): String;
const
  sbmlOp: array[0..3] of String = ('pow', 'ln','log', 'exp');
  jsMathOp: array[0..3] of String = ('Math.pow', 'Math.log', 'Math.log10', 'Math.exp');
var
 i: Integer;
 sbmlStr: String;
 jsStr: String;
 newStr, currStr: String;

begin
  newStr:= '';
  currStr:= eqStr;
  for i := 0 to Length(sbmlOp)-1 do
    begin
      sbmlStr:= sbmlOp[i] + '(';
      //console.log( ' Sbml Math op: ',sbmlStr);
      jsStr:= jsMathOp[i] + '(';
      currStr:= StringReplace(currStr, sbmlStr, jsStr, [rfReplaceAll, rfIgnoreCase]);

    end;
  console.log( 'JS Math string Replace:', currStr );
  result:= currStr;
end;


end.


