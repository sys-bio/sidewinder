unit uBuildRateLaw;
 // Build Mass action rate law.
 // Currently only forward reaction used:
 // Ex A + B --> C:  rate: kf_f0*(A^ORR_A)*(B^ORR_B)
interface

Uses SysUtils, Classes, Types, Math, WEBLib.Utils, WEBLib.JSON, System.Generics.Collections,
     uNetworkTypes, uSBMLClasses;

const
 RXN_R_EXP = 'ORR_'; //Order of reaction reactant exponent label
 RXN_P_EXP = 'ORP_'; //Order of reaction product exponent label
 RXN_k_F   = 'kf_';  // forward reaction rate label
 RXN_k_B   = 'kb_';  // backward reaction rate label

type

tRateLaw = class
  private
  products: array of string; // Does not hold the exact number of prods
  reactants: array of string;
  nR, nP: integer;           // number of reactants/products
  rS, pS: array of double;   // Stoich coeff
  rateFormula: string;
  rateP: TList<TSBMLparameter>;   // RXN Rate params
  procedure generateRateLaw();

  public
  constructor create( nReact: integer; nProds: integer; reactants: array of string;
               prods: array of string; rateParams :TList<TSBMLparameter> ) overload;
  constructor create(copy: tRateLaw) overload;
  function getRateFormula(): string;

end;

implementation

constructor tRateLaw.create( nReact: integer; nProds: integer; reactants: array of string;
               prods: array of string; rateParams: TList<TSBMLparameter> ) overload;
begin
  self.nR := nReact;
  self.nP := nProds;
  self.products := prods;
  self.reactants := reactants;
  self.rateP := rateParams;
  self.rateFormula := '';
  self.generateRateLaw();
end;

constructor tRateLaw.create(copy: tRateLaw) overload;
begin
  //TODO
end;

// Currently One-way rxns, one rate constant per reaction
procedure tRateLaw.generateRateLaw();
var i,j: integer;
   fRateVal: string;
   rORxn: TList<string>;

begin
  rORxn := TList<string>.create;
  for i := 0 to self.rateP.Count -1 do
    begin
      if self.rateP[i].getId.contains(RXN_k_F) then fRateVal := self.rateP[i].getId;
      if self.rateP[i].getId.contains(RXN_R_EXP) then rORxn.Add(self.rateP[i].getId);
    end;
  if True then

  if (self.nR = 1) and (self.nP < 3) then
  begin
    if self.rateP.Count >0 then
    begin
      self.rateFormula := fRateVal + '* pow(' + self.reactants[0] + ', ' ;
      if rORxn.Contains(RXN_R_EXP + self.reactants[0]) then
      begin
        j := rORxn.IndexOf(RXN_R_EXP + self.reactants[0]);
        self.rateFormula := self.rateFormula + rORxn[j] + ')';
      end;
    end;
  end
  else if (self.nR = 2) and (self.nP < 3) then
  begin
     self.rateFormula := fRateVal + '* pow(' + self.reactants[0] + ', ' ;
      if rORxn.Contains(RXN_R_EXP + self.reactants[0]) then
      begin
        j := rORxn.IndexOf(RXN_R_EXP + self.reactants[0]);
        self.rateFormula := self.rateFormula + rORxn[j] + ') * pow(';
      end;
      if rORxn.Contains(RXN_R_EXP + self.reactants[1]) then
      begin
        j := rORxn.IndexOf(RXN_R_EXP + self.reactants[1]);
        self.rateFormula := self.rateFormula + self.reactants[1]+ ', '+ rORxn[j] +')';
      end;

  end;

end;

function tRateLaw.getRateFormula(): string;
begin
  Result := self.rateFormula;
end;

end.
