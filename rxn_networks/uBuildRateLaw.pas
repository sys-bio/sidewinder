unit uBuildRateLaw;
 // Build Mass action rate law.
 // Currently only forward reaction used:
 // Ex A + B --> C:  rate: kf_f0*(A^ORR_A)*(B^ORR_B)
interface

Uses SysUtils, Classes, Types, Math, Web, WEBLib.Utils, WEBLib.JSON, Dialogs,
     System.Generics.Collections, uNetworkTypes, uSBMLClasses;

const
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
               prods: array of string; rStoich: array of double; pStoich: array of double;
               rateParams :TList<TSBMLparameter> ) overload;
  constructor create(copy: tRateLaw) overload;
  function getRateFormula(): string;

end;

implementation

constructor tRateLaw.create( nReact: integer; nProds: integer; reactants: array of string;
               prods: array of string; rStoich: array of double; pStoich: array of double;
               rateParams: TList<TSBMLparameter> ) overload;
               var i: integer;
begin
  self.nR := nReact;
  self.nP := nProds;
  setLength(self.rS,nReact);
  setLength(self.pS, nProds);
  for i := 0 to nReact -1 do
    self.rS[i] := rStoich[i];
  for i := 0 to nProds -1 do
    self.pS[i] := pStoich[i];

  //else ShowMessage('Number of product stoich coefficients does not match number of products');

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
    end;
  if True then

  if (self.nR = 1) and (self.nP < 3) then
  begin
    if self.rateP.Count >0 then
    begin
      self.rateFormula := fRateVal + '* pow(' + self.reactants[0] + ', ' ;
      self.rateFormula := self.rateFormula + self.rS[0].ToString + ')';
    end;
  end
  else if (self.nR = 2) and (self.nP < 3) then
  begin
     self.rateFormula := fRateVal + '* pow(' + self.reactants[0] + ', ' ;
     self.rateFormula := self.rateFormula + self.rS[0].ToString + ') * pow(';
     self.rateFormula := self.rateFormula + self.reactants[1]+ ', '+ self.rS[1].ToString +')';
  end;
  console.log('generateRateLaw: '+ self.rateFormula);
end;

function tRateLaw.getRateFormula(): string;
begin
  Result := self.rateFormula;
end;

end.
