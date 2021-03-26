import tellurium as te

r = te.loada('''
// Created by libAntimony v2.12.0.3
model *Alexander2010_Tcell_Regulation_Sys1()

  // Compartments and Species:
  compartment body;
  substanceOnly species A in body, R in body, E in body, G in body, $A_im in body;

  // Assignment Rules:
  mA := b1 + muA;
  mG := muG + v;
  R0 := f*v*lambdaE*gamma_/(mG*mA*muE);

  // Reactions:
  r1a: G => ; v*G;
  r1b: $A_im => A; f*v*G;
  r2:  => G; gamma_*E;
  r3:  => R; beta*A;
  r4:  => R; pi1*E*A;
  r5:  => E; lambdaE*A;
  r6: A => ; muA*A;
  r7: R => ; muR*R;
  r8: E => ; muE*E;
  r9: G => ; muG*G;
  r10: A => ; b1*A;
  r11: A => ; sigma1*A*R;

  // Species initializations:
  A = 1/body;
  R = 0;
  E = 0;
  G = 100000000/body;
  A_im = 0;

  // Compartment initializations:
  body = 1;

  // Variable initializations:
  v = 0.0025;
  v has per_day;
  f = 0.0001;
  f has dimensionless;
  gamma_ = 2000;
  gamma_ has per_day;
  beta = 200;
  beta has per_day;
  pi1 = 0.016;
  pi1 has per_day_per_item;
  lambdaE = 1000;
  lambdaE has per_day;
  muA = 0.25;
  muA has per_day;
  muR = 0.25;
  muR has per_day;
  muE = 0.25;
  muE has per_day;
  muG = 5;
  muG has per_day;
  b1 = 0.25;
  b1 has per_day;
  sigma1 = 3e-06;
  sigma1 has per_day_per_item;
  mA has per_day;
  mG has per_day;
  R0 has dimensionless;

  // Other declarations:
  var mA, mG, R0;
  const body, v, f, gamma_, beta, pi1, lambdaE, muA, muR, muE, muG, b1, sigma1;

  // Unit definitions:
  unit substance = item;
  unit time_unit = 86400 second;
  unit per_day = 1 / 86400 second;
  unit per_day_per_item = 1 / (86400 second * item);

  // Display Names:
  substance is "number";
  time_unit is "days";
  r1a is "r1a: self-antigen uptake";
  r1b is "r1b: pAPC maturation";
  r2 is "r2: self-antigen release triggered by E";
  r3 is "r3: R activation by A";
  r4 is "r4: R activation by A and E";
  r5 is "r5: E generation by A";
  r6 is "r6: A death";
  r7 is "r7: R death";
  r8 is "r8: E death";
  r9 is "r9: G clearance";
  r10 is "r10: A suppression by Tregs of other specificity";
  r11 is "r11: A suppression by R";

  // SBO terms:
  r1a.sboTerm = 185
  r1b.sboTerm = 182
  r2.sboTerm = 185
  r3.sboTerm = 170
  r4.sboTerm = 170
  r5.sboTerm = 393
  r6.sboTerm = 394
  r7.sboTerm = 394
  r8.sboTerm = 394
  r9.sboTerm = 394
  r10.sboTerm = 169
  r11.sboTerm = 169

  // CV terms:
  A hypernym "http://identifiers.org/cl/CL:0000145"
  R hypernym "http://identifiers.org/cl/CL:0000903"
  E hypernym "http://identifiers.org/cl/CL:0000911"
  G hypernym "http://identifiers.org/chebi/CHEBI:59132"
  A_im hypernym "http://identifiers.org/cl/CL:0000473"
end

Alexander2010_Tcell_Regulation_Sys1 is "Alexander2010_Tcell_Regulation_Sys1"

Alexander2010_Tcell_Regulation_Sys1 model_entity_is "http://identifiers.org/biomodels.db/MODEL1012220000"
Alexander2010_Tcell_Regulation_Sys1 model_entity_is "http://identifiers.org/biomodels.db/BIOMD0000000289"
Alexander2010_Tcell_Regulation_Sys1 description "http://identifiers.org/pubmed/20195912"
Alexander2010_Tcell_Regulation_Sys1 hypernym "http://identifiers.org/go/GO:0002667",
                                             "http://identifiers.org/doid/DOID:417"
Alexander2010_Tcell_Regulation_Sys1 taxon "http://identifiers.org/taxonomy/33208"
Alexander2010_Tcell_Regulation_Sys1 property "http://identifiers.org/mamo/MAMO_0000046"
''')
results = r.simulate(0, 2.5,251)
r.plot()
print(results)