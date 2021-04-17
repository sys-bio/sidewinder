unit uTestModel;

interface

function getTestModel : string;

implementation


function getTestModel : string;
begin
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
  '<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" level="3" version="1">' +
  '<model metaid="__main" id="__main">' +
  '  <listOfCompartments>' +
  '    <compartment sboTerm="SBO:0000410" id="default_compartment" spatialDimensions="3" size="1" constant="true"/>' +
  '  </listOfCompartments>' +
  '  <listOfSpecies>' +
  '    <species id="S1" compartment="default_compartment" initialConcentration="10" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
  '    <species id="S2" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
  '    <species id="S3" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
  '  </listOfSpecies>' +
  '  <listOfParameters>' +
  '    <parameter id="k1" value="0.1" constant="true"/>' +
  '    <parameter id="k2" value="0.05" constant="true"/>' +
  '  </listOfParameters>' +
  '  <listOfReactions>' +
  '    <reaction id="_J0" reversible="true" fast="false">' +
  '      <listOfReactants>' +
  '        <speciesReference species="S1" stoichiometry="1" constant="true"/>' +
  '      </listOfReactants>' +
  '      <listOfProducts>' +
  '        <speciesReference species="S2" stoichiometry="1" constant="true"/>' +
  '      </listOfProducts>' +
  '      <kineticLaw>' +
  '        <math xmlns="http://www.w3.org/1998/Math/MathML">' +
  '          <apply>' +
  '            <times/>' +
  '            <ci> k1 </ci>' +
  '            <ci> S1 </ci>' +
  '          </apply>' +
  '        </math>' +
  '      </kineticLaw>' +
  '    </reaction>' +
  '    <reaction id="_J1" reversible="true" fast="false">' +
  '      <listOfReactants>' +
  '        <speciesReference species="S2" stoichiometry="1" constant="true"/>' +
  '      </listOfReactants>' +
  '      <listOfProducts>' +
  '        <speciesReference species="S3" stoichiometry="1" constant="true"/>' +
  '      </listOfProducts>' +
  '      <kineticLaw>' +
  '        <math xmlns="http://www.w3.org/1998/Math/MathML">' +
  '          <apply>' +
  '            <times/>' +
  '            <ci> k2 </ci>' +
  '            <ci> S2 </ci>' +
  '          </apply>' +
  '        </math>' +
  '      </kineticLaw>' +
  '    </reaction>' +
  '  </listOfReactions>' +
  '</model>' +
  '</sbml>';
end;


end.
