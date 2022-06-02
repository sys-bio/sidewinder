unit uTestModel;

interface

function getTestModel( index: integer ) : string;

implementation


function getTestModel( index: integer) : string;
begin
  case index of
    0:
  result := '<?xml version="1.0" encoding="UTF-8"?>' +
' <!-- Created by libAntimony version v2.12.0.3 with libSBML version 5.18.1. -->' +
' <sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" level="3" version="1">' +
  ' <model metaid="__main" id="__main">' +
    ' <listOfCompartments>' +
      ' <compartment sboTerm="SBO:0000410" id="default_compartment" spatialDimensions="3" size="1" constant="true"/>' +
    ' </listOfCompartments>' +
    ' <listOfSpecies>' +
      ' <species id="S1" compartment="default_compartment" initialConcentration="20" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="S2" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="S3" compartment="default_compartment" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="S4" compartment="default_compartment" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
    ' </listOfSpecies>' +
    ' <listOfParameters>' +
      ' <parameter id="k1" value="0.1" constant="true"/>' +
      ' <parameter id="k2" value="0.2" constant="true"/>' +
      ' <parameter id="k3" value="0.1" constant="true"/>' +
      ' <parameter id="k4" value="0.2" constant="true"/>' +
    ' </listOfParameters>' +
    ' <listOfReactions>' +
      ' <reaction id="J1" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference species="S1" stoichiometry="1" constant="true"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference species="S2" stoichiometry="1" constant="true"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> k1 </ci>' +
              ' <ci> S1 </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J2" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference species="S2" stoichiometry="1" constant="true"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference species="S3" stoichiometry="1" constant="true"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> k2 </ci>' +
              ' <ci> S2 </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J3" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference species="S3" stoichiometry="1" constant="true"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference species="S4" stoichiometry="1" constant="true"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> k3 </ci>' +
              ' <ci> S3 </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
	  ' <reaction id="J4" reversible="true" fast="false">' +
       ' <listOfReactants>' +
      '    <speciesReference species="S4" stoichiometry="1" constant="true"/>' +
     '   </listOfReactants>' +
      '  <listOfProducts>' +
      '    <speciesReference species="S2" stoichiometry="1" constant="true"/>' +
     '   </listOfProducts>' +
     '   <kineticLaw>' +
     '     <math xmlns="http://www.w3.org/1998/Math/MathML">' +
     '       <apply>' +
     '         <times/>' +
     '         <ci> k4 </ci>' +
     '         <ci> S4 </ci>' +
     '       </apply>' +
     '     </math>' +
     '   </kineticLaw>' +
     ' </reaction>' +
    ' </listOfReactions>' +
  ' </model>' +
' </sbml>';


   1:
  Result :=
    '<?xml version="1.0" encoding="UTF-8"?>'+
'<!-- Created by libAntimony version v2.12.0.3 with libSBML version 5.18.1. -->'+
' <sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" level="3" version="1">'+
  ' <model metaid="feedback" id="feedback">'+
    ' <listOfCompartments>'+
      ' <compartment sboTerm="SBO:0000410" id="default_compartment" spatialDimensions="3" size="1" constant="true"/>'+
    ' </listOfCompartments>'+
    ' <listOfSpecies>'+
      ' <species id="X0" compartment="default_compartment" initialConcentration="10" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>'+
      ' <species id="S1" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>'+
      ' <species id="S4" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>'+
      ' <species id="S2" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>'+
      ' <species id="S3" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>'+
      ' <species id="X1" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>'+
    ' </listOfSpecies>'+
    ' <listOfParameters>'+
      ' <parameter id="VM1" value="10" constant="true"/>'+
      ' <parameter id="Keq1" value="10" constant="true"/>'+
      ' <parameter id="h" value="10" constant="true"/>'+
      ' <parameter id="V4" value="2.5" constant="true"/>'+
      ' <parameter id="KS4" value="0.5" constant="true"/>'+
    ' </listOfParameters>'+
    ' <listOfReactions>'+
      ' <reaction id="J0" reversible="true" fast="false">'+
        ' <listOfReactants>'+
          ' <speciesReference species="X0" stoichiometry="1" constant="true"/>'+
        ' </listOfReactants>'+
        ' <listOfProducts>'+
          ' <speciesReference species="S1" stoichiometry="1" constant="true"/>'+
        ' </listOfProducts>'+
        ' <listOfModifiers>'+
          ' <modifierSpeciesReference species="S4"/>'+
        ' </listOfModifiers>'+
        ' <kineticLaw>'+
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">'+
            ' <apply>'+
              ' <divide/>'+
              ' <apply>'+
                ' <times/>'+
                ' <ci> VM1 </ci>'+
                ' <apply>'+
                  ' <minus/>'+
                  ' <ci> X0 </ci>'+
                  ' <apply>'+
                    ' <divide/>'+
                    ' <ci> S1 </ci>'+
                    ' <ci> Keq1 </ci>'+
                 '   </apply>'+
              '  </apply>'+
              ' </apply>'+
              ' <apply>'+
                ' <plus/>'+
                ' <cn type="integer"> 1 </cn>'+
                ' <ci> X0 </ci>'+
                ' <ci> S1 </ci>'+
                ' <apply>'+
                  ' <power/>'+
                  ' <ci> S4 </ci>'+
                  ' <ci> h </ci>'+
                ' </apply>'+
              ' </apply>'+
            ' </apply>'+
          ' </math>'+
        ' </kineticLaw>'+
      ' </reaction>'+
      ' <reaction id="J1" reversible="true" fast="false">'+
        ' <listOfReactants>'+
          ' <speciesReference species="S1" stoichiometry="1" constant="true"/>'+
        ' </listOfReactants>'+
        ' <listOfProducts>'+
          ' <speciesReference species="S2" stoichiometry="1" constant="true"/>'+
        ' </listOfProducts>'+
        ' <kineticLaw>'+
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">'+
            ' <apply>'+
              ' <divide/>'+
              ' <apply>'+
                ' <minus/>'+
                ' <apply>'+
                  ' <times/>'+
                  ' <cn type="integer"> 10 </cn>'+
                  ' <ci> S1 </ci>'+
                ' </apply>'+
                ' <apply>'+
                  ' <times/>'+
                  ' <cn type="integer"> 2 </cn>'+
                  ' <ci> S2 </ci>'+
                ' </apply>'+
              ' </apply>'+
              ' <apply>'+
                ' <plus/>'+
                ' <cn type="integer"> 1 </cn>'+
                ' <ci> S1 </ci>'+
                ' <ci> S2 </ci>'+
              ' </apply>'+
            ' </apply>'+
          ' </math>'+
        ' </kineticLaw>'+
      ' </reaction>'+
      ' <reaction id="J2" reversible="true" fast="false">'+
        ' <listOfReactants>'+
          ' <speciesReference species="S2" stoichiometry="1" constant="true"/>'+
        ' </listOfReactants>'+
        ' <listOfProducts>'+
          ' <speciesReference species="S3" stoichiometry="1" constant="true"/>'+
        ' </listOfProducts>'+
        ' <kineticLaw>'+
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">'+
            ' <apply>'+
              ' <divide/>'+
              ' <apply>'+
                ' <minus/>'+
                ' <apply>'+
                  ' <times/>'+
                  ' <cn type="integer"> 10 </cn>'+
                  ' <ci> S2 </ci>'+
                ' </apply>'+
                ' <apply>'+
                  ' <times/>'+
                  ' <cn type="integer"> 2 </cn>'+
                  ' <ci> S3 </ci>'+
                ' </apply>'+
              ' </apply>'+
              ' <apply>'+
                ' <plus/>'+
                ' <cn type="integer"> 1 </cn>'+
                ' <ci> S2 </ci>'+
                ' <ci> S3 </ci>'+
              ' </apply>'+
            ' </apply>'+
          ' </math>'+
        ' </kineticLaw>'+
      ' </reaction>'+
      ' <reaction id="J3" reversible="true" fast="false">'+
        ' <listOfReactants>'+
          ' <speciesReference species="S3" stoichiometry="1" constant="true"/>'+
        ' </listOfReactants>'+
        ' <listOfProducts>'+
          ' <speciesReference species="S4" stoichiometry="1" constant="true"/>'+
        ' </listOfProducts>'+
        ' <kineticLaw>'+
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">'+
            ' <apply>'+
              ' <divide/>'+
              ' <apply>'+
                ' <minus/>'+
                ' <apply>'+
                  ' <times/>'+
                  ' <cn type="integer"> 10 </cn>'+
                  ' <ci> S3 </ci>'+
                ' </apply>'+
                ' <apply>'+
                  ' <times/>'+
                  ' <cn type="integer"> 2 </cn>'+
                  ' <ci> S4 </ci>'+
                ' </apply>'+
              ' </apply>'+
              ' <apply>'+
                ' <plus/>'+
                ' <cn type="integer"> 1 </cn>'+
                ' <ci> S3 </ci>'+
                ' <ci> S4 </ci>'+
              ' </apply>'+
            ' </apply>'+
          ' </math>'+
        ' </kineticLaw>'+
      ' </reaction>'+
      ' <reaction id="J4" reversible="true" fast="false">'+
        ' <listOfReactants>'+
          ' <speciesReference species="S4" stoichiometry="1" constant="true"/>'+
        ' </listOfReactants>'+
        ' <listOfProducts>'+
          ' <speciesReference species="X1" stoichiometry="1" constant="true"/>'+
        ' </listOfProducts>'+
        ' <kineticLaw>'+
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">'+
            ' <apply>'+
              ' <divide/>'+
              ' <apply>'+
                ' <times/>'+
                ' <ci> V4 </ci>'+
                ' <ci> S4 </ci>'+
              ' </apply>'+
              ' <apply>'+
                ' <plus/>'+
                ' <ci> KS4 </ci>'+
                ' <ci> S4 </ci>'+
              ' </apply>'+
            ' </apply>'+
          ' </math>'+
        ' </kineticLaw>'+
      ' </reaction>'+
    ' </listOfReactions>'+
  ' </model>'+
' </sbml>';

  2:
    Result := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>' +
' <sbml xmlns="http://www.sbml.org/sbml/level2" level="2" metaid="metaid_0000001" version="1">' +
  ' <model id="Goldbeter1990_CalciumSpike_CICR" metaid="metaid_0000002" name="Goldbeter1990_CalciumSpike_CICR">' +
      ' <listOfUnitDefinitions>' +
      ' <unitDefinition id="substance" metaid="metaid_0000031" name="micromole">' +
        ' <listOfUnits>' +
          ' <unit kind="mole" metaid="_8f729201-266f-41c7-ad59-1d15347574c6" scale="-6"/>' +
        ' </listOfUnits>' +
      ' </unitDefinition>' +
      ' <unitDefinition id="uM_per_sec" metaid="metaid_0000032" name="uM_per_sec">' +
        ' <listOfUnits>' +
          ' <unit kind="mole" metaid="c8ca223e-8c63-48e2-a5a4-dd77ec5c6bde" scale="-6"/>' +
          ' <unit exponent="-1" kind="litre" metaid="_831f4295-fce8-4870-956a-a4cc96019e66"/>' +
          ' <unit exponent="-1" kind="second" metaid="c6580c1f-c684-4993-bc09-8e209e69591a"/>' +
        ' </listOfUnits>' +
      ' </unitDefinition>' +
      ' <unitDefinition id="sec_inv" metaid="metaid_0000033" name="sec_inv">' +
        ' <listOfUnits>' +
          ' <unit exponent="-1" kind="second" metaid="_5bc4bfb8-4523-4969-941c-7a97213685bd"/>' +
        ' </listOfUnits>' +
      ' </unitDefinition>' +
      ' <unitDefinition id="uM" metaid="metaid_0000034" name="uM">' +
        ' <listOfUnits>' +
          ' <unit kind="mole" metaid="afcc2096-e246-4158-ad1c-00a378767e3f" scale="-6"/>' +
          ' <unit exponent="-1" kind="litre" metaid="_0997bfd1-ae97-4fbb-abcf-436ac0d71e1f"/>' +
        ' </listOfUnits>' +
      ' </unitDefinition>' +
    ' </listOfUnitDefinitions>' +
    ' <listOfCompartments>' +
      ' <compartment id="cytosol" metaid="metaid_0000016" name="cytosol" size="1">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000016">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0005829"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
            ' </compartment>' +
      ' <compartment id="store" metaid="metaid_0000030" name="store" size="1">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000030">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0005783"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
            ' </compartment>' +
    ' </listOfCompartments>' +
    ' <listOfSpecies>' +
      ' <species compartment="cytosol" id="Z" initialConcentration="0.15" metaid="metaid_0000018">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000018">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/chebi/CHEBI:29108"/>' +
	' <rdf:li rdf:resource="http://identifiers.org/kegg.compound/C00076"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
            ' </species>' +
      ' <species compartment="store" id="Y" initialConcentration="1.6" metaid="metaid_0000019">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000019">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/chebi/CHEBI:29108"/>' +
	' <rdf:li rdf:resource="http://identifiers.org/kegg.compound/C00076"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
            ' </species>' +
    ' </listOfSpecies>' +
    ' <listOfParameters>' +
      ' <parameter id="v0" metaid="metaid_0000003" units="uM_per_sec" value="1"/>' +
      ' <parameter id="v1" metaid="metaid_0000004" units="uM_per_sec" value="7.3"/>' +
      ' <parameter id="beta" metaid="metaid_0000005" units="dimensionless" value="0.301"/>' +
      ' <parameter id="Vm2" metaid="metaid_0000006" units="uM_per_sec" value="65"/>' +
      ' <parameter id="n" metaid="metaid_0000007" units="dimensionless" value="2"/>' +
      ' <parameter id="K2" metaid="metaid_0000008" units="uM" value="1"/>' +
      ' <parameter id="Vm3" metaid="metaid_0000009" units="uM_per_sec" value="500"/>' +
      ' <parameter id="m" metaid="metaid_0000010" units="dimensionless" value="2"/>' +
      ' <parameter id="Kr" metaid="metaid_0000012" units="uM" value="2"/>' +
      ' <parameter id="Ka" metaid="metaid_0000013" units="uM" value="0.9"/>' +
      ' <parameter id="kf" metaid="metaid_0000014" units="sec_inv" value="1"/>' +
      ' <parameter id="k" metaid="metaid_0000015" units="sec_inv" value="10"/>' +
      ' <parameter id="p" metaid="metaid_0000035" units="dimensionless" value="4"/>' +
    ' </listOfParameters>' +
    ' <listOfReactions>' +
      ' <reaction id="R0" metaid="metaid_0000020" name="Ca influx" reversible="false">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000020">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0006816"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
              ' <listOfProducts>' +
          ' <speciesReference metaid="e609dea7-c94f-47ed-8dcb-4c481df86646" species="Z"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw metaid="_7aeae61a-6842-42ba-9706-ae0f7a5a5f8e">' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML"> ' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> cytosol </ci>' +
              ' <ci> v0 </ci>' +
            ' </apply>' +
          ' </math>' +
                ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="R1" metaid="metaid_0000021" name="InsP3 dependent Ca influx" reversible="false">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000021">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0005220"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
              ' <listOfProducts>' +
          ' <speciesReference metaid="_72298131-bc67-44ff-99d3-b9b0c5d2b1b3" species="Z"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw metaid="f99fcd16-52cf-4bae-a23b-0a06027f175a">' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML"> ' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> cytosol </ci>' +
              ' <ci> v1 </ci>' +
              ' <ci> beta </ci>' +
            ' </apply>' +
          ' </math>' +
                ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="R2" metaid="metaid_0000022" name="ATP driven Ca pumping into store" reversible="false">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000022">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0032470"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
              ' <listOfReactants>' +
          ' <speciesReference metaid="af3e2e15-2528-4f65-bd8d-8c8c0d38b28c" species="Z"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference metaid="_041b63ed-fd20-49b3-930b-070127be137b" species="Y"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw metaid="_2a5c5a01-da8b-4d83-af57-b2413017b43a">' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML"> ' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> cytosol </ci>' +
              ' <apply>' +
                ' <divide/>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <ci> Vm2 </ci>' +
                  ' <apply>' +
                    ' <power/>' +
                    ' <ci> Z </ci>' +
                    ' <ci> n </ci>' +
                  ' </apply>' +
                ' </apply>' +
                ' <apply>' +
                  ' <plus/>' +
                  ' <apply>' +
                    ' <power/>' +
                    ' <ci> K2 </ci>' +
                    ' <ci> n </ci>' +
                  ' </apply>' +
                  ' <apply>' +
                    ' <power/>' +
                    ' <ci> Z </ci>' +
                    ' <ci> n </ci>' +
                  ' </apply>' +
                ' </apply>' +
              ' </apply>' +
            ' </apply>' +
          ' </math>' +
                ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="R3" metaid="metaid_0000023" name="ATP driven pumping into cytosol" reversible="false">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000023">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0007204"/>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0048763"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
              ' <listOfReactants>' +
          ' <speciesReference metaid="d906616e-84f8-4176-9ee1-820a54e2f9a1" species="Y"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference metaid="b1b4e20e-28f0-4d6a-8be7-54b681904657" species="Z"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw metaid="_646058f1-8b48-41c9-8422-962619f2e2d7">' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> store </ci>' +
              ' <apply>' +
                ' <divide/>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <ci> Vm3 </ci>' +
                  ' <apply>' +
                    ' <power/>' +
                    ' <ci> Y </ci>' +
                    ' <ci> m </ci>' +
                  ' </apply>' +
                  ' <apply>' +
                    ' <power/>' +
                    ' <ci> Z </ci>' +
                    ' <ci> p </ci>' +
                  ' </apply>' +
                ' </apply>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <apply>' +
                    ' <plus/>' +
                    ' <apply>' +
                      ' <power/>' +
                      ' <ci> Kr </ci>' +
                      ' <ci> m </ci>' +
                    ' </apply>' +
                    ' <apply>' +
                      ' <power/>' +
                      ' <ci> Y </ci>' +
                      ' <ci> m </ci>' +
                    ' </apply>' +
                  ' </apply>' +
                  ' <apply>' +
                    ' <plus/>' +
                    ' <apply>' +
                      ' <power/>' +
                      ' <ci> Ka </ci>' +
                      ' <ci> p </ci>' +
                    ' </apply>' +
                    ' <apply>' +
                      ' <power/>' +
                      ' <ci> Z </ci>' +
                      ' <ci> p </ci>' +
                    ' </apply>' +
                  ' </apply>' +
                ' </apply>' +
              ' </apply>' +
            ' </apply>' +
          ' </math>' +
                ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="Rf" metaid="metaid_0000024" name="Ca leak" reversible="false">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000024">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0007204"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
              ' <listOfReactants>' +
          ' <speciesReference metaid="a4edd980-5bf9-40b4-b5f8-4ec6b4543f57" species="Y"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference metaid="_6bad1f1b-178e-4ee1-8a39-757452b5b93c" species="Z"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw metaid="_0b2b03a6-7011-4502-b3c6-b91e6c9cec43">' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML"> ' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> store </ci>' +
              ' <ci> kf </ci>' +
              ' <ci> Y </ci>' +
            ' </apply>' +
          ' </math>' +
                ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="R_eff" metaid="metaid_0000025" name="Ca efflux" reversible="false">' +
        ' <annotation>' +
	' <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:bqmodel="http://biomodels.net/model-qualifiers/" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/">' +
            ' <rdf:Description rdf:about="#metaid_0000025">' +
	' <bqbiol:is>' +
	' <rdf:Bag>' +
	' <rdf:li rdf:resource="http://identifiers.org/go/GO:0006816"/>' +
	' </rdf:Bag>' +
	' </bqbiol:is>' +
	' </rdf:Description>' +

	' </rdf:RDF>' +
	' </annotation>' +
              ' <listOfReactants>' +
          ' <speciesReference metaid="_8fd801b7-4549-46b1-8349-f0caeae55771" species="Z"/>' +
        ' </listOfReactants>' +
        ' <kineticLaw metaid="cccfaae8-c10a-4270-bfb3-49e01116ab25">' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML"> ' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> cytosol </ci>' +
              ' <ci> k </ci>' +
              ' <ci> Z </ci>' +
            ' </apply>' +
          ' </math>' +
                ' </kineticLaw>' +
      ' </reaction>' +
    ' </listOfReactions>' +
  ' </model>' +
' </sbml>';
   
  3:
   Result := ' <?xml version="1.0" encoding="UTF-8"?>' +
  ' <sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" ' +
  'level="3" version="1" layout:required="false" render:required="false">' +
  ' <model id="Model_layout">' +
    ' <listOfCompartments>' +
      ' <compartment id="_compartment_default_" constant="true"/>' +
      ' <compartment id="compartment" constant="true"/>' +
    ' </listOfCompartments>' +
    ' <listOfSpecies>' +
      ' <species id="External_glucose" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="true"/>' +
      ' <species id="Glucose" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="ATP" compartment="compartment" initialConcentration="3" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="fructose_1_6_bisphosphate" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="ADP" compartment="compartment" initialConcentration="1" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="glyceraldehyde_3_phosphate" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="NADH" compartment="compartment" initialConcentration="0.5" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="NAD" compartment="compartment" initialConcentration="0.5" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="Glycerol" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="true"/>' +
      ' <species id="glycerate_3_phosphate" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="pyruvate" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="Acetyladehyde" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="ethanol" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="true"/>' +
      ' <species id="External_acetaldehyde" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      ' <species id="Sink" compartment="compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="true"/>' +
    ' </listOfSpecies>' +
    ' <listOfParameters>' +
      ' <parameter id="J0_inputFlux" value="50" constant="true"/>' +
      ' <parameter id="J1_k1" value="550" constant="true"/>' +
      ' <parameter id="J1_Ki" value="1" constant="true"/>' +
      ' <parameter id="J1_n" value="4" constant="true"/>' +
      ' <parameter id="J2_J2_k" value="9.8" constant="true"/>' +
      ' <parameter id="J3_J3_k" value="85.7" constant="true"/>' +
      ' <parameter id="J4_kg" value="323.8" constant="true"/>' +
      ' <parameter id="J4_kp" value="76411.1" constant="true"/>' +
      ' <parameter id="J4_ka" value="57823.1" constant="true"/>' +
      ' <parameter id="J4_kk" value="23.7" constant="true"/>' +
      ' <parameter id="J5_J5_k" value="80" constant="true"/>' +
      ' <parameter id="J6_J6_k" value="9.7" constant="true"/>' +
      ' <parameter id="J7_J7_k" value="2000" constant="true"/>' +
      ' <parameter id="J8_J8_k1" value="375" constant="true"/>' +
      ' <parameter id="J8_J8_k2" value="375" constant="true"/>' +
      ' <parameter id="J9_J9_k" value="28" constant="true"/>' +
      ' <parameter id="J10_J10_k" value="80" constant="true"/>' +
      ' <parameter id="J2_k" value="9.8" constant="true"/>' +
      ' <parameter id="J3_k" value="85.7" constant="true"/>' +
      ' <parameter id="J5_k" value="80" constant="true"/>' +
      ' <parameter id="J6_k" value="9.7" constant="true"/>' +
      ' <parameter id="J7_k" value="2000" constant="true"/>' +
      ' <parameter id="J8_k1" value="375" constant="true"/>' +
      ' <parameter id="J8_k2" value="375" constant="true"/>' +
      ' <parameter id="J9_k" value="28" constant="true"/>' +
      ' <parameter id="J10_k" value="80" constant="true"/>' +
    ' </listOfParameters>' +
    ' <listOfReactions>' +
      ' <reaction id="J0" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J0_rct0" species="External_glucose" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J0_prd0" species="Glucose" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <ci> J0_inputFlux </ci>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J1" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J1_rct0" species="Glucose" constant="false"/>' +
          ' <speciesReference id="SpecRef_J1_rct1" species="ATP" constant="false" stoichiometry="2"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J1_prd0" species="fructose_1_6_bisphosphate" constant="false"/>' +
          ' <speciesReference id="SpecRef_J1_prd1" species="ADP" constant="false" stoichiometry="2"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J1_k1 </ci>' +
              ' <ci> Glucose </ci>' +
              ' <ci> ATP </ci>' +
              ' <apply>' +
                ' <divide/>' +
                ' <cn type="integer"> 1 </cn>' +
                ' <apply>' +
                  ' <plus/>' +
                  ' <cn type="integer"> 1 </cn>' +
                  ' <apply>' +
                    ' <power/>' +
                    ' <apply>' +
                      ' <divide/>' +
                      ' <ci> ATP </ci>' +
                      ' <ci> J1_Ki </ci>' +
                    ' </apply>' +
                    ' <ci> J1_n </ci>' +
                  ' </apply>' +
                ' </apply>' +
              ' </apply>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J2" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J2_rct0" species="fructose_1_6_bisphosphate" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J2_prd0" species="glyceraldehyde_3_phosphate" constant="false"/>' +
          ' <speciesReference id="SpecRef_J2_prd1" species="glyceraldehyde_3_phosphate" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J2_J2_k </ci>' +
              ' <ci> fructose_1_6_bisphosphate </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J3" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J3_rct0" species="glyceraldehyde_3_phosphate" constant="false"/>' +
          ' <speciesReference id="SpecRef_J3_rct1" species="NADH" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J3_prd0" species="NAD" constant="false"/>' +
          ' <speciesReference id="SpecRef_J3_prd1" species="Glycerol" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J3_J3_k </ci>' +
              ' <ci> glyceraldehyde_3_phosphate </ci>' +
              ' <ci> NADH </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J4" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J4_rct0" species="glyceraldehyde_3_phosphate" constant="false"/>' +
          ' <speciesReference id="SpecRef_J4_rct1" species="ADP" constant="false"/>' +
          ' <speciesReference id="SpecRef_J4_rct2" species="NAD" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J4_prd0" species="ATP" constant="false"/>' +
          ' <speciesReference id="SpecRef_J4_prd1" species="glycerate_3_phosphate" constant="false"/>' +
          ' <speciesReference id="SpecRef_J4_prd2" species="NADH" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <divide/>' +
              ' <apply>' +
                ' <minus/>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <ci> J4_kg </ci>' +
                  ' <ci> J4_kp </ci>' +
                  ' <ci> glyceraldehyde_3_phosphate </ci>' +
                  ' <ci> NAD </ci>' +
                  ' <ci> ADP </ci>' +
                ' </apply>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <ci> J4_ka </ci>' +
                  ' <ci> J4_kk </ci>' +
                  ' <ci> glycerate_3_phosphate </ci>' +
                  ' <ci> ATP </ci>' +
                  ' <ci> NADH </ci>' +
                ' </apply>' +
              ' </apply>' +
              ' <apply>' +
                ' <plus/>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <ci> J4_ka </ci>' +
                  ' <ci> NADH </ci>' +
                ' </apply>' +
                ' <apply>' +
                  ' <times/>' +
                  ' <ci> J4_kp </ci>' +
                  ' <ci> ADP </ci>' +
                ' </apply>' +
              ' </apply>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J5" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J5_rct0" species="glycerate_3_phosphate" constant="false"/>' +
          ' <speciesReference id="SpecRef_J5_rct1" species="ADP" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J5_prd0" species="ATP" constant="false"/>' +
          ' <speciesReference id="SpecRef_J5_prd1" species="pyruvate" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J5_J5_k </ci>' +
              ' <ci> glycerate_3_phosphate </ci>' +
              ' <ci> ADP </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J6" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J6_rct0" species="pyruvate" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J6_prd0" species="Acetyladehyde" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J6_J6_k </ci>' +
              ' <ci> pyruvate </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J7" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J7_rct0" species="Acetyladehyde" constant="false"/>' +
          ' <speciesReference id="SpecRef_J7_rct1" species="NADH" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J7_prd0" species="NAD" constant="false"/>' +
          ' <speciesReference id="SpecRef_J7_prd1" species="ethanol" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J7_J7_k </ci>' +
              ' <ci> Acetyladehyde </ci>' +
              ' <ci> NADH </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J8" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J8_rct0" species="Acetyladehyde" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J8_prd0" species="External_acetaldehyde" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <minus/>' +
              ' <apply>' +
                ' <times/>' +
                ' <ci> J8_J8_k1 </ci>' +
                ' <ci> Acetyladehyde </ci>' +
              ' </apply>' +
              ' <apply>' +
                ' <times/>' +
                ' <ci> J8_J8_k2 </ci>' +
                ' <ci> External_acetaldehyde </ci>' +
              ' </apply>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J9" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J9_rct0" species="ATP" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J9_prd0" species="ADP" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J9_J9_k </ci>' +
              ' <ci> ATP </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
      ' <reaction id="J10" reversible="true" fast="false">' +
        ' <listOfReactants>' +
          ' <speciesReference id="SpecRef_J10_rct0" species="External_acetaldehyde" constant="false"/>' +
        ' </listOfReactants>' +
        ' <listOfProducts>' +
          ' <speciesReference id="SpecRef_J10_prd0" species="Sink" constant="false"/>' +
        ' </listOfProducts>' +
        ' <kineticLaw>' +
          ' <math xmlns="http://www.w3.org/1998/Math/MathML">' +
            ' <apply>' +
              ' <times/>' +
              ' <ci> J10_J10_k </ci>' +
              ' <ci> External_acetaldehyde </ci>' +
            ' </apply>' +
          ' </math>' +
        ' </kineticLaw>' +
      ' </reaction>' +
    ' </listOfReactions>' +
    ' <layout:listOfLayouts xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1">' +
      ' <layout:layout layout:id="Layout_1">' +
        ' <layout:dimensions layout:width="800" layout:height="800"/>' +
        ' <layout:listOfCompartmentGlyphs>' +
          ' <layout:compartmentGlyph layout:id="CompG_compartment" layout:compartment="compartment">' +
            ' <layout:boundingBox layout:id="bb_compartment">' +
              ' <layout:position layout:x="7" layout:y="10"/>' +
              ' <layout:dimensions layout:width="3900" layout:height="2400"/>' +
            ' </layout:boundingBox>' +
          ' </layout:compartmentGlyph>' +
        ' </layout:listOfCompartmentGlyphs>' +
        ' <layout:listOfSpeciesGlyphs>' +
          ' <layout:speciesGlyph layout:id="SpecG_External_glucose_idx_0" layout:species="External_glucose">' +
            ' <layout:boundingBox layout:id="bb_External_glucose_idx_0">' +
              ' <layout:position layout:x="118" layout:y="61"/>' +
              ' <layout:dimensions layout:width="97" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_Glucose_idx_1" layout:species="Glucose">' +
            ' <layout:boundingBox layout:id="bb_Glucose_idx_1">' +
              ' <layout:position layout:x="277" layout:y="84"/>' +
              ' <layout:dimensions layout:width="54" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ATP_idx_2" layout:species="ATP">' +
            ' <layout:boundingBox layout:id="bb_ATP_idx_2">' +
              ' <layout:position layout:x="506" layout:y="79"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_fructose_1_6_bisphosphate_idx_3" layout:species="fructose_1_6_bisphosphate">' +
            ' <layout:boundingBox layout:id="bb_fructose_1_6_bisphosphate_idx_3">' +
              ' <layout:position layout:x="290" layout:y="202"/>' +
              ' <layout:dimensions layout:width="150" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ADP_idx_4" layout:species="ADP">' +
            ' <layout:boundingBox layout:id="bb_ADP_idx_4">' +
              ' <layout:position layout:x="503" layout:y="172"/>' +
              ' <layout:dimensions layout:width="35" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_glyceraldehyde_3_phosphate_idx_5" layout:species="glyceraldehyde_3_phosphate">' +
            ' <layout:boundingBox layout:id="bb_glyceraldehyde_3_phosphate_idx_5">' +
              ' <layout:position layout:x="283" layout:y="314"/>' +
              ' <layout:dimensions layout:width="157" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_NADH_idx_6" layout:species="NADH">' +
            ' <layout:boundingBox layout:id="bb_NADH_idx_6">' +
              ' <layout:position layout:x="250" layout:y="245"/>' +
              ' <layout:dimensions layout:width="43" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_NAD_idx_7" layout:species="NAD">' +
            ' <layout:boundingBox layout:id="bb_NAD_idx_7">' +
              ' <layout:position layout:x="135" layout:y="247"/>' +
              ' <layout:dimensions layout:width="36" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_Glycerol_idx_8" layout:species="Glycerol">' +
            ' <layout:boundingBox layout:id="bb_Glycerol_idx_8">' +
              ' <layout:position layout:x="118" layout:y="350"/>' +
              ' <layout:dimensions layout:width="54" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ADP_idx_9" layout:species="ADP">' +
            ' <layout:boundingBox layout:id="bb_ADP_idx_9">' +
              ' <layout:position layout:x="474" layout:y="351"/>' +
              ' <layout:dimensions layout:width="35" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_NAD_idx_10" layout:species="NAD">' +
            ' <layout:boundingBox layout:id="bb_NAD_idx_10">' +
              ' <layout:position layout:x="229" layout:y="356"/>' +
              ' <layout:dimensions layout:width="36" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ATP_idx_11" layout:species="ATP">' +
            ' <layout:boundingBox layout:id="bb_ATP_idx_11">' +
              ' <layout:position layout:x="483" layout:y="415"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_glycerate_3_phosphate_idx_12" layout:species="glycerate_3_phosphate">' +
            ' <layout:boundingBox layout:id="bb_glycerate_3_phosphate_idx_12">' +
              ' <layout:position layout:x="299" layout:y="438"/>' +
              ' <layout:dimensions layout:width="128" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_NADH_idx_13" layout:species="NADH">' +
            ' <layout:boundingBox layout:id="bb_NADH_idx_13">' +
              ' <layout:position layout:x="224" layout:y="403"/>' +
              ' <layout:dimensions layout:width="43" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ADP_idx_14" layout:species="ADP">' +
            ' <layout:boundingBox layout:id="bb_ADP_idx_14">' +
              ' <layout:position layout:x="478" layout:y="474"/>' +
              ' <layout:dimensions layout:width="35" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ATP_idx_15" layout:species="ATP">' +
            ' <layout:boundingBox layout:id="bb_ATP_idx_15">' +
              ' <layout:position layout:x="480" layout:y="543"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_pyruvate_idx_16" layout:species="pyruvate">' +
            ' <layout:boundingBox layout:id="bb_pyruvate_idx_16">' +
              ' <layout:position layout:x="330" layout:y="569"/>' +
              ' <layout:dimensions layout:width="57" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_Acetyladehyde_idx_17" layout:species="Acetyladehyde">' +
            ' <layout:boundingBox layout:id="bb_Acetyladehyde_idx_17">' +
              ' <layout:position layout:x="317" layout:y="652"/>' +
              ' <layout:dimensions layout:width="87" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_NADH_idx_18" layout:species="NADH">' +
            ' <layout:boundingBox layout:id="bb_NADH_idx_18">' +
              ' <layout:position layout:x="235" layout:y="595"/>' +
              ' <layout:dimensions layout:width="43" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_NAD_idx_19" layout:species="NAD">' +
            ' <layout:boundingBox layout:id="bb_NAD_idx_19">' +
              ' <layout:position layout:x="112" layout:y="631"/>' +
              ' <layout:dimensions layout:width="36" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_ethanol_idx_20" layout:species="ethanol">' +
            ' <layout:boundingBox layout:id="bb_ethanol_idx_20">' +
              ' <layout:position layout:x="134" layout:y="738"/>' +
              ' <layout:dimensions layout:width="49" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_External_acetaldehyde_idx_21" layout:species="External_acetaldehyde">' +
            ' <layout:boundingBox layout:id="bb_External_acetaldehyde_idx_21">' +
              ' <layout:position layout:x="422" layout:y="722"/>' +
              ' <layout:dimensions layout:width="124" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
          ' <layout:speciesGlyph layout:id="SpecG_Sink_idx_22" layout:species="Sink">' +
            ' <layout:boundingBox layout:id="bb_Sink_idx_22">' +
              ' <layout:position layout:x="571" layout:y="777"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:speciesGlyph>' +
        ' </layout:listOfSpeciesGlyphs>' +
        ' <layout:listOfReactionGlyphs>' +
          ' <layout:reactionGlyph layout:id="RectionG_J0" layout:reaction="J0">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="235.25" layout:y="84.5"/>' +
                  ' <layout:end layout:x="235.25" layout:y="84.5"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J0_rct0" layout:speciesReference="SpecRef_J0_rct0" layout:speciesGlyph="SpecG_External_glucose_idx_0" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="166.5" layout:y="73"/>' +
                      ' <layout:end layout:x="235.25" layout:y="84.5"/>' +
                      ' <layout:basePoint1 layout:x="200.88" layout:y="78.75"/>' +
                      ' <layout:basePoint2 layout:x="212.33" layout:y="80.67"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J0_prd0" layout:speciesReference="SpecRef_J0_prd0" layout:speciesGlyph="SpecG_Glucose_idx_1" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="235.25" layout:y="84.5"/>' +
                      ' <layout:end layout:x="304" layout:y="96"/>' +
                      ' <layout:basePoint1 layout:x="258.17" layout:y="88.33"/>' +
                      ' <layout:basePoint2 layout:x="269.62" layout:y="90.25"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J1" layout:reaction="J1">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="425.125" layout:y="148.25"/>' +
                  ' <layout:end layout:x="425.125" layout:y="148.25"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J1_rct0" layout:speciesReference="SpecRef_J1_rct0" layout:speciesGlyph="SpecG_Glucose_idx_1" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="304" layout:y="96"/>' +
                      ' <layout:end layout:x="425.125" layout:y="148.25"/>' +
                      ' <layout:basePoint1 layout:x="383.06" layout:y="103.12"/>' +
                      ' <layout:basePoint2 layout:x="418.75" layout:y="117.5"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J1_rct1" layout:speciesReference="SpecRef_J1_rct1" layout:speciesGlyph="SpecG_ATP_idx_2" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="523" layout:y="91"/>' +
                      ' <layout:end layout:x="425.125" layout:y="148.25"/>' +
                      ' <layout:basePoint1 layout:x="450.56" layout:y="100.62"/>' +
                      ' <layout:basePoint2 layout:x="418.75" layout:y="117.5"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J1_prd0" layout:speciesReference="SpecRef_J1_prd0" layout:speciesGlyph="SpecG_fructose_1_6_bisphosphate_idx_3" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="425.125" layout:y="148.25"/>' +
                      ' <layout:end layout:x="365" layout:y="214"/>' +
                      ' <layout:basePoint1 layout:x="431.5" layout:y="179"/>' +
                      ' <layout:basePoint2 layout:x="409.56" layout:y="180.12"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J1_prd1" layout:speciesReference="SpecRef_J1_prd1" layout:speciesGlyph="SpecG_ADP_idx_4" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="425.125" layout:y="148.25"/>' +
                      ' <layout:end layout:x="520.5" layout:y="184"/>' +
                      ' <layout:basePoint1 layout:x="431.5" layout:y="179"/>' +
                      ' <layout:basePoint2 layout:x="476.31" layout:y="173.12"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J2" layout:reaction="J2">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="363.25" layout:y="274"/>' +
                  ' <layout:end layout:x="363.25" layout:y="274"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J2_rct0" layout:speciesReference="SpecRef_J2_rct0" layout:speciesGlyph="SpecG_fructose_1_6_bisphosphate_idx_3" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="365" layout:y="214"/>' +
                      ' <layout:end layout:x="363.25" layout:y="274"/>' +
                      ' <layout:basePoint1 layout:x="364.12" layout:y="242"/>' +
                      ' <layout:basePoint2 layout:x="363.83" layout:y="255.33"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J2_prd0" layout:speciesReference="SpecRef_J2_prd0" layout:speciesGlyph="SpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="363.25" layout:y="274"/>' +
                      ' <layout:end layout:x="361.5" layout:y="326"/>' +
                      ' <layout:basePoint1 layout:x="362.67" layout:y="292.67"/>' +
                      ' <layout:basePoint2 layout:x="362.38" layout:y="298"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J3" layout:reaction="J3">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="221.25" layout:y="305"/>' +
                  ' <layout:end layout:x="221.25" layout:y="305"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J3_rct0" layout:speciesReference="SpecRef_J3_rct0" layout:speciesGlyph="SpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="361.5" layout:y="326"/>' +
                      ' <layout:end layout:x="221.25" layout:y="305"/>' +
                      ' <layout:basePoint1 layout:x="296.88" layout:y="310"/>' +
                      ' <layout:basePoint2 layout:x="256.33" layout:y="296.67"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J3_rct1" layout:speciesReference="SpecRef_J3_rct1" layout:speciesGlyph="SpecG_NADH_idx_6" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="271.5" layout:y="257"/>' +
                      ' <layout:end layout:x="221.25" layout:y="305"/>' +
                      ' <layout:basePoint1 layout:x="259.88" layout:y="286"/>' +
                      ' <layout:basePoint2 layout:x="256.33" layout:y="296.67"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J3_prd0" layout:speciesReference="SpecRef_J3_prd0" layout:speciesGlyph="SpecG_NAD_idx_7" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="221.25" layout:y="305"/>' +
                      ' <layout:end layout:x="153" layout:y="259"/>' +
                      ' <layout:basePoint1 layout:x="186.17" layout:y="313.33"/>' +
                      ' <layout:basePoint2 layout:x="172.62" layout:y="295.5"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J3_prd1" layout:speciesReference="SpecRef_J3_prd1" layout:speciesGlyph="SpecG_Glycerol_idx_8" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="221.25" layout:y="305"/>' +
                      ' <layout:end layout:x="145" layout:y="362"/>' +
                      ' <layout:basePoint1 layout:x="186.17" layout:y="313.33"/>' +
                      ' <layout:basePoint2 layout:x="174.62" layout:y="324.5"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J4" layout:reaction="J4">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="363.25" layout:y="390.5"/>' +
                  ' <layout:end layout:x="363.25" layout:y="390.5"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J4_rct0" layout:speciesReference="SpecRef_J4_rct0" layout:speciesGlyph="SpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="361.5" layout:y="326"/>' +
                      ' <layout:end layout:x="363.25" layout:y="390.5"/>' +
                      ' <layout:basePoint1 layout:x="361.88" layout:y="357.25"/>' +
                      ' <layout:basePoint2 layout:x="362" layout:y="368"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J4_rct1" layout:speciesReference="SpecRef_J4_rct1" layout:speciesGlyph="SpecG_ADP_idx_9" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="491.5" layout:y="363"/>' +
                      ' <layout:end layout:x="363.25" layout:y="390.5"/>' +
                      ' <layout:basePoint1 layout:x="396.88" layout:y="360.75"/>' +
                      ' <layout:basePoint2 layout:x="362" layout:y="368"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J4_rct2" layout:speciesReference="SpecRef_J4_rct2" layout:speciesGlyph="SpecG_NAD_idx_10" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="247" layout:y="368"/>' +
                      ' <layout:end layout:x="363.25" layout:y="390.5"/>' +
                      ' <layout:basePoint1 layout:x="328.62" layout:y="365.25"/>' +
                      ' <layout:basePoint2 layout:x="362" layout:y="368"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J4_prd0" layout:speciesReference="SpecRef_J4_prd0" layout:speciesGlyph="SpecG_ATP_idx_11" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="363.25" layout:y="390.5"/>' +
                      ' <layout:end layout:x="500" layout:y="427"/>' +
                      ' <layout:basePoint1 layout:x="364.5" layout:y="413"/>' +
                      ' <layout:basePoint2 layout:x="388.12" layout:y="423.75"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J4_prd1" layout:speciesReference="SpecRef_J4_prd1" layout:speciesGlyph="SpecG_glycerate_3_phosphate_idx_12" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="363.25" layout:y="390.5"/>' +
                      ' <layout:end layout:x="363" layout:y="450"/>' +
                      ' <layout:basePoint1 layout:x="364.5" layout:y="413"/>' +
                      ' <layout:basePoint2 layout:x="365.62" layout:y="420.25"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J4_prd2" layout:speciesReference="SpecRef_J4_prd2" layout:speciesGlyph="SpecG_NADH_idx_13" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="363.25" layout:y="390.5"/>' +
                      ' <layout:end layout:x="245.5" layout:y="415"/>' +
                      ' <layout:basePoint1 layout:x="364.5" layout:y="413"/>' +
                      ' <layout:basePoint2 layout:x="322.38" layout:y="422.75"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J5" layout:reaction="J5">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="358.25" layout:y="518"/>' +
                  ' <layout:end layout:x="358.25" layout:y="518"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J5_rct0" layout:speciesReference="SpecRef_J5_rct0" layout:speciesGlyph="SpecG_glycerate_3_phosphate_idx_12" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="363" layout:y="450"/>' +
                      ' <layout:end layout:x="358.25" layout:y="518"/>' +
                      ' <layout:basePoint1 layout:x="360.12" layout:y="485.5"/>' +
                      ' <layout:basePoint2 layout:x="358.17" layout:y="483.33"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J5_rct1" layout:speciesReference="SpecRef_J5_rct1" layout:speciesGlyph="SpecG_ADP_idx_14" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="495.5" layout:y="486"/>' +
                      ' <layout:end layout:x="358.25" layout:y="518"/>' +
                      ' <layout:basePoint1 layout:x="398.38" layout:y="483"/>' +
                      ' <layout:basePoint2 layout:x="358.17" layout:y="483.33"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J5_prd0" layout:speciesReference="SpecRef_J5_prd0" layout:speciesGlyph="SpecG_ATP_idx_15" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="358.25" layout:y="518"/>' +
                      ' <layout:end layout:x="497" layout:y="555"/>' +
                      ' <layout:basePoint1 layout:x="358.33" layout:y="552.67"/>' +
                      ' <layout:basePoint2 layout:x="398.62" layout:y="550.5"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J5_prd1" layout:speciesReference="SpecRef_J5_prd1" layout:speciesGlyph="SpecG_pyruvate_idx_16" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="358.25" layout:y="518"/>' +
                      ' <layout:end layout:x="358.5" layout:y="581"/>' +
                      ' <layout:basePoint1 layout:x="358.33" layout:y="552.67"/>' +
                      ' <layout:basePoint2 layout:x="358.88" layout:y="544"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J6" layout:reaction="J6">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="359.5" layout:y="622.5"/>' +
                  ' <layout:end layout:x="359.5" layout:y="622.5"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J6_rct0" layout:speciesReference="SpecRef_J6_rct0" layout:speciesGlyph="SpecG_pyruvate_idx_16" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="358.5" layout:y="581"/>' +
                      ' <layout:end layout:x="359.5" layout:y="622.5"/>' +
                      ' <layout:basePoint1 layout:x="359" layout:y="601.75"/>' +
                      ' <layout:basePoint2 layout:x="359.17" layout:y="608.67"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J6_prd0" layout:speciesReference="SpecRef_J6_prd0" layout:speciesGlyph="SpecG_Acetyladehyde_idx_17" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="359.5" layout:y="622.5"/>' +
                      ' <layout:end layout:x="360.5" layout:y="664"/>' +
                      ' <layout:basePoint1 layout:x="359.83" layout:y="636.33"/>' +
                      ' <layout:basePoint2 layout:x="360" layout:y="643.25"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J7" layout:reaction="J7">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="224.125" layout:y="668.5"/>' +
                  ' <layout:end layout:x="224.125" layout:y="668.5"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J7_rct0" layout:speciesReference="SpecRef_J7_rct0" layout:speciesGlyph="SpecG_Acetyladehyde_idx_17" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="360.5" layout:y="664"/>' +
                      ' <layout:end layout:x="224.125" layout:y="668.5"/>' +
                      ' <layout:basePoint1 layout:x="292.31" layout:y="666.25"/>' +
                      ' <layout:basePoint2 layout:x="264.58" layout:y="650"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J7_rct1" layout:speciesReference="SpecRef_J7_rct1" layout:speciesGlyph="SpecG_NADH_idx_18" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="256.5" layout:y="607"/>' +
                      ' <layout:end layout:x="224.125" layout:y="668.5"/>' +
                      ' <layout:basePoint1 layout:x="254.81" layout:y="636.25"/>' +
                      ' <layout:basePoint2 layout:x="264.58" layout:y="650"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J7_prd0" layout:speciesReference="SpecRef_J7_prd0" layout:speciesGlyph="SpecG_NAD_idx_19" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="224.125" layout:y="668.5"/>' +
                      ' <layout:end layout:x="130" layout:y="643"/>' +
                      ' <layout:basePoint1 layout:x="183.67" layout:y="687"/>' +
                      ' <layout:basePoint2 layout:x="181.06" layout:y="673.25"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J7_prd1" layout:speciesReference="SpecRef_J7_prd1" layout:speciesGlyph="SpecG_ethanol_idx_20" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="224.125" layout:y="668.5"/>' +
                      ' <layout:end layout:x="158.5" layout:y="750"/>' +
                      ' <layout:basePoint1 layout:x="183.67" layout:y="687"/>' +
                      ' <layout:basePoint2 layout:x="176.31" layout:y="703.25"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J8" layout:reaction="J8">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="422.25" layout:y="699"/>' +
                  ' <layout:end layout:x="422.25" layout:y="699"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J8_rct0" layout:speciesReference="SpecRef_J8_rct0" layout:speciesGlyph="SpecG_Acetyladehyde_idx_17" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="360.5" layout:y="664"/>' +
                      ' <layout:end layout:x="422.25" layout:y="699"/>' +
                      ' <layout:basePoint1 layout:x="391.38" layout:y="681.5"/>' +
                      ' <layout:basePoint2 layout:x="401.67" layout:y="687.33"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J8_prd0" layout:speciesReference="SpecRef_J8_prd0" layout:speciesGlyph="SpecG_External_acetaldehyde_idx_21" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="422.25" layout:y="699"/>' +
                      ' <layout:end layout:x="484" layout:y="734"/>' +
                      ' <layout:basePoint1 layout:x="442.83" layout:y="710.67"/>' +
                      ' <layout:basePoint2 layout:x="453.12" layout:y="716.5"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J9" layout:reaction="J9">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="521.75" layout:y="137.5"/>' +
                  ' <layout:end layout:x="521.75" layout:y="137.5"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J9_rct0" layout:speciesReference="SpecRef_J9_rct0" layout:speciesGlyph="SpecG_ATP_idx_2" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="523" layout:y="91"/>' +
                      ' <layout:end layout:x="521.75" layout:y="137.5"/>' +
                      ' <layout:basePoint1 layout:x="522.38" layout:y="114.25"/>' +
                      ' <layout:basePoint2 layout:x="522.17" layout:y="122"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J9_prd0" layout:speciesReference="SpecRef_J9_prd0" layout:speciesGlyph="SpecG_ADP_idx_4" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="521.75" layout:y="137.5"/>' +
                      ' <layout:end layout:x="520.5" layout:y="184"/>' +
                      ' <layout:basePoint1 layout:x="521.33" layout:y="153"/>' +
                      ' <layout:basePoint2 layout:x="521.12" layout:y="160.75"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
          ' <layout:reactionGlyph layout:id="RectionG_J10" layout:reaction="J10">' +
            ' <layout:curve>' +
              ' <layout:listOfCurveSegments>' +
                ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  ' <layout:start layout:x="536" layout:y="761.5"/>' +
                  ' <layout:end layout:x="536" layout:y="761.5"/>' +
                ' </layout:curveSegment>' +
              ' </layout:listOfCurveSegments>' +
            ' </layout:curve>' +
            ' <layout:listOfSpeciesReferenceGlyphs>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J10_rct0" layout:speciesReference="SpecRef_J10_rct0" layout:speciesGlyph="SpecG_External_acetaldehyde_idx_21" layout:role="substrate">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="484" layout:y="734"/>' +
                      ' <layout:end layout:x="536" layout:y="761.5"/>' +
                      ' <layout:basePoint1 layout:x="510" layout:y="747.75"/>' +
                      ' <layout:basePoint2 layout:x="518.67" layout:y="752.33"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
              ' <layout:speciesReferenceGlyph layout:id="SpecRefG_J10_prd0" layout:speciesReference="SpecRef_J10_prd0" layout:speciesGlyph="SpecG_Sink_idx_22" layout:role="product">' +
                ' <layout:curve>' +
                  ' <layout:listOfCurveSegments>' +
                    ' <layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      ' <layout:start layout:x="536" layout:y="761.5"/>' +
                      ' <layout:end layout:x="588" layout:y="789"/>' +
                      ' <layout:basePoint1 layout:x="553.33" layout:y="770.67"/>' +
                      ' <layout:basePoint2 layout:x="562" layout:y="775.25"/>' +
                    ' </layout:curveSegment>' +
                  ' </layout:listOfCurveSegments>' +
                ' </layout:curve>' +
              ' </layout:speciesReferenceGlyph>' +
            ' </layout:listOfSpeciesReferenceGlyphs>' +
          ' </layout:reactionGlyph>' +
        ' </layout:listOfReactionGlyphs>' +
        ' <layout:listOfTextGlyphs>' +
          ' <layout:textGlyph layout:id="TextG_External_glucose_idx_0" layout:originOfText="SpecG_External_glucose_idx_0" layout:graphicalObject="SpecG_External_glucose_idx_0">' +
            ' <layout:boundingBox layout:id="bb_spec_text_External_glucose_idx_0">' +
              ' <layout:position layout:x="118" layout:y="61"/>' +
              ' <layout:dimensions layout:width="97" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_Glucose_idx_1" layout:originOfText="SpecG_Glucose_idx_1" layout:graphicalObject="SpecG_Glucose_idx_1">' +
            ' <layout:boundingBox layout:id="bb_spec_text_Glucose_idx_1">' +
              ' <layout:position layout:x="277" layout:y="84"/>' +
              ' <layout:dimensions layout:width="54" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ATP_idx_2" layout:originOfText="SpecG_ATP_idx_2" layout:graphicalObject="SpecG_ATP_idx_2">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ATP_idx_2">' +
              ' <layout:position layout:x="506" layout:y="79"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_fructose_1_6_bisphosphate_idx_3" layout:originOfText="SpecG_fructose_1_6_bisphosphate_idx_3" layout:graphicalObject="SpecG_fructose_1_6_bisphosphate_idx_3">' +
            ' <layout:boundingBox layout:id="bb_spec_text_fructose_1_6_bisphosphate_idx_3">' +
              ' <layout:position layout:x="290" layout:y="202"/>' +
              ' <layout:dimensions layout:width="150" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ADP_idx_4" layout:originOfText="SpecG_ADP_idx_4" layout:graphicalObject="SpecG_ADP_idx_4">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ADP_idx_4">' +
              ' <layout:position layout:x="503" layout:y="172"/>' +
              ' <layout:dimensions layout:width="35" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_glyceraldehyde_3_phosphate_idx_5" layout:originOfText="SpecG_glyceraldehyde_3_phosphate_idx_5" layout:graphicalObject="SpecG_glyceraldehyde_3_phosphate_idx_5">' +
            ' <layout:boundingBox layout:id="bb_spec_text_glyceraldehyde_3_phosphate_idx_5">' +
              ' <layout:position layout:x="283" layout:y="314"/>' +
              ' <layout:dimensions layout:width="157" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_NADH_idx_6" layout:originOfText="SpecG_NADH_idx_6" layout:graphicalObject="SpecG_NADH_idx_6">' +
            ' <layout:boundingBox layout:id="bb_spec_text_NADH_idx_6">' +
              ' <layout:position layout:x="250" layout:y="245"/>' +
              ' <layout:dimensions layout:width="43" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_NAD_idx_7" layout:originOfText="SpecG_NAD_idx_7" layout:graphicalObject="SpecG_NAD_idx_7">' +
            ' <layout:boundingBox layout:id="bb_spec_text_NAD_idx_7">' +
              ' <layout:position layout:x="135" layout:y="247"/>' +
              ' <layout:dimensions layout:width="36" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_Glycerol_idx_8" layout:originOfText="SpecG_Glycerol_idx_8" layout:graphicalObject="SpecG_Glycerol_idx_8">' +
            ' <layout:boundingBox layout:id="bb_spec_text_Glycerol_idx_8">' +
              ' <layout:position layout:x="118" layout:y="350"/>' +
              ' <layout:dimensions layout:width="54" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ADP_idx_9" layout:originOfText="SpecG_ADP_idx_9" layout:graphicalObject="SpecG_ADP_idx_9">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ADP_idx_9">' +
              ' <layout:position layout:x="474" layout:y="351"/>' +
              ' <layout:dimensions layout:width="35" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_NAD_idx_10" layout:originOfText="SpecG_NAD_idx_10" layout:graphicalObject="SpecG_NAD_idx_10">' +
            ' <layout:boundingBox layout:id="bb_spec_text_NAD_idx_10">' +
              ' <layout:position layout:x="229" layout:y="356"/>' +
              ' <layout:dimensions layout:width="36" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ATP_idx_11" layout:originOfText="SpecG_ATP_idx_11" layout:graphicalObject="SpecG_ATP_idx_11">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ATP_idx_11">' +
              ' <layout:position layout:x="483" layout:y="415"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_glycerate_3_phosphate_idx_12" layout:originOfText="SpecG_glycerate_3_phosphate_idx_12" layout:graphicalObject="SpecG_glycerate_3_phosphate_idx_12">' +
            ' <layout:boundingBox layout:id="bb_spec_text_glycerate_3_phosphate_idx_12">' +
              ' <layout:position layout:x="299" layout:y="438"/>' +
              ' <layout:dimensions layout:width="128" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_NADH_idx_13" layout:originOfText="SpecG_NADH_idx_13" layout:graphicalObject="SpecG_NADH_idx_13">' +
            ' <layout:boundingBox layout:id="bb_spec_text_NADH_idx_13">' +
              ' <layout:position layout:x="224" layout:y="403"/>' +
              ' <layout:dimensions layout:width="43" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ADP_idx_14" layout:originOfText="SpecG_ADP_idx_14" layout:graphicalObject="SpecG_ADP_idx_14">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ADP_idx_14">' +
              ' <layout:position layout:x="478" layout:y="474"/>' +
              ' <layout:dimensions layout:width="35" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ATP_idx_15" layout:originOfText="SpecG_ATP_idx_15" layout:graphicalObject="SpecG_ATP_idx_15">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ATP_idx_15">' +
              ' <layout:position layout:x="480" layout:y="543"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_pyruvate_idx_16" layout:originOfText="SpecG_pyruvate_idx_16" layout:graphicalObject="SpecG_pyruvate_idx_16">' +
            ' <layout:boundingBox layout:id="bb_spec_text_pyruvate_idx_16">' +
              ' <layout:position layout:x="330" layout:y="569"/>' +
              ' <layout:dimensions layout:width="57" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_Acetyladehyde_idx_17" layout:originOfText="SpecG_Acetyladehyde_idx_17" layout:graphicalObject="SpecG_Acetyladehyde_idx_17">' +
            ' <layout:boundingBox layout:id="bb_spec_text_Acetyladehyde_idx_17">' +
              ' <layout:position layout:x="317" layout:y="652"/>' +
              ' <layout:dimensions layout:width="87" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_NADH_idx_18" layout:originOfText="SpecG_NADH_idx_18" layout:graphicalObject="SpecG_NADH_idx_18">' +
            ' <layout:boundingBox layout:id="bb_spec_text_NADH_idx_18">' +
              ' <layout:position layout:x="235" layout:y="595"/>' +
              ' <layout:dimensions layout:width="43" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_NAD_idx_19" layout:originOfText="SpecG_NAD_idx_19" layout:graphicalObject="SpecG_NAD_idx_19">' +
            ' <layout:boundingBox layout:id="bb_spec_text_NAD_idx_19">' +
              ' <layout:position layout:x="112" layout:y="631"/>' +
              ' <layout:dimensions layout:width="36" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_ethanol_idx_20" layout:originOfText="SpecG_ethanol_idx_20" layout:graphicalObject="SpecG_ethanol_idx_20">' +
            ' <layout:boundingBox layout:id="bb_spec_text_ethanol_idx_20">' +
              ' <layout:position layout:x="134" layout:y="738"/>' +
              ' <layout:dimensions layout:width="49" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_External_acetaldehyde_idx_21" layout:originOfText="SpecG_External_acetaldehyde_idx_21" layout:graphicalObject="SpecG_External_acetaldehyde_idx_21">' +
            ' <layout:boundingBox layout:id="bb_spec_text_External_acetaldehyde_idx_21">' +
              ' <layout:position layout:x="422" layout:y="722"/>' +
              ' <layout:dimensions layout:width="124" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
          ' <layout:textGlyph layout:id="TextG_Sink_idx_22" layout:originOfText="SpecG_Sink_idx_22" layout:graphicalObject="SpecG_Sink_idx_22">' +
            ' <layout:boundingBox layout:id="bb_spec_text_Sink_idx_22">' +
              ' <layout:position layout:x="571" layout:y="777"/>' +
              ' <layout:dimensions layout:width="34" layout:height="24"/>' +
            ' </layout:boundingBox>' +
          ' </layout:textGlyph>' +
        ' </layout:listOfTextGlyphs>' +
        ' <render:listOfRenderInformation xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1">' +
          ' <render:renderInformation render:id="info" render:name="Render Information" render:programName="RenderInformation" render:programVersion="1.0">' +
            ' <render:listOfColorDefinitions>' +
              ' <render:colorDefinition render:id="comp_fill_color__compartment_default_" render:value="#ffffff"/>' +
              ' <render:colorDefinition render:id="comp_border_color__compartment_default_" render:value="#ffffff"/>' +
              ' <render:colorDefinition render:id="comp_fill_color_compartment" render:value="#ffffff"/>' +
              ' <render:colorDefinition render:id="comp_border_color_compartment" render:value="#ffffff"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_External_glucose" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_External_glucose" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_External_glucose" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_Glucose" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_Glucose" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_Glucose" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ATP" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ATP" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ATP" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_fructose_1_6_bisphosphate" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_fructose_1_6_bisphosphate" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_fructose_1_6_bisphosphate" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ADP" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ADP" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ADP" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_glyceraldehyde_3_phosphate" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_glyceraldehyde_3_phosphate" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_glyceraldehyde_3_phosphate" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_NADH" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_NADH" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_NADH" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_NAD" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_NAD" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_NAD" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_Glycerol" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_Glycerol" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_Glycerol" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ADP" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ADP" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ADP" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_NAD" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_NAD" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_NAD" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ATP" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ATP" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ATP" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_glycerate_3_phosphate" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_glycerate_3_phosphate" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_glycerate_3_phosphate" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_NADH" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_NADH" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_NADH" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ADP" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ADP" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ADP" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ATP" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ATP" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ATP" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_pyruvate" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_pyruvate" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_pyruvate" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_Acetyladehyde" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_Acetyladehyde" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_Acetyladehyde" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_NADH" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_NADH" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_NADH" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_NAD" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_NAD" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_NAD" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_ethanol" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_ethanol" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_ethanol" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_External_acetaldehyde" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_External_acetaldehyde" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_External_acetaldehyde" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="spec_fill_color_Sink" render:value="#ffcc99c8"/>' +
              ' <render:colorDefinition render:id="spec_border_color_Sink" render:value="#ff6c09"/>' +
              ' <render:colorDefinition render:id="text_line_color_Sink" render:value="#000000"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J0" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J1" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J2" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J3" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J4" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J5" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J6" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J7" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J8" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J9" render:value="#5bb0fd"/>' +
              ' <render:colorDefinition render:id="reaction_fill_color_J10" render:value="#5bb0fd"/>' +
            ' </render:listOfColorDefinitions>' +
            ' <render:listOfStyles>' +
              ' <render:style render:id="compStyle__compartment_default_" render:typeList="COMPARTMENTGLYPH" render:idList="_compartment_default_">' +
                ' <render:g render:stroke="comp_border_color__compartment_default_" render:stroke-width="2" render:fill="comp_fill_color__compartment_default_">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="compStyle_compartment" render:typeList="COMPARTMENTGLYPH" render:idList="compartment">' +
                ' <render:g render:stroke="comp_border_color_compartment" render:stroke-width="2" render:fill="comp_fill_color_compartment">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_External_glucose" render:typeList="SPECIESGLYPH" render:idList="External_glucose">' +
                ' <render:g render:stroke="spec_border_color_External_glucose" render:stroke-width="2" render:fill="spec_fill_color_External_glucose">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="External_glucose">' +
                ' <render:g render:stroke="text_line_color_External_glucose" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_Glucose" render:typeList="SPECIESGLYPH" render:idList="Glucose">' +
                ' <render:g render:stroke="spec_border_color_Glucose" render:stroke-width="2" render:fill="spec_fill_color_Glucose">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="Glucose">' +
                ' <render:g render:stroke="text_line_color_Glucose" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ATP" render:typeList="SPECIESGLYPH" render:idList="ATP">' +
                ' <render:g render:stroke="spec_border_color_ATP" render:stroke-width="2" render:fill="spec_fill_color_ATP">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ATP">' +
                ' <render:g render:stroke="text_line_color_ATP" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_fructose_1_6_bisphosphate" render:typeList="SPECIESGLYPH" render:idList="fructose_1_6_bisphosphate">' +
                ' <render:g render:stroke="spec_border_color_fructose_1_6_bisphosphate" render:stroke-width="2" render:fill="spec_fill_color_fructose_1_6_bisphosphate">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="fructose_1_6_bisphosphate">' +
                ' <render:g render:stroke="text_line_color_fructose_1_6_bisphosphate" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ADP" render:typeList="SPECIESGLYPH" render:idList="ADP">' +
                ' <render:g render:stroke="spec_border_color_ADP" render:stroke-width="2" render:fill="spec_fill_color_ADP">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ADP">' +
                ' <render:g render:stroke="text_line_color_ADP" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_glyceraldehyde_3_phosphate" render:typeList="SPECIESGLYPH" render:idList="glyceraldehyde_3_phosphate">' +
                ' <render:g render:stroke="spec_border_color_glyceraldehyde_3_phosphate" render:stroke-width="2" render:fill="spec_fill_color_glyceraldehyde_3_phosphate">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="glyceraldehyde_3_phosphate">' +
                ' <render:g render:stroke="text_line_color_glyceraldehyde_3_phosphate" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_NADH" render:typeList="SPECIESGLYPH" render:idList="NADH">' +
                ' <render:g render:stroke="spec_border_color_NADH" render:stroke-width="2" render:fill="spec_fill_color_NADH">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="NADH">' +
                ' <render:g render:stroke="text_line_color_NADH" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_NAD" render:typeList="SPECIESGLYPH" render:idList="NAD">' +
                ' <render:g render:stroke="spec_border_color_NAD" render:stroke-width="2" render:fill="spec_fill_color_NAD">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="NAD">' +
                ' <render:g render:stroke="text_line_color_NAD" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_Glycerol" render:typeList="SPECIESGLYPH" render:idList="Glycerol">' +
                ' <render:g render:stroke="spec_border_color_Glycerol" render:stroke-width="2" render:fill="spec_fill_color_Glycerol">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="Glycerol">' +
                ' <render:g render:stroke="text_line_color_Glycerol" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ADP" render:typeList="SPECIESGLYPH" render:idList="ADP">' +
                ' <render:g render:stroke="spec_border_color_ADP" render:stroke-width="2" render:fill="spec_fill_color_ADP">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ADP">' +
                ' <render:g render:stroke="text_line_color_ADP" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_NAD" render:typeList="SPECIESGLYPH" render:idList="NAD">' +
                ' <render:g render:stroke="spec_border_color_NAD" render:stroke-width="2" render:fill="spec_fill_color_NAD">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="NAD">' +
                ' <render:g render:stroke="text_line_color_NAD" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ATP" render:typeList="SPECIESGLYPH" render:idList="ATP">' +
                ' <render:g render:stroke="spec_border_color_ATP" render:stroke-width="2" render:fill="spec_fill_color_ATP">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ATP">' +
                ' <render:g render:stroke="text_line_color_ATP" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_glycerate_3_phosphate" render:typeList="SPECIESGLYPH" render:idList="glycerate_3_phosphate">' +
                ' <render:g render:stroke="spec_border_color_glycerate_3_phosphate" render:stroke-width="2" render:fill="spec_fill_color_glycerate_3_phosphate">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="glycerate_3_phosphate">' +
                ' <render:g render:stroke="text_line_color_glycerate_3_phosphate" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_NADH" render:typeList="SPECIESGLYPH" render:idList="NADH">' +
                ' <render:g render:stroke="spec_border_color_NADH" render:stroke-width="2" render:fill="spec_fill_color_NADH">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="NADH">' +
                ' <render:g render:stroke="text_line_color_NADH" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ADP" render:typeList="SPECIESGLYPH" render:idList="ADP">' +
                ' <render:g render:stroke="spec_border_color_ADP" render:stroke-width="2" render:fill="spec_fill_color_ADP">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ADP">' +
                ' <render:g render:stroke="text_line_color_ADP" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ATP" render:typeList="SPECIESGLYPH" render:idList="ATP">' +
                ' <render:g render:stroke="spec_border_color_ATP" render:stroke-width="2" render:fill="spec_fill_color_ATP">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ATP">' +
                ' <render:g render:stroke="text_line_color_ATP" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_pyruvate" render:typeList="SPECIESGLYPH" render:idList="pyruvate">' +
                ' <render:g render:stroke="spec_border_color_pyruvate" render:stroke-width="2" render:fill="spec_fill_color_pyruvate">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="pyruvate">' +
                ' <render:g render:stroke="text_line_color_pyruvate" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_Acetyladehyde" render:typeList="SPECIESGLYPH" render:idList="Acetyladehyde">' +
                ' <render:g render:stroke="spec_border_color_Acetyladehyde" render:stroke-width="2" render:fill="spec_fill_color_Acetyladehyde">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="Acetyladehyde">' +
                ' <render:g render:stroke="text_line_color_Acetyladehyde" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_NADH" render:typeList="SPECIESGLYPH" render:idList="NADH">' +
                ' <render:g render:stroke="spec_border_color_NADH" render:stroke-width="2" render:fill="spec_fill_color_NADH">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="NADH">' +
                ' <render:g render:stroke="text_line_color_NADH" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_NAD" render:typeList="SPECIESGLYPH" render:idList="NAD">' +
                ' <render:g render:stroke="spec_border_color_NAD" render:stroke-width="2" render:fill="spec_fill_color_NAD">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="NAD">' +
                ' <render:g render:stroke="text_line_color_NAD" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_ethanol" render:typeList="SPECIESGLYPH" render:idList="ethanol">' +
                ' <render:g render:stroke="spec_border_color_ethanol" render:stroke-width="2" render:fill="spec_fill_color_ethanol">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="ethanol">' +
                ' <render:g render:stroke="text_line_color_ethanol" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_External_acetaldehyde" render:typeList="SPECIESGLYPH" render:idList="External_acetaldehyde">' +
                ' <render:g render:stroke="spec_border_color_External_acetaldehyde" render:stroke-width="2" render:fill="spec_fill_color_External_acetaldehyde">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="External_acetaldehyde">' +
                ' <render:g render:stroke="text_line_color_External_acetaldehyde" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="specStyle_Sink" render:typeList="SPECIESGLYPH" render:idList="Sink">' +
                ' <render:g render:stroke="spec_border_color_Sink" render:stroke-width="2" render:fill="spec_fill_color_Sink">' +
                  ' <render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' +
                ' </render:g>' +
              ' </render:style>' +
              ' <render:style render:id="textStyle" render:typeList="TEXTGLYPH" render:idList="Sink">' +
                ' <render:g render:stroke="text_line_color_Sink" render:stroke-width="1" render:font-size="11"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J0" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J0">' +
                ' <render:g render:stroke="reaction_fill_color_J0" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J1" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J1">' +
                ' <render:g render:stroke="reaction_fill_color_J1" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J2" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J2">' +
                ' <render:g render:stroke="reaction_fill_color_J2" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J3" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J3">' +
                ' <render:g render:stroke="reaction_fill_color_J3" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J4" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J4">' +
                ' <render:g render:stroke="reaction_fill_color_J4" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J5" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J5">' +
                ' <render:g render:stroke="reaction_fill_color_J5" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J6" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J6">' +
                ' <render:g render:stroke="reaction_fill_color_J6" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J7" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J7">' +
                ' <render:g render:stroke="reaction_fill_color_J7" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J8" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J8">' +
                ' <render:g render:stroke="reaction_fill_color_J8" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J9" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J9">' +
                ' <render:g render:stroke="reaction_fill_color_J9" render:stroke-width="3"/>' +
              ' </render:style>' +
              ' <render:style render:id="reactionStyle_J10" render:typeList="REACTIONGLYPH SPECIESREFERENCEGLYPH" render:idList="J10">' +
                ' <render:g render:stroke="reaction_fill_color_J10" render:stroke-width="3"/>' +
              ' </render:style>' +
            ' </render:listOfStyles>' +
          ' </render:renderInformation>' +
        ' </render:listOfRenderInformation>' +
      ' </layout:layout>' +
    ' </layout:listOfLayouts>' +
  ' </model>' +
' </sbml>' ;


 end;
end;


end.
