unit uTestSBML_ReadModels;
 // More SBML models for testing use...
interface

const SBML_TEST_MODELS = 3; // Number of SBML models listed below:

function getSBMLReadTestModel( index: integer ) : string;

implementation

function getSBMLReadTestModel( index: integer ) : string;
begin
  case index of
  0:  // Test SBML parmeter initial assignment and assignment rules combined.
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
'<!-- Created by libAntimony version v2.12.0.3 with libSBML version 5.18.1. -->' + sLineBreak +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" level="3" version="1">' + sLineBreak +
  '<model metaid="feedback" id="feedback">' + sLineBreak +
    '<listOfCompartments>' + sLineBreak +
      '<compartment sboTerm="SBO:0000410" id="default_compartment" spatialDimensions="3" size="1" constant="true"/>' + sLineBreak +
    '</listOfCompartments>' + sLineBreak +
    '<listOfSpecies>' + sLineBreak +
      '<species id="X0" compartment="default_compartment" initialConcentration="10" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' + sLineBreak +
      '<species id="S1" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S4" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S2" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S3" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="X1" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' + sLineBreak +
    '</listOfSpecies>' + sLineBreak +
    '<listOfParameters>' + sLineBreak +
      '<parameter id="VM1" constant="false"/>' + sLineBreak +
      '<parameter id="Keq1" constant="false"/>' + sLineBreak +
      '<parameter id="h" constant="false"/>' + sLineBreak +
      '<parameter id="V4" value="2.5" constant="true"/>' + sLineBreak +
      '<parameter id="KS4" value="0.5" constant="true"/>' + sLineBreak +
    '</listOfParameters>' + sLineBreak +
    '<listOfInitialAssignments>' + sLineBreak +
      '<initialAssignment symbol="h">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<ci> Keq1 </ci>' + sLineBreak +
            '<cn type="integer"> 2 </cn>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</initialAssignment>' + sLineBreak +
    '</listOfInitialAssignments>' + sLineBreak +
    '<listOfRules>' + sLineBreak +
      '<assignmentRule variable="VM1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<cn type="integer"> 10 </cn>' + sLineBreak +
            '<ci> S2 </ci>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</assignmentRule>' + sLineBreak +
      '<assignmentRule variable="Keq1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> S1 </ci>' + sLineBreak +
              '<cn type="integer"> 10 </cn>' + sLineBreak +
            '</apply>' + sLineBreak +
            '<cn type="integer"> 10 </cn>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</assignmentRule>' + sLineBreak +
    '</listOfRules>' + sLineBreak +
    '<listOfReactions>' + sLineBreak +
      '<reaction id="J0" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="X0" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<listOfModifiers>' + sLineBreak +
          '<modifierSpeciesReference species="S4"/>' + sLineBreak +
        '</listOfModifiers>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<times/>' + sLineBreak +
                '<ci> VM1 </ci>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<minus/>' + sLineBreak +
                  '<ci> X0 </ci>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<divide/>' + sLineBreak +
                    '<ci> S1 </ci>' + sLineBreak +
                    '<ci> Keq1 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> X0 </ci>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<power/>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                  '<ci> h </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J1" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S2" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S1 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J2" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S2" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S3" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J3" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S3" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S4" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J4" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S4" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="X1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<times/>' + sLineBreak +
                '<ci> V4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<ci> KS4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
    '</listOfReactions>' + sLineBreak +
  '</model>' + sLineBreak +
'</sbml>';

    1:  // Test SBML species initial assignment:
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
   '<!-- Created by libAntimony version v2.12.0.3 with libSBML version 5.18.1. -->' + sLineBreak +
   '<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" level="3" version="1">' + sLineBreak +
    '<model metaid="feedback" id="feedback">' + sLineBreak +
    '<listOfCompartments>' + sLineBreak +
      '<compartment sboTerm="SBO:0000410" id="default_compartment" spatialDimensions="3" size="1" constant="true"/>' + sLineBreak +
    '</listOfCompartments>' + sLineBreak +
    '<listOfSpecies>' + sLineBreak +
      '<species id="X0" compartment="default_compartment" initialConcentration="10" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' + sLineBreak +
      '<species id="S1" compartment="default_compartment" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S4" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S2" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="S3" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' + sLineBreak +
      '<species id="X1" compartment="default_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' + sLineBreak +
    '</listOfSpecies>' + sLineBreak +
    '<listOfParameters>' + sLineBreak +
      '<parameter id="VM1" value="10" constant="true"/>' + sLineBreak +
      '<parameter id="Keq1" value="10" constant="true"/>' + sLineBreak +
      '<parameter id="h" value="10" constant="true"/>' + sLineBreak +
      '<parameter id="V4" value="2.5" constant="true"/>' + sLineBreak +
      '<parameter id="KS4" value="0.5" constant="true"/>' + sLineBreak +
    '</listOfParameters>' + sLineBreak +
    '<listOfInitialAssignments>' + sLineBreak +
      '<initialAssignment symbol="S1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<ci> Keq1 </ci>' + sLineBreak +
            '<ci> h </ci>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</initialAssignment>' + sLineBreak +
    '</listOfInitialAssignments>' + sLineBreak +
    '<listOfReactions>' + sLineBreak +
      '<reaction id="J0" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="X0" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<listOfModifiers>' + sLineBreak +
          '<modifierSpeciesReference species="S4"/>' + sLineBreak +
        '</listOfModifiers>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<times/>' + sLineBreak +
                '<ci> VM1 </ci>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<minus/>' + sLineBreak +
                  '<ci> X0 </ci>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<divide/>' + sLineBreak +
                    '<ci> S1 </ci>' + sLineBreak +
                    '<ci> Keq1 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> X0 </ci>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<power/>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                  '<ci> h </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J1" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S2" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S1 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J2" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S2" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S3" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J3" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S3" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S4" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<minus/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 10 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<cn type="integer"> 2 </cn>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<cn type="integer"> 1 </cn>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction id="J4" reversible="true" fast="false">' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S4" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="X1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<times/>' + sLineBreak +
                '<ci> V4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<plus/>' + sLineBreak +
                '<ci> KS4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
    '</listOfReactions>' + sLineBreak +
  '</model>' + sLineBreak +
  '</sbml>';
    2:    // COPASI SBML model with function def in rate law and assignment rule
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
'<!-- Created by COPASI version 4.36 (Build 260) on 2022-06-30 08:56 with libSBML version 5.19.4. -->' + sLineBreak +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="1" layout:required="false" render:required="false">' + sLineBreak +
  '<model metaid="feedback" id="feedback" name="NoName" substanceUnits="substance" timeUnits="time" volumeUnits="volume" areaUnits="area" lengthUnits="length" extentUnits="substance">' + sLineBreak +
    '<annotation>' + sLineBreak +
      '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
        '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
          '<rdf:Description rdf:about="#feedback">' + sLineBreak +
            '<dcterms:created>' + sLineBreak +
              '<rdf:Description>' + sLineBreak +
                '<dcterms:W3CDTF>2022-06-30T15:56:04Z</dcterms:W3CDTF>' + sLineBreak +
              '</rdf:Description>' + sLineBreak +
            '</dcterms:created>' + sLineBreak +
          '</rdf:Description>' + sLineBreak +
        '</rdf:RDF>' + sLineBreak +
      '</COPASI>' + sLineBreak +
    '</annotation>' + sLineBreak +
    '<listOfFunctionDefinitions>' + sLineBreak +
      '<functionDefinition metaid="COPASI17" id="Function_for_J0" name="Function for J0">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI17"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<lambda>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> Keq1 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S1 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S4 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> VM1 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> X0 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> h </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<divide/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<ci> VM1 </ci>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<minus/>' + sLineBreak +
                    '<ci> X0 </ci>' + sLineBreak +
                    '<apply>' + sLineBreak +
                      '<divide/>' + sLineBreak +
                      '<ci> S1 </ci>' + sLineBreak +
                      '<ci> Keq1 </ci>' + sLineBreak +
                    '</apply>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<plus/>' + sLineBreak +
                  '<cn type="integer"> 1 </cn>' + sLineBreak +
                  '<ci> X0 </ci>' + sLineBreak +
                  '<ci> S1 </ci>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<power/>' + sLineBreak +
                    '<ci> S4 </ci>' + sLineBreak +
                    '<ci> h </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</lambda>' + sLineBreak +
        '</math>' + sLineBreak +
      '</functionDefinition>' + sLineBreak +
      '<functionDefinition metaid="COPASI18" id="Function_for_J1" name="Function for J1">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI18"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<lambda>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S1 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S2 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<divide/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<minus/>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<times/>' + sLineBreak +
                    '<cn type="integer"> 10 </cn>' + sLineBreak +
                    '<ci> S1 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<times/>' + sLineBreak +
                    '<cn type="integer"> 2 </cn>' + sLineBreak +
                    '<ci> S2 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<plus/>' + sLineBreak +
                  '<cn type="integer"> 1 </cn>' + sLineBreak +
                  '<ci> S1 </ci>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</lambda>' + sLineBreak +
        '</math>' + sLineBreak +
      '</functionDefinition>' + sLineBreak +
      '<functionDefinition metaid="COPASI19" id="Function_for_J2" name="Function for J2">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI19"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<lambda>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S2 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S3 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<divide/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<minus/>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<times/>' + sLineBreak +
                    '<cn type="integer"> 5 </cn>' + sLineBreak +
                    '<ci> S2 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<times/>' + sLineBreak +
                    '<cn type="integer"> 2 </cn>' + sLineBreak +
                    '<ci> S3 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<plus/>' + sLineBreak +
                  '<cn type="integer"> 1 </cn>' + sLineBreak +
                  '<ci> S2 </ci>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</lambda>' + sLineBreak +
        '</math>' + sLineBreak +
      '</functionDefinition>' + sLineBreak +
      '<functionDefinition metaid="COPASI20" id="Function_for_J3" name="Function for J3">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI20"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<lambda>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S3 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S4 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<divide/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<minus/>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<times/>' + sLineBreak +
                    '<cn type="integer"> 10 </cn>' + sLineBreak +
                    '<ci> S3 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                  '<apply>' + sLineBreak +
                    '<times/>' + sLineBreak +
                    '<cn type="integer"> 2 </cn>' + sLineBreak +
                    '<ci> S4 </ci>' + sLineBreak +
                  '</apply>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<plus/>' + sLineBreak +
                  '<cn type="integer"> 1 </cn>' + sLineBreak +
                  '<ci> S3 </ci>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</lambda>' + sLineBreak +
        '</math>' + sLineBreak +
      '</functionDefinition>' + sLineBreak +
      '<functionDefinition metaid="COPASI21" id="Function_for_J4" name="Function for J4">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI21"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<lambda>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> KS4 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> S4 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> V4 </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<bvar>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</bvar>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<divide/>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<divide/>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<times/>' + sLineBreak +
                  '<ci> V4 </ci>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
                '<apply>' + sLineBreak +
                  '<plus/>' + sLineBreak +
                  '<ci> KS4 </ci>' + sLineBreak +
                  '<ci> S4 </ci>' + sLineBreak +
                '</apply>' + sLineBreak +
              '</apply>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</lambda>' + sLineBreak +
        '</math>' + sLineBreak +
      '</functionDefinition>' + sLineBreak +
    '</listOfFunctionDefinitions>' + sLineBreak +
    '<listOfUnitDefinitions>' + sLineBreak +
      '<unitDefinition id="length" name="length">' + sLineBreak +
        '<listOfUnits>' + sLineBreak +
          '<unit kind="metre" exponent="1" scale="0" multiplier="1"/>' + sLineBreak +
        '</listOfUnits>' + sLineBreak +
      '</unitDefinition>' + sLineBreak +
      '<unitDefinition id="area" name="area">' + sLineBreak +
        '<listOfUnits>' + sLineBreak +
          '<unit kind="metre" exponent="2" scale="0" multiplier="1"/>' + sLineBreak +
        '</listOfUnits>' + sLineBreak +
      '</unitDefinition>' + sLineBreak +
      '<unitDefinition id="volume" name="volume">' + sLineBreak +
        '<listOfUnits>' + sLineBreak +
          '<unit kind="litre" exponent="1" scale="0" multiplier="1"/>' + sLineBreak +
        '</listOfUnits>' + sLineBreak +
      '</unitDefinition>' + sLineBreak +
      '<unitDefinition id="time" name="time">' + sLineBreak +
        '<listOfUnits>' + sLineBreak +
          '<unit kind="second" exponent="1" scale="0" multiplier="1"/>' + sLineBreak +
        '</listOfUnits>' + sLineBreak +
      '</unitDefinition>' + sLineBreak +
      '<unitDefinition id="substance" name="substance">' + sLineBreak +
        '<listOfUnits>' + sLineBreak +
          '<unit kind="mole" exponent="1" scale="0" multiplier="1"/>' + sLineBreak +
        '</listOfUnits>' + sLineBreak +
      '</unitDefinition>' + sLineBreak +
    '</listOfUnitDefinitions>' + sLineBreak +
    '<listOfCompartments>' + sLineBreak +
      '<compartment metaid="COPASI0" sboTerm="SBO:0000410" id="default_compartment" name="default_compartment" spatialDimensions="3" size="1" units="volume" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:vCard="http://www.w3.org/2001/vcard-rdf/3.0#" xmlns:vCard4="http://www.w3.org/2006/vcard/ns#" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/" xmlns:bqmodel="http://biomodels.net/model-qualifiers/">' + sLineBreak +
            '<rdf:Description rdf:about="#COPASI0">' + sLineBreak +
              '<bqbiol:is>' + sLineBreak +
                '<rdf:Bag>' + sLineBreak +
                  '<rdf:li rdf:resource="http://identifiers.org/SBO:0000410"/>' + sLineBreak +
                '</rdf:Bag>' + sLineBreak +
              '</bqbiol:is>' + sLineBreak +
            '</rdf:Description>' + sLineBreak +
          '</rdf:RDF>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:CopasiMT="http://www.copasi.org/RDF/MiriamTerms#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI0">' + sLineBreak +
                '<CopasiMT:is rdf:resource="urn:miriam:sbo:SBO:0000410"/>' + sLineBreak +
              '</rdf:Description>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</compartment>' + sLineBreak +
    '</listOfCompartments>' + sLineBreak +
    '<listOfSpecies>' + sLineBreak +
      '<species metaid="COPASI1" id="X0" name="X0" compartment="default_compartment" initialConcentration="10" substanceUnits="substance" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI1"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</species>' + sLineBreak +
      '<species metaid="COPASI2" id="S1" name="S1" compartment="default_compartment" initialConcentration="0" substanceUnits="substance" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI2"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</species>' + sLineBreak +
      '<species metaid="COPASI3" id="S4" name="S4" compartment="default_compartment" initialConcentration="0" substanceUnits="substance" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI3"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</species>' + sLineBreak +
      '<species metaid="COPASI4" id="S2" name="S2" compartment="default_compartment" initialConcentration="0" substanceUnits="substance" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI4"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</species>' + sLineBreak +
      '<species metaid="COPASI5" id="S3" name="S3" compartment="default_compartment" initialConcentration="0" substanceUnits="substance" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI5"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</species>' + sLineBreak +
      '<species metaid="COPASI6" id="X1" name="X1" compartment="default_compartment" initialConcentration="0" substanceUnits="substance" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI6"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</species>' + sLineBreak +
    '</listOfSpecies>' + sLineBreak +
    '<listOfParameters>' + sLineBreak +
      '<parameter metaid="COPASI7" id="VM1" name="VM1" value="10" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI7"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</parameter>' + sLineBreak +
      '<parameter metaid="COPASI8" id="Keq1" name="Keq1" value="5" constant="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI8"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</parameter>' + sLineBreak +
      '<parameter metaid="COPASI9" id="h" name="h" value="10" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI9"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</parameter>' + sLineBreak +
      '<parameter metaid="COPASI10" id="V4" name="V4" value="2.5" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI10"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</parameter>' + sLineBreak +
      '<parameter metaid="COPASI11" id="KS4" name="KS4" value="0.5" constant="true">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI11"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
      '</parameter>' + sLineBreak +
    '</listOfParameters>' + sLineBreak +
    '<listOfRules>' + sLineBreak +
      '<assignmentRule variable="Keq1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> S2 </ci>' + sLineBreak +
              '<cn> 5 </cn>' + sLineBreak +
            '</apply>' + sLineBreak +
            '<cn> 5 </cn>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</assignmentRule>' + sLineBreak +
    '</listOfRules>' + sLineBreak +
    '<listOfReactions>' + sLineBreak +
      '<reaction metaid="COPASI12" id="J0" name="J0" reversible="true" fast="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI12"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="X0" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<listOfModifiers>' + sLineBreak +
          '<modifierSpeciesReference species="S4"/>' + sLineBreak +
        '</listOfModifiers>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<ci> Function_for_J0 </ci>' + sLineBreak +
                '<ci> Keq1 </ci>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
                '<ci> VM1 </ci>' + sLineBreak +
                '<ci> X0 </ci>' + sLineBreak +
                '<ci> default_compartment </ci>' + sLineBreak +
                '<ci> h </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction metaid="COPASI13" id="J1" name="J1" reversible="true" fast="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI13"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S2" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<ci> Function_for_J1 </ci>' + sLineBreak +
                '<ci> S1 </ci>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
                '<ci> default_compartment </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction metaid="COPASI14" id="J2" name="J2" reversible="true" fast="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI14"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S2" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S3" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<ci> Function_for_J2 </ci>' + sLineBreak +
                '<ci> S2 </ci>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
                '<ci> default_compartment </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction metaid="COPASI15" id="J3" name="J3" reversible="true" fast="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI15"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S3" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="S4" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<ci> Function_for_J3 </ci>' + sLineBreak +
                '<ci> S3 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
                '<ci> default_compartment </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
      '<reaction metaid="COPASI16" id="J4" name="J4" reversible="true" fast="false">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI16"/>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
        '</annotation>' + sLineBreak +
        '<listOfReactants>' + sLineBreak +
          '<speciesReference species="S4" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfReactants>' + sLineBreak +
        '<listOfProducts>' + sLineBreak +
          '<speciesReference species="X1" stoichiometry="1" constant="true"/>' + sLineBreak +
        '</listOfProducts>' + sLineBreak +
        '<kineticLaw>' + sLineBreak +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
            '<apply>' + sLineBreak +
              '<times/>' + sLineBreak +
              '<ci> default_compartment </ci>' + sLineBreak +
              '<apply>' + sLineBreak +
                '<ci> Function_for_J4 </ci>' + sLineBreak +
                '<ci> KS4 </ci>' + sLineBreak +
                '<ci> S4 </ci>' + sLineBreak +
                '<ci> V4 </ci>' + sLineBreak +
                '<ci> default_compartment </ci>' + sLineBreak +
              '</apply>' + sLineBreak +
            '</apply>' + sLineBreak +
          '</math>' + sLineBreak +
        '</kineticLaw>' + sLineBreak +
      '</reaction>' + sLineBreak +
    '</listOfReactions>' + sLineBreak +
  '</model>' + sLineBreak +
'</sbml>';

  // *************************************************************
  else Result := '';
  end;

end;

end.
