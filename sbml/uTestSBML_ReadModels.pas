unit uTestSBML_ReadModels;
 // More SBML models for testing use...
interface

const SBML_TEST_MODELS = 5; // Number of SBML models listed below:

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
   // ***************************************************************************************
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
  // *********************************************************************************
    2:    // COPASI SBML model with function def in rate law and assignment rule      *
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

// ************************************************************************************************
 3:    // COPASI SBML model with global Render information
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
'<!-- Created by COPASI version 4.36 (Build 260) on 2022-07-07 12:12 with libSBML version 5.19.4. -->' + sLineBreak +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="1" layout:required="false" render:required="false">' + sLineBreak +
  '<model metaid="feedback" id="feedback_GlobalRender" name="NoName" substanceUnits="substance" timeUnits="time" volumeUnits="volume" areaUnits="area" lengthUnits="length" extentUnits="substance">' + sLineBreak +
    '<listOfFunctionDefinitions>' + sLineBreak +
      '<functionDefinition metaid="COPASI22" id="Function_for_J0" name="Function for J0">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI22"/>' + sLineBreak +
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
      '<functionDefinition metaid="COPASI23" id="Function_for_J1" name="Function for J1">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI23"/>' + sLineBreak +
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
      '<functionDefinition metaid="COPASI24" id="Function_for_J2" name="Function for J2">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI24"/>' + sLineBreak +
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
      '<functionDefinition metaid="COPASI25" id="Function_for_J3" name="Function for J3">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI25"/>' + sLineBreak +
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
      '<functionDefinition metaid="COPASI26" id="Function_for_J4" name="Function for J4">' + sLineBreak +
        '<annotation>' + sLineBreak +
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:dcterms="http://purl.org/dc/terms/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI26"/>' + sLineBreak +
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
          '<COPASI xmlns="http://www.copasi.org/static/sbml">' + sLineBreak +
            '<rdf:RDF xmlns:CopasiMT="http://www.copasi.org/RDF/MiriamTerms#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + sLineBreak +
              '<rdf:Description rdf:about="#COPASI0">' + sLineBreak +
                '<CopasiMT:is rdf:resource="urn:miriam:sbo:SBO:0000410"/>' + sLineBreak +
              '</rdf:Description>' + sLineBreak +
            '</rdf:RDF>' + sLineBreak +
          '</COPASI>' + sLineBreak +
          '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:vCard="http://www.w3.org/2001/vcard-rdf/3.0#" xmlns:vCard4="http://www.w3.org/2006/vcard/ns#" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/" xmlns:bqmodel="http://biomodels.net/model-qualifiers/">' + sLineBreak +
            '<rdf:Description rdf:about="#COPASI0">' + sLineBreak +
              '<bqbiol:is>' + sLineBreak +
                '<rdf:Bag>' + sLineBreak +
                  '<rdf:li rdf:resource="http://identifiers.org/SBO:0000410"/>' + sLineBreak +
                '</rdf:Bag>' + sLineBreak +
              '</bqbiol:is>' + sLineBreak +
            '</rdf:Description>' + sLineBreak +
          '</rdf:RDF>' + sLineBreak +
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
    '<layout:listOfLayouts xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1">' + sLineBreak +
      '<layout:layout layout:id="layout_0" layout:name="COPASI autolayout">' + sLineBreak +
        '<layout:dimensions layout:width="96.1373438530456" layout:height="83.7041550748293"/>' + sLineBreak +
        '<layout:listOfSpeciesGlyphs>' + sLineBreak +
          '<layout:speciesGlyph layout:id="layout_glyph_0" layout:species="S1">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="283.272798346204" layout:y="59.7323682144198"/>' + sLineBreak +
              '<layout:dimensions layout:width="36" layout:height="28"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:speciesGlyph>' + sLineBreak +
          '<layout:speciesGlyph layout:id="layout_glyph_1" layout:species="X0">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="158.718043857074" layout:y="0"/>' + sLineBreak +
              '<layout:dimensions layout:width="36" layout:height="28"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:speciesGlyph>' + sLineBreak +
          '<layout:speciesGlyph layout:id="layout_glyph_2" layout:species="S3">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="272.05458204873" layout:y="270.37091562488"/>' + sLineBreak +
              '<layout:dimensions layout:width="36" layout:height="28"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:speciesGlyph>' + sLineBreak +
          '<layout:speciesGlyph layout:id="layout_glyph_3" layout:species="X1">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="0" layout:y="154.162571271122"/>' + sLineBreak +
              '<layout:dimensions layout:width="36" layout:height="28"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:speciesGlyph>' + sLineBreak +
          '<layout:speciesGlyph layout:id="layout_glyph_4" layout:species="S4">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="155.125452446903" layout:y="171.241585003035"/>' + sLineBreak +
              '<layout:dimensions layout:width="36" layout:height="28"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:speciesGlyph>' + sLineBreak +
          '<layout:speciesGlyph layout:id="layout_glyph_5" layout:species="S2">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="381.175818391212" layout:y="169.519917085474"/>' + sLineBreak +
              '<layout:dimensions layout:width="36" layout:height="28"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:speciesGlyph>' + sLineBreak +
        '</layout:listOfSpeciesGlyphs>' + sLineBreak +
        '<layout:listOfReactionGlyphs>' + sLineBreak +
          '<layout:reactionGlyph layout:id="layout_glyph_6" layout:reaction="J3">' + sLineBreak +
            '<layout:curve>' + sLineBreak +
              '<layout:listOfCurveSegments>' + sLineBreak +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' + sLineBreak +
                  '<layout:start layout:x="219.457504199267" layout:y="262.122456144769" layout:z="0"/>' + sLineBreak +
                  '<layout:end layout:x="207.764591239084" layout:y="252.209523082585" layout:z="0"/>' + sLineBreak +
                '</layout:curveSegment>' + sLineBreak +
              '</layout:listOfCurveSegments>' + sLineBreak +
            '</layout:curve>' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="213.611047719175" layout:y="257.165989613677"/>' + sLineBreak +
              '<layout:dimensions layout:width="0" layout:height="0"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
            '<layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_7" layout:speciesGlyph="layout_glyph_2" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="219.457504199267" layout:y="262.122456144769" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="267.05458204873" layout:y="279.554327389641" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="231.150417159449" layout:y="272.035389206954" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="252.025727844135" layout:y="278.273091563844" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_8" layout:speciesGlyph="layout_glyph_4" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="207.764591239084" layout:y="252.209523082585" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="180.766820468434" layout:y="204.241585003035" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="196.071678278901" layout:y="242.2965900204" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="185.496021133622" layout:y="220.790854246172" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
            '</layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
          '</layout:reactionGlyph>' + sLineBreak +
          '<layout:reactionGlyph layout:id="layout_glyph_9" layout:reaction="J0">' + sLineBreak +
            '<layout:curve>' + sLineBreak +
              '<layout:listOfCurveSegments>' + sLineBreak +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' + sLineBreak +
                  '<layout:start layout:x="212.36614508771" layout:y="81.1716212614926" layout:z="0"/>' + sLineBreak +
                  '<layout:end layout:x="224.821620536623" layout:y="87.1448580829346" layout:z="0"/>' + sLineBreak +
                '</layout:curveSegment>' + sLineBreak +
              '</layout:listOfCurveSegments>' + sLineBreak +
            '</layout:curve>' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="218.593882812166" layout:y="84.1582396722136"/>' + sLineBreak +
              '<layout:dimensions layout:width="0" layout:height="0"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
            '<layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_10" layout:speciesGlyph="layout_glyph_1" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="212.36614508771" layout:y="81.1716212614926" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="183.918558934141" layout:y="33" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="199.910669638797" layout:y="75.1983844400506" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="188.800745424241" layout:y="52.6058830146648" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_11" layout:speciesGlyph="layout_glyph_0" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="224.821620536623" layout:y="87.1448580829346" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="278.272798346204" layout:y="80.6995815962833" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="237.277095985536" layout:y="93.1180949043766" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="260.888816028098" layout:y="88.4021474556904" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_12" layout:speciesGlyph="layout_glyph_4" layout:role="modifier">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="181.616497088903" layout:y="166.241585003035" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="214.269744487579" layout:y="93.1749922888621" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="197.943120788241" layout:y="129.708288645949" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="209.945606162992" layout:y="102.191744905511" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
            '</layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
          '</layout:reactionGlyph>' + sLineBreak +
          '<layout:reactionGlyph layout:id="layout_glyph_13" layout:reaction="J4">' + sLineBreak +
            '<layout:curve>' + sLineBreak +
              '<layout:listOfCurveSegments>' + sLineBreak +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' + sLineBreak +
                  '<layout:start layout:x="99.5025281600643" layout:y="195.839768592242" layout:z="0"/>' + sLineBreak +
                  '<layout:end layout:x="83.989982915374" layout:y="194.131867219051" layout:z="0"/>' + sLineBreak +
                '</layout:curveSegment>' + sLineBreak +
              '</layout:listOfCurveSegments>' + sLineBreak +
            '</layout:curve>' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="91.7462555377192" layout:y="194.985817905646"/>' + sLineBreak +
              '<layout:dimensions layout:width="0" layout:height="0"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
            '<layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_14" layout:speciesGlyph="layout_glyph_4" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="99.5025281600643" layout:y="195.839768592242" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="150.125452446903" layout:y="190.112314790047" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="115.015073404755" layout:y="197.547669965433" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="136.448399237001" layout:y="194.256967721038" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_15" layout:speciesGlyph="layout_glyph_3" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="83.989982915374" layout:y="194.131867219051" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="41" layout:y="179.217254332878" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="68.4774376706837" layout:y="192.423965845859" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="50.8605825241693" layout:y="185.393634746071" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
            '</layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
          '</layout:reactionGlyph>' + sLineBreak +
          '<layout:reactionGlyph layout:id="layout_glyph_16" layout:reaction="J1">' + sLineBreak +
            '<layout:curve>' + sLineBreak +
              '<layout:listOfCurveSegments>' + sLineBreak +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' + sLineBreak +
                  '<layout:start layout:x="371.970776811952" layout:y="99.1471782282335" layout:z="0"/>' + sLineBreak +
                  '<layout:end layout:x="381.761078816453" layout:y="110.125933115339" layout:z="0"/>' + sLineBreak +
                '</layout:curveSegment>' + sLineBreak +
              '</layout:listOfCurveSegments>' + sLineBreak +
            '</layout:curve>' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="376.865927814203" layout:y="104.636555671786"/>' + sLineBreak +
              '<layout:dimensions layout:width="0" layout:height="0"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
            '<layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_17" layout:speciesGlyph="layout_glyph_0" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="371.970776811952" layout:y="99.1471782282335" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="324.272798346204" layout:y="79.1837215939162" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="362.180474807451" layout:y="88.1684233411281" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="340.779061075703" layout:y="80.9313837457458" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_18" layout:speciesGlyph="layout_glyph_5" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="381.761078816453" layout:y="110.125933115339" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="396.854841348218" layout:y="164.519917085474" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="391.551380820954" layout:y="121.104688002444" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="396.650686585711" layout:y="145.556991265735" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
            '</layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
          '</layout:reactionGlyph>' + sLineBreak +
          '<layout:reactionGlyph layout:id="layout_glyph_19" layout:reaction="J2">' + sLineBreak +
            '<layout:curve>' + sLineBreak +
              '<layout:listOfCurveSegments>' + sLineBreak +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' + sLineBreak +
                  '<layout:start layout:x="373.712745814801" layout:y="254.863478138621" layout:z="0"/>' + sLineBreak +
                  '<layout:end layout:x="362.800622180553" layout:y="264.948577992561" layout:z="0"/>' + sLineBreak +
                '</layout:curveSegment>' + sLineBreak +
              '</layout:listOfCurveSegments>' + sLineBreak +
            '</layout:curve>' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="368.256683997677" layout:y="259.906028065591"/>' + sLineBreak +
              '<layout:dimensions layout:width="0" layout:height="0"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
            '<layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_20" layout:speciesGlyph="layout_glyph_5" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="373.712745814801" layout:y="254.863478138621" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="394.662678092078" layout:y="202.519917085474" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="384.624869449049" layout:y="244.77837828468" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="392.371804679126" layout:y="221.127872721592" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
              '<layout:speciesReferenceGlyph layout:id="layout_glyph_21" layout:speciesGlyph="layout_glyph_2" layout:role="product">' + sLineBreak +
                '<layout:curve>' + sLineBreak +
                  '<layout:listOfCurveSegments>' + sLineBreak +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' + sLineBreak +
                      '<layout:start layout:x="362.800622180553" layout:y="264.948577992561" layout:z="0"/>' + sLineBreak +
                      '<layout:end layout:x="313.05458204873" layout:y="280.897797940169" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint1 layout:x="351.888498546305" layout:y="275.033677846502" layout:z="0"/>' + sLineBreak +
                      '<layout:basePoint2 layout:x="329.743509388955" layout:y="280.487012856821" layout:z="0"/>' + sLineBreak +
                    '</layout:curveSegment>' + sLineBreak +
                  '</layout:listOfCurveSegments>' + sLineBreak +
                '</layout:curve>' + sLineBreak +
              '</layout:speciesReferenceGlyph>' + sLineBreak +
            '</layout:listOfSpeciesReferenceGlyphs>' + sLineBreak +
          '</layout:reactionGlyph>' + sLineBreak +
        '</layout:listOfReactionGlyphs>' + sLineBreak +
        '<layout:listOfTextGlyphs>' + sLineBreak +
          '<layout:textGlyph layout:id="layout_glyph_22" layout:originOfText="S1" layout:graphicalObject="layout_glyph_0">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="283.272798346204" layout:y="59.7323682144198"/>' + sLineBreak +
              '<layout:dimensions layout:width="32" layout:height="24"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:textGlyph>' + sLineBreak +
          '<layout:textGlyph layout:id="layout_glyph_23" layout:originOfText="X0" layout:graphicalObject="layout_glyph_1">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="158.718043857074" layout:y="0"/>' + sLineBreak +
              '<layout:dimensions layout:width="32" layout:height="24"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:textGlyph>' + sLineBreak +
          '<layout:textGlyph layout:id="layout_glyph_24" layout:originOfText="S3" layout:graphicalObject="layout_glyph_2">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="272.05458204873" layout:y="270.37091562488"/>' + sLineBreak +
              '<layout:dimensions layout:width="32" layout:height="24"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:textGlyph>' + sLineBreak +
          '<layout:textGlyph layout:id="layout_glyph_25" layout:originOfText="X1" layout:graphicalObject="layout_glyph_3">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="0" layout:y="154.162571271122"/>' + sLineBreak +
              '<layout:dimensions layout:width="32" layout:height="24"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:textGlyph>' + sLineBreak +
          '<layout:textGlyph layout:id="layout_glyph_26" layout:originOfText="S4" layout:graphicalObject="layout_glyph_4">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="155.125452446903" layout:y="171.241585003035"/>' + sLineBreak +
              '<layout:dimensions layout:width="32" layout:height="24"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:textGlyph>' + sLineBreak +
          '<layout:textGlyph layout:id="layout_glyph_27" layout:originOfText="S2" layout:graphicalObject="layout_glyph_5">' + sLineBreak +
            '<layout:boundingBox>' + sLineBreak +
              '<layout:position layout:x="381.175818391212" layout:y="169.519917085474"/>' + sLineBreak +
              '<layout:dimensions layout:width="32" layout:height="24"/>' + sLineBreak +
            '</layout:boundingBox>' + sLineBreak +
          '</layout:textGlyph>' + sLineBreak +
        '</layout:listOfTextGlyphs>' + sLineBreak +
      '</layout:layout>' + sLineBreak +
      '<render:listOfGlobalRenderInformation xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1">' + sLineBreak +
        '<render:renderInformation render:id="GlobalRenderInformation_0" render:name="Copasi simple style" render:backgroundColor="#FFFFFFFF">' + sLineBreak +
          '<render:listOfColorDefinitions>' + sLineBreak +
            '<render:colorDefinition render:id="black" render:value="#000000"/>' + sLineBreak +
            '<render:colorDefinition render:id="white" render:value="#ffffff"/>' + sLineBreak +
            '<render:colorDefinition render:id="transparent" render:value="#ffffff00"/>' + sLineBreak +
            '<render:colorDefinition render:id="EmptySetOutline" render:value="#808080"/>' + sLineBreak +
            '<render:colorDefinition render:id="EmptySetGradientStart" render:value="#ffffff"/>' + sLineBreak +
            '<render:colorDefinition render:id="EmptySetGradientEnd" render:value="#d3d3d3"/>' + sLineBreak +
            '<render:colorDefinition render:id="CompartmentBorder" render:value="#e69600b0"/>' + sLineBreak +
            '<render:colorDefinition render:id="CloneMarkerColor" render:value="#ffa500"/>' + sLineBreak +
            '<render:colorDefinition render:id="CurveColor" render:value="#000000a0"/>' + sLineBreak +
            '<render:colorDefinition render:id="ModulationCurveColor" render:value="#0000a0a0"/>' + sLineBreak +
          '</render:listOfColorDefinitions>' + sLineBreak +
          '<render:listOfGradientDefinitions>' + sLineBreak +
            '<render:linearGradient render:id="cloneMarker" render:x1="50%" render:x2="50%">' + sLineBreak +
              '<render:stop render:offset="0" render:stop-color="transparent"/>' + sLineBreak +
              '<render:stop render:offset="0.75" render:stop-color="transparent"/>' + sLineBreak +
              '<render:stop render:offset="0.76" render:stop-color="CloneMarkerColor"/>' + sLineBreak +
              '<render:stop render:offset="1" render:stop-color="CloneMarkerColor"/>' + sLineBreak +
            '</render:linearGradient>' + sLineBreak +
            '<render:linearGradient render:id="EmptySetGradient">' + sLineBreak +
              '<render:stop render:offset="0" render:stop-color="EmptySetGradientStart"/>' + sLineBreak +
              '<render:stop render:offset="100%" render:stop-color="EmptySetGradientEnd"/>' + sLineBreak +
            '</render:linearGradient>' + sLineBreak +
          '</render:listOfGradientDefinitions>' + sLineBreak +
          '<render:listOfLineEndings>' + sLineBreak +
            '<render:lineEnding render:id="ActivationHead">' + sLineBreak +
              '<layout:boundingBox layout:id="bb">' + sLineBreak +
                '<layout:position layout:x="-12" layout:y="-6"/>' + sLineBreak +
                '<layout:dimensions layout:width="12" layout:height="12"/>' + sLineBreak +
              '</layout:boundingBox>' + sLineBreak +
              '<render:g render:stroke="CurveColor" render:stroke-width="1" render:fill="white" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top">' + sLineBreak +
                '<render:ellipse render:stroke="black" render:stroke-width="1" render:cx="50%" render:cy="50%" render:rx="50%"/>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:lineEnding>' + sLineBreak +
            '<render:lineEnding render:id="TransitionHead">' + sLineBreak +
              '<layout:boundingBox layout:id="bb">' + sLineBreak +
                '<layout:position layout:x="-8" layout:y="-6"/>' + sLineBreak +
                '<layout:dimensions layout:width="12" layout:height="12"/>' + sLineBreak +
              '</layout:boundingBox>' + sLineBreak +
              '<render:g render:stroke="CurveColor" render:stroke-width="0.001" render:fill="CurveColor" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top">' + sLineBreak +
                '<render:polygon render:fill="CurveColor">' + sLineBreak +
                  '<render:listOfElements>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="100%" render:y="50%"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="100%"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="33%" render:y="50%"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' + sLineBreak +
                  '</render:listOfElements>' + sLineBreak +
                '</render:polygon>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:lineEnding>' + sLineBreak +
            '<render:lineEnding render:id="ModulationHead">' + sLineBreak +
              '<layout:boundingBox layout:id="bb">' + sLineBreak +
                '<layout:position layout:x="-5" layout:y="-5"/>' + sLineBreak +
                '<layout:dimensions layout:width="10" layout:height="10"/>' + sLineBreak +
              '</layout:boundingBox>' + sLineBreak +
              '<render:g render:stroke="ModulationCurveColor" render:stroke-width="1" render:fill="ModulationCurveColor" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top">' + sLineBreak +
                '<render:ellipse render:cx="50%" render:cy="50%" render:rx="45%"/>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:lineEnding>' + sLineBreak +
            '<render:lineEnding render:id="InhibitionHead">' + sLineBreak +
              '<layout:boundingBox layout:id="bb">' + sLineBreak +
                '<layout:position layout:x="-0.5" layout:y="-4"/>' + sLineBreak +
                '<layout:dimensions layout:width="0.6" layout:height="8"/>' + sLineBreak +
              '</layout:boundingBox>' + sLineBreak +
              '<render:g render:stroke="black" render:stroke-width="2" render:fill="black" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top">' + sLineBreak +
                '<render:polygon>' + sLineBreak +
                  '<render:listOfElements>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0.3" render:y="0"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0.3" render:y="8"/>' + sLineBreak +
                    '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="8"/>' + sLineBreak +
                  '</render:listOfElements>' + sLineBreak +
                '</render:polygon>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:lineEnding>' + sLineBreak +
          '</render:listOfLineEndings>' + sLineBreak +
          '<render:listOfStyles>' + sLineBreak +
            '<render:style render:roleList="invisible">' + sLineBreak +
              '<render:g render:stroke="#ffffff00" render:stroke-width="0" render:fill="#ffffff00" render:fill-rule="nonzero" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="defaultText" render:typeList="TEXTGLYPH">' + sLineBreak +
              '<render:g render:stroke="black" render:stroke-width="1" render:fill-rule="nonzero" render:font-family="Verdana" render:font-weight="normal" render:font-style="normal" render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="sidesubstrate substrate" render:typeList="REACTIONGLYPH">' + sLineBreak +
              '<render:g render:stroke="CurveColor" render:stroke-width="3" render:fill-rule="nonzero" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="SBO-0000169 inhibition inhibitor">' + sLineBreak +
              '<render:g render:stroke="CurveColor" render:stroke-width="3" render:fill-rule="nonzero" render:endHead="InhibitionHead" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="SBO-0000168 modifier">' + sLineBreak +
              '<render:g render:stroke="ModulationCurveColor" render:stroke-width="3" render:fill="white" render:fill-rule="nonzero" render:endHead="ModulationHead" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="SBO-0000172 activator catalysis">' + sLineBreak +
              '<render:g render:stroke="CurveColor" render:stroke-width="3" render:fill="white" render:fill-rule="nonzero" render:endHead="ActivationHead" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="product sideproduct" render:typeList="product sideproduct">' + sLineBreak +
              '<render:g render:stroke="CurveColor" render:stroke-width="3" render:fill-rule="nonzero" render:endHead="TransitionHead" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0"/>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="NO-SBO SBO-0000285" render:typeList="SPECIESGLYPH">' + sLineBreak +
              '<render:g render:stroke-width="0" render:fill="#a0e0a030" render:fill-rule="nonzero" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0">' + sLineBreak +
                '<render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:roleList="SBO-0000289" render:typeList="COMPARTMENTGLYPH">' + sLineBreak +
              '<render:g render:stroke="CompartmentBorder" render:stroke-width="7" render:fill-rule="nonzero" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0">' + sLineBreak +
                '<render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%" render:rx="20" render:ry="20"/>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:style>' + sLineBreak +
            '<render:style render:typeList="ANY">' + sLineBreak +
              '<render:g render:stroke="black" render:stroke-width="0" render:fill="#f0707070" render:fill-rule="nonzero" render:font-family="sans-serif" render:font-weight="normal" render:font-style="normal" render:text-anchor="start" render:vtext-anchor="top" render:font-size="0">' + sLineBreak +
                '<render:rectangle render:x="0" render:y="0" render:width="100%" render:height="100%"/>' + sLineBreak +
              '</render:g>' + sLineBreak +
            '</render:style>' + sLineBreak +
          '</render:listOfStyles>' + sLineBreak +
        '</render:renderInformation>' + sLineBreak +
      '</render:listOfGlobalRenderInformation>' + sLineBreak +
    '</layout:listOfLayouts>' + sLineBreak +
  '</model>' + sLineBreak +
'</sbml>';

// *****************************************************************************
  4: // Read in model with 2 init assignments and 1 assignment rule
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
	  '<initialAssignment symbol="VM1">' + sLineBreak +
        '<math xmlns="http://www.w3.org/1998/Math/MathML">' + sLineBreak +
          '<apply>' + sLineBreak +
            '<plus/>' + sLineBreak +
            '<cn type="integer"> 10 </cn>' + sLineBreak +
            '<ci> S2 </ci>' + sLineBreak +
          '</apply>' + sLineBreak +
        '</math>' + sLineBreak +
      '</initialAssignment>' + sLineBreak +
    '</listOfInitialAssignments>' + sLineBreak +
    '<listOfRules>' + sLineBreak +
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
'</sbml>'


  // *************************************************************
  else Result := '';
  end;

end;

end.
