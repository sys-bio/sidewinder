unit uTestSBML_ReadModels;
 // More SBML models for testing use...
interface

const SBML_TEST_MODELS = 2; // Number of SBML models listed below:

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

  // *************************************************************
  else Result := '';
  end;

end;

end.
