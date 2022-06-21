unit uTestModel;

interface

const SBML_EXAMPLE_MODELS = 4; // Number of example SBML models listed below

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
  Result := '<?xml version="1.0" encoding="UTF-8"?>' +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="2" layout:required="false" render:required="false">' +
  '<model>' +
    '<listOfCompartments>' +
      '<compartment id="unit_compartment" size="1" constant="true"/>' +
    '</listOfCompartments>' +
    '<listOfSpecies>' +
      '<species id="X0" compartment="unit_compartment" initialConcentration="10" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
      '<species id="S1" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="S4" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="S2" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="S3" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="X1" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
    '</listOfSpecies>' +
    '<listOfParameters>' +
      '<parameter id="VM1" value="10" constant="true"/>' +
      '<parameter id="Keq1" value="10" constant="true"/>' +
      '<parameter id="h" value="10" constant="true"/>' +
      '<parameter id="default_compartment" value="1" constant="false"/>' +
      '<parameter id="V4" value="2.5" constant="true"/>' +
      '<parameter id="KS4" value="0.5" constant="true"/>' +
    '</listOfParameters>' +
    '<listOfReactions>' +
      '<reaction id="J0" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="X0J0" name="X0" species="X0" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="S1J0" name="S1" species="S1" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<divide/>' +
              '<apply>' +
                '<times/>' +
                '<ci> VM1 </ci>' +
                '<apply>' +
                  '<minus/>' +
                  '<ci> X0 </ci>' +
                  '<apply>' +
                    '<divide/>' +
                    '<ci> S1 </ci>' +
                    '<ci> Keq1 </ci>' +
                  '</apply>' +
                '</apply>' +
              '</apply>' +
              '<apply>' +
                '<plus/>' +
                '<cn type="integer"> 1 </cn>' +
                '<ci> X0 </ci>' +
                '<ci> S1 </ci>' +
                '<apply>' +
                  '<power/>' +
                  '<ci> S4 </ci>' +
                  '<ci> h </ci>' +
                '</apply>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J1" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="S1J1" name="S1" species="S1" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="S2J1" name="S2" species="S2" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<divide/>' +
              '<apply>' +
                '<minus/>' +
                '<apply>' +
                  '<times/>' +
                  '<cn type="integer"> 10 </cn>' +
                  '<ci> S1 </ci>' +
                '</apply>' +
                '<apply>' +
                  '<times/>' +
                  '<cn type="integer"> 2 </cn>' +
                  '<ci> S2 </ci>' +
                '</apply>' +
              '</apply>' +
              '<apply>' +
                '<plus/>' +
                '<cn type="integer"> 1 </cn>' +
                '<ci> S1 </ci>' +
                '<ci> S2 </ci>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J2" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="S2J2" name="S2" species="S2" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="S3J2" name="S3" species="S3" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<divide/>' +
              '<apply>' +
                '<minus/>' +
                '<apply>' +
                  '<times/>' +
                  '<cn type="integer"> 10 </cn>' +
                  '<ci> S2 </ci>' +
                '</apply>' +
                '<apply>' +
                  '<times/>' +
                  '<cn type="integer"> 2 </cn>' +
                  '<ci> S3 </ci>' +
                '</apply>' +
              '</apply>' +
              '<apply>' +
                '<plus/>' +
                '<cn type="integer"> 1 </cn>' +
                '<ci> S2 </ci>' +
                '<ci> S3 </ci>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J3" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="S3J3" name="S3" species="S3" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="S4J3" name="S4" species="S4" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<divide/>' +
              '<apply>' +
                '<minus/>' +
                '<apply>' +
                  '<times/>' +
                  '<cn type="integer"> 10 </cn>' +
                  '<ci> S3 </ci>' +
                '</apply>' +
                '<apply>' +
                  '<times/>' +
                  '<cn type="integer"> 2 </cn>' +
                  '<ci> S4 </ci>' +
                '</apply>' +
              '</apply>' +
              '<apply>' +
                '<plus/>' +
                '<cn type="integer"> 1 </cn>' +
                '<ci> S3 </ci>' +
                '<ci> S4 </ci>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J4" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="S4J4" name="S4" species="S4" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="X1J4" name="X1" species="X1" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<divide/>' +
              '<apply>' +
                '<times/>' +
                '<ci> V4 </ci>' +
                '<ci> S4 </ci>' +
              '</apply>' +
              '<apply>' +
                '<plus/>' +
                '<ci> KS4 </ci>' +
                '<ci> S4 </ci>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
    '</listOfReactions>' +
    '<layout:listOfLayouts xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1">' +
      '<layout:layout layout:id="layouttestNetwork">' +
        '<layout:dimensions layout:width="863" layout:height="636"/>' +
        '<layout:listOfSpeciesGlyphs>' +
          '<layout:speciesGlyph layout:id="speciesGlyphX0" layout:species="X0">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="204.808629732497" layout:y="192.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphS1" layout:species="S1">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="170.563342861622" layout:y="336.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphS4" layout:species="S4">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="659.865504755851" layout:y="253.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphS2" layout:species="S2">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="334.903954537822" layout:y="388.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphS3" layout:species="S3">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="515.817311698465" layout:y="331.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphX1" layout:species="X1">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="703.041256413743" layout:y="405.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
        '</layout:listOfSpeciesGlyphs>' +
        '<layout:listOfReactionGlyphs>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ0" layout:reaction="J0">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="217.685986297059" layout:y="284.5"/>' +
                  '<layout:end layout:x="217.685986297059" layout:y="284.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphX0J0" layout:speciesReference="X0J0" layout:speciesGlyph="speciesGlyphX0" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="228.331" layout:y="239.1"/>' +
                      '<layout:end layout:x="217.685986297059" layout:y="284.5"/>' +
                      '<layout:basePoint1 layout:x="217.685986297059" layout:y="284.5"/>' +
                      '<layout:basePoint2 layout:x="226.247308014778" layout:y="248.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS1J0" layout:speciesReference="S1J0" layout:speciesGlyph="speciesGlyphS1" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="217.685986297059" layout:y="284.5"/>' +
                      '<layout:end layout:x="207.05" layout:y="327.837"/>' +
                      '<layout:basePoint1 layout:x="209.124664579341" layout:y="320.5"/>' +
                      '<layout:basePoint2 layout:x="212.417480624617" layout:y="306.653846153846"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ1" layout:reaction="J1">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="282.733648699722" layout:y="382.5"/>' +
                  '<layout:end layout:x="282.733648699722" layout:y="382.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS1J1" layout:speciesReference="S1J1" layout:speciesGlyph="speciesGlyphS1" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="237.914" layout:y="367.704"/>' +
                      '<layout:end layout:x="282.733648699722" layout:y="382.5"/>' +
                      '<layout:basePoint1 layout:x="282.733648699722" layout:y="382.5"/>' +
                      '<layout:basePoint2 layout:x="241.648495780672" layout:y="369.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS2J1" layout:speciesReference="S2J1" layout:speciesGlyph="speciesGlyphS2" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="282.733648699722" layout:y="382.5"/>' +
                      '<layout:end layout:x="325.989" layout:y="395.205"/>' +
                      '<layout:basePoint1 layout:x="323.818801618772" layout:y="395.5"/>' +
                      '<layout:basePoint2 layout:x="308.016819726829" layout:y="390.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ2" layout:reaction="J2">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="455.360633118143" layout:y="380"/>' +
                  '<layout:end layout:x="455.360633118143" layout:y="380"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS2J2" layout:speciesReference="S2J2" layout:speciesGlyph="speciesGlyphS2" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="401.096" layout:y="396.902"/>' +
                      '<layout:end layout:x="455.360633118143" layout:y="380"/>' +
                      '<layout:basePoint1 layout:x="455.360633118143" layout:y="380"/>' +
                      '<layout:basePoint2 layout:x="410.132293827982" layout:y="394.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS3J2" layout:speciesReference="S3J2" layout:speciesGlyph="speciesGlyphS3" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="455.360633118143" layout:y="380"/>' +
                      '<layout:end layout:x="506.887" layout:y="363.511"/>' +
                      '<layout:basePoint1 layout:x="500.588972408304" layout:y="365.75"/>' +
                      '<layout:basePoint2 layout:x="483.193457296704" layout:y="371.230769230769"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ3" layout:reaction="J3">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="617.841408227158" layout:y="312.5"/>' +
                  '<layout:end layout:x="617.841408227158" layout:y="312.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS3J3" layout:speciesReference="S3J3" layout:speciesGlyph="speciesGlyphS3" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="582.79" layout:y="330.677"/>' +
                      '<layout:end layout:x="617.841408227158" layout:y="312.5"/>' +
                      '<layout:basePoint1 layout:x="617.841408227158" layout:y="312.5"/>' +
                      '<layout:basePoint2 layout:x="581.829359962812" layout:y="332"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS4J3" layout:speciesReference="S4J3" layout:speciesGlyph="speciesGlyphS4" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="617.841408227158" layout:y="312.5"/>' +
                      '<layout:end layout:x="650.479" layout:y="294.216"/>' +
                      '<layout:basePoint1 layout:x="653.853456491505" layout:y="293"/>' +
                      '<layout:basePoint2 layout:x="640.002668697525" layout:y="300.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ4" layout:reaction="J4">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="711.453380584797" layout:y="349.5"/>' +
                  '<layout:end layout:x="711.453380584797" layout:y="349.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphS4J4" layout:speciesReference="S4J4" layout:speciesGlyph="speciesGlyphS4" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="696.362" layout:y="300.437"/>' +
                      '<layout:end layout:x="711.453380584797" layout:y="349.5"/>' +
                      '<layout:basePoint1 layout:x="711.453380584797" layout:y="349.5"/>' +
                      '<layout:basePoint2 layout:x="700.659442670324" layout:y="311.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphX1J4" layout:speciesReference="X1J4" layout:speciesGlyph="speciesGlyphX1" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="711.453380584797" layout:y="349.5"/>' +
                      '<layout:end layout:x="724.979" layout:y="396.803"/>' +
                      '<layout:basePoint1 layout:x="722.24731849927" layout:y="387.5"/>' +
                      '<layout:basePoint2 layout:x="718.09580391678" layout:y="372.884615384615"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
        '</layout:listOfReactionGlyphs>' +
        '<layout:listOfTextGlyphs>' +
          '<layout:textGlyph layout:id="txtGlyphX0" layout:text="X0" layout:graphicalObject="speciesGlyphX0">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="204.808629732497" layout:y="192.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphS1" layout:text="S1" layout:graphicalObject="speciesGlyphS1">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="170.563342861622" layout:y="336.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphS4" layout:text="S4" layout:graphicalObject="speciesGlyphS4">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="659.865504755851" layout:y="253.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphS2" layout:text="S2" layout:graphicalObject="speciesGlyphS2">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="334.903954537822" layout:y="388.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphS3" layout:text="S3" layout:graphicalObject="speciesGlyphS3">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="515.817311698465" layout:y="331.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphX1" layout:text="X1" layout:graphicalObject="speciesGlyphX1">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="703.041256413743" layout:y="405.5"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
        '</layout:listOfTextGlyphs>' +
        '<render:listOfRenderInformation xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1">' +
          '<render:renderInformation render:id="renderInfotestNetwork" render:programName="Sidewinder">' +
            '<render:listOfColorDefinitions>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphX0" render:value="#a6caf0"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphX0" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphS1" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphS1" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphS4" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphS4" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphS2" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphS2" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphS3" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphS3" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphX1" render:value="#a6caf0"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphX1" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphX0J0" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS1J0" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS1J1" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS2J1" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS2J2" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS3J2" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS3J3" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS4J3" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphS4J4" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphX1J4" render:value="#b0c4de"/>' +
            '</render:listOfColorDefinitions>' +
            '<render:listOfLineEndings>' +
              '<render:lineEnding render:id="arrowHead_productJ0">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS1J0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS1J0">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ1">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS2J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS2J1">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ2">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS3J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS3J2">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ3">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS4J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS4J3">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ4">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphX1J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphX1J4">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
            '</render:listOfLineEndings>' +
            '<render:listOfStyles>' +
              '<render:style render:id="speciesStyle_X0" render:idList="speciesGlyphX0">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphX0" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphX0" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_X0" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_S1" render:idList="speciesGlyphS1">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphS1" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphS1" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_S1" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_S4" render:idList="speciesGlyphS4">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphS4" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphS4" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_S4" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_S2" render:idList="speciesGlyphS2">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphS2" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphS2" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_S2" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_S3" render:idList="speciesGlyphS3">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphS3" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphS3" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_S3" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_X1" render:idList="speciesGlyphX1">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphX1" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphX1" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_X1" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J0" render:roleList="reactant" render:idList="specRefGlyphX0J0">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphX0J0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphX0J0" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J0" render:roleList="product" render:idList="specRefGlyphS1J0">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS1J0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS1J0" render:endHead="arrowHead_productJ0" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J1" render:roleList="reactant" render:idList="specRefGlyphS1J1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS1J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS1J1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J1" render:roleList="product" render:idList="specRefGlyphS2J1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS2J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS2J1" render:endHead="arrowHead_productJ1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J2" render:roleList="reactant" render:idList="specRefGlyphS2J2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS2J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS2J2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J2" render:roleList="product" render:idList="specRefGlyphS3J2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS3J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS3J2" render:endHead="arrowHead_productJ2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J3" render:roleList="reactant" render:idList="specRefGlyphS3J3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS3J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS3J3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J3" render:roleList="product" render:idList="specRefGlyphS4J3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS4J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS4J3" render:endHead="arrowHead_productJ3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J4" render:roleList="reactant" render:idList="specRefGlyphS4J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphS4J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphS4J4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J4" render:roleList="product" render:idList="specRefGlyphX1J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphX1J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphX1J4" render:endHead="arrowHead_productJ4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
            '</render:listOfStyles>' +
          '</render:renderInformation>' +
        '</render:listOfRenderInformation>' +
      '</layout:layout>' +
    '</layout:listOfLayouts>' +
  '</model>' +
'</sbml>' ;

  2:
    Result := '<?xml version="1.0" encoding="UTF-8"?>' +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="2" layout:required="false" render:required="false">' +
  '<model>' +
    '<listOfCompartments>' +
      '<compartment id="unit_compartment" size="1" constant="true"/>' +
    '</listOfCompartments>' +
    '<listOfSpecies>' +
      '<species id="Z" compartment="unit_compartment" initialConcentration="0.15" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="Y" compartment="unit_compartment" initialConcentration="1.6" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="R0_Null" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
      '<species id="R1_Null" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
      '<species id="R_eff_Null" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
    '</listOfSpecies>' +
    '<listOfParameters>' +
      '<parameter id="v0" value="1" constant="true"/>' +
      '<parameter id="cytosol" value="1" constant="false"/>' +
      '<parameter id="store" value="1" constant="false"/>' +
      '<parameter id="v1" value="7.3" constant="true"/>' +
      '<parameter id="beta" value="0.301" constant="true"/>' +
      '<parameter id="Vm2" value="65" constant="true"/>' +
      '<parameter id="n" value="2" constant="true"/>' +
      '<parameter id="K2" value="1" constant="true"/>' +
      '<parameter id="m" value="2" constant="true"/>' +
      '<parameter id="p" value="4" constant="true"/>' +
      '<parameter id="Vm3" value="500" constant="true"/>' +
      '<parameter id="Kr" value="2" constant="true"/>' +
      '<parameter id="Ka" value="0.9" constant="true"/>' +
      '<parameter id="kf" value="1" constant="true"/>' +
      '<parameter id="k" value="10" constant="true"/>' +
    '</listOfParameters>' +
    '<listOfReactions>' +
      '<reaction id="R0" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="R0_NullR0" name="R0_Null" species="R0_Null" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="ZR0" name="Z" species="Z" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> cytosol </ci>' +
              '<ci> v0 </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="R1" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="R1_NullR1" name="R1_Null" species="R1_Null" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="ZR1" name="Z" species="Z" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> cytosol </ci>' +
              '<ci> v1 </ci>' +
              '<ci> beta </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="R2" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="ZR2" name="Z" species="Z" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="YR2" name="Y" species="Y" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> cytosol </ci>' +
              '<apply>' +
                '<divide/>' +
                '<apply>' +
                  '<times/>' +
                  '<ci> Vm2 </ci>' +
                  '<apply>' +
                    '<power/>' +
                    '<ci> Z </ci>' +
                    '<ci> n </ci>' +
                  '</apply>' +
                '</apply>' +
                '<apply>' +
                  '<plus/>' +
                  '<apply>' +
                    '<power/>' +
                    '<ci> K2 </ci>' +
                    '<ci> n </ci>' +
                  '</apply>' +
                  '<apply>' +
                    '<power/>' +
                    '<ci> Z </ci>' +
                    '<ci> n </ci>' +
                  '</apply>' +
                '</apply>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="R3" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="YR3" name="Y" species="Y" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="ZR3" name="Z" species="Z" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> store </ci>' +
              '<apply>' +
                '<divide/>' +
                '<apply>' +
                  '<times/>' +
                  '<ci> Vm3 </ci>' +
                  '<apply>' +
                    '<power/>' +
                    '<ci> Y </ci>' +
                    '<ci> m </ci>' +
                  '</apply>' +
                  '<apply>' +
                    '<power/>' +
                    '<ci> Z </ci>' +
                    '<ci> p </ci>' +
                  '</apply>' +
                '</apply>' +
                '<apply>' +
                  '<times/>' +
                  '<apply>' +
                    '<plus/>' +
                    '<apply>' +
                      '<power/>' +
                      '<ci> Kr </ci>' +
                      '<ci> m </ci>' +
                    '</apply>' +
                    '<apply>' +
                      '<power/>' +
                      '<ci> Y </ci>' +
                      '<ci> m </ci>' +
                    '</apply>' +
                  '</apply>' +
                  '<apply>' +
                    '<plus/>' +
                    '<apply>' +
                      '<power/>' +
                      '<ci> Ka </ci>' +
                      '<ci> p </ci>' +
                    '</apply>' +
                    '<apply>' +
                      '<power/>' +
                      '<ci> Z </ci>' +
                      '<ci> p </ci>' +
                    '</apply>' +
                  '</apply>' +
                '</apply>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="Rf" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="YRf" name="Y" species="Y" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="ZRf" name="Z" species="Z" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> store </ci>' +
              '<ci> kf </ci>' +
              '<ci> Y </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="R_eff" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="ZR_eff" name="Z" species="Z" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="R_eff_NullR_eff" name="R_eff_Null" species="R_eff_Null" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> cytosol </ci>' +
              '<ci> k </ci>' +
              '<ci> Z </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
    '</listOfReactions>' +
    '<layout:listOfLayouts xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1">' +
      '<layout:layout layout:id="layouttestNetwork">' +
        '<layout:dimensions layout:width="987" layout:height="636"/>' +
        '<layout:listOfSpeciesGlyphs>' +
          '<layout:speciesGlyph layout:id="speciesGlyphZ" layout:species="Z">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="500.012771956977" layout:y="277.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphY" layout:species="Y">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="568.380844366125" layout:y="169.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphR0_Null" layout:species="R0_Null">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="325.656306996162" layout:y="160.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphR1_Null" layout:species="R1_Null">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="388.703523066882" layout:y="456.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphR_eff_Null" layout:species="R_eff_Null">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="684.746553613855" layout:y="384.3"/>' +
              '<layout:dimensions layout:width="67.1" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
        '</layout:listOfSpeciesGlyphs>' +
        '<layout:listOfReactionGlyphs>' +
          '<layout:reactionGlyph layout:id="reactionGlyphR0" layout:reaction="R0">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="442.834539476569" layout:y="238.8"/>' +
                  '<layout:end layout:x="442.834539476569" layout:y="238.8"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphR0_NullR0" layout:speciesReference="R0_NullR0" layout:speciesGlyph="speciesGlyphR0_Null" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="392.512" layout:y="204.982"/>' +
                      '<layout:end layout:x="442.834539476569" layout:y="238.8"/>' +
                      '<layout:basePoint1 layout:x="442.834539476569" layout:y="238.8"/>' +
                      '<layout:basePoint2 layout:x="399.245423236365" layout:y="209.55"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphZR0" layout:speciesReference="ZR0" layout:speciesGlyph="speciesGlyphZ" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="442.834539476569" layout:y="238.8"/>' +
                      '<layout:end layout:x="491.587" layout:y="271.299"/>' +
                      '<layout:basePoint1 layout:x="486.423655716773" layout:y="268.05"/>' +
                      '<layout:basePoint2 layout:x="469.658611009002" layout:y="256.8"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphR1" layout:reaction="R1">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="474.358147511929" layout:y="386.8"/>' +
                  '<layout:end layout:x="474.358147511929" layout:y="386.8"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphR1_NullR1" layout:speciesReference="R1_NullR1" layout:speciesGlyph="speciesGlyphR1_Null" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="434.835" layout:y="448.152"/>' +
                      '<layout:end layout:x="474.358147511929" layout:y="386.8"/>' +
                      '<layout:basePoint1 layout:x="474.358147511929" layout:y="386.8"/>' +
                      '<layout:basePoint2 layout:x="446.530835289406" layout:y="431.55"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphZR1" layout:speciesReference="ZR1" layout:speciesGlyph="speciesGlyphZ" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="474.358147511929" layout:y="386.8"/>' +
                      '<layout:end layout:x="511.424" layout:y="325.576"/>' +
                      '<layout:basePoint1 layout:x="502.185459734453" layout:y="342.05"/>' +
                      '<layout:basePoint2 layout:x="491.482647341175" layout:y="359.261538461538"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphR2" layout:reaction="R2">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="564.196808161551" layout:y="243.3"/>' +
                  '<layout:end layout:x="564.196808161551" layout:y="243.3"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphZR2" layout:speciesReference="ZR2" layout:speciesGlyph="speciesGlyphZ" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="546.899" layout:y="269.155"/>' +
                      '<layout:end layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:basePoint1 layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:basePoint2 layout:x="547.104790059264" layout:y="270.3"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphYR2" layout:speciesReference="YR2" layout:speciesGlyph="speciesGlyphY" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:end layout:x="579.456" layout:y="217.989"/>' +
                      '<layout:basePoint1 layout:x="581.288826263838" layout:y="216.3"/>' +
                      '<layout:basePoint2 layout:x="574.714973147574" layout:y="226.684615384615"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphR3" layout:reaction="R3">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="564.196808161551" layout:y="243.3"/>' +
                  '<layout:end layout:x="564.196808161551" layout:y="243.3"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphYR3" layout:speciesReference="YR3" layout:speciesGlyph="speciesGlyphY" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="581.096" layout:y="216.842"/>' +
                      '<layout:end layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:basePoint1 layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:basePoint2 layout:x="581.288826263838" layout:y="216.3"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphZR3" layout:speciesReference="ZR3" layout:speciesGlyph="speciesGlyphZ" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:end layout:x="547.337" layout:y="268.831"/>' +
                      '<layout:basePoint1 layout:x="547.104790059264" layout:y="270.3"/>' +
                      '<layout:basePoint2 layout:x="553.678643175528" layout:y="259.915384615385"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphRf" layout:reaction="Rf">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="564.196808161551" layout:y="243.3"/>' +
                  '<layout:end layout:x="564.196808161551" layout:y="243.3"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphYRf" layout:speciesReference="YRf" layout:speciesGlyph="speciesGlyphY" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="581.096" layout:y="216.842"/>' +
                      '<layout:end layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:basePoint1 layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:basePoint2 layout:x="581.288826263838" layout:y="216.3"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphZRf" layout:speciesReference="ZRf" layout:speciesGlyph="speciesGlyphZ" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="564.196808161551" layout:y="243.3"/>' +
                      '<layout:end layout:x="547.337" layout:y="268.831"/>' +
                      '<layout:basePoint1 layout:x="547.104790059264" layout:y="270.3"/>' +
                      '<layout:basePoint2 layout:x="553.678643175528" layout:y="259.915384615385"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphR_eff" layout:reaction="R_eff">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="624.154662785416" layout:y="350.8"/>' +
                  '<layout:end layout:x="624.154662785416" layout:y="350.8"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphZR_eff" layout:speciesReference="ZR_eff" layout:speciesGlyph="speciesGlyphZ" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="567.249" layout:y="318.038"/>' +
                      '<layout:end layout:x="624.154662785416" layout:y="350.8"/>' +
                      '<layout:basePoint1 layout:x="624.154662785416" layout:y="350.8"/>' +
                      '<layout:basePoint2 layout:x="577.083717371196" layout:y="324.05"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphR_eff_NullR_eff" layout:speciesReference="R_eff_NullR_eff" layout:speciesGlyph="speciesGlyphR_eff_Null" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="624.154662785416" layout:y="350.8"/>' +
                      '<layout:end layout:x="675.296" layout:y="379.044"/>' +
                      '<layout:basePoint1 layout:x="671.225608199635" layout:y="377.55"/>' +
                      '<layout:basePoint2 layout:x="653.121398424935" layout:y="367.261538461538"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
        '</layout:listOfReactionGlyphs>' +
        '<layout:listOfTextGlyphs>' +
          '<layout:textGlyph layout:id="txtGlyphZ" layout:text="Z" layout:graphicalObject="speciesGlyphZ">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="500.012771956977" layout:y="277.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphY" layout:text="Y" layout:graphicalObject="speciesGlyphY">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="568.380844366125" layout:y="169.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphR0_Null" layout:text="R0_Null" layout:graphicalObject="speciesGlyphR0_Null">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="325.656306996162" layout:y="160.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphR1_Null" layout:text="R1_Null" layout:graphicalObject="speciesGlyphR1_Null">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="388.703523066882" layout:y="456.3"/>' +
              '<layout:dimensions layout:width="60" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphR_eff_Null" layout:text="R_eff_Null" layout:graphicalObject="speciesGlyphR_eff_Null">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="684.746553613855" layout:y="384.3"/>' +
              '<layout:dimensions layout:width="67.1" layout:height="40"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
        '</layout:listOfTextGlyphs>' +
        '<render:listOfRenderInformation xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1">' +
          '<render:renderInformation render:id="renderInfotestNetwork" render:programName="Sidewinder">' +
            '<render:listOfColorDefinitions>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphZ" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphZ" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphY" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphY" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphR0_Null" render:value="#c0c0c0"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphR0_Null" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphR1_Null" render:value="#c0c0c0"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphR1_Null" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphR_eff_Null" render:value="#c0c0c0"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphR_eff_Null" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphR0_NullR0" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphZR0" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphR1_NullR1" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphZR1" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphZR2" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphYR2" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphYR3" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphZR3" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphYRf" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphZRf" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphZR_eff" render:value="#b0c4de"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphR_eff_NullR_eff" render:value="#b0c4de"/>' +
            '</render:listOfColorDefinitions>' +
            '<render:listOfLineEndings>' +
              '<render:lineEnding render:id="arrowHead_productR0">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR0">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productR1">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR1">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productR2">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphYR2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphYR2">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productR3">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR3">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productRf">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZRf" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZRf">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productR_eff">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphR_eff_NullR_eff" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphR_eff_NullR_eff">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
            '</render:listOfLineEndings>' +
            '<render:listOfStyles>' +
              '<render:style render:id="speciesStyle_Z" render:idList="speciesGlyphZ">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphZ" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphZ" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_Z" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_Y" render:idList="speciesGlyphY">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphY" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphY" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_Y" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_R0_Null" render:idList="speciesGlyphR0_Null">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphR0_Null" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphR0_Null" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_R0_Null" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_R1_Null" render:idList="speciesGlyphR1_Null">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphR1_Null" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphR1_Null" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="60" render:height="40" render:rx="24%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_R1_Null" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_R_eff_Null" render:idList="speciesGlyphR_eff_Null">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphR_eff_Null" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphR_eff_Null" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="67.1" render:height="40" render:rx="26.84%" render:ry="24%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_R_eff_Null" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_R0" render:roleList="reactant" render:idList="specRefGlyphR0_NullR0">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphR0_NullR0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphR0_NullR0" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_R0" render:roleList="product" render:idList="specRefGlyphZR0">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR0" render:endHead="arrowHead_productR0" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_R1" render:roleList="reactant" render:idList="specRefGlyphR1_NullR1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphR1_NullR1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphR1_NullR1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_R1" render:roleList="product" render:idList="specRefGlyphZR1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR1" render:endHead="arrowHead_productR1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_R2" render:roleList="reactant" render:idList="specRefGlyphZR2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_R2" render:roleList="product" render:idList="specRefGlyphYR2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphYR2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphYR2" render:endHead="arrowHead_productR2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_R3" render:roleList="reactant" render:idList="specRefGlyphYR3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphYR3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphYR3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_R3" render:roleList="product" render:idList="specRefGlyphZR3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR3" render:endHead="arrowHead_productR3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_Rf" render:roleList="reactant" render:idList="specRefGlyphYRf">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphYRf" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphYRf" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_Rf" render:roleList="product" render:idList="specRefGlyphZRf">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZRf" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZRf" render:endHead="arrowHead_productRf" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_R_eff" render:roleList="reactant" render:idList="specRefGlyphZR_eff">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphZR_eff" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphZR_eff" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_R_eff" render:roleList="product" render:idList="specRefGlyphR_eff_NullR_eff">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphR_eff_NullR_eff" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphR_eff_NullR_eff" render:endHead="arrowHead_productR_eff" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
            '</render:listOfStyles>' +
          '</render:renderInformation>' +
        '</render:listOfRenderInformation>' +
      '</layout:layout>' +
    '</layout:listOfLayouts>' +
  '</model>' +
'</sbml>' ;


  3:
   Result := '<?xml version="1.0" encoding="UTF-8"?>' +
'<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1" xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1" level="3" version="2" layout:required="false" render:required="false">' +
  '<model>' +
    '<listOfCompartments>' +
      '<compartment id="unit_compartment" size="1" constant="true"/>' +
    '</listOfCompartments>' +
    '<listOfSpecies>' +
      '<species id="External_glucose" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
      '<species id="Glucose" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="ATP" compartment="unit_compartment" initialConcentration="3" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="fructose_1_6_bisphosphate" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="ADP" compartment="unit_compartment" initialConcentration="1" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="glyceraldehyde_3_phosphate" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="NADH" compartment="unit_compartment" initialConcentration="0.5" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="NAD" compartment="unit_compartment" initialConcentration="0.5" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="Glycerol" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
      '<species id="glycerate_3_phosphate" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="pyruvate" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="Acetyladehyde" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="ethanol" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
      '<species id="External_acetaldehyde" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="false" constant="false"/>' +
      '<species id="Sink" compartment="unit_compartment" initialConcentration="0" hasOnlySubstanceUnits="false" boundaryCondition="true" constant="false"/>' +
    '</listOfSpecies>' +
    '<listOfParameters>' +
      '<parameter id="J0_inputFlux" value="50" constant="true"/>' +
      '<parameter id="unit_compartment" value="1" constant="false"/>' +
      '<parameter id="J1_k1" value="550" constant="true"/>' +
      '<parameter id="J1_Ki" value="1" constant="true"/>' +
      '<parameter id="J1_n" value="4" constant="true"/>' +
      '<parameter id="J2_J2_k" value="9.8" constant="true"/>' +
      '<parameter id="J2_k" value="9.8" constant="true"/>' +
      '<parameter id="J3_J3_k" value="85.7" constant="true"/>' +
      '<parameter id="J3_k" value="85.7" constant="true"/>' +
      '<parameter id="J4_kg" value="323.8" constant="true"/>' +
      '<parameter id="J4_kp" value="76411.1" constant="true"/>' +
      '<parameter id="J4_ka" value="57823.1" constant="true"/>' +
      '<parameter id="J4_kk" value="23.7" constant="true"/>' +
      '<parameter id="J5_J5_k" value="80" constant="true"/>' +
      '<parameter id="J5_k" value="80" constant="true"/>' +
      '<parameter id="J6_J6_k" value="9.7" constant="true"/>' +
      '<parameter id="J6_k" value="9.7" constant="true"/>' +
      '<parameter id="J7_J7_k" value="2000" constant="true"/>' +
      '<parameter id="J7_k" value="2000" constant="true"/>' +
      '<parameter id="J8_J8_k1" value="375" constant="true"/>' +
      '<parameter id="J8_J8_k2" value="375" constant="true"/>' +
      '<parameter id="J8_k1" value="375" constant="true"/>' +
      '<parameter id="J8_k2" value="375" constant="true"/>' +
      '<parameter id="J9_J9_k" value="28" constant="true"/>' +
      '<parameter id="J9_k" value="28" constant="true"/>' +
      '<parameter id="J10_J10_k" value="80" constant="true"/>' +
      '<parameter id="J10_k" value="80" constant="true"/>' +
    '</listOfParameters>' +
    '<listOfReactions>' +
      '<reaction id="J0" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_External_glucose_idx_0J0" name="External_glucose" species="External_glucose" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_Glucose_idx_1J0" name="Glucose" species="Glucose" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<ci> J0_inputFlux </ci>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J1" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_Glucose_idx_1J1" name="Glucose" species="Glucose" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_ATP_idx_2J1" name="ATP" species="ATP" stoichiometry="2" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" name="fructose_1_6_bisphosphate" species="fructose_1_6_bisphosphate" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_ADP_idx_4J1" name="ADP" species="ADP" stoichiometry="2" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J1_k1 </ci>' +
              '<ci> Glucose </ci>' +
              '<ci> ATP </ci>' +
              '<apply>' +
                '<divide/>' +
                '<cn type="integer"> 1 </cn>' +
                '<apply>' +
                  '<plus/>' +
                  '<cn type="integer"> 1 </cn>' +
                  '<apply>' +
                    '<power/>' +
                    '<apply>' +
                      '<divide/>' +
                      '<ci> ATP </ci>' +
                      '<ci> J1_Ki </ci>' +
                    '</apply>' +
                    '<ci> J1_n </ci>' +
                  '</apply>' +
                '</apply>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J2" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2" name="fructose_1_6_bisphosphate" species="fructose_1_6_bisphosphate" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" name="glyceraldehyde_3_phosphate" species="glyceraldehyde_3_phosphate" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" name="glyceraldehyde_3_phosphate" species="glyceraldehyde_3_phosphate" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J2_J2_k </ci>' +
              '<ci> fructose_1_6_bisphosphate </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J3" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3" name="glyceraldehyde_3_phosphate" species="glyceraldehyde_3_phosphate" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_NADH_idx_6J3" name="NADH" species="NADH" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_NAD_idx_7J3" name="NAD" species="NAD" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_Glycerol_idx_8J3" name="Glycerol" species="Glycerol" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J3_J3_k </ci>' +
              '<ci> glyceraldehyde_3_phosphate </ci>' +
              '<ci> NADH </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J4" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4" name="glyceraldehyde_3_phosphate" species="glyceraldehyde_3_phosphate" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_ADP_idx_9J4" name="ADP" species="ADP" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_NAD_idx_10J4" name="NAD" species="NAD" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_ATP_idx_11J4" name="ATP" species="ATP" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_glycerate_3_phosphate_idx_12J4" name="glycerate_3_phosphate" species="glycerate_3_phosphate" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_NADH_idx_13J4" name="NADH" species="NADH" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<divide/>' +
              '<apply>' +
                '<minus/>' +
                '<apply>' +
                  '<times/>' +
                  '<ci> J4_kg </ci>' +
                  '<ci> J4_kp </ci>' +
                  '<ci> glyceraldehyde_3_phosphate </ci>' +
                  '<ci> NAD </ci>' +
                  '<ci> ADP </ci>' +
                '</apply>' +
                '<apply>' +
                  '<times/>' +
                  '<ci> J4_ka </ci>' +
                  '<ci> J4_kk </ci>' +
                  '<ci> glycerate_3_phosphate </ci>' +
                  '<ci> ATP </ci>' +
                  '<ci> NADH </ci>' +
                '</apply>' +
              '</apply>' +
              '<apply>' +
                '<plus/>' +
                '<apply>' +
                  '<times/>' +
                  '<ci> J4_ka </ci>' +
                  '<ci> NADH </ci>' +
                '</apply>' +
                '<apply>' +
                  '<times/>' +
                  '<ci> J4_kp </ci>' +
                  '<ci> ADP </ci>' +
                '</apply>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J5" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_glycerate_3_phosphate_idx_12J5" name="glycerate_3_phosphate" species="glycerate_3_phosphate" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_ADP_idx_14J5" name="ADP" species="ADP" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_ATP_idx_15J5" name="ATP" species="ATP" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_pyruvate_idx_16J5" name="pyruvate" species="pyruvate" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J5_J5_k </ci>' +
              '<ci> glycerate_3_phosphate </ci>' +
              '<ci> ADP </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J6" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_pyruvate_idx_16J6" name="pyruvate" species="pyruvate" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_Acetyladehyde_idx_17J6" name="Acetyladehyde" species="Acetyladehyde" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J6_J6_k </ci>' +
              '<ci> pyruvate </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J7" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_Acetyladehyde_idx_17J7" name="Acetyladehyde" species="Acetyladehyde" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_NADH_idx_18J7" name="NADH" species="NADH" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_NAD_idx_19J7" name="NAD" species="NAD" stoichiometry="1" constant="true"/>' +
          '<speciesReference id="speciesGlyphSpecG_ethanol_idx_20J7" name="ethanol" species="ethanol" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J7_J7_k </ci>' +
              '<ci> Acetyladehyde </ci>' +
              '<ci> NADH </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J8" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_Acetyladehyde_idx_17J8" name="Acetyladehyde" species="Acetyladehyde" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_External_acetaldehyde_idx_21J8" name="External_acetaldehyde" species="External_acetaldehyde" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<minus/>' +
              '<apply>' +
                '<times/>' +
                '<ci> J8_J8_k1 </ci>' +
                '<ci> Acetyladehyde </ci>' +
              '</apply>' +
              '<apply>' +
                '<times/>' +
                '<ci> J8_J8_k2 </ci>' +
                '<ci> External_acetaldehyde </ci>' +
              '</apply>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J9" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_ATP_idx_2J9" name="ATP" species="ATP" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_ADP_idx_4J9" name="ADP" species="ADP" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J9_J9_k </ci>' +
              '<ci> ATP </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
      '<reaction id="J10" reversible="false" compartment="unit_compartment">' +
        '<listOfReactants>' +
          '<speciesReference id="speciesGlyphSpecG_External_acetaldehyde_idx_21J10" name="External_acetaldehyde" species="External_acetaldehyde" stoichiometry="1" constant="true"/>' +
        '</listOfReactants>' +
        '<listOfProducts>' +
          '<speciesReference id="speciesGlyphSpecG_Sink_idx_22J10" name="Sink" species="Sink" stoichiometry="1" constant="true"/>' +
        '</listOfProducts>' +
        '<kineticLaw>' +
          '<math xmlns="http://www.w3.org/1998/Math/MathML">' +
            '<apply>' +
              '<times/>' +
              '<ci> J10_J10_k </ci>' +
              '<ci> External_acetaldehyde </ci>' +
            '</apply>' +
          '</math>' +
        '</kineticLaw>' +
      '</reaction>' +
    '</listOfReactions>' +
    '<layout:listOfLayouts xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:layout="http://www.sbml.org/sbml/level3/version1/layout/version1">' +
      '<layout:layout layout:id="layouttestNetwork">' +
        '<layout:dimensions layout:width="1159" layout:height="664"/>' +
        '<layout:listOfSpeciesGlyphs>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0" layout:species="External_glucose">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="77" layout:y="58"/>' +
              '<layout:dimensions layout:width="112.2" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" layout:species="Glucose">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="282" layout:y="85"/>' +
              '<layout:dimensions layout:width="54" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ATP_idx_2" layout:species="ATP">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="506" layout:y="79"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" layout:species="fructose_1_6_bisphosphate">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="290" layout:y="202"/>' +
              '<layout:dimensions layout:width="179.3" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ADP_idx_4" layout:species="ADP">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="503" layout:y="172"/>' +
              '<layout:dimensions layout:width="35" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:species="glyceraldehyde_3_phosphate">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="283" layout:y="314"/>' +
              '<layout:dimensions layout:width="191.4" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_NADH_idx_6" layout:species="NADH">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="250" layout:y="245"/>' +
              '<layout:dimensions layout:width="43" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_NAD_idx_7" layout:species="NAD">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="135" layout:y="247"/>' +
              '<layout:dimensions layout:width="36" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8" layout:species="Glycerol">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="118" layout:y="350"/>' +
              '<layout:dimensions layout:width="54" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ADP_idx_9" layout:species="ADP">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="474" layout:y="351"/>' +
              '<layout:dimensions layout:width="35" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_NAD_idx_10" layout:species="NAD">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="229" layout:y="356"/>' +
              '<layout:dimensions layout:width="36" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ATP_idx_11" layout:species="ATP">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="483" layout:y="415"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" layout:species="glycerate_3_phosphate">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="299" layout:y="438"/>' +
              '<layout:dimensions layout:width="151.8" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_NADH_idx_13" layout:species="NADH">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="224" layout:y="403"/>' +
              '<layout:dimensions layout:width="43" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ADP_idx_14" layout:species="ADP">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="478" layout:y="474"/>' +
              '<layout:dimensions layout:width="35" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ATP_idx_15" layout:species="ATP">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="480" layout:y="543"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" layout:species="pyruvate">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="330" layout:y="569"/>' +
              '<layout:dimensions layout:width="57" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" layout:species="Acetyladehyde">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="313.190476190476" layout:y="655.952380952381"/>' +
              '<layout:dimensions layout:width="95.7" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_NADH_idx_18" layout:species="NADH">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="235" layout:y="595"/>' +
              '<layout:dimensions layout:width="43" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_NAD_idx_19" layout:species="NAD">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="112" layout:y="631"/>' +
              '<layout:dimensions layout:width="36" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_ethanol_idx_20" layout:species="ethanol">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="134" layout:y="738"/>' +
              '<layout:dimensions layout:width="49" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" layout:species="External_acetaldehyde">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="443.285714285714" layout:y="734.619047619048"/>' +
              '<layout:dimensions layout:width="148.5" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
          '<layout:speciesGlyph layout:id="speciesGlyphspeciesGlyphSpecG_Sink_idx_22" layout:species="Sink">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="598.761904761905" layout:y="797.047619047619"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:speciesGlyph>' +
        '</layout:listOfSpeciesGlyphs>' +
        '<layout:listOfReactionGlyphs>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ0" layout:reaction="J0">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="235.25" layout:y="84.5"/>' +
                  '<layout:end layout:x="235.25" layout:y="84.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_External_glucose_idx_0J0" layout:speciesReference="speciesGlyphSpecG_External_glucose_idx_0J0" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="196.887" layout:y="77.528"/>' +
                      '<layout:end layout:x="235.25" layout:y="84.5"/>' +
                      '<layout:basePoint1 layout:x="200.88" layout:y="78.75"/>' +
                      '<layout:basePoint2 layout:x="215.33" layout:y="79.67"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0" layout:speciesReference="speciesGlyphSpecG_Glucose_idx_1J0" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="235.25" layout:y="84.5"/>' +
                      '<layout:end layout:x="273.957" layout:y="91.077"/>' +
                      '<layout:basePoint1 layout:x="255.17" layout:y="89.33"/>' +
                      '<layout:basePoint2 layout:x="269.62" layout:y="90.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ1" layout:reaction="J1">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="425.125" layout:y="148.25"/>' +
                  '<layout:end layout:x="425.125" layout:y="148.25"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J1" layout:speciesReference="speciesGlyphSpecG_Glucose_idx_1J1" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="343.852" layout:y="100.861"/>' +
                      '<layout:end layout:x="425.125" layout:y="148.25"/>' +
                      '<layout:basePoint1 layout:x="383.06" layout:y="103.12"/>' +
                      '<layout:basePoint2 layout:x="418.75" layout:y="117.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ATP_idx_2J1" layout:speciesReference="speciesGlyphSpecG_ATP_idx_2J1" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ATP_idx_2" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="498.657" layout:y="94.054"/>' +
                      '<layout:end layout:x="425.125" layout:y="148.25"/>' +
                      '<layout:basePoint1 layout:x="450.56" layout:y="100.62"/>' +
                      '<layout:basePoint2 layout:x="418.75" layout:y="117.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" layout:speciesReference="speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="425.125" layout:y="148.25"/>' +
                      '<layout:end layout:x="399.581" layout:y="193.071"/>' +
                      '<layout:basePoint1 layout:x="431.5" layout:y="179"/>' +
                      '<layout:basePoint2 layout:x="409.56" layout:y="180.12"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ADP_idx_4J1" layout:speciesReference="speciesGlyphSpecG_ADP_idx_4J1" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ADP_idx_4" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="425.125" layout:y="148.25"/>' +
                      '<layout:end layout:x="494.913" layout:y="178.875"/>' +
                      '<layout:basePoint1 layout:x="431.5" layout:y="179"/>' +
                      '<layout:basePoint2 layout:x="476.31" layout:y="173.12"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ2" layout:reaction="J2">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="373" layout:y="268.708737864078"/>' +
                  '<layout:end layout:x="373" layout:y="268.708737864078"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2" layout:speciesReference="speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="376.451" layout:y="233.356"/>' +
                      '<layout:end layout:x="373" layout:y="268.708737864078"/>' +
                      '<layout:basePoint1 layout:x="375.12" layout:y="240"/>' +
                      '<layout:basePoint2 layout:x="373.58" layout:y="265.038737864078"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" layout:speciesReference="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="373" layout:y="268.708737864078"/>' +
                      '<layout:end layout:x="375.882" layout:y="305.937"/>' +
                      '<layout:basePoint1 layout:x="372.42" layout:y="272.378737864078"/>' +
                      '<layout:basePoint2 layout:x="375.38" layout:y="297"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" layout:speciesReference="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="373" layout:y="268.708737864078"/>' +
                      '<layout:end layout:x="375.882" layout:y="305.937"/>' +
                      '<layout:basePoint1 layout:x="372.42" layout:y="272.378737864078"/>' +
                      '<layout:basePoint2 layout:x="375.38" layout:y="297"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ3" layout:reaction="J3">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="221.25" layout:y="305"/>' +
                  '<layout:end layout:x="221.25" layout:y="305"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3" layout:speciesReference="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="281.442" layout:y="306.586"/>' +
                      '<layout:end layout:x="221.25" layout:y="305"/>' +
                      '<layout:basePoint1 layout:x="296.88" layout:y="310"/>' +
                      '<layout:basePoint2 layout:x="256.33" layout:y="296.67"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_NADH_idx_6J3" layout:speciesReference="speciesGlyphSpecG_NADH_idx_6J3" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_NADH_idx_6" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="262.305" layout:y="276.93"/>' +
                      '<layout:end layout:x="221.25" layout:y="305"/>' +
                      '<layout:basePoint1 layout:x="259.88" layout:y="286"/>' +
                      '<layout:basePoint2 layout:x="256.33" layout:y="296.67"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3" layout:speciesReference="speciesGlyphSpecG_NAD_idx_7J3" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_NAD_idx_7" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="221.25" layout:y="305"/>' +
                      '<layout:end layout:x="165.249" layout:y="279.808"/>' +
                      '<layout:basePoint1 layout:x="186.17" layout:y="313.33"/>' +
                      '<layout:basePoint2 layout:x="172.62" layout:y="295.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Glycerol_idx_8J3" layout:speciesReference="speciesGlyphSpecG_Glycerol_idx_8J3" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="221.25" layout:y="305"/>' +
                      '<layout:end layout:x="161.169" layout:y="341.666"/>' +
                      '<layout:basePoint1 layout:x="186.17" layout:y="313.33"/>' +
                      '<layout:basePoint2 layout:x="174.62" layout:y="324.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ4" layout:reaction="J4">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="363.25" layout:y="390.5"/>' +
                  '<layout:end layout:x="363.25" layout:y="390.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4" layout:speciesReference="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="368.582" layout:y="345.387"/>' +
                      '<layout:end layout:x="363.25" layout:y="390.5"/>' +
                      '<layout:basePoint1 layout:x="361.88" layout:y="357.25"/>' +
                      '<layout:basePoint2 layout:x="362" layout:y="368"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ADP_idx_9J4" layout:speciesReference="speciesGlyphSpecG_ADP_idx_9J4" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ADP_idx_9" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="466.205" layout:y="362.535"/>' +
                      '<layout:end layout:x="363.25" layout:y="390.5"/>' +
                      '<layout:basePoint1 layout:x="396.88" layout:y="360.75"/>' +
                      '<layout:basePoint2 layout:x="362" layout:y="368"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_NAD_idx_10J4" layout:speciesReference="speciesGlyphSpecG_NAD_idx_10J4" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_NAD_idx_10" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="272.642" layout:y="367.274"/>' +
                      '<layout:end layout:x="363.25" layout:y="390.5"/>' +
                      '<layout:basePoint1 layout:x="328.62" layout:y="365.25"/>' +
                      '<layout:basePoint2 layout:x="362" layout:y="368"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4" layout:speciesReference="speciesGlyphSpecG_ATP_idx_11J4" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ATP_idx_11" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="363.25" layout:y="390.5"/>' +
                      '<layout:end layout:x="474.993" layout:y="425.556"/>' +
                      '<layout:basePoint1 layout:x="364.5" layout:y="413"/>' +
                      '<layout:basePoint2 layout:x="388.12" layout:y="423.75"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J4" layout:speciesReference="speciesGlyphSpecG_glycerate_3_phosphate_idx_12J4" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="363.25" layout:y="390.5"/>' +
                      '<layout:end layout:x="368.208" layout:y="429.603"/>' +
                      '<layout:basePoint1 layout:x="364.5" layout:y="413"/>' +
                      '<layout:basePoint2 layout:x="365.62" layout:y="420.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_NADH_idx_13J4" layout:speciesReference="speciesGlyphSpecG_NADH_idx_13J4" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_NADH_idx_13" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="363.25" layout:y="390.5"/>' +
                      '<layout:end layout:x="275.664" layout:y="417.201"/>' +
                      '<layout:basePoint1 layout:x="364.5" layout:y="413"/>' +
                      '<layout:basePoint2 layout:x="322.38" layout:y="422.75"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ5" layout:reaction="J5">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="358.25" layout:y="518"/>' +
                  '<layout:end layout:x="358.25" layout:y="518"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J5" layout:speciesReference="speciesGlyphSpecG_glycerate_3_phosphate_idx_12J5" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="365.817" layout:y="469.917"/>' +
                      '<layout:end layout:x="358.25" layout:y="518"/>' +
                      '<layout:basePoint1 layout:x="360.12" layout:y="485.5"/>' +
                      '<layout:basePoint2 layout:x="358.17" layout:y="483.33"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ADP_idx_14J5" layout:speciesReference="speciesGlyphSpecG_ADP_idx_14J5" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ADP_idx_14" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="470.736" layout:y="485.43"/>' +
                      '<layout:end layout:x="358.25" layout:y="518"/>' +
                      '<layout:basePoint1 layout:x="398.38" layout:y="483"/>' +
                      '<layout:basePoint2 layout:x="358.17" layout:y="483.33"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5" layout:speciesReference="speciesGlyphSpecG_ATP_idx_15J5" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ATP_idx_15" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="358.25" layout:y="518"/>' +
                      '<layout:end layout:x="471.147" layout:y="553.624"/>' +
                      '<layout:basePoint1 layout:x="358.33" layout:y="552.67"/>' +
                      '<layout:basePoint2 layout:x="398.62" layout:y="550.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J5" layout:speciesReference="speciesGlyphSpecG_pyruvate_idx_16J5" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="358.25" layout:y="518"/>' +
                      '<layout:end layout:x="357.998" layout:y="560.847"/>' +
                      '<layout:basePoint1 layout:x="358.33" layout:y="552.67"/>' +
                      '<layout:basePoint2 layout:x="358.88" layout:y="544"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ6" layout:reaction="J6">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="359.5" layout:y="622.5"/>' +
                  '<layout:end layout:x="359.5" layout:y="622.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J6" layout:speciesReference="speciesGlyphSpecG_pyruvate_idx_16J6" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="358.889" layout:y="600.591"/>' +
                      '<layout:end layout:x="359.5" layout:y="622.5"/>' +
                      '<layout:basePoint1 layout:x="360.904761904762" layout:y="623.654761904762"/>' +
                      '<layout:basePoint2 layout:x="359.17" layout:y="599.14619047619"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6" layout:speciesReference="speciesGlyphSpecG_Acetyladehyde_idx_17J6" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="359.5" layout:y="622.5"/>' +
                      '<layout:end layout:x="359.827" layout:y="646.946"/>' +
                      '<layout:basePoint1 layout:x="359.83" layout:y="645.85380952381"/>' +
                      '<layout:basePoint2 layout:x="360" layout:y="643.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ7" layout:reaction="J7">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="224.125" layout:y="668.5"/>' +
                  '<layout:end layout:x="224.125" layout:y="668.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J7" layout:speciesReference="speciesGlyphSpecG_Acetyladehyde_idx_17J7" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="305.02" layout:y="663.197"/>' +
                      '<layout:end layout:x="224.125" layout:y="668.5"/>' +
                      '<layout:basePoint1 layout:x="292.31" layout:y="666.25"/>' +
                      '<layout:basePoint2 layout:x="264.58" layout:y="650"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_NADH_idx_18J7" layout:speciesReference="speciesGlyphSpecG_NADH_idx_18J7" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_NADH_idx_18" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="255.776" layout:y="626.643"/>' +
                      '<layout:end layout:x="224.125" layout:y="668.5"/>' +
                      '<layout:basePoint1 layout:x="254.81" layout:y="636.25"/>' +
                      '<layout:basePoint2 layout:x="264.58" layout:y="650"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7" layout:speciesReference="speciesGlyphSpecG_NAD_idx_19J7" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_NAD_idx_19" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="224.125" layout:y="668.5"/>' +
                      '<layout:end layout:x="156.14" layout:y="659.413"/>' +
                      '<layout:basePoint1 layout:x="183.67" layout:y="687"/>' +
                      '<layout:basePoint2 layout:x="181.06" layout:y="673.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ethanol_idx_20J7" layout:speciesReference="speciesGlyphSpecG_ethanol_idx_20J7" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ethanol_idx_20" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="224.125" layout:y="668.5"/>' +
                      '<layout:end layout:x="166.559" layout:y="729.278"/>' +
                      '<layout:basePoint1 layout:x="183.67" layout:y="687"/>' +
                      '<layout:basePoint2 layout:x="176.31" layout:y="703.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ8" layout:reaction="J8">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="422.25" layout:y="699"/>' +
                  '<layout:end layout:x="422.25" layout:y="699"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J8" layout:speciesReference="speciesGlyphSpecG_Acetyladehyde_idx_17J8" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="400.383" layout:y="686.192"/>' +
                      '<layout:end layout:x="422.25" layout:y="699"/>' +
                      '<layout:basePoint1 layout:x="391.38" layout:y="681.5"/>' +
                      '<layout:basePoint2 layout:x="401.67" layout:y="687.33"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8" layout:speciesReference="speciesGlyphSpecG_External_acetaldehyde_idx_21J8" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="422.25" layout:y="699"/>' +
                      '<layout:end layout:x="473.872" layout:y="725.134"/>' +
                      '<layout:basePoint1 layout:x="442.83" layout:y="710.67"/>' +
                      '<layout:basePoint2 layout:x="453.12" layout:y="716.5"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ9" layout:reaction="J9">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="521.75" layout:y="137.5"/>' +
                  '<layout:end layout:x="521.75" layout:y="137.5"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ATP_idx_2J9" layout:speciesReference="speciesGlyphSpecG_ATP_idx_2J9" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ATP_idx_2" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="522.239" layout:y="110.343"/>' +
                      '<layout:end layout:x="521.75" layout:y="137.5"/>' +
                      '<layout:basePoint1 layout:x="522.38" layout:y="114.25"/>' +
                      '<layout:basePoint2 layout:x="522.17" layout:y="122"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9" layout:speciesReference="speciesGlyphSpecG_ADP_idx_4J9" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_ADP_idx_4" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="521.75" layout:y="137.5"/>' +
                      '<layout:end layout:x="520.758" layout:y="163.606"/>' +
                      '<layout:basePoint1 layout:x="521.33" layout:y="153"/>' +
                      '<layout:basePoint2 layout:x="521.12" layout:y="160.75"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
          '<layout:reactionGlyph layout:id="reactionGlyphJ10" layout:reaction="J10">' +
            '<layout:curve>' +
              '<layout:listOfCurveSegments>' +
                '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="LineSegment">' +
                  '<layout:start layout:x="547.619047619048" layout:y="770.728925874557"/>' +
                  '<layout:end layout:x="547.619047619048" layout:y="770.728925874557"/>' +
                '</layout:curveSegment>' +
              '</layout:listOfCurveSegments>' +
            '</layout:curve>' +
            '<layout:listOfSpeciesReferenceGlyphs>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J10" layout:speciesReference="speciesGlyphSpecG_External_acetaldehyde_idx_21J10" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" layout:role="substrate">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="539.364" layout:y="765.457"/>' +
                      '<layout:end layout:x="547.619047619048" layout:y="770.728925874557"/>' +
                      '<layout:basePoint1 layout:x="530.952380952381" layout:y="762.035714285714"/>' +
                      '<layout:basePoint2 layout:x="530.289047619048" layout:y="761.558925874557"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
              '<layout:speciesReferenceGlyph layout:id="specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10" layout:speciesReference="speciesGlyphSpecG_Sink_idx_22J10" layout:speciesGlyph="speciesGlyphspeciesGlyphSpecG_Sink_idx_22" layout:role="product">' +
                '<layout:curve>' +
                  '<layout:listOfCurveSegments>' +
                    '<layout:curveSegment xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CubicBezier">' +
                      '<layout:start layout:x="547.619047619048" layout:y="770.728925874557"/>' +
                      '<layout:end layout:x="589.509" layout:y="792.322"/>' +
                      '<layout:basePoint1 layout:x="564.949047619048" layout:y="779.898925874557"/>' +
                      '<layout:basePoint2 layout:x="562" layout:y="775.25"/>' +
                    '</layout:curveSegment>' +
                  '</layout:listOfCurveSegments>' +
                '</layout:curve>' +
              '</layout:speciesReferenceGlyph>' +
            '</layout:listOfSpeciesReferenceGlyphs>' +
          '</layout:reactionGlyph>' +
        '</layout:listOfReactionGlyphs>' +
        '<layout:listOfTextGlyphs>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_External_glucose_idx_0" layout:text="speciesGlyphSpecG_External_glucose_idx_0" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="77" layout:y="58"/>' +
              '<layout:dimensions layout:width="112.2" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_Glucose_idx_1" layout:text="speciesGlyphSpecG_Glucose_idx_1" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_Glucose_idx_1">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="282" layout:y="85"/>' +
              '<layout:dimensions layout:width="54" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ATP_idx_2" layout:text="speciesGlyphSpecG_ATP_idx_2" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ATP_idx_2">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="506" layout:y="79"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" layout:text="speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="290" layout:y="202"/>' +
              '<layout:dimensions layout:width="179.3" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ADP_idx_4" layout:text="speciesGlyphSpecG_ADP_idx_4" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ADP_idx_4">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="503" layout:y="172"/>' +
              '<layout:dimensions layout:width="35" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:text="speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="283" layout:y="314"/>' +
              '<layout:dimensions layout:width="191.4" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_NADH_idx_6" layout:text="speciesGlyphSpecG_NADH_idx_6" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_NADH_idx_6">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="250" layout:y="245"/>' +
              '<layout:dimensions layout:width="43" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_NAD_idx_7" layout:text="speciesGlyphSpecG_NAD_idx_7" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_NAD_idx_7">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="135" layout:y="247"/>' +
              '<layout:dimensions layout:width="36" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_Glycerol_idx_8" layout:text="speciesGlyphSpecG_Glycerol_idx_8" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="118" layout:y="350"/>' +
              '<layout:dimensions layout:width="54" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ADP_idx_9" layout:text="speciesGlyphSpecG_ADP_idx_9" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ADP_idx_9">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="474" layout:y="351"/>' +
              '<layout:dimensions layout:width="35" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_NAD_idx_10" layout:text="speciesGlyphSpecG_NAD_idx_10" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_NAD_idx_10">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="229" layout:y="356"/>' +
              '<layout:dimensions layout:width="36" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ATP_idx_11" layout:text="speciesGlyphSpecG_ATP_idx_11" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ATP_idx_11">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="483" layout:y="415"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" layout:text="speciesGlyphSpecG_glycerate_3_phosphate_idx_12" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="299" layout:y="438"/>' +
              '<layout:dimensions layout:width="151.8" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_NADH_idx_13" layout:text="speciesGlyphSpecG_NADH_idx_13" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_NADH_idx_13">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="224" layout:y="403"/>' +
              '<layout:dimensions layout:width="43" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ADP_idx_14" layout:text="speciesGlyphSpecG_ADP_idx_14" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ADP_idx_14">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="478" layout:y="474"/>' +
              '<layout:dimensions layout:width="35" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ATP_idx_15" layout:text="speciesGlyphSpecG_ATP_idx_15" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ATP_idx_15">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="480" layout:y="543"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_pyruvate_idx_16" layout:text="speciesGlyphSpecG_pyruvate_idx_16" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="330" layout:y="569"/>' +
              '<layout:dimensions layout:width="57" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" layout:text="speciesGlyphSpecG_Acetyladehyde_idx_17" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="313.190476190476" layout:y="655.952380952381"/>' +
              '<layout:dimensions layout:width="95.7" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_NADH_idx_18" layout:text="speciesGlyphSpecG_NADH_idx_18" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_NADH_idx_18">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="235" layout:y="595"/>' +
              '<layout:dimensions layout:width="43" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_NAD_idx_19" layout:text="speciesGlyphSpecG_NAD_idx_19" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_NAD_idx_19">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="112" layout:y="631"/>' +
              '<layout:dimensions layout:width="36" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_ethanol_idx_20" layout:text="speciesGlyphSpecG_ethanol_idx_20" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_ethanol_idx_20">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="134" layout:y="738"/>' +
              '<layout:dimensions layout:width="49" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" layout:text="speciesGlyphSpecG_External_acetaldehyde_idx_21" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="443.285714285714" layout:y="734.619047619048"/>' +
              '<layout:dimensions layout:width="148.5" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
          '<layout:textGlyph layout:id="txtGlyphspeciesGlyphSpecG_Sink_idx_22" layout:text="speciesGlyphSpecG_Sink_idx_22" layout:graphicalObject="speciesGlyphspeciesGlyphSpecG_Sink_idx_22">' +
            '<layout:boundingBox>' +
              '<layout:position layout:x="598.761904761905" layout:y="797.047619047619"/>' +
              '<layout:dimensions layout:width="34" layout:height="24"/>' +
            '</layout:boundingBox>' +
          '</layout:textGlyph>' +
        '</layout:listOfTextGlyphs>' +
        '<render:listOfRenderInformation xmlns:render="http://www.sbml.org/sbml/level3/version1/render/version1">' +
          '<render:renderInformation render:id="renderInfotestNetwork" render:programName="Sidewinder">' +
            '<render:listOfColorDefinitions>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_2" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_2" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_4" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_4" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_6" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_6" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_7" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_7" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_9" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_9" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_10" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_10" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_11" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_11" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_13" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_13" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_14" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_14" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_15" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_15" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_18" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_18" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_19" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_19" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ethanol_idx_20" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ethanol_idx_20" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Sink_idx_22" render:value="#ffcc99"/>' +
              '<render:colorDefinition render:id="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Sink_idx_22" render:value="#ff6600"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_glucose_idx_0J0" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J1" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_2J1" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J1" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_6J3" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glycerol_idx_8J3" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_9J4" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_10J4" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J4" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_13J4" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J5" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_14J5" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J5" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J6" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J7" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_18J7" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ethanol_idx_20J7" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J8" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_2J9" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J10" render:value="#2d7be1"/>' +
              '<render:colorDefinition render:id="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10" render:value="#2d7be1"/>' +
            '</render:listOfColorDefinitions>' +
            '<render:listOfLineEndings>' +
              '<render:lineEnding render:id="arrowHead_productJ0">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ1">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ2">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ3">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ4">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ5">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ6">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ7">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ8">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ9">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
              '<render:lineEnding render:id="arrowHead_productJ10">' +
                '<layout:boundingBox>' +
                  '<layout:position layout:x="-4.5" layout:y="-7"/>' +
                  '<layout:dimensions layout:width="9" layout:height="14"/>' +
                '</layout:boundingBox>' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10">' +
                  '<render:polygon>' +
                    '<render:listOfElements>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="14"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="3" render:y="7"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="0" render:y="0"/>' +
                      '<render:element xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="RenderPoint" render:x="9" render:y="7"/>' +
                    '</render:listOfElements>' +
                  '</render:polygon>' +
                '</render:g>' +
              '</render:lineEnding>' +
            '</render:listOfLineEndings>' +
            '<render:listOfStyles>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_External_glucose_idx_0" render:idList="speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_External_glucose_idx_0" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="112.2" render:height="24" render:rx="44.88%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_External_glucose_idx_0" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_Glucose_idx_1" render:idList="speciesGlyphspeciesGlyphSpecG_Glucose_idx_1">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Glucose_idx_1" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="54" render:height="24" render:rx="21.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_Glucose_idx_1" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ATP_idx_2" render:idList="speciesGlyphspeciesGlyphSpecG_ATP_idx_2">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_2" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_2" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="34" render:height="24" render:rx="13.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ATP_idx_2" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" render:idList="speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="179.3" render:height="24" render:rx="71.72%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ADP_idx_4" render:idList="speciesGlyphspeciesGlyphSpecG_ADP_idx_4">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_4" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_4" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="35" render:height="24" render:rx="14%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ADP_idx_4" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" render:idList="speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="191.4" render:height="24" render:rx="76.56%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_NADH_idx_6" render:idList="speciesGlyphspeciesGlyphSpecG_NADH_idx_6">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_6" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_6" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="43" render:height="24" render:rx="17.2%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_NADH_idx_6" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_NAD_idx_7" render:idList="speciesGlyphspeciesGlyphSpecG_NAD_idx_7">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_7" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_7" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="36" render:height="24" render:rx="14.4%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_NAD_idx_7" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_Glycerol_idx_8" render:idList="speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Glycerol_idx_8" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="54" render:height="24" render:rx="21.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_Glycerol_idx_8" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ADP_idx_9" render:idList="speciesGlyphspeciesGlyphSpecG_ADP_idx_9">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_9" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_9" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="35" render:height="24" render:rx="14%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ADP_idx_9" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_NAD_idx_10" render:idList="speciesGlyphspeciesGlyphSpecG_NAD_idx_10">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_10" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_10" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="36" render:height="24" render:rx="14.4%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_NAD_idx_10" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ATP_idx_11" render:idList="speciesGlyphspeciesGlyphSpecG_ATP_idx_11">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_11" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_11" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="34" render:height="24" render:rx="13.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ATP_idx_11" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_glycerate_3_phosphate_idx_12" render:idList="speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="151.8" render:height="24" render:rx="60.72%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_glycerate_3_phosphate_idx_12" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_NADH_idx_13" render:idList="speciesGlyphspeciesGlyphSpecG_NADH_idx_13">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_13" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_13" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="43" render:height="24" render:rx="17.2%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_NADH_idx_13" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ADP_idx_14" render:idList="speciesGlyphspeciesGlyphSpecG_ADP_idx_14">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_14" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ADP_idx_14" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="35" render:height="24" render:rx="14%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ADP_idx_14" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ATP_idx_15" render:idList="speciesGlyphspeciesGlyphSpecG_ATP_idx_15">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_15" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ATP_idx_15" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="34" render:height="24" render:rx="13.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ATP_idx_15" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_pyruvate_idx_16" render:idList="speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_pyruvate_idx_16" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="57" render:height="24" render:rx="22.8%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_pyruvate_idx_16" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_Acetyladehyde_idx_17" render:idList="speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="95.7" render:height="24" render:rx="38.28%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_Acetyladehyde_idx_17" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_NADH_idx_18" render:idList="speciesGlyphspeciesGlyphSpecG_NADH_idx_18">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_18" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NADH_idx_18" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="43" render:height="24" render:rx="17.2%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_NADH_idx_18" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_NAD_idx_19" render:idList="speciesGlyphspeciesGlyphSpecG_NAD_idx_19">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_19" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_NAD_idx_19" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="36" render:height="24" render:rx="14.4%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_NAD_idx_19" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_ethanol_idx_20" render:idList="speciesGlyphspeciesGlyphSpecG_ethanol_idx_20">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_ethanol_idx_20" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_ethanol_idx_20" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="49" render:height="24" render:rx="19.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_ethanol_idx_20" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_External_acetaldehyde_idx_21" render:idList="speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="148.5" render:height="24" render:rx="59.4%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_External_acetaldehyde_idx_21" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="speciesStyle_speciesGlyphSpecG_Sink_idx_22" render:idList="speciesGlyphspeciesGlyphSpecG_Sink_idx_22">' +
                '<render:g render:stroke="outlineColorSpecies_speciesGlyphspeciesGlyphSpecG_Sink_idx_22" render:stroke-width="3" render:fill="fillColorSpecies_speciesGlyphspeciesGlyphSpecG_Sink_idx_22" render:text-anchor="middle" render:vtext-anchor="middle">' +
                  '<render:rectangle render:x="0" render:y="0" render:width="34" render:height="24" render:rx="13.6%" render:ry="14.4%"/>' +
                '</render:g>' +
              '</render:style>' +
              '<render:style render:id="textStyle_speciesGlyphSpecG_Sink_idx_22" render:typeList="TEXTGLYPH">' +
                '<render:g render:text-anchor="middle" render:vtext-anchor="middle" render:font-size="12"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J0" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_External_glucose_idx_0J0">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_glucose_idx_0J0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_glucose_idx_0J0" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J0" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J0" render:endHead="arrowHead_productJ0" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J1" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glucose_idx_1J1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J1" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_ATP_idx_2J1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_2J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_2J1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J1" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J1" render:endHead="arrowHead_productJ1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J1" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_ADP_idx_4J1">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J1" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J1" render:endHead="arrowHead_productJ1" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J2" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_fructose_1_6_bisphosphate_idx_3J2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J2" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:endHead="arrowHead_productJ2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J2" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J2" render:endHead="arrowHead_productJ2" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J3" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J3" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_NADH_idx_6J3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_6J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_6J3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J3" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_7J3" render:endHead="arrowHead_productJ3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J3" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_Glycerol_idx_8J3">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glycerol_idx_8J3" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Glycerol_idx_8J3" render:endHead="arrowHead_productJ3" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J4" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glyceraldehyde_3_phosphate_idx_5J4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J4" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_ADP_idx_9J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_9J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_9J4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J4" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_NAD_idx_10J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_10J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_10J4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J4" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_11J4" render:endHead="arrowHead_productJ4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J4" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J4" render:endHead="arrowHead_productJ4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J4" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_NADH_idx_13J4">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_13J4" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_13J4" render:endHead="arrowHead_productJ4" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J5" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J5">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J5" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_glycerate_3_phosphate_idx_12J5" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J5" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_ADP_idx_14J5">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_14J5" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_14J5" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J5" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_15J5" render:endHead="arrowHead_productJ5" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J5" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J5">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J5" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J5" render:endHead="arrowHead_productJ5" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J6" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J6">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J6" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_pyruvate_idx_16J6" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J6" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J6" render:endHead="arrowHead_productJ6" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J7" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J7">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J7" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J7" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J7" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_NADH_idx_18J7">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_18J7" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NADH_idx_18J7" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J7" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_NAD_idx_19J7" render:endHead="arrowHead_productJ7" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J7" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_ethanol_idx_20J7">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ethanol_idx_20J7" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ethanol_idx_20J7" render:endHead="arrowHead_productJ7" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J8" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J8">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J8" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Acetyladehyde_idx_17J8" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J8" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J8" render:endHead="arrowHead_productJ8" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J9" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_ATP_idx_2J9">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_2J9" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ATP_idx_2J9" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J9" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_ADP_idx_4J9" render:endHead="arrowHead_productJ9" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_reactant_Style_J10" render:roleList="reactant" render:idList="specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J10">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J10" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_External_acetaldehyde_idx_21J10" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
              '<render:style render:id="reaction_product_Style_J10" render:roleList="product" render:idList="specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10">' +
                '<render:g render:stroke="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10" render:stroke-width="3" render:fill="fillColorReaction_specRefGlyphspeciesGlyphSpecG_Sink_idx_22J10" render:endHead="arrowHead_productJ10" render:text-anchor="middle" render:vtext-anchor="middle"/>' +
              '</render:style>' +
            '</render:listOfStyles>' +
          '</render:renderInformation>' +
        '</render:listOfRenderInformation>' +
      '</layout:layout>' +
    '</layout:listOfLayouts>' +
  '</model>' +
'</sbml>';


 end;
end;


end.
