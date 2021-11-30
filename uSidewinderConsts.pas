unit uSidewinderConsts;

interface

Uses Graphics; //System.UIConsts, System.UITypes;

const
   MAJOR_VERSION = 0;
   MINOR_VERSION = 9;
   BUG_FIX_VERSION = 0;
   MARKER_HANDLE_RADIUS = 2;
   CONTROL_HANDLE_RADIUS = 6;
   MAX_CONTROL_POINTS = 4;

   // The following constant is the distance between the outer
   // and inner rectangles, the 'dead' space between a node and
   // the launch point for a reaction arc (edge)
   NODE_ARC_DEADSPACE = 8;

   SELECTED_NODE_EDGE_COLOR = clRed;
   SELECTED_NODE_FILL_COLOR = clWhite;

   UniversalVolumeName = 'compartment';

   MAX_BEZIER_SEGMENTS = 29;  // Number of segments used to construct bezier

   peachOrange : TColor = $FFCC99;
   safetyOrange : TColor = $FF6600;

   DEGREE_OF_SHADOW_FACTOR = 4;
   THICKNESS_OF_BOUNDARY_SPECIES_EDGE = 1.55;
   ALIAS_BOUNDARY_COLOR = clRed;

   gridPAD_X : integer = 75; gridPAD_Y : integer = 75;
   gridNColumns : integer = 6;

implementation

end.
