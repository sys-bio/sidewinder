unit uSidewinderGlobal;

interface

Uses Graphics, uNetworkTypes, uSidewinderConsts, Types;

var
  BezPoints       : array[0..MAX_BEZIER_SEGMENTS] of TPointF;
  BezJ, BezJPrime : array[0..MAX_BEZIER_SEGMENTS, 0..4] of double;


implementation

end.
