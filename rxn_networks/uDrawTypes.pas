unit uDrawTypes;

interface

Uses System.Types;

type
 TNOMPoint = record
     x, y : double;
  end;
  TArrayOfNOMPoints = array of TNOMPoint;

  TNOMSize = record
     w, h : double;
  end;

  TNOMPointF = record
      x, y : double;
      constructor create (x, y : double);
  end;

  TBezierCoords = record
    src : TNOMPoint;
    h1, h2 : TNOMPoint;
    dest : TNOMPoint;
  end;

  TLineSegment = record p, q : TNOMPoint; end;  // A line segment
  //TBoundingBoxSegments = array[1..4] of TLineSegment;   // A Square

  TReactionType = (eUniUni, eUniBi, eAnyToAny);
  TLineType = (ltLine, ltBezier);



implementation

constructor TNOMPointF.create (x, y : double);
begin
  self.x := x; self.y := y;
end;


end.
