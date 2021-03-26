unit uMath;

interface

function myRandom : double;
function myRandomInt (maxValue : integer) : integer;
procedure setSeed (seed : integer);

implementation

Uses SysUtils, Classes;

CONST
   pi=3.1415926;
VAR
   x1,x2,x3,x4 : real;
   iy : ARRAY [1..3] OF integer;
   yprob : ARRAY [1..3] OF real;
   glix1,glix2,glix3 : integer;
   glr : ARRAY [1..97] OF real;

   mySeed: integer;

procedure setSeed (seed : integer);
begin
 mySeed := -seed;
end;

function myRandomInt (maxValue : integer) : integer;
begin
  result := trunc (maxValue * myRandom);
end;

FUNCTION myRandom : double;
(* Programs using RAN1 must declare the following variables
VAR
   glix1,glix2,glix3: integer;
   glr: ARRAY [1..97] OF real;
in the main program. *)
CONST
   m1=259200;
   ia1=7141;
   ic1=54773;
   rm1=3.8580247e-6;   (* 1.0/m1 *)
   m2=134456;
   ia2=8121;
   ic2=28411;
   rm2=7.4373773e-6;   (* 1.0/m2 *)
   m3=243000;
   ia3=4561;
   ic3=51349;
VAR
   j: integer;
begin
   if  (mySeed < 0) then
      BEGIN
      glix1 := (ic1-mySeed) MOD m1;
      glix1 := (ia1*glix1+ic1) MOD m1;
      glix2 := glix1 MOD m2;
      glix1 := (ia1*glix1+ic1) MOD m1;
      glix3 := glix1 MOD m3;
      FOR j := 1 TO 97 DO
          BEGIN
          glix1 := (ia1*glix1+ic1) MOD m1;
          glix2 := (ia2*glix2+ic2) MOD m2;
          glr[j] := (glix1+glix2*rm2)*rm1
          END;
      mySeed := 1
      END;
   glix1 := (ia1*glix1+ic1) MOD m1;
   glix2 := (ia2*glix2+ic2) MOD m2;
   glix3 := (ia3*glix3+ic3) MOD m3;
   j := 1 + (97*glix3) DIV m3;
   IF ((j > 97) OR (j < 1)) THEN BEGIN
      raise Exception.Create ('Exception in myrandom');
   end;
   glr[j] := (glix1+glix2*rm2)*rm1;
   result := glr[j];
end;

end.
