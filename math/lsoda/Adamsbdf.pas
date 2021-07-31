unit adamsbdf;

{ Lsoda differential equation solver Delphied. H M Sauro Dec 1996

  Revision 1.0a - May 1997. Memory allocation in constructor fixed, now
  allocates memory according to size of model.

  Original Pascal translation by Joao Pedro Monij-Barreto and Ronny Shuster.
  Original FORTRAN (version march30, 1987) to C translation by

  From tam@dragonfly.wri.com Wed Apr 24 01:35:52 1991
  Return-Path: <tam>
  Date: Wed, 24 Apr 91 03:35:24 CDT
  From: tam@dragonfly.wri.com
  To: whitbeck@wheeler.wrc.unr.edu
  Subject: lsoda.c
  Cc: augenbau@sparc0.brc.uconn.edu

  I'm told by Steve Nichols at Georgia Tech that you are interested in
  a stiff integrator.  Here's a translation of the fortran code LSODA.

  Please note that there is no comment.  The interface is the same as the FORTRAN
  code and I believe the documentation in LSODA will suffice.
  As usual, a free software comes with no guarantee.

  Hon Wah Tam
  Wolfram Research, Inc.
  tam@wri.com

  I have done some additions to lsoda.c . These were mainly to fill the
  gap of some features that were available in the fortran code and were
  missing in the C version.

  Changes are: all messages printed by lsoda routines will start with
  a '\n' (instead of ending with one); xsetf() was added so that user
  can control printing of messages by lsoda. xsetf should be called before
  calling lsoda: xsetf(0) switches printing off xsetf(1) swithces printing
  on (default) this implies one new global variable prfl (print flag).
  xsetf(0) will stop *any* printing by lsoda functions.
  Turning printing off means losing valuable information but will not scramble
  your stderr output ;-) This function is part of the original FORTRAN version.
  xsetf() and intdy() are now callable from outside the library as assumed
  in the FORTRAN version; created lsoda.h that should be included in blocks
  calling functions in lsoda's library.  iwork5 can now have an extra value:
  0 - no extra printing (default), 1 - print data on each method switch,
  ->  2 - print useful information at *each* stoda  step (one lsoda call
  has performs many stoda calls) and also data on each method switch
  note that a xsetf(0) call will prevent any printing even if iwork5 > 0;
  hu, tn were made available as extern variables.
  eMail:      INTERNET: prm@aber.ac.uk
  Pedro Mendes, Dept. of Biological Sciences, University College of Wales,
  Aberystwyth, Dyfed, SY23 3DA, United Kingdom.

  Further minor changes: 10 June 1992 by H Sauro and Pedro Mendes  }

  
  // This version is a Delphi compatible object translated by Herbert M Sauro Dec 1996  
  // Email HSauro@fssc.demon.co.uk
  { -------------------------------------------------------------------------- }

  { Quick usage instructions:

  1. Create Lsoda object specifying dimension of problem
  2. Initialise rtol and atol arrays (error tolerances, relative and absolute)
  3. Initialise t and tout (t = initial val, tout = requested solution point)
  4. Set itol to 4
  5. Set itask to 1, indicating normal computation
  6. Set istart to 1, indicating first call to lsoda
  7. Set iopt = 0 for no optional inputs
  8. jt = 2 for internal evaluation of jacobian
  9. Call Setfcn (fcn) to install your dydt routine
  10. Call lsoda (y, t, tout) to perfom one iterationm, use execute method }

  { See lsoda.doc for further details of interface. There may be further changes to
  this source in the future. The object interface is not quite right yet, but it
  does work, see included example. Also some works needs to be done to the body
  of the code particularly error trapping }

  { Note on TVector. TVector implements a dynamic array type of doubles. Use
  v := TVector.Create (10) to create 10 element array. Access data via v[i].
  v.size returns number of elements in vector. See vector.pas for more details }

  { Note on TMat. TMat is a simple matrix object type which serves a similar role
  to TVector except of course TMatrix is a 2D version }

  { LsodaMat includes two routines for doing LU decomposition and backward-
  substitution, painfully translated from FORTRAN code, couldn't use my own coz'
  I think LSODA requires a particular structure to LU result. These routines use a
  TVectori type (included with vector.pas) which simply handles dynamic arrays of
  integers }

  { Note to FORTRAN coders: please stop playing 'neat' tricks with arrays, it makes
  translating decent algorithms written in FORTRAN a hellish experience! }

  // Modified to pass in parameters, p, as an array of doubles to ODE eqs.
  // If using pascal 'TODE_eqs' class for equations then must uncomment
  // code to use 'procedure Setfcn (odeClass_fcn : TODE_eqs);'
  // instead of 'fcnProc'.

interface

uses SysUtils, uVector, uMatrix, LsodaMat, Dialogs, odeEquations, JS, Web;

type
    ELsodaException = class (Exception);

    { This is the type for the dydt function }
  fcnProc = function (t : double; y, p: array of double):TVector of object;

    TErrorType = (eNone, eDerivative, eBuildAlphaBeta, eChiSqr, eDelta,
                  eNormalisation, eMatrixInversion, ePoorConvergence);

    vector13 = array [1..13] of double;      { Used in declaring pc in cfode }


    TLsoda = class (TObject)
                   private
                     neq : integer;   { # of first-order odes }

                     tn, h, hu, tsw, tolsf : double;
                     nje, nfe, prfl, nq, nqu, meth, mused, nst, imxer : integer;

                     { static variables for lsoda() }
                     ccmax, el0, hmin, hmxi, rc, pdnorm  : double;
                     illin, init, mxstep, mxhnil, nhnil, ntrep,
                     nslast, nyh, ierpj, iersl, jcur, jstart, kflag, l,
                     miter, maxord, maxcor, msbp, mxncf, n,
                     ixpr, jtyp, mxordn, mxords : integer;

                     { non-static variable for prja(), solsy() }
                     { static variables for stoda() }

                     conit, crate, hold, rmax,pdest, pdlast, ratio: double;
                     elco, tesco : TMatrix;
                     ialth, ipup, lmax, meo, nslp, icount, irflag : integer;
                     cm1, cm2, el : TVector;

                     { static variables for various vectors and the Jacobian. }

                     yh   : array[1..13] of TVector;
                     wm   : TMatrix;
                     perm : TVectori;   { Permuation vector for LU decomp }
                     ewt, savf, acor : TVector;

                     sqrteta : double;  { sqrt (ETA) }

                     Frtol, Fatol : TVector;
                     Fitol, Fitask, Fistate, Fiopt, Fjt, Fiwork5, Fiwork6 : integer;
                     Fiwork7, Fiwork8, Fiwork9 : integer;
                     Frwork1, Frwork5, Frwork6, Frwork7 : double;
                    // FDerivatives : fcnProc;
                     odeClass: TODE_eqs;    // Added.

                     procedure terminate(var istate: integer );
                     procedure terminate2 (var y : TVector; var t : double);
                     procedure successreturn(var y : TVector;var t : double;
                                   itask, ihit : integer; tcrit : double; var istate : integer);
                     procedure ewset(itol : integer; rtol, atol, ycur : TVector);
                     procedure prja (neq : integer; var y : TVector);
                     procedure corfailure(var told, rh : double; var ncf, corflag : integer);
                     procedure solsy(var y : TVector);
                     procedure methodswitch( dsm, pnorm: double; var pdh, rh: double);
                     procedure endstoda;
                     procedure orderswitch (var rhup : double; dsm : double; var pdh, rh : double;
                                             var orderflag : integer);
                     procedure resetcoeff;
                     procedure correction (neq : integer; var y : TVector; var corflag : integer;
                                           pnorm : double; var del, delp, told : double;
                                           var ncf : integer;  var rh : double; var m : integer);
                     procedure intdy (t : double; k : integer; var dky : TVector; var iflag : integer);
                     procedure cfode(meth : integer);
                     procedure scaleh (var rh, pdh : double);
                     procedure stoda (var neq : integer; var y : TVector);
                     function arrayToTVector(a_arr: array of double): TVector;
                     function tVectorToArray(a_tvect: TVector): array of double;

                   public
                     constructor Create (n : integer); virtual;
                     destructor  destroy; override;
                     procedure   Setfcn (fcn : fcnProc);        // <----- Use if js ODE eqs
                     //procedure   Setfcn (odeClass_fcn : TODE_eqs);  // <--- Use if all pascal code

                     { y = array of initial values of variables. t = initial value of
                     independent variable, tout, value of t when output is required.
                     On output, y holds new values of variables and t updated to tout }
                     procedure Execute (var y : TVector; var t, tout : double);
                     function  Getrtol (i : integer) : double;
                     procedure Setrtol (i : integer; d : double);
                     function  Getatol (i : integer) : double;
                     procedure Setatol (i : integer; d : double);

                     property  rtol[i : Integer] : double read Getrtol write Setrtol;
                     property  atol[i : Integer] : double read Getatol write Setatol;
                     property  itol   : integer read Fitol   write Fitol;
                     property  itask  : integer read Fitask  write Fitask;
                     property  istate : integer read Fistate write Fistate;
                     property  iopt   : integer read Fiopt   write Fiopt;
                     property  jt     : integer read Fjt     write Fjt;
                     property  iwork5 : integer read Fiwork5 write Fiwork5;
                     property  iwork6 : integer read Fiwork6 write Fiwork6;
                     property  iwork7 : integer read Fiwork7 write Fiwork7;
                     property  iwork8 : integer read Fiwork8 write Fiwork8;
                     property  iwork9 : integer read Fiwork9 write Fiwork9;
                     property  rwork1 : double  read Frwork1 write Frwork1;
                     property  rwork5 : double  read Frwork5 write Frwork5;
                     property  rwork6 : double  read Frwork6 write Frwork6;
                     property  rwork7 : double  read Frwork7 write Frwork7;
                     var p: array of double;  // parameters used in ODE eqs.

                     FDerivatives : fcnProc;  // Originally private.
                   end;


{ ------------------------------------------------------------------------- }


implementation

const
     ETA = 2.2204460492503131e-16;
     mord : array[1..2] of integer = (12, 5) ;
     sm1 : array[0..12] of double = ( 0., 0.5, 0.575, 0.55, 0.45, 0.35, 0.25,
                                      0.2, 0.15, 0.1, 0.075, 0.05, 0.025 );


{ Excecuted if model function not assigned by user }
procedure DummyFcn (t : double; y, dydt :TVector); //far;
begin
  raise ELsodaException.Create ('No function assigned to evaluate dydts (Use Setfcn)!');
end;


{ Create a Lsoda object with n differential equations }
constructor TLsoda.Create (n : integer);
var i : integer;
begin
  neq := n;

  el    := TVector.Create (14);
  elco  := TMatrix.Create (13, 14);
  tesco := TMatrix.Create (13, 4);
  cm1   := TVector.Create (13);
  cm2   := TVector.Create (6);

  { Dimensions in following vectors now dependent on size of model, May 97 }
  for i := 1 to 13 do yh[i] := TVector.Create (n);
  wm   := TMatrix.Create (neq, neq);
  perm := TVectori.Create (neq);
  ewt  := TVector.Create (neq);
  savf := TVector.Create (neq);
  acor := TVector.Create (neq);

  Frtol := TVector.Create (neq);
  Fatol := TVector.Create (neq);

 // FDerivatives := DummyFcn;  { Install the default fcn handler, in case user forgets }
end;



destructor TLsoda.destroy;
var i : integer;
begin
  el.free; elco.free;
  tesco.free; cm1.free; cm2.free;

  for i := 1 to 13 do
      yh[i].free;
  wm.free;

  perm.free;
  ewt.free;
  savf.free;
  acor.free;
  Frtol.free;
  Fatol.free;
end;


procedure TLsoda.Setfcn (fcn : fcnProc);
//procedure TLsoda.Setfcn (odeClass_fcn : TODE_eqs);  // Use for all pascal code
begin

  self.FDerivatives := fcn;
 // self.FDerivatives:= odeClass_fcn.ComputeODEs;  // <--- Use for all pascal code
end;



{**************************************************************}
function max(a, b : double) : double;
begin
 if(a>b) then max:=a
  else max:=b;
end;

{**************************************************************}
function min(a, b : double) : double;
begin
 if(a>b) then min:=b
  else min:=a;
end;

{**************************************************************}
function maxi(a, b : integer) : integer;
begin
 if(a>b) then maxi:=a
  else maxi:=b;
end;

{**************************************************************}
function mini(a, b : integer) : integer;
begin
 if(a>b) then mini:=b
  else mini:=a;
end;

{**************************************************************}
function pow(a : double; x : double) : double;
begin
  if a=0 then pow := 0 else pow := exp(x*ln(abs(a)))*abs(a)/a;
end;


function TLsoda.Getrtol (i : integer) : double;
begin
  result := Frtol[i];
end;


procedure TLsoda.Setrtol (i : integer; d : double);
begin
  Frtol[i] := d;
end;


function TLsoda.Getatol (i : integer) : double;
begin
  result := Fatol[i];
end;


procedure TLsoda.Setatol (i : integer; d : double);
begin
  Fatol[i] := d;
end;


{**************************************************************}
{ Terminate lsoda due to illegal input. }
procedure TLsoda.terminate(var istate: integer );
begin
   if ( illin = 5 ) then
    begin
      writeln('lsoda -- repeated occurrence of illegal input');
      writeln('         run aborted.. apparent infinite loop');
    end
   else
    begin
      illin:=illin+1;
      istate := -3;
    end;
end;         {   end terminate   }

{**************************************************************}
{  Terminate lsoda due to various error conditions. }
procedure TLsoda.terminate2 (var y : TVector; var t : double);
var i: integer;
begin
  for i := 1 to n do y[i] := yh[1][i];
  t := tn;
  illin := 0;
end; { end terminate2 }

{**************************************************************}
{  The following block handles all successful returns from lsoda.
   If itask != 1, y is loaded from yh and t is set accordingly.
   *Istate is set to 2, the illegal input counter is zeroed, and the
   optional outputs are loaded into the work arrays before returning.}

procedure TLsoda.successreturn(var y: TVector; var t:double;
                            itask, ihit:integer;
                            tcrit :double;
                        var istate:integer );
var i: integer;
begin
  for i := 1 to n do y[i] := yh[1][i];
  t := tn;
  if (itask = 4) or (itask = 5) then
      if (ihit = 0) then
         t := tcrit;
  istate := 2;
  illin := 0;
end;   { end successreturn }

{**************************************************************}
procedure TLsoda.ewset(itol : integer; rtol, atol, ycur : TVector);
var i: integer;
begin
   case itol of
    1 : for i := 1 to n do
         ewt[i] := rtol[1] * abs( ycur[i] ) + atol[1];
    2 : for i := 1 to n do
         ewt[i] := rtol[1] * abs( ycur[i] ) + atol[i];
    3 : for i := 1 to n do
         ewt[i] := rtol[i] * abs( ycur[i] ) + atol[1];
    4 : for i := 1 to n do
         ewt[i] := rtol[i] * abs( ycur[i] ) + atol[i];
   end;
end;           {   end ewset   }

{**************************************************************}
function vmnorm(v, w : TVector) : double;
{  This function routine computes the weighted max-norm
   of the vector of length n contained in the array v, with weights
   contained in the array w of length n.

   vmnorm = max( i = 1, ..., n ) fabs( v[i] ) * w[i]. }
var i, n : integer;
begin
  result := 0.0; n := v.size;
  for i := 1 to n do
      result := max(result, abs(v[i]) * w[i]);
end;    { end vmnorm }


{**************************************************************}
function fnorm(a : TMatrix; w : TVector) : double;

{  This subroutine computes the norm of a full n by n matrix,
   stored in the array a, that is consistent with the weighted max-norm
   on vectors, with weights stored in the array w.

      fnorm = max(i=1,...,n) ( w[i] * sum(j=1,...,n) fabs( a[i][j] ) / w[j] ) }
var
   i, j, n : integer;
   sum : double;
begin
  result := 0.; n := a.r;
  for i := 1 to n do
      begin
      sum := 0.;
      for j := 1 to n do
          sum := sum + (abs(a[i,j]) / w[j]);
      result := max(result, sum * w[i]);
      end;
end;  { end fnorm }



{**************************************************************}
procedure TLsoda.prja (neq : integer; var y : TVector);
var i, j : integer;
   fac, hl0, r, r0, yj : double;
   ier : byte;
   y_arr: array of double;
   dy_tv:TVector;
{  prja is called by stoda to compute and process the matrix
   P = I - h * el[1] * J, where J is an approximation to the Jacobian.
   Here J is computed by finite differencing.
   J, scaled by -h * el[1], is stored in wm.  Then the norm of J ( the
   matrix norm consistent with the weighted max-norm on vectors given
   by vmnorm ) is computed, and J is overwritten by P.  P is then
   subjected to LU decomposition in preparation for later solution
   of linear systems with p as coefficient matrix.  This is done
   by LUfactor if miter = 2, and by dgbfa if miter = 5.  }

begin
   nje := nje + 1;
   ierpj := 0;
   jcur := 1;
   hl0 := h * el0;

{ If miter = 2, make n calls to f to approximate J. }

   if miter <> 2 then
      begin
      if prfl = 1 then writeln('prja -- miter != 2');
      exit;
      end;

   if miter = 2 then
      begin
      fac := vmnorm(savf, ewt);
      r0 := 1000. * abs(h) * ETA * n * fac;
      if r0 = 0. then
          r0 := 1.;
      for j := 1 to n do
          begin
          yj := y[j];
          r := max(sqrteta * abs( yj ), r0 / ewt[j]);
          y[j] := y[j] + r;
          fac := -hl0 / r;
          // convert from vectors to arrays:
          y_arr:= tVectorToArray(y);
          acor:= FDerivatives (tn, y_arr, self.p);

          for i := 1 to n do
              wm[i,j] := (acor[i] - savf[i]) * fac;
          y[j] := yj;
          end;
      nfe := nfe + n;

      { Compute norm of Jacobian }
      pdnorm := fnorm(wm, ewt ) / abs( hl0 );

      { Add identity matrix. }
      for i := 1 to n do wm[i,i] := wm[i,i]+1.;

      { Do LU decomposition on P.}
      LUfactor (wm, perm);
      end;
end; { end prja }


{**************************************************************}
procedure TLsoda.corfailure(var told, rh : double; var ncf, corflag : integer);
var j, i1, i : integer;
begin
  ncf:= ncf+1;
  rmax := 2.;
  tn := told;
  for j := nq downto 1 do
      for i1 := j to nq do
         for i := 1 to n do
            yh[i1][i] := yh[i1][i] - yh[i1+1][i];
  if (abs(h) <= hmin * 1.00001) or (ncf = mxncf ) then
     begin
     //console.log('In TLsoda.corfailure, setting corflag to 2');
     corflag := 2;
     exit;
     end;
  corflag := 1;
  rh := 0.25;
  ipup := miter;
end;  { end corfailure }


{**************************************************************}
procedure TLsoda.solsy(var y : TVector);

{  This routine manages the solution of the linear system arising from
   a chord iteration.  It is called if miter != 0.
   If miter is 2, it calls dgesl to accomplish this.
   If miter is 5, it calls dgbsl.

   y = the right-hand side vector on input, and the solution vector
       on output. }
var ier : byte;
begin
  iersl := 0;
  if miter <> 2 then
     begin
     if ( prfl=1 ) then console.log('solsy -- miter != 2');
     exit;
     end;

  if miter = 2 then LUsolve (wm, perm, y);
end; { end solsy }


{**************************************************************}
procedure TLsoda.methodswitch( dsm, pnorm: double; var pdh, rh: double);
var
   lm1, lm1p1, lm2, lm2p1, nqm1, nqm2 : integer;
   rh1, rh2, rh1it, exm2, dm2, exm1, dm1, alpha, exsm : double;

{  We are current using an Adams method.  Consider switching to bdf.
   If the current order is greater than 5, assume the problem is
   not stiff, and skip this section.
   If the Lipschitz constant and error estimate are not polluted
   by roundoff, perform the usual test.
   Otherwise, switch to the bdf methods if the last step was
   restricted to insure stability ( irflag = 1 ), and stay with Adams
   method if not.  When switching to bdf with polluted error estimates,
   in the absence of other information, double the step size.

   When the estimates are ok, we make the usual test by computing
   the step size we could have (ideally) used on this step,
   with the current (Adams) method, and also that for the bdf.
   If nq > mxords, we consider changing to order mxords on switching.
   Compare the two step sizes to decide whether to switch.
   The step size advantage must be at least ratio = 5 to switch.}

begin
  if meth = 1 then
     begin
     if nq > 5 then exit;
     if (dsm <= ( 100. * pnorm * ETA )) or (pdest = 0. ) then
        begin
        if irflag = 0 then exit;
        rh2 := 2.;
        nqm2 := mini(nq, mxords );
        end
     else
        begin
        exsm := 1. / l;
        rh1 := 1. / ( 1.2 * pow( dsm, exsm ) + 0.0000012 );
        rh1it := 2. * rh1;
        pdh := pdlast * abs( h );
        if (pdh * rh1) > 0.00001 then rh1it := sm1[nq] / pdh;
        rh1 := min(rh1, rh1it);
        if nq > mxords then
           begin
           nqm2 := mxords;
           lm2 := mxords + 1;
           exm2 := 1. / lm2;
           lm2p1 := lm2 + 1;
           dm2 := vmnorm(yh[lm2p1], ewt ) / cm2[mxords];
           rh2 := 1. / ( 1.2 * pow( dm2, exm2 ) + 0.0000012 );
           end
        else
           begin
           dm2 := dsm * ( cm1[nq] / cm2[nq] );
           rh2 := 1. / ( 1.2 * pow( dm2, exsm ) + 0.0000012 );
           nqm2 := nq;
           end;
        if rh2 < ratio * rh1 then exit;
        end;

     { The method switch test passed.  Reset relevant quantities for bdf. }

     rh := rh2;
     icount := 20;
     meth := 2;
     miter := jtyp;
     pdlast := 0.;
     nq := nqm2;
     l := nq + 1;
     exit;
     end;   { end if ( meth == 1 ) }

{  We are currently using a bdf method, considering switching to Adams.
   Compute the step size we could have (ideally) used on this step,
   with the current (bdf) method, and also that for the Adams.
   If nq > mxordn, we consider changing to order mxordn on switching.
   Compare the two step sizes to decide whether to switch.
   The step size advantage must be at least 5/ratio = 1 to switch.
   If the step size for Adams would be so small as to cause
   roundoff pollution, we stay with bdf. }

  exsm := 1. / l;
  if mxordn < nq then
     begin
     nqm1 := mxordn;
     lm1 := mxordn + 1;
     exm1 := 1. / lm1;
     lm1p1 := lm1 + 1;
     dm1 := vmnorm(yh[lm1p1], ewt ) / cm1[mxordn];
     rh1 := 1. / ( 1.2 * pow( dm1, exm1 ) + 0.0000012 );
     end
  else
     begin
     dm1 := dsm * ( cm2[nq] / cm1[nq] );
     rh1 := 1. / ( 1.2 * pow( dm1, exsm ) + 0.0000012 );
     nqm1 := nq;
     exm1 := exsm;
     end;
  rh1it := 2. * rh1;
  pdh := pdnorm * abs( h );
  if ( pdh * rh1 ) > 0.00001 then rh1it := sm1[nqm1] / pdh;
  rh1 := min( rh1, rh1it );
  rh2 := 1. / ( 1.2 * pow( dsm, exsm ) + 0.0000012 );
  if (( rh1 * ratio ) < ( 5. * rh2 )) then exit;
  alpha := max( 0.001, rh1 );
  dm1 := dm1 * pow( alpha, exm1 );
  if (dm1 <= 1000. * ETA * pnorm) then exit;

  { The switch test passed.  Reset relevant quantities for Adams. }

  rh := rh1;
  icount := 20;
  meth := 1;
  miter := 0;
  pdlast := 0.;
  nq := nqm1;
  l := nq + 1;
end;     { end methodswitch }


{**************************************************************}
{  This routine returns from stoda to lsoda.  Hence freevectors() is
   not executed. }
procedure TLsoda.endstoda;
var
   r : double;
   i : integer;
begin
  r := 1. / tesco[nqu,2];
  for i := 1 to n do
      acor[i] := acor[i] * r;
  hold := h;
  jstart := 1;
end;  { end endstoda }


{**************************************************************}
procedure TLsoda.orderswitch( var rhup: double;
                       dsm: double;
                       var pdh, rh: double;
                       var orderflag: integer);

{  Regardless of the success or failure of the step, factors
   rhdn, rhsm, and rhup are computed, by which h could be multiplied
   at order nq - 1, order nq, or order nq + 1, respectively.
   In the case of a failure, rhup = 0. to avoid an order increase.
   The largest of these is determined and the new order chosen
   accordingly.  If the order is to be increased, we compute one
   additional scaled derivative.

   orderflag = 0  : no change in h or nq,
               1  : change in h but not nq,
               2  : change in both h and nq. }

var
   newq, i: integer;
   exsm, rhdn, rhsm, ddn, exdn, r: double;

begin
  orderflag := 0;
  exsm := 1. / l;
  rhsm := 1. / ( 1.2 * pow( dsm, exsm ) + 0.0000012 );

  rhdn := 0.;
  if nq <> 1 then
     begin
     ddn := vmnorm(yh[l], ewt ) / tesco[nq,1];
     exdn := 1. / nq;
     rhdn := 1. / ( 1.3 * pow( ddn, exdn ) + 0.0000013 );
     end;

  { If meth = 1, limit rh accordinfg to the stability region also. }

  if meth = 1 then
     begin
     pdh := max( abs( h ) * pdlast, 0.000001 );
     if l < lmax then
        rhup := min( rhup, sm1[l] / pdh );
     rhsm := min( rhsm, sm1[nq] / pdh );
     if nq > 1 then
        rhdn := min( rhdn, sm1[nq-1] / pdh );
     pdest := 0.;
     end;
  if rhsm >= rhup then
     begin
     if rhsm >= rhdn then
        begin
        newq := nq;
        rh := rhsm;
        end
     else
        begin
        newq := nq - 1;
        rh := rhdn;
        if (kflag < 0) AND (rh > 1.0) then
            rh := 1.;
        end;
     end
  else
     begin
     if ( rhup <= rhdn ) then
        begin
        newq := nq - 1;
        rh := rhdn;
        if ( kflag < 0) AND (rh > 1. ) then
           rh := 1.;
        end
     else
        begin
        rh := rhup;
        if ( rh >= 1.1 ) then
           begin
           r := el[l] / l;
           nq := l;
           l := nq + 1;
           for i := 1 to n do

               yh[l][i] := acor[i] * r;
              // console.log( ' line ~771, yh[l][',i,']: ',yh[l][i]);
           orderflag := 2;
           exit;
           end
        else
          begin
          ialth := 3;
          exit;
          end;
        end;
     end;

     { If meth = 1 and h is restricted by stability, bypass 10 percent test. }

     if meth = 1 then
        begin
        if (( rh * pdh * 1.00001 ) < sm1[newq]) then
           if (kflag = 0) AND (rh < 1.1) then
              begin
              ialth := 3;
              exit;
              end;
        end
     else
        begin
        if (kflag = 0) AND (rh < 1.1 ) then
           begin
           ialth := 3;
           exit;
           end;
        end;
   if kflag <= -2 then
      rh := min( rh, 0.2 );

{  If there is a change of order, reset nq, l, and the coefficients.
   In any case h is reset according to rh and the yh array is rescaled.
   Then exit or redo the step. }

   if (newq = nq) then
      begin
      orderflag := 1;
      exit;
      end;
   nq := newq;
   l := nq + 1;
   orderflag := 2;
end;      {   end orderswitch   }


{**************************************************************}
procedure TLsoda.resetcoeff;
{  The el vector and related constants are reset
   whenever the order nq is changed, or at the start of the problem. }
var i : integer;
begin
  for i := 1 to l do el[i] := elco[nq,i];
  rc := rc * el[1] / el0;
  el0 := el[1];
  conit := 0.5 / ( nq + 2 );
end;     { end resetcoeff }


{**************************************************************}
procedure TLsoda.correction( neq:integer;
                      var y:TVector;
                      var corflag:integer;
                      pnorm: double;
                      var del, delp, told: double;
                      var ncf: integer;
                      var rh: double;
                      var m: integer);

{  *corflag = 0 : corrector converged,
              1 : step size to be reduced, redo prediction,
              2 : corrector cannot converge, failure flag. }

var
   i: integer;
   rm, rate, dcon: double;
   y_arr: array of double;
{  Up to maxcor corrector iterations are taken.  A convergence test is
   made on the r.m.s. norm of each correction, weighted by the error
   weight vector ewt.  The sum of the corrections is accumulated in the
   vector acor[i].  The yh array is not altered in the corrector loop. }

begin
   m := 0;
   corflag := 0;
   rate := 0.;
   del := 0.;
   for i := 1 to n do
      y[i] := yh[1][i];
   //savf:= FDerivatives (tn, y);
   y_arr:= tVectorToArray(y);
   savf:= FDerivatives(tn, y_arr, self.p);
   nfe := nfe + 1;

{  If indicated, the matrix P = I - h * el[1] * J is reevaluated and
   preprocessed before starting the corrector iteration.  ipup is set
   to 0 as an indicator that this has been done. }

   while ( 1=1 ) do
    begin
      if ( m = 0 ) then
       begin
         if ( ipup > 0 ) then
          begin
            prja( neq, y);
            ipup := 0;
            rc := 1.;
            nslp := nst;
            crate := 0.7;
            if ( ierpj <> 0 ) then
             begin
               corfailure( told, rh, ncf, corflag );
               exit;
             end;
          end;
         for i := 1 to n do
            acor[i] := 0.;
       end;   {   end if ( *m == 0 )   }
      if ( miter = 0 ) then
       begin
{  In case of functional iteration, update y directly from
   the result of the last function evaluation. }

         for i := 1 to n do
             begin
             savf[i] := h * savf[i] - yh[2][i];
             y[i] := savf[i] - acor[i];
             end;
         del := vmnorm(y, ewt );
         for i := 1 to n do
             begin
             y[i] := yh[1][i] + el[1] * savf[i];
             acor[i] := savf[i];
             end;
       end      {   end functional iteration   }

{  In the case of the chord method, compute the corrector error,
   and solve the linear system with that as right-hand side and
   P as coefficient matrix. }

      else
       begin
         for i := 1 to n do
            y[i] := h * savf[i] - ( yh[2][i] + acor[i] );
         solsy( y );
         del := vmnorm(y, ewt );
         for i := 1 to n do
          begin
            acor[i] := acor[i] + y[i];
            y[i] := yh[1][i] + el[1] * acor[i];
          end;
      end;   {   end chord method   }

{  Test for convergence.  If *m > 0, an estimate of the convergence
   rate constant is stored in crate, and this is used in the test.

   We first check for a change of iterates that is the size of
   roundoff error.  If this occurs, the iteration has converged, and a
   new rate estimate is not formed.
   In all other cases, force at least two iterations to estimate a
   local Lipschitz constant estimate for Adams method.
   On convergence, form pdest = local maximum Lipschitz constant
   estimate.  pdlast is the most recent nonzero estimate. }

      if ( del <= 100. * pnorm * ETA ) then exit;
      if ( m <> 0) OR (meth <> 1 ) then
       begin
         if ( m <> 0 ) then
            begin
            rm := 1024.0;
            if ( del <= ( 1024. * delp ) ) then
               rm := del / delp;
            rate := max( rate, rm );
            crate := max( 0.2 * crate, rm );
            end;
         dcon := del * min( 1., 1.5 * crate ) / ( tesco[nq,2] * conit );
         if ( dcon <= 1. ) then
            begin
            pdest := max( pdest, rate / abs( h * el[1] ) );
            if ( pdest <> 0. ) then
               pdlast := pdest;
            exit;
            end;
       end;

{  The corrector iteration failed to converge.
   If miter != 0 and the Jacobian is out of date, prja is called for
   the next try.   Otherwise the yh array is retracted to its values
   before prediction, and h is reduced, if possible.  If h cannot be
   reduced or mxncf failures have occured, exit with corflag = 2. }

      m:=m+1;
      if ( m = maxcor) OR (( m >= 2) AND (del > 2. * delp ))  then
         begin
         if ( miter = 0) OR (jcur = 1 ) then
            begin
            corfailure( told, rh, ncf, corflag );
            exit;
           end;
         ipup := miter;

     { Restart corrector if Jacobian is recomputed. }

         m := 0;
         rate := 0.;
         del := 0.;
         for i := 1 to n do y[i] := yh[1][i];

         //savf:= FDerivatives (tn, y);
         y_arr:= tVectorToArray(y);
         savf:= FDerivatives(tn,y_arr,self.p);

         nfe := nfe + 1;
         end

{  Iterate corrector. }

      else
         begin
         delp := del;
        // savf:= FDerivatives (tn, y);
         y_arr:= tVectorToArray(y);
         savf:= FDerivatives(tn,y_arr,self.p);
         nfe := nfe + 1;
         end;
   end;   {   end while   }
end;       {   end correction   }


{**************************************************************}
procedure TLsoda.intdy (t : double; k : integer; var dky : TVector; var iflag : integer);

{  intdy computes interpolated values of the k-th derivative of the
   dependent variable vector y, and stores it in dky.  This routine
   is called within the package with k = 0 and *t = tout, but may
   also be called by the user for any k up to the current order.
   ( See detailed instructions in the usage documentation. )

   The computed values in dky are gotten by interpolation using the
   Nordsieck history array yh.  This array corresponds uniquely to a
   vector-valued polynomial of degree nqcur or less, and dky is set
   to the k-th derivative of this polynomial at t.
   The formula for dky is

             q
   dky[i] = sum c[k][j] * ( t - tn )(j-k) * h(-j) * yh[j+1][i]
            j=k

   where c[k][j] = j*(j-1)*...*(j-k+1), q = nqcur, tn = tcur, h = hcur.
   The quantities nq = nqcur, l = nq+1, n = neq, tn, and h are declared
   static globally.  The above sum is done in reverse order.
   *iflag is returned negative if either k or t is out of bounds. }

var
   i, ic, j, jj, jp1: integer;
   c, r, s, tp: double;

begin
   iflag := 0;
   if ( k < 0) OR (k > nq ) then
    begin
      if ( prfl=1 ) then
         writeln('intdy -- k = ',k,' illegal');
      iflag := -1;
      exit;
    end;
   tp := tn - hu - 100. * ETA * ( tn + hu );
   if ( ( t - tp ) * ( t - tn ) > 0. ) then
    begin
      if ( prfl=1 ) then
       begin
         console.log('intdy -- t = ',t,' illegal');
         console.log('         t not in interval tcur - hu to tcur');
       end;
      iflag := -2;
      exit;
    end;

   s := ( t - tn ) / h;
   ic := 1;
   for jj := l - k to nq do
      ic := ic * jj;
   c := ic;
   for i := 1 to n do
      dky[i] := c * yh[l][i];
   for j := nq - 1 downto k do
    begin
      jp1 := j + 1;
      ic := 1;
      for jj := jp1 - k to j do
         ic := ic * jj;
      c := ic;
      for i := 1 to n do
         dky[i] := c * yh[jp1][i] + s * dky[i];
    end;
   if ( k = 0 ) then exit;
   r := pow( h,  -k );
   for i := 1 to n do
      dky[i] := dky[i] * r;

end;      {   end intdy   }


{**************************************************************}
procedure TLsoda.cfode(meth : integer);
var
   i, nq, nqm1, nqp1: integer;
   agamq, fnq, fnqm1, pint, ragq,rqfac, rq1fac, tsign, xpin : double;
   pc : vector13;

{  cfode is called by the integrator routine to set coefficients
   needed there.  The coefficients for the current method, as
   given by the value of meth, are set for all orders and saved.
   The maximum order assumed here is 12 if meth = 1 and 5 if meth = 2.
   ( A smaller value of the maximum order is also allowed. )
   cfode is called once at the beginning of the problem, and
   is not called again unless and until meth is changed.

   The elco array contains the basic method coefficients.
   The coefficients el[i], 1 < i < nq+1, for the method of
   order nq are stored in elco[nq][i].  They are given by a generating
   polynomial, i.e.,

      l(x) = el[1] + el[2]*x + ... + el[nq+1]*xnq.

   For the implicit Adams method, l(x) is given by

      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),   l(-1) = 0.

   For the bdf methods, l(x) is given by

      l(x) = (x+1)*(x+2)*...*(x+nq)/k,

   where   k = factorial(nq)*(1+1/2+...+1/nq).

   The tesco array contains test constants used for the
   local error test and the selection of step size and/or order.
   At order nq, tesco[nq][k] is used for the selection of step
   size at order nq-1 if k = 1, at order nq if k = 2, and at order
   nq+1 if k = 3. }

begin
  if (meth = 1) then
     begin
     elco[1,1]   := 1.;
     elco[1,2]   := 1.;
     tesco[1,1]  := 0.;
     tesco[1,2]  := 2.;
     tesco[2,1]  := 1.;
     tesco[12,3] := 0.;
     pc[1] := 1.;
     rqfac := 1.;
     for nq := 2 to 12 do
         begin
         { The pc array will contain the coefficients of the polynomial

         p(x) = (x+1)*(x+2)*...*(x+nq-1).

         Initially, p(x) = 1. }

         rq1fac := rqfac;
         rqfac := rqfac / nq;
         nqm1 := nq - 1;
         fnqm1 := nqm1;
         nqp1 := nq + 1;

         { Form coefficients of p(x)*(x+nq-1). }

         pc[nq] := 0.;
         for i := nq downto 2 do
             pc[i] := pc[i-1] + fnqm1 * pc[i];
         pc[1] := fnqm1 * pc[1];

         { Compute integral, -1 to 0, of p(x) and x*p(x). }

         pint := pc[1];
         xpin := pc[1] / 2.;
         tsign := 1.;
         for i := 2 to nq do
             begin
             tsign := -tsign;
             pint := pint + tsign * pc[i] / i;
             xpin := xpin + tsign * pc[i] / ( i + 1 );
             end;

         { Store coefficients in elco and tesco. }

         elco[nq,1] := pint * rq1fac;
         elco[nq,2] := 1.;
         for i := 2 to nq do
             elco[nq,i+1] := rq1fac * pc[i] / i;
         agamq := rqfac * xpin;
         ragq := 1. / agamq;
         tesco[nq,2] := ragq;
         if (nq < 12) then
             tesco[nqp1,1] := ragq * rqfac / nqp1;
         tesco[nqm1,3] := ragq;
         end;      { end for }
      exit;
  end; { end if meth == 1 }

  { meth = 2. }

  pc[1] := 1.;
  rq1fac := 1.;

  { The pc array will contain the coefficients of the polynomial

  p(x) = (x+1)*(x+2)*...*(x+nq).

  Initially, p(x) = 1. }

  for nq := 1 to 5 do
      begin
      fnq := nq;
      nqp1 := nq + 1;

      { Form coefficients of p(x)*(x+nq). }

      pc[nqp1] := 0.;
      for i := nq + 1 downto 2 do
         pc[i] := pc[i-1] + fnq * pc[i];
      pc[1] := pc[1]*fnq;

      { Store coefficients in elco and tesco. }

      for i := 1 to nqp1 do
          elco[nq,i] := pc[i] / pc[2];
      elco[nq,2] := 1.;
      tesco[nq,1] := rq1fac;
      tesco[nq,2] :=  nqp1 / elco[nq,1];
      tesco[nq,3] := ( nq + 2 ) / elco[nq,1];
      rq1fac := rq1fac/fnq;
      end;
end;       {   end cfode   }

{**************************************************************}
procedure TLsoda.scaleh( var rh, pdh: double );
var
   r: double;
   j, i: integer;

{  If h is being changed, the h ratio rh is checked against
   rmax, hmin, and hmxi, and the yh array is rescaled.  ialth is set to
   l = nq + 1 to prevent a change of h for that many steps, unless
   forced by a convergence or error test failure. }

begin
   rh := min( rh, rmax );
   rh := rh / max( 1., abs( h ) * hmxi * rh );

{  If meth = 1, also restrict the new step size by the stability region.        
   If this reduces h, set irflag to 1 so that if there are roundoff
   problems later, we can assume that is the cause of the trouble.}

   if ( meth = 1 ) then
    begin
      irflag := 0;
      pdh := max( abs( h ) * pdlast, 0.000001 );
      if ( ( rh * pdh * 1.00001 ) >= sm1[nq] ) then
       begin
         rh := sm1[nq] / pdh;
         irflag := 1;
       end;
    end;
   r := 1.;
   for j := 2 to l do
    begin
      r := r * rh;
      for i := 1 to n do
         yh[j][i] := yh[j][i] * r;
    end;
   h := h * rh;
   rc := rc * rh;
   ialth := l;
end;     {   end scaleh   }


{***************************************************************}
function TLsoda.arrayToTVector(a_arr: array of double): TVector;
var i:integer;
    a_vect: TVector;
begin
 a_vect:= TVector.create(length(a_arr));
 for i := 1 to length(a_arr) do
   begin
    a_vect[i]:= a_arr[i-1];
   end;
 Result:= a_vect;
end;

function TLsoda.tVectorToArray(a_tvect: TVector): array of double;
var i:integer;
    a_arr: array of double;
begin
  // convert from tvectors to arrays:
  SetLength(a_arr,a_tvect.Size);
    for i := 1 to a_tvect.Size do
      begin
        a_arr[i-1]:= a_tvect[i];
      end;
  Result:= a_arr;
end;

{**************************************************************}
procedure TLsoda.stoda(var neq : integer; var y : TVector);
var
   corflag, orderflag,i, i1, j, jb, m, ncf: integer;
   del, delp, dsm, dup, exup, r, rh, rhup, told, pdh, pnorm: double;
   y_arr: array of double;
{  stoda performs one step of the integration of an initial value
   problem for a system of ordinary differential equations.
   Note.. stoda is independent of the value of the iteration method
   indicator miter, when this is != 0, and hence is independent
   of the type of chord method used, or the Jacobian structure.
   Communication with stoda is done with the following variables:

   jstart = an integer used for input only, with the following
            values and meanings:

               0  perform the first step,
             > 0  take a new step continuing from the last,
              -1  take the next step with a new value of h,
                  n, meth, miter, and/or matrix parameters.
              -2  take the next step with a new value of h,
                  but with other inputs unchanged.

   kflag = a completion code with the following meanings:

             0  the step was successful,
            -1  the requested error could not be achieved,
            -2  corrector convergence could not be achieved,
            -3  fatal error in prja or solsy.

   miter = corrector iteration method:

             0  functional iteration,
            >0  a chord method corresponding to jacobian type jt. }

begin
   kflag := 0;
   told := tn;
   ncf := 0;
   ierpj := 0;
   iersl := 0;
   jcur := 0;
   delp := 0.;

{  On the first call, the order is set to 1, and other variables are
   initialized.  rmax is the maximum ratio by which h can be increased
   in a single step.  It is initially 1.e4 to compensate for the small
   initial h, but then is normally equal to 10.  If a filure occurs
   (in corrector convergence or error test), rmax is set at 2 for
   the next increase.
   cfode is called to get the needed coefficients for both methods. }

   if ( jstart = 0 ) then
    begin
      lmax := maxord + 1;
      nq := 1;
      l := 2;
      ialth := 2;
      rmax := 10000.;
      rc := 0.;
      el0 := 1.;
      crate := 0.7;
      hold := h;
      nslp := 0;
      ipup := miter;

{ Initialize switching parameters.  meth = 1 is assumed initially. }

      icount := 20;
      irflag := 0;
      pdest := 0.;
      pdlast := 0.;
      ratio := 5.;
      cfode( 2 );
      for i := 1 to 5 do
         cm2[i] := tesco[i,2] * elco[i,i+1];
      cfode( 1 );
      for  i := 1 to 12 do
         cm1[i] := tesco[i,2] * elco[i,i+1];
      resetcoeff;
    end;     {   end if ( jstart == 0 )   }

{  The following block handles preliminaries needed when jstart = -1.
   ipup is set to miter to force a matrix update.
   If an order increase is about to be considered ( ialth = 1 ),
   ialth is reset to 2 to postpone consideration one more step.
   If the caller has changed meth, cfode is called to reset
   the coefficients of the method.
   If h is to be changed, yh must be rescaled.
   If h or meth is being changed, ialth is reset to l = nq + 1
   to prevent further changes in h for that many steps. }

   if ( jstart = -1 ) then
    begin
      ipup := miter;
      lmax := maxord + 1;
      if ( ialth = 1 ) then
         ialth := 2;
      if ( meth <> mused ) then
       begin
         cfode( meth );
         ialth := l;
         resetcoeff;
       end;
      if ( h <> hold ) then
       begin
         rh := h / hold;
         h := hold;
         scaleh( rh, pdh );
       end;
    end;      {   if ( jstart == -1 )   }

   if ( jstart = -2 ) then
    begin
      if ( h <> hold ) then
       begin
         rh := h / hold;
         h := hold;
         scaleh( rh, pdh );
       end;
    end;     {   if ( jstart == -2 )   }

{  Prediction.
   This section computes the predicted values by effectively
   multiplying the yh array by the pascal triangle matrix.
   rc is the ratio of new to old values of the coefficient h * el[1].
   When rc differs from 1 by more than ccmax, ipup is set to miter
   to force pjac to be called, if a jacobian is involved.
   In any case, prja is called at least every msbp steps. }
   while (1=1 ) do
    begin
      repeat
         if ( abs( rc - 1. ) > ccmax ) then
            ipup := miter;
         if ( nst >= nslp + msbp ) then
            ipup := miter;
         tn := tn+h;
         for j := nq downto 1 do
            for i1 := j to nq do
             begin
               for i := 1 to n do
                  yh[i1][i] := yh[i1][i]+yh[i1+1][i];
             end;
         pnorm := vmnorm(yh[1], ewt );
         correction( neq, y, corflag, pnorm, del, delp, told, ncf,rh, m );
         if ( corflag = 1 ) then
          begin
            rh := max( rh, hmin / abs( h ) );
            scaleh( rh, pdh );
          end;
         if ( corflag = 2 ) then
          begin
         // console.log('line ~ 1392, corflag set to 2 and kflag set to -2');
            kflag := -2;
            hold := h;
            jstart := 1;
            exit;
          end;
       until (corflag=0);      {   end inner while ( corrector loop )   }

{  The corrector has converged.  jcur is set to 0
   to signal that the Jacobian involved may need updating later.
   The local error test is done now. }

      jcur := 0;
      if ( m = 0 ) then
         dsm := del / tesco[nq,2];
      if ( m > 0 ) then
         dsm := vmnorm(acor, ewt ) / tesco[nq,2];
      if ( dsm <= 1. ) then
       begin

{  After a successful step, update the yh array.
   Decrease icount by 1, and if it is -1, consider switching methods.
   If a method switch is made, reset various parameters,
   rescale the yh array, and exit.  If there is no switch,
   consider changing h if ialth = 1.  Otherwise decrease ialth by 1.
   If ialth is then 1 and nq < maxord, then acor is saved for
   use in a possible order increase on the next step.
   If a change in h is considered, an increase or decrease in order
   by one is considered also.  A change in h is made only if it is by
   a factor of at least 1.1.  If not, ialth is set to 3 to prevent
   testing for that many steps. }

         kflag := 0;
         nst:=nst+1;
         hu := h;
         nqu := nq;
         mused := meth;
         for j := 1 to l do
             begin
             r := el[j];
             for i := 1 to n do
                 yh[j][i] := yh[j][i] + r * acor[i];
             end;
         icount:=icount-1;
         if ( icount < 0 ) then
            begin
            methodswitch( dsm, pnorm, pdh, rh );
            if ( meth <> mused ) then
               begin
               rh := max( rh, hmin / abs( h ) );
               scaleh( rh, pdh );
               rmax := 10.;
               endstoda;
               exit;
               end;
            end;

{  No method switch is being made.  Do the usual step/order selection. }

         ialth:=ialth-1;
         if ( ialth = 0 ) then
            begin
            rhup := 0.;
            if ( l <> lmax ) then
               begin
               for i := 1 to n do
                   savf[i] := acor[i] - yh[lmax][i];
               dup := vmnorm(savf, ewt ) / tesco[nq,3];
               exup := 1. / ( l + 1 );
               rhup := 1. / ( 1.4 * pow( dup, exup ) + 0.0000014 );
               end;
            orderswitch( rhup, dsm, pdh, rh, orderflag );

{  No change in h or nq. }

            if ( orderflag = 0 ) then
               begin
               endstoda;
               exit;
               end;

{  h is changed, but not nq. }

            if ( orderflag = 1 ) then
               begin
               rh := max( rh, hmin / abs( h ) );
               scaleh( rh, pdh );
               rmax := 10.;
               endstoda;
               exit;
               end;

{  both nq and h are changed. }

            if ( orderflag = 2 ) then
               begin
               resetcoeff;
               rh := max( rh, hmin / abs( h ) );
               scaleh( rh, pdh );
               rmax := 10.;
               endstoda;
               exit;
               end;
            end;            {   end if ( ialth == 0 )   }

         if ( ialth > 1) OR (l = lmax ) then
            begin
            endstoda;
            exit;
            end;
         for i := 1 to n do
            yh[lmax][i] := acor[i];
         endstoda;
         exit;
       end       {   end if ( dsm <= 1. )   }

{  The error test failed.  kflag keeps track of multiple failures.
   Restore tn and the yh array to their previous values, and prepare
   to try the step again.  Compute the optimum step size for this or
   one lower.  After 2 or more failures, h is forced to decrease
   by a factor of 0.2 or less.    }

      else
       begin
         kflag:=kflag-1;
         tn := told;
         for j := nq downto 1 do
             for i1 := j to nq do
             begin
             for i := 1 to n do
                 yh[i1][i] := yh[i1][i]-yh[i1+1][i];
             end;
         rmax := 2.;
         if ( abs( h ) <= hmin * 1.00001 ) then
            begin
            kflag := -1;
            hold := h;
            jstart := 1;
            exit;
            end;
         if ( kflag > -3 ) then
            begin
            rhup := 0.;
            orderswitch( rhup, dsm, pdh, rh, orderflag );
            if ( orderflag = 1) OR (orderflag = 0 ) then
               begin
               if ( orderflag = 0 ) then
                  rh := min( rh, 0.2 );
               rh := max( rh, hmin / abs( h ) );
               scaleh( rh, pdh );
               end;
            if ( orderflag = 2 ) then
               begin
               resetcoeff;
               rh := max( rh, hmin / abs( h ) );
               scaleh( rh, pdh );
               end;
            end     {   if ( kflag > -3 )   }

{  Control reaches this section if 3 or more failures have occurred.
   If 10 failures have occurred, exit with kflag = -1.
   It is assumed that the derivatives that have accumulated in the
   yh array have errors of the wrong order.  Hence the first
   derivative is recomputed, and the order is set to 1.  Then
   h is reduced by a factor of 10, and the step is retried,
   until it succeeds or h reaches hmin. }

         else
          begin
            if ( kflag = -10 ) then
               begin
               kflag := -1;
               hold := h;
               jstart := 1;
               exit;
               end
            else
               begin
               rh := 0.1;
               rh := max( hmin / abs( h ) , rh );
               h := h * rh;
               for i := 1 to n do y[i] := yh[1][i];
               //savf:= FDerivatives (tn, y);
               y_arr:= tVectorToArray(y);
               savf:= FDerivatives(tn,y_arr,self.p);
               nfe:=nfe+1;
               for i := 1 to n do
                  yh[2][i] := h * savf[i];
               ipup := miter;
               ialth := 5;
               if ( nq <> 1 ) then
                  begin
                  nq := 1;
                  l := 2;
                  resetcoeff;
                  end;
               end;
          end;     {   end else -- kflag <= -3 }
       end;     {   end error failure handling   }
    end;      {   end outer while   }
end;           {   end stoda   }

{**************************************************************}
procedure TLsoda.Execute (var y : TVector; var t, tout : double);

{  If the user does not supply any of these values, the calling program
   should initialize those untouched working variables to zero.

   ml = iwork1
   mu = iwork2
   ixpr = iwork5
   mxstep = iwork6
   mxhnil = iwork7
   mxordn = iwork8
   mxords = iwork9

   tcrit = rwork1
   h0 = rwork5
   hmax = rwork6
   hmin = rwork7 }

var
    mxstp0, mxhnl0,i, i1, i2, iflag, kgo, lf0, lenyh, ihit: integer;
    atoli, ayi, big, ewti, h0, hmax, hmx, rh, rtoli,
    tcrit, tdist, tnext, tol, tp, size, sum, w0: double;
    dy_tv: TVector;
    y_arr: array of double;

begin
  mxstp0 := 500;
  mxhnl0 := 10;
{  Block a.
   This code block is executed on every call.
   It tests *istate and itask for legality and branches appropriately.
   If *istate > 1 but the flag init shows that initialization has not
   yet been done, an error return occurs.
   If *istate = 1 and tout = t, return immediately. }

   if (istate=1) then
    begin
     illin:=0; init:=0; ntrep:=0; ixpr:=0;
    end;
   if ( istate < 1) OR (istate > 3 ) then
    begin
      if ( prfl=1 ) then
   //      console.log('line ~ 1632, lsoda -- illegal istate =',istate );
      terminate( Fistate );
      exit;
    end;
   if ( itask < 1) OR (itask > 5 ) then
    begin
      if ( prfl=1 ) then
         writeln('lsoda -- illegal itask =',itask );
      terminate( Fistate );
      exit;
    end;
   if ( init = 0) AND (( istate = 2) OR (istate = 3 ) ) then
    begin
      if ( prfl=1 ) then
         console.log('lsoda -- istate > 1 but lsoda not initialized');
      terminate( Fistate );
      exit;
    end;
   if (istate = 1 ) then
    begin
      init := 0;
      if ( tout = t ) then
       begin
        ntrep:=ntrep+1;
        if ( ntrep < 5 ) then exit;
        if ( prfl=1 ) then
         begin
          console.log('lsoda -- repeated calls with istate = 1 and tout = t');
          console.log('         run aborted.. apparent infinite loop');
         end;
        exit;
       end;
    end;
{  Block b.
   The next code block is executed for the initial call ( *istate = 1 ),
   or for a continuation call with parameter changes ( *istate = 3 ).
   It contains checking of all inputs and various initializations.
   First check legality of the non-optional inputs neq, itol, iopt,
   jt, ml, and mu. }

   if ( istate = 1) OR (istate = 3 ) then
    begin
      ntrep := 0;
      if ( neq <= 0 ) then
       begin
         if ( prfl=1 ) then
            console.log('lsoda -- neq =',neq,' is less than 1');
         terminate( Fistate );
         exit;
       end;
      if ( istate = 3) AND (neq > n ) then
       begin
         if ( prfl=1 ) then
            console.log('lsoda -- istate = 3 and neq increased');
         terminate( Fistate );
         exit;
       end;
      n := neq;
      if ( itol < 1) OR (itol > 4 ) then
       begin
         if ( prfl=1 ) then
            writeln('lsoda -- itol = ',itol,' illegal');
         terminate( Fistate );
         exit;
       end;
      if ( iopt < 0) OR (iopt > 1 ) then
       begin
         if ( prfl=1 ) then
            console.log('lsoda -- iopt = ',iopt,' illegal');
         terminate( Fistate );
         exit;
       end;
      if ( jt = 3) OR (jt < 1) OR (jt > 5 ) then
       begin
         if ( prfl=1 ) then
            console.log('lsoda -- jt = ',jt,' illegal');
         terminate( Fistate );
         exit;
       end;
      jtyp := jt;

{ Next process and check the optional inputs.   }
{ Default options.   }

      if ( iopt = 0 ) then
       begin
         ixpr := 0;
         mxstep := mxstp0;
         mxhnil := mxhnl0;
         hmxi := 0.;
         hmin := 0.;
         if ( istate = 1 ) then
          begin
            h0 := 0.;
            mxordn := mord[1];
            mxords := mord[2];
          end;
       end        { end if ( iopt == 0 ) }

{ Optional inputs. }

      else              { if ( iopt = 1 )  }
       begin
         ixpr := iwork5;
         if ( ixpr < 0) OR (ixpr > 2 ) then
          begin
            if ( prfl=1 ) then
               console.log('lsoda -- ixpr = ',ixpr,' is illegal');
            terminate( Fistate );
            exit;
          end;
         mxstep := iwork6;
         if ( mxstep < 0 ) then
          begin
              if ( prfl=1 ) then
               console.log('lsoda -- mxstep < 0');
            terminate( Fistate );
            exit;
          end;
         if ( mxstep = 0 ) then
            mxstep := mxstp0;
         mxhnil := iwork7;
         if ( mxhnil < 0 ) then
          begin
            if ( prfl=1 ) then
               writeln('lsoda -- mxhnil < 0');
            terminate( Fistate );
            exit;
          end;
         if ( istate = 1 ) then
          begin
            h0 := rwork5;
            mxordn := iwork8;
            if ( mxordn < 0 ) then
             begin
               if ( prfl=1 ) then
                  console.log('lsoda -- mxordn = ',mxordn,' is less than 0');
               terminate( Fistate );
               exit;
             end;
            if ( mxordn = 0 ) then
               mxordn := 100;
            mxordn := mini( mxordn, mord[1] );
            mxords := iwork9;
            if ( mxords < 0 ) then
             begin
               if ( prfl=1 ) then
                   console.log('lsoda -- mxords = ',mxords,' is less than 0');
               terminate( Fistate );
               exit;
             end;
            if ( mxords = 0 ) then
               mxords := 100;
            mxords := mini( mxords, mord[2] );
            if ( ( tout - t ) * h0 < 0. ) then
             begin
               if ( prfl=1 ) then
                begin
                   console.log('lsoda -- tout = ',tout,' behind t = ',t);
                   console.log('         integration direction is given by ',h0);
                end;
               terminate( Fistate );
               exit;
             end;
          end;         {  end if ( *istate == 1 )  }
         hmax := rwork6;
         if ( hmax < 0. ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- hmax < 0.');
            terminate( Fistate );
            exit;
          end;
         hmxi := 0.;
         if ( hmax > 0 ) then
            hmxi := 1. / hmax;
         hmin := rwork7;
         if ( hmin < 0. ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- hmin < 0.');
            terminate( Fistate );
            exit;
          end;
       end;      { end else }      { end iopt = 1  }
    end;    { end if ( *istate == 1 || *istate == 3 ) }

{  If *istate = 1, meth is initialized to 1. }

   if ( istate = 1 ) then
    begin

{  If memory were not freed, *istate = 3 need not reallocate memory.
   Hence this section is not executed by *istate = 3. }

      sqrteta := Sqrt( ETA );
      meth := 1;
      nyh := n;
      lenyh := 1 + maxi( mxordn, mxords );
      { ############# - Memory allocation occurs in constructor }
    end;

{  Check rtol and atol for legality. }

   if ( istate = 1) OR (istate = 3 ) then
    begin
      rtoli := rtol[1];
      atoli := atol[1];
      for  i := 1 to n do
       begin
         if ( itol >= 3 ) then
            rtoli := rtol[i];
         if ( itol = 2) OR (itol = 4 ) then
            atoli := atol[i];
         if ( rtoli < 0. ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- rtol = ',rtoli,' is less than 0.');
            terminate( Fistate );
            exit;
          end;
         if ( atoli < 0. ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- atol = ',atoli,' is less than 0.');
            terminate( Fistate );
            exit;
          end;
       end;     {   end for   }
    end;   {   end if ( *istate == 1 || *istate == 3 )   }

{  If *istate = 3, set flag to signal parameter changes to stoda. }

   if ( istate = 3 ) then
      jstart := -1;

{  Block c.
   The next block is for the initial call only ( *istate = 1 ).
   It contains all remaining initializations, the initial call to f,
   and the calculation of the initial step size.
   The error weights in ewt are inverted after being loaded. }

   if ( istate = 1 ) then
    begin
      tn := t;
      tsw := t;
      maxord := mxordn;
      if ( itask = 4) OR (itask = 5 ) then
       begin
         tcrit := rwork1;
         if ( ( tcrit - tout ) * ( tout - t )  < 0. ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- itask = 4 or 5 and tcrit behind tout');
            terminate( Fistate );
            exit;
          end;
         if ( h0 <> 0) AND (( t + h0 - tcrit ) * h0 > 0. ) then
            h0 := tcrit - t;
       end;
      jstart := 0;
      nhnil := 0;
      nst := 0;
      nje := 0;
      nslast := 0;
      hu := 0.;
      nqu := 0;
      mused := 0;
      miter := 0;
      ccmax := 0.3;
      maxcor := 3;
      msbp := 20;
      mxncf := 10;

      { Initial call to fonction  }
      //yh[2]:= FDerivatives (t, y);
      y_arr:= tVectorToArray(y);
      dy_tv:= TVector.create(yh[2].size);
      dy_tv[1]:=0.4;
      yh[2]:= FDerivatives(t,y_arr,self.p);

      nfe := 1;

      { Load the initial value vector in yh.}

      for  i := 1 to  n do
         yh[1][i] := y[i];

      { Load and invert the ewt array.  ( h is temporarily set to 1. ) }

      nq := 1;
      h := 1.;
      ewset( Fitol, Frtol, Fatol, y );
      for i := 1 to n do
       begin
         if ( ewt[i] <= 0. ) then
          begin
          console.log('lsoda exiting: -- ewt[',i,'] = ',ewt[i],' <= 0.');
            if ( prfl=1 ) then
                console.log('lsoda -- ewt[',i,'] = ',ewt[i],' <= 0.');
            terminate2( y, t);
            exit;
          end;
         ewt[i] := 1. / ewt[i];
       end;

{  The coding below computes the step size, h0, to be attempted on the
   first step, unless the user has supplied a value for this.
   First check that tout - *t differs significantly from zero.                  
   A scalar tolerance quantity tol is computed, as max(rtol[i])                 
   if this is positive, or max(atol[i]/fabs(y[i])) otherwise, adjusted          
   so as to be between 100*ETA and 0.001.                                       
   Then the computed value h0 is given by
                                                                                
      h0(-2) = 1. / ( tol * w02 ) + tol * ( norm(f) )2

   where   w0     = max( fabs(*t), fabs(tout) ),
           f      = the initial value of the vector f(t,y), and
           norm() = the weighted vector norm used throughout, given by
                    the vmnorm function routine, and weighted by the
                    tolerances initially loaded into the ewt array.

   The sign of h0 is inferred from the initial values of tout and *t.
   fabs(h0) is made < fabs(tout-*t) in any case. }

      if ( h0 = 0. ) then
       begin
         tdist := abs( tout - t );
         w0 := max( abs( t ), abs( tout ) );
         if ( tdist < 2. * ETA * w0 ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- tout too close to t to start integration');
            terminate( Fistate );
            exit;
          end;
         tol := rtol[1];
         if ( itol > 2 ) then
          begin
            for i := 2 to n do
               tol := max( tol, rtol[i] );
          end;
         if ( tol <= 0. ) then
          begin
            atoli := atol[1];
            for i := 1 to n do
             begin
               if ( itol = 2) OR (itol = 4 ) then
                  atoli := atol[i];
               ayi := abs( y[i] );
               if ( ayi <> 0. ) then
                  tol := max( tol, atoli / ayi );
             end;
          end;
         tol := max( tol, 100. * ETA );
         tol := min( tol, 0.001 );
         sum := vmnorm(yh[2], ewt );
         sum := 1. / ( tol * w0 * w0 ) + tol * sum * sum;
         h0 := 1. / sqrt( sum );
         h0 := min( h0, tdist );
         if(tout-t < 0) then h0:=-h0;
       end;                 {   end if ( h0 == 0. )   }

{  Adjust h0 if necessary to meet hmax bound. }

      rh := abs( h0 ) * hmxi;
      if ( rh > 1. ) then
         h0 := h0/rh;

{  Load h with h0 and scale yh[2] by h0. }

      h := h0;
      for  i := 1 to n do
         yh[2][i] := yh[2][i]*h0;
    end;         { if ( *istate == 1 )   }

{  Block d.
   The next code block is for continuation calls only ( *istate = 2 or 3 )
   and is to check stop conditions before taking a step. }

   if ( istate = 2) OR (istate = 3 ) then
    begin
      nslast := nst;
      case itask of
       1 :
         begin
          if ( ( tn - tout ) * h >= 0. ) then
           begin
            intdy( tout, 0, y, iflag );
            if ( iflag <> 0 ) then
             begin
               if ( prfl=1 ) then
                   console.log('lsoda -- trouble from intdy, itask = ',itask,', tout = ',tout);
               terminate( Fistate );
               exit;
             end;
            t := tout;
            istate := 2;
            illin := 0;
            exit;
          end;
         end;
       2 :
         begin end;
       3 :
         begin
          tp := tn - hu * ( 1. + 100. * ETA );
          if ( ( tp - tout ) * h > 0. ) then
           begin
            if ( prfl=1 ) then
                console.log('lsoda -- itask = ',itask,' and tout behind tcur - hu');
            terminate( Fistate );
            exit;
           end;
          if ( ( tn - tout ) * h >= 0. ) then
            begin
             successreturn( y, t, itask, ihit, tcrit, Fistate );
             exit;
            end;
         end;
       4 :
         begin
          tcrit := rwork1;
          if ( ( tn - tcrit ) * h > 0. ) then
           begin
            if ( prfl=1 ) then
                console.log('lsoda -- itask = 4 or 5 and tcrit behind tcur');
            terminate( Fistate );
            exit;
           end;
          if ( ( tcrit - tout ) * h < 0. ) then
           begin
            if ( prfl=1 ) then
                console.log('lsoda -- itask = 4 or 5 and tcrit behind tout');
            terminate( Fistate );
            exit;
           end;
          if ( ( tn - tout ) * h >= 0. ) then
           begin
            intdy( tout, 0, y, iflag );
            if ( iflag <> 0 ) then
             begin
               if ( prfl=1 ) then
                  console.log('lsoda -- trouble from intdy, itask = ',itask,', tout = ',tout);
               terminate( Fistate );
               exit;
             end;
            t := tout;
            istate := 2;
            illin := 0;
            exit;
           end;
         end;
       5 :
         begin
          if ( itask = 5 ) then
           begin
            tcrit := rwork1;
            if ( ( tn - tcrit ) * h > 0. ) then
             begin
               if ( prfl=1 ) then
                   console.log('lsoda -- itask = 4 or 5 and tcrit behind tcur');
               terminate( Fistate );
               exit;
             end;
           end;
          hmx := abs( tn ) + abs( h );
          if (abs( tn - tcrit ) <= ( 100. * ETA * hmx )) then ihit:=1
           else ihit:=0;
          if ( ihit=1 ) then
           begin
            t := tcrit;
            successreturn( y, t, itask, ihit, tcrit, Fistate );
            exit;
           end;
          tnext := tn + h * ( 1. + 4. * ETA );
          if ( ( tnext - tcrit ) * h > 0. ) then
           begin
            h := ( tcrit - tn ) * ( 1. - 4. * ETA );
            if ( istate = 2 ) then  jstart := -2;
           end;
         end;
       end;      {   end switch   }
    end;      {   end if ( *istate == 2 || *istate == 3 )   }

{  Block e.
   The next block is normally executed for all calls and contains
   the call to the one-step core integrator stoda.

   This is a looping point for the integration steps.

   First check for too many steps being taken, update ewt ( if not at
   start of problem).  Check for too much accuracy being requested, and
   check for h below the roundoff level in *t. }

   while (1=1 ) do
    begin
      if ( istate <> 1) OR (nst <> 0 ) then
       begin
         if ( ( nst - nslast ) >= mxstep ) then
          begin
            if ( prfl=1 ) then
                console.log('lsoda -- ',mxstep,' steps taken before reaching tout');
            istate := -1;
            terminate2( y, t );
            exit;
          end;
         ewset( Fitol, Frtol, Fatol, yh[1] );
         for  i := 1 to  n do
          begin
            if ( ewt[i] <= 0. ) then
             begin
               if ( prfl=1 ) then
                   console.log('lsoda -- ewt[',i,'] = ',ewt[i],' <= 0.');
               istate := -6;
               terminate2( y, t );
               exit;
             end;
            ewt[i] := 1. / ewt[i];
          end;
       end;
      tolsf := ETA * vmnorm(yh[1], ewt );
      if ( tolsf > 0.01 ) then
       begin
         tolsf := tolsf * 200.;
         if ( nst = 0 ) then
          begin
            if ( prfl=1 ) then
             begin
                console.log('lsoda -- at start of problem, too much accuracy');
                console.log('         requested for precision of machine,');
                console.log('         suggested scaling factor = ',tolsf);
             end;
            terminate( Fistate );
            exit;
          end;
         if ( prfl=1 ) then
          begin
             console.log('lsoda -- at t = ',t,', too much accuracy requested');
             console.log('         for precision of machine, suggested');
             console.log('         scaling factor = ',tolsf );
          end;
         istate := -2;
         terminate2( y, t );
         exit;
       end;
      if ( ( tn + h ) = tn ) then
       begin
         nhnil:=nhnil+1;
         if ( nhnil <= mxhnil ) then
          begin
            if ( prfl=1 ) then
             begin
                console.log('lsoda -- warning..internal t = ',tn,' and h = ',h,' are');
                console.log('         such that in the machine, t + h = t on the next step');
                console.log('         solver will continue anyway.');
             end;
            if  ( nhnil = mxhnil ) AND (prfl=1 ) then
             begin
                console.log('lsoda -- above warning has been issued ',nhnil,' times,');
                console.log('         it will not be issued again for this problem');
             end;
          end;
       end;

{  Call stoda }

      stoda( neq, y);
{ Print extra information  }

      if  ( ixpr = 2 ) AND (prfl=1 ) then
       begin
         console.log('meth= ',meth,',   order= ',nq,',   nfe= ',nfe,',   nje= ',nje);
         console.log('t= ',tn,',   h= ',h,',   nst= ',nst);
       end;

      if ( kflag = 0 ) then
       begin

{  Block f.
   The following block handles the case of a successful return from the
   core integrator ( kflag = 0 ).
   If a method switch was just made, record tsw, reset maxord,
   set jstart to -1 to signal stoda to complete the switch,
   and do extra printing of data if ixpr != 0.
   Then, in any case, check for stop conditions. }

         init := 1;
         if ( meth <> mused ) then
          begin
            tsw := tn;
            maxord := mxordn;
            if ( meth = 2 ) then
               maxord := mxords;
            jstart := -1;
            if ( ixpr=1) AND (prfl=1 ) then
             begin
               if ( meth = 2 ) then
                   console.log('lsoda -- a switch to the stiff method has occurred');
               if ( meth = 1 ) then
                   console.log('lsoda -- a switch to the nonstiff method has occurred');
               writeln('         at t = ',tn,', tentative step size h = ',h,', step nst = ',nst);
             end;
          end;         {   end if ( meth != mused )   }

{  itask = 1.
   If tout has been reached, interpolate. }

         if ( itask = 1 ) then
          begin
            if ( ( tn - tout ) * h >= 0. ) then
             begin
              intdy( tout, 0, y, iflag );
              t := tout;
              istate := 2;
              illin := 0;
              exit;
             end;
          end;

{  itask = 2. }

         if ( itask = 2 ) then
          begin
            successreturn( y, t, itask, ihit, tcrit, Fistate );
            exit;
          end;

{  itask = 3.
   Jump to exit if tout was reached. }

         if ( itask = 3 ) then
          begin
            if ( ( tn - tout ) * h >= 0. ) then
             begin
               successreturn( y, t, itask, ihit, tcrit, Fistate );
               exit;
             end;
          end;

{  itask = 4.
   See if tout or tcrit was reached.  Adjust h if necessary. }

         if ( itask = 4 ) then
          begin
            if ( ( tn - tout ) * h >= 0. ) then
             begin
               intdy( tout, 0, y, iflag );
               t := tout;
               istate := 2;
               illin := 0;
               exit;
             end
            else
             begin
               hmx := abs( tn ) + abs( h );
               if(abs( tn - tcrit ) <= ( 100. * ETA * hmx )) then ihit:=1
                else ihit:=0;
               if ( ihit=1 ) then
                begin
                  successreturn( y, t, itask, ihit, tcrit, Fistate );
                  exit;
                end;
               tnext := tn + h * ( 1. + 4. * ETA );
               if ( ( tnext - tcrit ) * h < 0. ) then
                begin
                 h := ( tcrit - tn ) * ( 1. - 4. * ETA );
                 jstart := -2;
                end;
             end;
          end;      {   end if ( itask == 4 )   }

{  itask = 5.
   See if tcrit was reached and jump to exit. }

         if ( itask = 5 ) then
          begin
            hmx := abs( tn ) + abs( h );
            if (abs( tn - tcrit ) <= ( 100. * ETA * hmx )) then ihit:=1
             else ihit:=0;
            successreturn( y, t, itask, ihit, tcrit, Fistate );
            exit;
          end;
       end;   {   end if ( kflag == 0 )   }

{  kflag = -1, error test failed repeatedly or with fabs(h) = hmin.
   kflag = -2, convergence failed repeatedly or with fabs(h) = hmin. }
    //  console.log('--> kflag: ', kflag);
      if ( kflag = -1) OR (kflag = -2 ) then
       begin
         if ( prfl=1 ) then
            writeln('lsoda -- at t = ',tn,' and step size h = ',h,', the');
         if ( kflag = -1 ) then
          begin
            if ( prfl=1 ) then
             begin
               writeln('         error test failed repeatedly or');
               writeln('         with abs(h) = hmin');
             end;
            istate := -4;
          end;
         if ( kflag = -2 ) then
          begin
            if ( prfl=1 ) then
             begin
                console.log('         corrector convergence failed repeatedly or');
                console.log('         with abs(h) = hmin');
             end;
          //  console.log('line ~ 2334: kflag set to -2, istate set to -5');
            istate := -5;
          end;
         big := 0.;
         imxer := 1;
         for  i := 1 to n do
          begin
            size := abs( acor[i] ) * ewt[i];
            if ( big < size ) then
             begin
               big := size;
               imxer := i;
             end;
          end;

          console.log('line ~ 2348, y[1]: ', y[1]);
         terminate2( y, t );
         exit;
       end;     {   end if ( kflag == -1 || kflag == -2 )   }
    end;   {   end while   }
end;     {   end lsoda   }
{**************************************************************}
end. { of unit lsoda }
