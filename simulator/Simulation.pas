unit Simulation;

// Set up an execute a simulation.

interface

 Uses Classes, Types, JS, Web, SysUtils, LSODA.test, adamsbdf, uVector;

type
  TUpdateValEvent = procedure(time:double; updatedVals: array of double) of object;
  ODESolver = (EULER, RK4, LSODAS);
 SimulationJS = class (TObject)
   private
    np, ny: Integer;  // Number of parameters, species (ny).
    dydt: TDoubleDynArray;
    eqList: String;   // Euler, RK4 ODE eq list.
    LSODAeq: String;  // Formated ODE eq list for LSODA solver.
    step: double;       // time stepsize
    time: double;       // Current time of run.
    runTime: double;    // Length of simulation.
    solverUsed: ODESolver;
    lode: TLsoda;       // LSODA solver
    FUpdate: TUpdateValEvent;// Used to send Updated values (species amts) to listeners.

   public
    p : TDoubleDynArray;   // System/Model Parameters
    constructor Create ( runTime, nStepSize:double; ny, np :Integer; newList: String; solver:ODESolver ); Overload ;
    procedure setODEsolver(solverToUse: ODESolver);
    function  eval (newTime: double; s: array of double) : double;
    function  eval2 ( time:double; s: array of double) : double;
    property OnUpdate: TUpdateValEvent read FUpdate write FUpdate;
    { Triggers the event if anything is registered }
    procedure updateVals(time:double; updatedVals: array of double);
    procedure setStepSize(newStep: double);
    function getStepSize(): double;
    procedure testLSODA();  // Solve test equations. All pascal code.
 end;

implementation

constructor SimulationJS.Create ( runTime, nStepSize:double; ny, np : Integer; newList: String; solver:ODESolver ); Overload ;
var lsodaStr: String;  // LSODA test string. can remove.
  i:integer;
begin
  self.ny:= ny;
  self.np:= np;
  self.eqList:= newList;
  // TEST string for LSODA:
  lsodaStr:= 'let dydt_s = pas.uVector.TVector.$create("create$1",[s.length]);';
  lsodaStr:= lsodaStr + ' dydt_s.setVal(2, (p[0] * s[0]));dydt_s.setVal(1, ((-1)* (p[0] * s[0]))); return dydt_s ;';
  //self.LSODAeq:= lsodaStr;
  self.LSODAeq:= newList;

  setLength(self.p,self.np);
  setLength(self.dydt,self.ny);
  self.step:= nStepSize;
  if self.step <=0 then
    self.step:= 1; //  Default

  if runTime >0 then
    self.runTime:= runTime
  else self.runTime:= 10;

  self.solverUsed:= solver;
  self.time:= 0;

  // lsoda setup
  if self.solverUsed = LSODAS then
  begin
    self.lode :=  TLsoda.create(ny);
    self.lode.rtol[1] := 1e-4; self.lode.rtol[2] := 1e-4; self.lode.rtol[3] := 1e-4;
    self.lode.atol[1] := 1e-6; self.lode.atol[2] := 1e-10; self.lode.atol[3] := 1e-6;
    self.lode.itol := 2;
    self.lode.itask := 1;
    self.lode.istate := 1;
    self.lode.iopt := 0;
    self.lode.jt := 2;
   asm
    console.log('LSODA Funct: ', this.LSODAeq);
    var ODE_func2 = new Function('time', 's','p', this.LSODAeq);
    this.lode.Setfcn (ODE_func2);
   end;
  end;
end;
                             // want to use updated time
function  SimulationJS.eval ( newTime: double; s : array of double ) : double;
//function  SimulationJS.eval ( s,par : array of double ) : double;
 var i,j: Integer;
 var numSteps: Integer;
 var dydt_s: array of double;
    // l : TLsoda;
     y: TVector;
     t, tNext: double;
begin
   numSteps:= Round(self.runTime/self.step);
   if self.solverUsed = LSODAS then
   begin
    self.lode.p:= self.p; // set param vals.
 //console.log('self_p: ',self.lode.p[0]);
    tNext:= self.step;
    y:= TVector.create(Length(s));
    for i := 1 to Length(s) do
     begin
      y[i]:= s[i-1];
     end;
   end;// end of lsoda setup
   self.time:= newTime; // added ....
  asm

   var s_init = [...s];
   switch(this.solverUsed) {
   case 0:
     var ODE_func = new Function('p','s','dydt_s', 'time', this.eqList);
    // console.log('EULER being used.');
     // Euler solver:
     var calcODE = new EulerIntegrator(this.step);
     calcODE.calc_dydt(this.p,s, this.dydt, this.time , ODE_func);
     this.time = this.time + this.step;
     for (var j=0; j< this.ny; j++) {
    //console.log('dydt ',j,': ',this.dydt[j]);
      s[j] = s[j]+this.dydt[j];
     }
   break;
   case 1:
      // ************************
      // Rk4 :      //y0, deriv, t, dt, p
      console.log('RK4 being used.');
      var ODE_func = new Function('p','s','dydt_s', 'time', this.eqList);
      s = [...s_init];
      var rk4Calc = new RK4Integrator( s,ODE_func,this.time,this.step,this.p);
      rk4Calc.step();
      for (var j=0; j< this.ny; j++) {
        s[j] = rk4Calc.y[j];
      }
      this.time = rk4Calc.t;
      this.updateVals(this.time,s);
     console.log( 'Time: ', this.time, 's[0]: ',s[0],', s[1]: ', s[1] );
    break; // end of case RK4

   case 2:
    // *** LSODA:
    console.log('LSODA Funct: ', this.LSODAeq);
    var ODE_func2 = new Function('time', 's','p', this.LSODAeq);
    this.lode.Setfcn (ODE_func2);
    break; // end of case LSODA

   }  // end of case Block.

    this.updateVals(this.time,s);
   end;  // end of asm block

   if self.solverUsed = LSODAS then
   begin

    self.time:= 0; // reset time.
    for i:= 1 to numSteps do
    begin
     self.lode.Execute (y, self.time, tNext);
     console.log('t: ',self.time, 'y_1: ', y[1], 'y_2: ',y[2]);

     // Convert TVector back to array of double ( y ->s)
      for j:= 0 to Length(s)-1 do
     begin
      s[j]:= y[j+1];
     end;
     self.updateVals(self.time,s);
     if self.lode.istate < 0 then
           begin

           console.log ('Error, istate = ', self.lode.istate);
           break;
           end;

      tNext:= tNext + self.step;
    end;
   end;
 //

end;

function  SimulationJS.eval2 ( time:double; s: array of double) : double;
 var i,j: Integer;
 var numSteps: Integer;
 var dydt_s: array of double;
     y: TVector;
     t, tNext: double;
begin
   numSteps:= Round(self.runTime/self.step);  // change to small amt, maybe 5 steps?
   if self.solverUsed = LSODAS then
   begin
    self.lode.p:= self.p; // set param vals.
    tNext:= time + self.step;
    y:= TVector.create(Length(s));
    for i := 1 to Length(s) do
      begin
        y[i]:= s[i-1];
      end;

    self.time:= time; // reset time to current time.
    self.lode.Execute (y, self.time, tNext);
    console.log('t: ',self.time, 'y_1: ', y[1], 'y_2: ',y[2]);
    if self.time=tNext then console.log('Self.time moved to next step: ', self.time)
      else self.time:=tNext;    // remove this once understand what is going on in LSODA code.
     // Convert TVector back to array of double ( y ->s)
    for i:= 0 to Length(s)-1 do
      begin
        s[i]:= y[i+1];
      end;
    self.updateVals(self.time,s);
    if self.lode.istate < 0 then
      begin

        console.log ('Error, istate = ', self.lode.istate);
      end;
      tNext:= tNext + self.step;

   end;

  end;
// return current time of run and variable values to listener:
procedure SimulationJS.UpdateVals( time: double; updatedVals: array of double);
 begin
   if Assigned(FUpdate) then
     FUpdate(time, updatedVals);
 end;

procedure SimulationJS.setODEsolver(solverToUse: ODESolver);
 begin
   self.solverUsed:= solverToUse;
 end;


procedure SimulationJS.setStepSize(newStep: double);
 begin
    self.step:= newStep;
 end;

function SimulationJS.getStepSize(): double;
 begin
  Result:= self.step;
 end;

procedure SimulationJS.testLSODA();

 begin
   // LSODA.test.test(); // Use with all Pascal code.
 end;

end.
