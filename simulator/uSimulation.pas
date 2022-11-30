unit uSimulation;

// Set up and execute a simulation.

interface

 Uses Classes, Types, JS, Web, SysUtils, System.Generics.Collections, adamsbdf,
     uVector, uModel, uODE_FormatUtility, WEBLib.ExtCtrls, uSidewinderTypes;

type
  TUpdateValEvent = procedure(time:double; updatedVals: TVarNameValList) of object;
  TSimResults = procedure(simResults: TList<TTimeVarNameValList>) of object;
  ODESolver = (EULER, RK4, LSODAS);
 TSimulationJS = class (TObject)
   private
    np, ny: Integer;  // Number of parameters, species (ny).
    dydt: TDoubleDynArray;
    s_Vals: array of Double; // species, Changes, one to one correlation: s_Vals[n] <=> s_Names[n]
    s_Names: array of String; // Use species ID as name
  //  s_ValsInit: array of Double; // vals at t = 0;
    s_InitAssignEqs: string; // Eqs to be eval at t = 0
  //  p_ValsInit: array of Double; // vals at t = 0
    simResultsList: TList<TTimeVarNameValList>;
    p_InitAssignEqs: string; // Eqs to be eval at t = 0
    p_NameValAr: TVarNameValList;  // user can change
    eqList: String;   // Euler, RK4 ODE eq list.
    LSODAeq: String;  // Formated ODE eq list for LSODA solver.
    step: double;       // time stepsize
    time: double;       // Current time of run.
    runTime: double;    // Length of simulation.
    solverUsed: ODESolver;
    lode: TLsoda;       // LSODA solver
    model: TModel;     // SBML model to simulate
    WebTimer1: TWebTimer;
    online: Boolean; // Simulation running
    ODEready: Boolean; // TRUE: ODE solver is setup. NEEDED ??
    FUpdate: TUpdateValEvent;// Used to send Updated values (species amts) to listeners.
    FStaticSimResults: TSimResults;
    procedure WebTimer1Timer(Sender: TObject);


   public
    p : TDoubleDynArray;   // System/Model Parameters
    paramUpdated: Boolean; // true if a parameter val has been updated.
    staticSimRun: boolean; // true if run sim to the end, then report results.

    constructor Create ( runTime, nStepSize: double; newModel: TModel; solver: ODESolver ); Overload ;
    procedure setODEsolver(solverToUse: ODESolver);
    procedure nextEval(newTime: double; s: array of double; newPVals: array of double);
    procedure eval (newTime: double; s: array of double) ; // currently not used
    procedure eval2 ( time:double; s: array of double); // LSODA integrator
    property OnUpdate: TUpdateValEvent read FUpdate write FUpdate;
    { Notify listener of updated values }
    property OnSimResultsNotify: TSimResults read FStaticSimResults write FStaticSimResults;
    procedure updateVals(time:double; updatedVals: array of double);
    function  getStepSize(): double;
    procedure setStepSize(newStep: double);
    procedure generateEquations(); // Take SBML model and generate eqs compatible for solver.
    function  getLSODAeqs(): string;
    function  getParamInitAssignEqs(): string;
    function  getSpeciesInitAssignEqs(): string;
    function  IsOnline(): Boolean;
    function  IsStaticSimRun(): Boolean;
    procedure setStaticSimRun( staticRun: boolean );
    procedure SetOnline(bOnline: Boolean);
    procedure SetTimerEnabled(bTimer: Boolean);
    procedure SetTimerInterval(nInterval: Integer);
    procedure stopTimer();
    procedure startTimer();
    function  getTime():double;
    // TODO: procedure updateInitialAssignments(); If any InitAssignments, calc them here then set param, species init vals to these.
    procedure setRuntime( newRunTime: double );
    procedure setTime( newTime: double );
    procedure startSimulation();
    procedure startStaticSimulation();
    procedure updateSimulation();
    procedure updateP_Val( index: integer; newVal: double );
    function  getP_Vals(): TVarNameValList;
    procedure setInitValues(); // set init vals of params. species at t=0
    procedure testLSODA();  // Solve test equations. All pascal code.
 end;

implementation

constructor TSimulationJS.Create ( runTime, nStepSize: double; newModel: TModel; solver: ODESolver ); Overload ;
var
  i: integer;
begin
  self.staticSimRun := false;  // default
  self.WebTimer1 := TWebTimer.Create(nil);
  self.WebTimer1.OnTimer := WebTimer1Timer;
  self.WebTimer1.Enabled := false;
  self.WebTimer1.Interval := 100;  // default, msec
  self.online := false;
  self.model := newModel;
  self.ny := length(self.model.getS_Vals);
  for i := 0 to ny - 1 do
    begin
    self.s_Vals[i] := self.model.getS_Vals()[i];
    self.s_Names[i] := self.model.getS_Names[i];
    end;
  self.np := length(self.model.getP_Vals);
  self.solverUsed:= solver;
  self.generateEquations();
  setLength(self.p,self.np);

  self.p_NameValAr := TVarNameValList.create;
  self.p_NameValAr.copy( self.model.getP_NameValAr ); // copy parameters from model
  self.p := self.p_NameValAr.getValAr;  // get parameter values array for integrator

  setLength(self.dydt,self.ny);
 // console.log(' nStepSize: ',nStepSize,' web interval: ',self.WebTimer1.Interval  );
  self.step:= nStepSize;
  if self.step <=0 then
    self.step:= 0.1; //  Default

  if runTime >0 then
    self.runTime:= runTime
  else self.runTime:= 500;
  self.time:= 0;

  // lsoda setup
  if self.solverUsed = LSODAS then
  begin
    self.lode :=  TLsoda.create(ny);
    for i := 1 to ny do
    begin
      self.lode.rtol[i] := 1e-4;
      self.lode.atol[i] := 1e-6;
    end;
    self.lode.itol := 2;
    self.lode.itask := 1;
    self.lode.istate := 1;
    self.lode.iopt := 0;
    self.lode.jt := 2;
   asm
    //console.log('LSODA Funct: ', this.LSODAeq);
    var ODE_func2 = new Function('time', 's','p', this.LSODAeq);
    this.lode.Setfcn (ODE_func2);
   end;
  end;

end;

procedure TSimulationJS.startSimulation();
begin
  self.p := self.p_NameValAr.getValAr;
  self.ODEready := true;
  //console.log('Init time: ', self.time);
  self.updateSimulation;
end;

procedure TSimulationJS.startStaticSimulation();
var i, totalNumberOfEvals: integer;
begin
  if self.online then
  begin
    // TODO: stop current run and reset everything? Maybe just have maincontroller reset sim
    self.stopTimer;
  end;
  totalNumberOfEvals := round(self.runTime / self.step);
  self.ODEready := true; // ?? need to check instead ?
  self.StaticSimRun := true;
  if self.simResultsList <> nil then self.simResultsList.free
  else self.simResultsList := TList<TTimeVarNameValList>.create;
  self.online := false;
  for i := 0 to totalNumberOfEvals -1 do
    begin
    self.p := self.p_NameValAr.getValAr;
    self.updateSimulation;
    end;
  console.log('Simulation done: iter: ', i); // Now notify listener....
  if Assigned(FStaticSimResults) then
       begin
       FStaticSimResults( self.simResultsList );
       end;
end;


procedure TSimulationJS.updateSimulation();
begin
  if self.ODEready = true then
    begin
    //  self.setStepSize(self.WebTimer1.Interval * 0.001); // 1 sec = 1000 msec;
      if self.time = 0.0 then // assume run starts at 0.0
        begin
        self.setInitValues;
        end;
      if self.paramUpdated then
        begin
        self.paramUpdated := false;
        end;
      //  console.log('updateSimulation: self.time, s[0]: ',self.time,', ',self.s_Vals);
      self.nextEval(self.time, self.s_Vals, self.p);
    end
    // else error msg needed?
  else
    self.startSimulation();
end;

procedure TSimulationJS.generateEquations();
var
  i: Integer;
  odeFormat: TFormatODEs;
begin
  odeFormat := TFormatODEs.create(self.model);
  // Run Simulation using info from odeFormat:
  odeFormat.buildFinalEqSet();
 // console.log(' ODE eq set2:',odeFormat.getODEeqSet2());
   if self.solverUsed = LSODAS then
    self.LSODAeq := odeFormat.getODEeqSet2()
  else
    self.eqList := odeFormat.getODEeqSet();
  self.p_InitAssignEqs := '';
  self.s_InitAssignEqs := '';
  // Need to add rate assignments as well:  Assignment rules before initial assignments?
  for i := 0 to length(odeFormat.getAssignRuleParamEqs) -1 do
    self.p_InitAssignEqs := self.p_InitAssignEqs + odeFormat.getAssignRuleParamEqs[i] + sLineBreak;
  for i := 0 to length(odeFormat.getAssignRuleSpeciesEqs) -1 do
    self.s_InitAssignEqs := self.s_InitAssignEqs + odeFormat.getAssignRuleSpeciesEqs[i] + sLineBreak;
  for i := 0 to odeFormat.getInitialAssignParamEqs.Count -1 do
    self.p_InitAssignEqs := self.p_InitAssignEqs + odeFormat.getInitialAssignParamEqs[i] + sLineBreak;
  for i := 0 to odeFormat.getInitialAssignSpeciesEqs.count -1 do
    self.s_InitAssignEqs := self.s_InitAssignEqs + odeFormat.getInitialAssignSpeciesEqs[i] + sLineBreak;
  if self.p_InitAssignEqs <> '' then
    self.p_InitAssignEqs := self.p_InitAssignEqs + 'return p;' + sLineBreak;
  if self.s_InitAssignEqs <> '' then
    self.s_InitAssignEqs := self.s_InitAssignEqs + 'return s;' + sLineBreak;
//  console.log( '*** p Init eqs: ', self.p_InitAssignEqs );
//  console.log( '** s Init eqs: ', self.s_InitAssignEqs );
end;

procedure TSimulationJS.nextEval(newTime: double; s: array of double; newPVals: array of double);
begin
//??  self.setStepSize(self.step);
  self.p := newPVals;
   // Get last time and s values and pass into eval2:
  if length(s) > 0 then
    begin
      if self.solverUsed = LSODAS then
        self.eval2(newTime, s)
      else
        self.eval(newTime, s);
    end;
end;
  // eval code probably no longer works as we use LSODA exclusively. See self.eval2()
procedure  TSimulationJS.eval ( newTime: double; s : array of double );
 var i, j: Integer;
 var numSteps: Integer;
     y: TVector;
     tNext: double;
begin
   numSteps:= Round(self.runTime/self.step);
   if self.solverUsed = LSODAS then
   begin
    self.lode.p:= self.p; // set param vals.
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
   //  console.log( 'Time: ', this.time, 's[0]: ',s[0],', s[1]: ', s[1] );
    break; // end of case RK4

   case 2:
    // *** LSODA:
   // console.log('LSODA Funct: ', this.LSODAeq);
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
     //console.log('t: ',self.time, 'y_1: ', y[1], 'y_2: ',y[2]);

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

procedure  TSimulationJS.eval2 ( time:double; s: array of double);
 var i,j: Integer;
 var numSteps: Integer;
 var dydt_s: TVector;
     y: TVector;
     tNext: double;
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
    self.time := tNext;
   
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
procedure TSimulationJS.UpdateVals( time: double; updatedVals: array of double);
var i: integer;
    updatedList: TVarNameValList;
    updatedTimeVarList: TTimeVarNameValList;
 begin

   updatedList := TVarNameValList.create;
     for i := 0 to Length(updatedVals) -1 do
       begin
       updatedList.add( TVarNameVal.create( self.s_Names[i], updatedVals[i] ) );
       end;

   if self.staticSimRun then
     begin
     //if self.simResultsList <> nil then
     //  self.simResultsList := TList<TTimeVarNameValList>.create;
     updatedTimeVarList := TTimeVarNameValList.create(time, updatedList);
     self.simResultsList.add(updatedTimeVarList);
     end
   else
     if Assigned(FUpdate) then
       begin
     //  updatedList := TVarNameValList.create;
     //  for i := 0 to Length(updatedVals) -1 do
     //    begin
     //    updatedList.add( TVarNameVal.create( self.s_Names[i], updatedVals[i] ) );
     //    end;
       FUpdate( time, updatedList );
       end;
 end;

procedure TSimulationJS.updateP_Val( index: integer; newVal: double );
begin

  if (length(self.p) > index) and (index > -1) then
    begin
    self.p[index] := newVal;
    self.p_NameValAr.setVal( index, newVal );
    end;
end;

function TSimulationJS.getP_Vals(): TVarNameValList;
begin
  Result := self.p_NameValAr;
end;

procedure TSimulationJS.setODEsolver(solverToUse: ODESolver);
begin
   self.solverUsed:= solverToUse;
end;

function  TSimulationJS.getTime():double;
begin
  Result := self.time;
end;

procedure TSimulationJS.setTime( newTime: double );
begin
  if newTime >= 0 then
    self.time := newTime;
end;

procedure TSimulationJS.setRuntime( newRunTime: double );
begin
  if newRunTime >0 then
    self.runTime := newRunTime;
end;
procedure TSimulationJS.setStepSize(newStep: double);
begin
  if newStep > 0.0 then self.step:= newStep
  else self.step := 0.1;
end;

function TSimulationJS.getStepSize(): double;
begin
  Result:= self.step;
end;

function TSimulationJS.IsOnline(): Boolean;
begin
  Result := self.online;
end;

procedure TSimulationJS.SetOnline(bOnline: Boolean);
begin
  self.online := bOnline;
end;

function  TSimulationJS.IsStaticSimRun(): Boolean;
begin
  Result := self.staticSimRun;
end;

procedure TSimulationJS.setStaticSimRun( staticRun: boolean );
begin
  self.staticSimRun := staticRun;
end;

procedure TSimulationJS.SetTimerEnabled(bTimer: Boolean);
begin
  self.WebTimer1.Enabled := bTimer;
end;

procedure TSimulationJS.SetTimerInterval(nInterval: Integer);
begin
  self.WebTimer1.Interval := nInterval;
end;

procedure TSimulationJS.stopTimer();
begin
  self.WebTimer1.enabled := false;
end;

procedure TSimulationJS.startTimer();
begin
  self.WebTimer1.enabled := true;
end;

procedure TSimulationJS.WebTimer1Timer(Sender: TObject);
begin

  self.updateSimulation();
  if self.time > runTime then
    self.WebTimer1.enabled := false;
end;

procedure TSimulationJS.setInitValues();
var i: integer;
begin
//  setLength(self.s_ValsInit, length(self.s_Vals));
//  setLength(self.p_ValsInit, length(self.p));
  //self.p_ValsInit[0] := 2;

  if self.p_InitAssignEqs <> '' then
    begin
    asm
    //  console.log(this.p_InitAssignEqs);
      var initParamFunc = new Function( 's','p', this.p_InitAssignEqs);
      this.p = initParamFunc(this.s_Vals, this.p);
    //  console.log('new p: ', this.p);
    end
    end;
  // Now check if species init vals need to be calculated:
  if self.s_InitAssignEqs <> '' then
    begin
    asm
    //  console.log(this.s_InitAssignEqs);
      var initSpFunc = new Function( 's','p', this.s_InitAssignEqs);
     // this.s = initSpFunc(this.s_Vals, this.p);
      this.s_Vals = initSpFunc(this.s_Vals, this.p);
    //  console.log('new s init vals: ', this.s);
    end
    end;
  self.UpdateVals( 0, self.s_Vals);  // Pass init values back to listeners
end;

function TSimulationJS.getLSODAeqs(): string;
  begin
  Result := self.LSODAeq;
  end;

function  TSimulationJS.getParamInitAssignEqs(): string;
  begin
  Result := self.p_InitAssignEqs;
  end;

function  TSimulationJS.getSpeciesInitAssignEqs(): string;
  begin
  Result := self.s_InitAssignEqs;
  end;

  procedure TSimulationJS.testLSODA();

 begin
   // LSODA.test.test(); //? Use with all Pascal code.
 end;

end.
