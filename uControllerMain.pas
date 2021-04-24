unit uControllerMain;

interface

uses System.SysUtils, uSimulation, uModel, uSBMLClasses, uODE_FormatUtility,
     WebLib.Forms, WEBLib.ExtCtrls, Web, uSBMLReader;

type
 TUpdateSimEvent = procedure(time:double; updatedVals: array of double) of object;
 TModelUpdateEvent = procedure() of object; // model has been loaded or updated

 TControllerMain = class
  private
    sbmlText: String;
    sbmlmodel: TModel;
    mainForm: TWebForm;
    numbRxns: Integer;
    modelLoaded: Boolean;
    solverUsed: ODEsolver;
    runTime: Double; // Total runtime of simulation (sec)
    runSim: TSimulationJS;
    stepSize: Double; // (msec) (integration step)
    pixelStepAr: array of Integer; // pixel equiv of time (integration) step
    currTime: Double;
    s_Vals: array of Double; // one to one correlation: s_Vals[n] = s_name's[n]
    s_Names: array of String; // Use species ID as name
    p_Vals: array of Double; // Includes compartments
    p_Names: array of String;
    online: Boolean; // Simulation running
    ODEready: Boolean; // TRUE: ODE solver is setup.
    WebTimer1: TWebTimer;
    FUpdate: TUpdateSimEvent;// Send Updated Sim values (time,species amts) to listeners.
    FModelUpdate: TModelUpdateEvent;
    procedure setupSimulation();
    procedure startSimulation(odeEqs: String; odeFormat: TFormatODEs);
    procedure WebTimer1Timer(Sender: TObject);

  public
    paramUpdated: Boolean; // true if a parameter has been updated.
    constructor Create(newMainForm: TWebForm);// timer - pass in nil, do not pass in webform move timer to simulator..
    procedure SBMLLoaded(); // Notify when done loading libSBMLjs
    procedure fillSpeciesArray();
    procedure fillParameterArray();
    procedure setODESolver();
    function getODESolver(): ODEsolver;
    function getS_Names(): array of String;
    function getS_Vals(): array of Double;
    function getP_Names(): array of String;
    function getP_Vals(): array of Double;
    function getP_Val(pos: integer): Double;
    function setP_Val(pos: integer; newVal: Double): Boolean;
    procedure SetRunTime(newTime: Double);
    function GetRunTime(): Double;
    function GetStepSize(): Double;
    procedure SetStepSize(newStepSize: integer);
    function getCurrTime: Double;
    function IsOnline(): Boolean;
    procedure SetOnline(bOnline: Boolean);
    procedure SetModelLoaded(bModelLoaded: boolean);
    function IsModelLoaded(): Boolean;
    procedure SetTimerEnabled(bTimer: Boolean);
    procedure SetTimerInterval(nInterval: Integer);
    procedure updateSimulation();
    procedure LoadSBML(sbmlStr: String {;networkViewer} );
    property OnUpdate: TUpdateSimEvent read FUpdate write FUpdate;
    property OnModelUpdate: TModelUpdateEvent read FModelUpdate write FModelUpdate;
    procedure UpdateModel(); // Ping listener that model has been loaded or changed.
    procedure UpdateVals( time: double; updatedVals: array of double);
            // Send new values to listeners.
    procedure getVals(newTime: Double; newVals: array of Double);
            // Get new values from simulation run.
 end;

implementation
constructor TControllerMain.Create(newMainForm: TWebForm);
begin
  self.mainForm := newMainForm;
  self.sbmlText := '';
  self.modelLoaded := false;
  self.currTime := 0;
  self.stepSize := 0.1; // 100 msec
  self.WebTimer1 := TWebTimer.Create(mainForm);
  self.WebTimer1.OnTimer := WebTimer1Timer;
  self.WebTimer1.Enabled := false;
  self.WebTimer1.Interval := 100;  // default, msec
  self.online := false;

end;

// Grab SBML model information when notified:
procedure TControllerMain.SBMLLoaded();
begin
  self.modelLoaded := true;
  self.UpdateModel();
end;


  // Get initial vals for Species from SBML model
// TODO: add code for nonSBML models? No, nonSBML model stored as SBML.
procedure TControllerMain.fillSpeciesArray();
var
  i: Integer;
  spAr: array of TSBMLSpecies;
begin
  if self.sbmlmodel.getSpeciesNumb() > 0 then // TODO: chk if sbml model first
    begin
      spAr := self.sbmlmodel.getSBMLdynamicSpeciesAr;
      SetLength(self.s_Vals, Length(spAr));
      SetLength(self.s_Names, length(self.s_Vals));
      for i := 0 to Length(spAr) - 1 do
        begin
          if spAr[i].isSetInitialAmount() then
            self.s_Vals[i] := spAr[i].getInitialAmount()
          else if spAr[i].isSetInitialConcentration() then
            self.s_Vals[i] := spAr[i].getInitialConcentration();
          self.s_Names[i] := spAr[i].getID();
          // Use species ID as name
        end;
    end;
end;


procedure TControllerMain.fillParameterArray();
var i: integer;
    odeFormat: TFormatODEs;
begin
   odeFormat := TFormatODEs.create(sbmlmodel);

  if length(p_Vals) < 1 then
    begin
      SetLength(p_Vals, length(odeFormat.get_pVals())); // Keep params for later
      SetLength(p_Names, length(p_Vals));
      for i := 0 to length(odeFormat.get_pVals()) - 1 do
        begin
          p_Vals[i] := odeFormat.get_pVals[i];
          p_Names[i] := odeFormat.get_paramsStrAr[i];
        end;
    end;
end;

// return current time of run and variable values to listener:
procedure TControllerMain.UpdateVals( time: double; updatedVals: array of double);
 begin
   if Assigned(FUpdate) then
     FUpdate(time, updatedVals);
 end;

procedure TControllerMain.UpdateModel();
  begin
    if Assigned(FModelUpdate) then
      FModelUpdate();
  end;

procedure TControllerMain.setODESolver();
begin
  // If want choice then:
  // case ODEsolverWRadioGroup.ItemIndex of
  // 0: solverUsed:= EULER;
  // 1: solverUsed:= RK4;
  // 2: solverUsed:= LSODAS;
  // else solverUsed:= LSODAS;
  // end;
  solverUsed := LSODAS;
end;

function TControllerMain.getODESolver(): ODEsolver;
begin
  Result := self.solverUsed;
end;

function TControllerMain.getS_Names(): array of String;
begin
  Result := self.s_Names;
end;

function TControllerMain.getS_Vals(): array of Double;
begin
  Result := self.s_Vals;
end;

function TControllerMain.getP_Names(): array of String;
begin
  Result := self.p_Names;
end;

function TControllerMain.getP_Vals(): array of Double;
begin
  Result := self.p_Vals;
end;

function TControllerMain.getP_Val(pos: integer): Double;
begin
  Result := self.p_Vals[pos];
end;

function TControllerMain.setP_Val(pos: integer; newVal: Double): Boolean;
begin
  if (Length(self.p_Vals) > (pos + 1)) and (pos >= 0) then
  begin
    self.p_Vals[pos] := newVal;
    Result := true;
  end
  else  Result := false;
end;

function TControllerMain.GetStepSize(): Double;
begin
  Result := self.StepSize;
end;

procedure TControllerMain.SetStepSize(newStepSize: integer);
begin
  self.StepSize := newStepSize * 0.001;
end;

function TControllerMain.IsOnline(): Boolean;
begin
  Result := self.online;
end;
procedure TControllerMain.SetOnline(bOnline: Boolean);
begin
  self.online := bOnline;
end;

function TControllerMain.getCurrTime: Double;
begin
    Result := self.currTime;
end;


procedure TControllerMain.SetModelLoaded(bModelLoaded: boolean);
begin
  self.modelLoaded :=  bModelLoaded;
end;

function TControllerMain.IsModelLoaded(): Boolean;
begin
  Result := self.modelLoaded;
end;

procedure TControllerMain.SetTimerEnabled(bTimer: Boolean);
begin
  self.WebTimer1.Enabled := bTimer;
end;

procedure TControllerMain.SetTimerInterval(nInterval: Integer);
begin
  self.WebTimer1.Interval := nInterval;
end;

procedure TControllerMain.SetRunTime(newTime: Double);
begin
  self.runTime := newTime;
end;

function TControllerMain.GetRunTime(): Double;
begin
  Result := self.runTime;
end;

procedure TControllerMain.setupSimulation();
var
  i: Integer;
  odeFormat: TFormatODEs;
begin
  ODEready := false;
  self.stepSize := self.WebTimer1.Interval * 0.001; // 1 sec = 1000 msec
  odeFormat := TFormatODEs.create(sbmlmodel);

  if length(p_Vals) < 1 then
    begin
      SetLength(p_Vals, length(odeFormat.get_pVals())); // Keep params for later
      SetLength(p_Names, length(p_Vals));
      for i := 0 to length(odeFormat.get_pVals()) - 1 do
        begin
          p_Vals[i] := odeFormat.get_pVals[i];
          p_Names[i] := odeFormat.get_paramsStrAr[i];
        end;
    end;

  // Run Simulation using info from odeFormat:
  odeFormat.buildFinalEqSet();
  if self.getODESolver = LSODAS then
    self.startSimulation(odeFormat.getODEeqSet2(), odeFormat)
  else
    self.startSimulation(odeFormat.getODEeqSet(), odeFormat);
end;


procedure TControllerMain.startSimulation(odeEqs: String; odeFormat: TFormatODEs);
begin
  runSim := TSimulationJS.create(runTime, stepSize, length(s_Vals), length(odeFormat.get_pVals()), odeEqs, solverUsed);
  runSim.OnUpdate := self.getVals; // register callback function.
  runSim.p := odeFormat.get_pVals();
  currTime := 0;
  if solverUsed = LSODAS then
    runSim.eval2(currTime, s_Vals)
  else
    runSim.eval(currTime, s_Vals);
  ODEready := true;

  // Debug:
  // printSpeciesParamsArr(odeFormat.get_sVals(), odeFormat.get_speciesStrAr());
  // printSpeciesParamsArr(runSim.p, odeFormat.get_paramsStr());
end;


procedure TControllerMain.UpdateSimulation();
begin
  WebTimer1.enabled := true;
  //console.log('Cur time: ',self.currTime);
  stepSize := self.WebTimer1.Interval * 0.001; // 1 sec = 1000 msec
  if ODEready = true then
    begin
      runSim.setStepSize(stepSize);
      if self.paramUpdated then
        begin
          runSim.p := p_Vals; // Update parameters ...
          self.paramUpdated := false;
        end;
      // Get last time and s values and pass into eval2:
      if length(s_Vals) > 0 then
        begin
          if solverUsed = LSODAS then
            runSim.eval2(currTime, s_Vals)
          else
            runSim.eval(currTime, s_Vals);
        end;
    end
    // else error msg needed?
  else
    self.setupSimulation();
end;

procedure TControllerMain.getVals(newTime: Double; newVals: array of Double);
begin
  self.currTime := newTime;
  self.UpdateVals(newTime, newVals); // pass on to listeners.
end;

procedure TControllerMain.LoadSBML(sbmlStr: String {;networkViewer} );
var SBMLReader: TSBMLRead;
begin
  sbmlText := sbmlStr; // store the sbml model as text.
  // Check if sbmlmodel already created, if so, destroy before creating ?
  sbmlmodel := TModel.create();
  sbmlmodel.OnPing := SBMLLoaded; // *** Register the callback function
  //sbmlmodel.readSBML(self.sbmlText); // Process text with libsbml.js

  SBMLReader := TSBMLRead.create(sbmlmodel, self.sbmlText );// Process text with libsbml.js

end;

procedure TControllerMain.WebTimer1Timer(Sender: TObject);
begin
  self.updateSimulation();
  if self.currTime > runTime then
    WebTimer1.enabled := false;
end;

end.
