unit uControllerMain;

interface

uses System.SysUtils, Dialogs, uSimulation, uModel, uSBMLClasses, uODE_FormatUtility,
     WebLib.Forms, WEBLib.ExtCtrls, Web, uSBMLReader, uControllerNetwork, uSBMLWriter,
     uNetwork, uSidewinderTypes;

type
 TUpdateSimEvent = procedure(time:double; updatedVals: array of double) of object;
 // Let listeners know of new simulation update event.
 TModelUpdateEvent = procedure(updatedModel: TModel) of object; // model has been loaded or updated

 TControllerMain = class
  private
    sbmlText: String;
    sbmlmodel: TModel;
    numbRxns: Integer;
    modelLoaded: Boolean;
    solverUsed: ODEsolver;
    runTime: Double; // Total runtime of simulation (sec)
    runSim: TSimulationJS;
    stepSize: Double; // (msec) (integration step)
    pixelStepAr: array of Integer; // pixel equiv of time (integration) step
    currTime: Double;
    online: Boolean; // Simulation running
    ODEready: Boolean; // TRUE: ODE solver is setup.
    WebTimer1: TWebTimer;
    FUpdate: TUpdateSimEvent;// Send Updated Sim values (time,species amts) to listeners.
    FModelUpdate: TModelUpdateEvent; // Notify ufMain that sbml model loaded/changed.
    FModelUpdate2: TModelUpdateEvent; // Notify uControllerNetwork that model loaded/changed.

    procedure setupSimulation();
    procedure startSimulation(odeEqs: String; odeFormat: TFormatODEs);
    procedure WebTimer1Timer(Sender: TObject);

  public
    paramUpdated: Boolean; // true if a parameter val has been updated.
    constructor Create();
    procedure SBMLLoaded(); // Notify when done loading libSBMLjs
    procedure setODESolver();
    function getODESolver(): ODEsolver;
    function getModel: TModel;
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
    procedure loadSBML(sbmlStr: String; networkController: TController );
    procedure saveSBML(fileName: String);
    procedure stopTimer();
    procedure startTimer();
    procedure networkUpdated(updatedNetwork: TNetwork); // Network has changed, update model
    property OnSimUpdate: TUpdateSimEvent read FUpdate write FUpdate;
    property OnModelUpdate: TModelUpdateEvent read FModelUpdate write FModelUpdate;
    property OnModelUpdate2: TModelUpdateEvent read FModelUpdate2 write FModelUpdate2;
    procedure ModelUpdated(); // Ping listeners that model has been loaded or changed.

    procedure UpdateVals( time: double; updatedVals: array of double);
            // Send new values to listeners.
    procedure getVals(newTime: Double; newVals: array of Double);
            // Get new values from simulation run.
 end;

implementation
constructor TControllerMain.Create();
begin
  self.sbmlText := '';
  self.modelLoaded := false;
  self.currTime := 0;
  self.stepSize := 0.1; // 100 msec
  self.runTime := 500; // sec
  self.WebTimer1 := TWebTimer.Create(nil);
  self.WebTimer1.OnTimer := WebTimer1Timer;
  self.WebTimer1.Enabled := false;
  self.WebTimer1.Interval := 100;  // default, msec
  self.online := false;

end;

// Grab SBML model information when notified by model of change:
procedure TControllerMain.SBMLLoaded();
begin
//console.log(' In TControllerMain.SBMLLoaded()');
  self.modelLoaded := true;
  self.setUpSimulation();
  self.ModelUpdated();
end;


// return current time of run and variable values to listener:
procedure TControllerMain.UpdateVals( time: double; updatedVals: array of double);
 begin
   if Assigned(FUpdate) then
     FUpdate(time, updatedVals);
 end;

// Sendout notification model has been updated:
procedure TControllerMain.ModelUpdated();
  begin
  console.log(' TControllerMain.ModelUpdated');
    if Assigned(FModelUpdate) then
       FModelUpdate(self.sbmlmodel);
    if Assigned(FModelUpdate2) then
       FModelUpdate2(self.sbmlmodel);
  end;

  // Network has changed, update model
procedure TControllerMain.networkUpdated(updatedNetwork: TNetwork);
begin
  // TODO    Update Model to reflect any Network changes.
  console.log('Network changed');
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

function TControllerMain.getModel: TModel;
begin
  Result := self.SBMLModel;
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
  // Run Simulation using info from odeFormat:
  odeFormat.buildFinalEqSet();
  if self.getODESolver = LSODAS then
    self.startSimulation(odeFormat.getODEeqSet2(), odeFormat)
  else
    self.startSimulation(odeFormat.getODEeqSet(), odeFormat);
end;


procedure TControllerMain.startSimulation(odeEqs: String; odeFormat: TFormatODEs);
begin
  runSim := TSimulationJS.create(runTime, stepSize, length(self.SBMLmodel.getS_Vals), length(self.SBMLmodel.getP_Vals()), odeEqs, solverUsed);
  runSim.OnUpdate := self.getVals; // register callback function.
  runSim.p := self.SBMLmodel.getP_Vals();
  currTime := 0;
  if solverUsed = LSODAS then
    runSim.eval2(currTime, self.SBMLmodel.getS_Vals)
  else
    runSim.eval(currTime, self.SBMLmodel.getS_Vals);
  ODEready := true;
end;


procedure TControllerMain.UpdateSimulation();  // Move most of this to TSimulationJS class
begin
  WebTimer1.enabled := true;
  //console.log('Cur time: ',self.currTime);
  stepSize := self.WebTimer1.Interval * 0.001; // 1 sec = 1000 msec
  if ODEready = true then
    begin
      runSim.setStepSize(stepSize);
      if self.paramUpdated then
        begin
          runSim.p := self.SBMLmodel.getP_Vals; // Update parameters ...
          self.paramUpdated := false;
        end;
      // Get last time and s values and pass into eval2:
      if length(self.SBMLmodel.getS_Vals) > 0 then
        begin
          if solverUsed = LSODAS then
            runSim.eval2(currTime, self.SBMLmodel.getS_Vals)
          else
            runSim.eval(currTime, self.SBMLmodel.getS_Vals);
        end;
    end
    // else error msg needed?
  else
    self.setupSimulation();
end;

procedure TControllerMain.stopTimer();
begin
  self.WebTimer1.enabled := false;
end;

procedure TControllerMain.startTimer();
begin
  self.WebTimer1.enabled := true;
end;

procedure TControllerMain.getVals(newTime: Double; newVals: array of Double);
begin
  self.currTime := newTime;
  self.UpdateVals(newTime, newVals); // pass on to listeners.
end;

procedure TControllerMain.loadSBML(sbmlStr: String ;networkController: TController );
var SBMLReader: TSBMLRead;
begin
  sbmlText := sbmlStr; // store the sbml model as text.
  if sbmlText <> '' then
  begin
    // Check if sbmlmodel already created, if so, destroy before creating ?
    self.sbmlmodel := TModel.create();
    self.sbmlmodel.OnPing := self.SBMLLoaded; // Register callback function
    SBMLReader := TSBMLRead.create(sbmlmodel, self.sbmlText );// Process text with libsbml.js
    // Register callback function so that network viewer can be updated.
  end
  else showMessage ('SBML text empty.');

end;

procedure TControllerMain.saveSBML(fileName: String);
var sbmlStr: String;
    writerSBML: TSBMLWriter;
begin

  sbmlStr := '';
  // TODO
 // writerSBML := TSBMLWriter.create(self.sbmlmodel);
 // sbmlStr := writerSBML.getSBMLStr();   // need to be notified, async

end;

procedure TControllerMain.WebTimer1Timer(Sender: TObject);
begin
  self.updateSimulation();
  if self.currTime > runTime then
    self.WebTimer1.enabled := false;
end;

end.
