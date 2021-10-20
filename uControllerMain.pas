unit uControllerMain;

interface

uses System.SysUtils, System.Classes, Dialogs, uSimulation, uModel, uSBMLClasses,
     uODE_FormatUtility, WebLib.Forms, Web, uSBMLReader, uControllerNetwork,
     uSBMLWriter, uNetwork, uSidewinderTypes;

type
 TUpdateSimEvent = procedure(time:double; updatedVals: array of double) of object;
 // Let listeners know of new simulation update event.
  TUpdateModelEvent = procedure(model: TModel) of object; // SBML model loaded.
  TNetworkChangeEvent = procedure() of object; // Notify network has changed, may want to update model.

 TControllerMain = class
  private
    sbmlText: String;
    sbmlmodel: TModel;
    writeSBMLFileName: String;
    numbRxns: Integer;
    modelLoaded: Boolean;
   // sbmlFileLoaded: Boolean; // true if sbml file and model just loaded
    solverUsed: ODEsolver;
    runTime: Double; // Total runtime of simulation (sec)
    runSim: TSimulationJS;
    stepSize: Double; // (msec) (integration step)
    pixelStepAr: array of Integer; // pixel equiv of time (integration) step
    currTime: Double;
    online: Boolean; // Simulation running
    saveSBMLFlag: Boolean;
    ODEready: Boolean; // TRUE: ODE solver is setup.
    networkUpdate: Boolean; // Use to prevent circular update when network is changed.
    FUpdate: TUpdateSimEvent;// Send Updated Sim values (time,species amts) to listeners.
    FNetworkChanged: TNetworkChangeEvent; // Notify listener that network has changed,
    FSBMLUpdate: TUpdateModelEvent;
    FSBMLUpdate2: TUpdateModelEvent;
    currNetworkCtrl : TController;

  public
    paramUpdated: Boolean; // true if a parameter val has been updated.

    constructor Create(networkCtrl: TController);
    procedure createModel(); // Instatiate TModel and attach listener
    procedure createSimulation();
    procedure SBMLLoaded(); // Notify when done loading libSBMLjs
    procedure setODESolver();
    function getODESolver(): ODEsolver;
    function getModel: TModel;
    procedure setModel(newModel: TModel);
    procedure SetRunTime(newTime: Double);
    function GetRunTime(): Double;
    function GetStepSize(): Double;
    procedure SetStepSize(newStepSize: integer);
    function getCurrTime: Double;
    function IsOnline(): Boolean;
    procedure SetOnline(bOnline: Boolean);
    procedure SetModelLoaded(bModelLoaded: boolean);
    function IsModelLoaded(): Boolean;
   // function IsNetworkUpdated(): Boolean;  // true: network changed, need to update model.
    function hasNetworkChanged(): Boolean;  // true: network changed, need to update model.
    procedure SetTimerEnabled(bTimer: Boolean);
    procedure SetTimerInterval(nInterval: Integer);
    procedure loadSBML(sbmlStr: String);
    procedure saveSBML(fileName: String);
    procedure modelWritten(modelStr: String); // SBML string created and ready to use.
    procedure changeParameterVal(pos: Integer; newVal: Double);
    procedure stopTimer();
    procedure startTimer();
    procedure resetCurrTime();
    procedure writeSimData(fileName: string; data: TStrings);
    function getSimulation(): TSimulationJS;
    procedure networkUpdated(); // Network has changed, update model
    property OnNetworkChange: TNetworkChangeEvent read FNetworkChanged write FNetworkChanged;
    property OnSimUpdate: TUpdateSimEvent read FUpdate write FUpdate;
    property OnSBMLUpdate: TUpdateModelEvent read FSBMLUpdate write FSBMLUpdate;
    property OnSBMLUpdate2: TUpdateModelEvent read FSBMLUpdate2 write FSBMLUpdate2;
    procedure UpdateVals( time: double; updatedVals: array of double);
            // Send new values to listeners.
    procedure getVals(newTime: Double; newVals: array of Double);
            // Get new values from simulation run.
 end;

implementation
constructor TControllerMain.Create(networkCtrl: TController);
begin
  self.sbmlText := '';
  self.modelLoaded := false;
 // self.sbmlFileLoaded := false;
  self.resetCurrTime;
  self.stepSize := 0.1; // 100 msec
  self.runTime := 500; // sec
  self.online := false;
  self.networkUpdate := false;
  self.saveSBMLFlag := false;
  self.createModel;
  self.currNetworkCtrl := networkCtrl;
  self.OnSBMLUpdate := networkCtrl.SBMLUpdated;
  networkCtrl.network.OnNetworkEvent := self.networkUpdated;
end;

procedure TControllerMain.resetCurrTime();
begin
  self.currTime := 0;
end;

// Grab SBML model information when notified by model of change:
procedure TControllerMain.SBMLLoaded();
begin
  console.log('TControllerMain.SBMLLoaded. creating new simulation');
  self.modelLoaded := true;
  self.createSimulation;

  if Assigned(FSBMLUpdate) then
    FSBMLUpdate(self.sbmlmodel);

  if Assigned(FSBMLUpdate2) then
    FSBMLUpdate2(self.sbmlmodel);
end;

procedure TControllerMain.createModel();
begin
//console.log('TControllerMain.createModel');
  if self.sbmlmodel <> nil then self.sbmlmodel.Free;
  self.sbmlmodel := TModel.create();
  self.sbmlmodel.OnPing := self.SBMLLoaded;  // Register callback function
  if self.networkUpdate then  // Create from Network
    begin
      self.sbmlmodel := self.currNetworkCtrl.createSBMLModel(self.sbmlmodel);
      self.sbmlmodel.SBML_UpdateEvent; // Notify listeners
      self.networkUpdate := false;
    end;

end;

procedure TControllerMain.createSimulation();
begin
//console.log('TControllerMain.createSimulation');
  if self.runSim <> nil then
  begin
    self.runSim.Free;
    self.SBMLmodel.resetS_Vals();  // TODO: Move S_vals to simulator.
    self.online := false;
  end;
  self.currTime := 0;
  self.runSim := TSimulationJS.create(self.runTime, self.stepSize, self.SBMLmodel, self.solverUsed);
  self.runSim.OnUpdate := self.getVals; // register callback function.
end;

// return current time of run and variable values to listener:
procedure TControllerMain.UpdateVals( time: double; updatedVals: array of double);
 begin
   if Assigned(FUpdate) then
     FUpdate(time, updatedVals);
 end;

  // Network has changed, notify any listeners
procedure TControllerMain.networkUpdated();
begin
  console.log('Network changed');
  self.networkUpdate := true;      // after model updated, change to false.
  if Assigned(FNetworkChanged) then
    FNetworkChanged();
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

procedure TControllerMain.setModel(newModel: TModel);
begin
  self.SBMLModel := newModel;
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
  Result := self.runSim.IsOnline;
end;
procedure TControllerMain.SetOnline(bOnline: Boolean);
begin
  if self.runSim <> nil then
    self.runSim.SetOnline(bOnline);
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
function TControllerMain.hasNetworkChanged(): Boolean;
begin
  Result := self.NetworkUpdate;
end;

procedure TControllerMain.SetTimerEnabled(bTimer: Boolean);
begin
  self.runSim.SetTimerEnabled(bTimer);
  if bTimer then self.runSim.updateSimulation();

end;

procedure TControllerMain.SetTimerInterval(nInterval: Integer);
begin
  self.runSim.SetTimerInterval(nInterval);
end;

procedure TControllerMain.SetRunTime(newTime: Double);
begin
  self.runTime := newTime;
end;

function TControllerMain.GetRunTime(): Double;
begin
  Result := self.runTime;
end;

function TControllerMain.getSimulation(): TSimulationJS;
begin
  Result := self.runSim;
end;


procedure TControllerMain.stopTimer();
begin
  self.runSim.stopTimer;
end;

procedure TControllerMain.startTimer();
begin
  self.runSim.startTimer;
end;

procedure TControllerMain.getVals(newTime: Double; newVals: array of Double);
begin
  self.currTime := newTime;
  self.UpdateVals(newTime, newVals); // pass on to listeners.
end;

procedure TControllerMain.loadSBML(sbmlStr: String );
var SBMLReader: TSBMLRead;
begin
  sbmlText := sbmlStr; // store the sbml model as text.
  if sbmlText <> '' then
  begin
    SBMLReader := TSBMLRead.create(sbmlmodel, self.sbmlText );// Process text with libsbml.js
   // self.sbmlFileLoaded := true;
  end
  else showMessage ('SBML text empty.');

end;

procedure TControllerMain.saveSBML(fileName: String);
var writerSBML: TSBMLWriter;
begin
  self.writeSBMLFileName := fileName;
  self.saveSBMLFlag := true;
  // currently can have both sbml loaded from file and network model.
  if self.networkUpdate = true then
    begin
      self.createModel;
      self.networkUpdate := false;
    end;

  if self.sbmlmodel <> nil then
  begin
    writerSBML := TSBMLWriter.create();
    writerSBML.OnNotify := self.modelWritten;
    writerSBML.buildSBMLString(self.sbmlmodel);
  end;
  self.saveSBMLFlag := false;
end;

procedure TControllerMain.modelWritten(modelStr: String);
 begin
   Application.DownloadTextFile(modelStr, self.writeSBMLFileName);
 end;

procedure TControllerMain.changeParameterVal(pos: Integer; newVal: Double);
begin
  self.sbmlmodel.changeParamVal(pos, newVal);
end;


procedure TControllerMain.writeSimData(fileName: string; data: TStrings);
var simData: string;
    i: integer;
begin
  simData := '';
  for i := 0 to data.count -1 do
    begin
     // console.log( '***** sim results:  ', data[i]);
      simData := simData + data[i] + sLineBreak;
    end;
  console.log( ' end of results');
   try
     Application.DownloadTextFile(simData, fileName);
   except
       on E: Exception do
          notifyUser(E.message);
   end;

end;


end.
