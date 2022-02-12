unit uControllerMain;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, Dialogs,
     uSimulation, uModel, uSBMLClasses, uODE_FormatUtility, WebLib.Forms, Web,
     uSBMLReader, uControllerNetwork, uSBMLWriter, uNetwork, uSidewinderTypes;

type
  TUpdateSimEvent = procedure( time:double; updatedVals: TVarNameValList ) of object;
 // Let listeners know of new simulation update event.
  TUpdateModelEvent = procedure( model: TModel ) of object; // SBML model loaded.
  TNetworkChangeEvent = procedure( sender: TObject ) of object; // Notify network has changed, may want to update model.

 TControllerMain = class
  private
    sbmlText: String;
    sbmlmodel: TModel;
    writeSBMLFileName: String;
    numbRxns: Integer;
    modelLoaded: Boolean;
    solverUsed: ODEsolver;
    runTime: Double; // Total runtime of simulation (sec)
    runSim: TSimulationJS;
    stepSize: Double; // (msec) (integration step)
    pixelStepAr: array of Integer; // pixel equiv of time (integration) step
    currTime: Double;
    saveSBMLFlag: Boolean;
    ODEready: Boolean; // TRUE: ODE solver is setup.
    networkUpdate: Boolean; // Use to prevent circular update when network is changed.
    FSimUpdateAr: array of TUpdateSimEvent;// Send Updated Sim values (time,species amts) to listeners.
    FNetworkChangedAr: array of TNetworkChangeEvent; // Notify listener that network has changed
    FSBMLUpdateAr: array of TUpdateModelEvent; // notify listeners of sbml model change
    currNetworkCtrl : TController;

  public
    paramUpdated: Boolean; // true if a parameter val has been updated.

    constructor Create(networkCtrl: TController);
    procedure createModel(); // Instatiate TModel and attach listener
    procedure createSimulation();
    procedure SBMLLoaded(); // Notify when done loading libSBMLjs
    procedure setODESolver();
    function  getODESolver(): ODEsolver;
    function  getModel: TModel;
    procedure setModel(newModel: TModel);
    procedure SetRunTime(newTime: Double);
    function  GetRunTime(): Double;
    function  GetStepSize(): Double;
    procedure SetStepSize(newStepSize: double); // default 0.1 = 100 msec
    function  getCurrTime: double;
    procedure setCurrTime( newTime: double );
    function  IsOnline(): Boolean;
    procedure SetOnline(bOnline: Boolean);
    procedure SetModelLoaded(bModelLoaded: boolean);
    function  IsModelLoaded(): Boolean;
    procedure clearModel();
    procedure clearSim();
    function  hasNetworkChanged(): Boolean;  // true: network changed, need to update model.
    procedure SetTimerEnabled(bTimer: Boolean);
    procedure SetTimerInterval(nInterval: Integer);// nInterval is msec
    procedure loadSBML(sbmlStr: String);
    procedure saveSBML(fileName: String);
    procedure modelWritten(modelStr: String); // SBML string created and ready to use.
    procedure changeParameterVal(pos: Integer; newVal: Double); // change model parameter value.
    procedure changeSimParameterVal( pos: Integer; newVal: Double );
    procedure stopTimer();
    procedure startTimer();
    procedure resetCurrTime();
    procedure writeSimData(fileName: string; data: TStrings);
    function  getSimulation(): TSimulationJS;
    function  updateSpeciesNodeConc(node: TNode): boolean;
    procedure networkUpdated(sender: TObject); // Network has changed, update model
    procedure UpdateVals( time: double; updatedVals: TVarNameValList );
            // Send new values to listeners.
    procedure getVals(newTime: Double; newVals: TVarNameValList);
            // Get new values from simulation run.
    procedure resetSimParamValues(); // Reset simulator p values to orig model p values.
    procedure resetSimSpeciesValues(); // Reset simulator s values to orig model values.
    procedure addSimListener( newListener: TUpdateSimEvent );
    procedure addSBMLListener( newListener: TUpdateModelEvent );
    procedure addNetworkListener( newListener: TNetworkChangeEvent );
 end;

implementation
constructor TControllerMain.Create(networkCtrl: TController);
begin
  self.sbmlText := '';
  self.modelLoaded := false;
  self.resetCurrTime;
  self.stepSize := 0.1; // default, 100 msec
  self.runTime := 500; // sec
  self.networkUpdate := false;
  self.saveSBMLFlag := false;
  self.currNetworkCtrl := networkCtrl;
  self.addSBMLListener( @networkCtrl.SBMLUpdated );
  networkCtrl.network.OnNetworkEvent := self.networkUpdated;
end;

procedure TControllerMain.resetCurrTime();
begin
  self.currTime := 0;
end;

// Grab SBML model information when notified by model of change:
procedure TControllerMain.SBMLLoaded();
var i: integer;
begin
 // console.log('TControllerMain.SBMLLoaded. creating new simulation');
  self.modelLoaded := true;
  self.createSimulation;
  if length(self.FSBMLUpdateAr) > 0 then
    begin
    for i := 0 to length(self.FSBMLUpdateAr) -1 do
      self.FSBMLUpdateAr[i](self.sbmlmodel);
    end;

end;

procedure TControllerMain.addSBMLListener( newListener: TUpdateModelEvent );
var i: integer;
begin
  i := length(self.FSBMLUpdateAr);
  setLength(self.FSBMLUpdateAr, i +1 );
  self.FSBMLUpdateAr[i] := newListener;
end;

procedure TControllerMain.createModel();
begin
  self.clearModel;
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
  self.clearSim;
  self.currTime := 0;
  if self.stepSize < 0.0 then self.stepSize := 0.1; // TODO fix this issue, why necessary?
 // console.log('StepSize when creating sim: ', floattostr(self.stepSize) );
  self.runSim := TSimulationJS.create(self.runTime, self.stepSize, self.SBMLmodel, self.solverUsed);
 // set timerinterval ?
  self.runSim.OnUpdate := self.getVals; // register callback function.
end;

procedure TControllerMain.clearModel();
begin
  if self.sbmlModel <> nil then
    begin
    self.sbmlModel.Free;
    self.modelLoaded := false;
    end;
end;

procedure TControllerMain.clearSim();
begin
  if self.runSim <> nil then
    begin
    self.runSim.Free;
    end;
end;

// return current time of run and variable values to listener:
procedure TControllerMain.UpdateVals( time: double; updatedVals: TVarNameValList);
var i: integer;
begin
  if length(self.FSimUpdateAr) > 0 then
  begin
    for i := 0  to length(self.FSimUpdateAr) -1 do
      self.FSimUpdateAr[i](time, updatedVals);
  end;
end;

procedure TControllerMain.addSimListener( newListener: TUpdateSimEvent );
var i: integer;
begin
  i := length(self.FSimUpdateAr);
  setLength(self.FSimUpdateAr, i +1 );
  self.FSimUpdateAr[i] := newListener;
end;


  // Network has changed, notify any listeners
procedure TControllerMain.networkUpdated(sender: TObject);
var i: integer;
begin
  if sender is TNode then
    begin
     //console.log('TControllerMain.networkUpdated: Node conc changed');
     if self.sbmlmodel <> nil then
       self.networkUpdate := self.updateSpeciesNodeConc( sender as TNode )
     else self.networkUpdate := true;
    end
  else self.networkUpdate := true;      // after model updated, change to false.
  if self.networkUpdate then
    begin
    if length(self.FNetworkChangedAr) > 0 then
      begin
      for i := 0  to length(self.FNetworkChangedAr) -1 do
        self.FNetworkChangedAr[i](sender);
      end;

    end;

end;

procedure TControllerMain.addNetworkListener( newListener: TNetworkChangeEvent );
var i: integer;
begin
  i := length(self.FNetworkChangedAr);
  setLength(self.FNetworkChangedAr, i +1 );
  self.FNetworkChangedAr[i] := newListener;
end;

function TControllerMain.updateSpeciesNodeConc(node: TNode): boolean;
// returns 'true' : node val has not changed, something else with node has changed.
//                  Build new model and setup new simulation.
//         'false': node val changed, do not set networkUpdate flag. Keep same model.

// TODO: need to update params? some SBMLspecies are BC and const Species and are
//       treated as params for simulations.
var i: integer;
    aVal: double;
begin
  Result := true;
  for i := 0 to self.sbmlmodel.getSpeciesNumb -1 do
      begin
      if self.sbmlmodel.getSBMLspecies(i).getID = node.state.id then
        begin
        aVal := node.state.conc;
        if self.sbmlmodel.getSBMLspecies(i).isSetInitialAmount then
          begin
          if self.sbmlmodel.getSBMLspecies(i).getInitialAmount <> aVal then
            begin
            self.sbmlmodel.getSBMLspecies(i).setInitialAmount(aVal);
            self.sbmlmodel.resetS_Vals();
            Result := false; // updated model species val above
            end;
          end
        else
          begin
          if self.sbmlmodel.getSBMLspecies(i).getInitialConcentration <> aVal then
            begin
            self.sbmlmodel.getSBMLspecies(i).setInitialConcentration(aVal);
            self.sbmlmodel.resetS_Vals();
            Result := false;
            end;
          end;
        end;
      end;

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

procedure TControllerMain.SetStepSize(newStepSize: double);
begin
  if newStepSize > 0.0 then
    self.StepSize := newStepSize
  else self.stepSize := 0.1;
end;

function TControllerMain.IsOnline(): Boolean;
begin
  if runSim <> nil then
    Result := self.runSim.IsOnline
  else Result := false;
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

procedure TControllerMain.setCurrTime( newTime: double );
begin
  self.currTime := newTime;
  self.runSim.setTime( newTime );
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
                        // nInterval is msec
procedure TControllerMain.SetTimerInterval(nInterval: Integer);
begin
  //self.SetStepSize( nInterval );  // decouple stepsize from timer interval
  if self.runSim <> nil then
    self.runSim.SetTimerInterval(nInterval); // 100 = 100 msec
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

procedure TControllerMain.getVals(newTime: Double; newVals: TVarNameValList);
begin
  self.currTime := newTime;
  self.UpdateVals( newTime, newVals ); // pass on to listeners.
end;

procedure TControllerMain.loadSBML(sbmlStr: String );
var SBMLReader: TSBMLRead;
begin
  sbmlText := sbmlStr; // store the sbml model as text.
  if sbmlText <> '' then
    begin
    // check if sbmlmodel has stuff, if so, clear out....
    self.networkUpdate := false;
    self.createModel();
    self.currNetworkCtrl.network.clear; // clear out old network;
    SBMLReader := TSBMLRead.create(self.sbmlmodel, self.sbmlText );// Process text with libsbml.js
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

procedure TControllerMain.changeSimParameterVal(pos: Integer; newVal: Double );
begin
  self.runSim.updateP_Val( pos, newVal );
end;

procedure TControllerMain.resetSimParamValues(); // grab model p values and pass to sim.
var i: integer;
begin
  for i := 0 to self.sbmlmodel.getP_NameValAr.getNumPairs -1 do
    self.changeSimParameterVal( i, self.sbmlmodel.getP_Vals()[i] );
end;

procedure TControllerMain.resetSimSpeciesValues();
var curTime: double;
    i: integer;
    curParamVals: array of double;
begin
  curTime := self.getCurrTime;
  setLength( curParamVals,self.runSim.getP_Vals.getNumPairs );
  curParamVals := self.runSim.getP_Vals.getValAr;
  self.createSimulation();
  self.setCurrTime(curTime);
  for i := 0 to length(curParamVals) -1 do
    begin
      self.changeSimParameterVal(i,curParamVals[i]);
    end;

end;

procedure TControllerMain.writeSimData(fileName: string; data: TStrings);
var simData: string;
    i: integer;
begin
  simData := '';
  for i := 0 to data.count -1 do
    begin
      simData := simData + data[i] + sLineBreak;
    end;

   try
     Application.DownloadTextFile(simData, fileName);
   except
       on E: Exception do
          notifyUser(E.message);
   end;

end;


end.
