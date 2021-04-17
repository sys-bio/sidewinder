unit ufMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, Types, WEBLib.Graphics,
  WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Dialogs, WEBLib.ExtCtrls,
  WEBLib.WebCtrls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.Buttons, Vcl.Imaging.pngimage,
  Vcl.Graphics,
  uController, uNetworkCanvas, uNetwork, Vcl.TMSFNCTypes, Vcl.TMSFNCUtils,
  Vcl.TMSFNCGraphics,
  Vcl.TMSFNCGraphicsTypes, Vcl.TMSFNCCustomControl, Vcl.TMSFNCScrollBar,
  Vcl.TMSFNCButton, Vcl.TMSFNCToolBar,
  uNetworkTypes, Vcl.Imaging.pngimage, WEBLib.Lists, Vcl.Forms, SBML.helper,
  SBML.model, Simulation,
  ODE_FormatUtility, GraphP, Vcl.Menus, WEBLib.Menus, paramSelectForm,
  speciesSelectForm, plotLayout,
  paramSlider, paramSliderLayout;

const
  SLIDERPHEIGHT = 50; // Param Sliders WebPanel width/height

const
  PLOT_WIDTH_PERCENTAGE = 0.6;
  // This means a plot extends to 60% of the right panel width

type
  TspeciesAr = array of String;

  TMainForm = class(TWebForm)
    TopWPanel: TWebPanel;
    newNetworkBtn: TWebButton;
    networkSaveBtn: TWebButton;
    bottomWPanel: TWebPanel;
    btnRandomNetwork: TWebButton;
    btnAutoLayout: TWebButton;
    btnSampleLayout: TWebButton;
    btnAbout: TWebButton;
    btnCenter: TWebButton;
    onLineSimButton: TWebButton;
    WebPanel1: TWebPanel;
    xLbl: TWebLabel;
    yLbl: TWebLabel;
    zoomLbl: TWebLabel;
    zoomFactorLbl1: TWebLabel; // Displays simulation results
    SBMLmodelMemo: TWebMemo;
    WebTimer1: TWebTimer;
    rtLengthEdit1: TWebEdit;
    rtLabel1: TWebLabel;
    stepSizeLabel1: TWebLabel;
    stepSizeEdit1: TWebEdit;
    ZoomCntrlPanel: TWebPanel;
    zoomCtlLabel: TWebLabel;
    zoomTrackBar: TWebTrackBar;
    addPlotButton: TWebButton;
    paramSliderBtn: TWebButton;
    NetworkJSONOpenDialog: TWebOpenDialog;
    loadNetworkButton: TWebButton;
    SBMLOpenDialog: TWebOpenDialog;
    SBMLloadButton: TWebButton;
    WebPanel2: TWebPanel;
    LeftWPanel: TWebPanel;
    btnUniUni: TWebSpeedButton;
    btnUniBi: TWebSpeedButton;
    btnBiUni: TWebSpeedButton;
    btnBiBi: TWebSpeedButton;
    btnIdle: TWebSpeedButton;
    btnAddNode: TWebSpeedButton;
    pnlNodePanel: TWebPanel;
    nodeOutlineLabel: TWebLabel;
    nodeFillLabel: TWebLabel;
    editNodeLabel: TWebLabel;
    btnNodeOutlineColor: TWebColorPicker;
    editNodeId: TWebEdit;
    btnNodeFillColor: TWebColorPicker;
    RightWPanel: TWebPanel;
    simResultsMemo: TWebMemo;
    plotEditLB: TWebListBox;
    SliderEditLB: TWebListBox;
    pnlCenter: TWebPanel;
    networkPB1: TWebPaintBox;
    netDrawScrollBarVert: TTMSFNCScrollBar;
    netDrawScrollBarHoriz: TTMSFNCScrollBar;
    splitter: TWebSplitter;
    btnSimple: TWebButton;

    procedure btnUniUniClick(Sender: TObject);
    procedure btnBiBiClick(Sender: TObject);
    procedure btnBiUniClick(Sender: TObject);
    procedure btnUniBiClick(Sender: TObject);
    procedure btnIdleClick(Sender: TObject);
    procedure btnCenterClick(Sender: TObject);
    procedure btnRandomNetworkClick(Sender: TObject);
    procedure btnAutoLayoutClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure zoomTrackBarChange(Sender: TObject);
    procedure btnNodeOutlineColorClick(Sender: TObject);
    procedure btnNodeFillColorClick(Sender: TObject);
    procedure btnAddNodeClick(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure editNodeIdExit(Sender: TObject);
    procedure networkPB1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure networkPB1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure networkPB1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure networkPB1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure networkPB1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure networkPB1Paint(Sender: TObject);
    procedure WebFormResize(Sender: TObject);
    procedure netDrawScrollBarVertValueChanged(Sender: TObject; Value: Double);
    procedure netDrawScrollBarHorizValueChanged(Sender: TObject; Value: Double);

    procedure onLineSimButtonClick(Sender: TObject);
    procedure fillSpeciesArray();
    procedure setODEsolver();
    procedure plotsPBArPaint(Sender: TObject);
    procedure WebTimer1Timer(Sender: TObject);
    procedure addPlotButtonClick(Sender: TObject);
    procedure ParamSliderBtnClick(Sender: TObject);
    procedure loadNetworkButtonClick(Sender: TObject);
    procedure NetworkJSONOpenDialogChange(Sender: TObject);
    procedure NetworkJSONOpenDialogGetFileAsText(Sender: TObject;
      AFileIndex: Integer; AText: string);
    procedure SBMLloadButtonClick(Sender: TObject);
    procedure SBMLOpenDialogChange(Sender: TObject);
    procedure SBMLOpenDialogGetFileAsText(Sender: TObject; AFileIndex: Integer;
      AText: string);
    procedure btnClearClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure ParamSliderOnChange(Sender: TObject);
    // User changes value of parameter
    procedure plotEditLBClick(Sender: TObject);
    procedure SliderEditLBClick(Sender: TObject);
    procedure splitterMoved(Sender: TObject);
    procedure btnSimpleClick(Sender: TObject);
    // User clicks choice from plot edit list

  private
    numbPlots: Integer; // Number of plots displayed
    numbSliders: Integer; // Number of parameter sliders
    procedure GetSBMLInfo(); // Grab sbml model info.
    function setupSimulation(): String; // returns eq list as String.
    procedure startSimulation(odeEqs: String; odeFormat: TFormatODEs);
    procedure updateSimulation();
    procedure addPlot(); // Add a plot
    procedure selectPlotSpecies(plotnumb: Integer);
    procedure addParamSlider();
    procedure SetSliderParamValues(sn, paramForSlider: Integer);
    procedure selectParameter(sNumb: Integer); // Get parameter for slider
    procedure LoadJSONFile();
    procedure DeletePlot(plotn: Integer);
    procedure EditPlotList(plotn: Integer);
    // delete, change plot species, other added as needed using TWebListBox.
    procedure EditSliderList(sn: Integer);
    // delete, change param slider as needed using TWebListBox.
    procedure DeleteSlider(sn: Integer);
    procedure testAddingModel();
    // just a test for generating a model for the simulator, remove

  public
    network: TNetwork;
    controller: TController;
    networkCanvas: TNetworkCanvas;
    origin: TPointF;
    fileName: string;
    graphBitmap1: TBitmap; // get rid of
    maxYValueAr: array of Integer; // max Y screen dimension for each plot
    currentGeneration: Integer;
    plotSpeciesForm: TSpeciesSWForm;
    plotSpecies: array of TspeciesAr; // species to graph for each plot
    plotsPBAr: array of TWebPaintBox; // Plot paint boxes
    listOfPlots: array of TPlotGraph; // Plotting class for each plot
    xscaleHeight: Integer;
    { This is the space reserved for the x axis labelling, remove }
    xscaleHeightAr: array of Integer;
    // This is the space reserved for the x axis labelling of each plot
    sliderParamForm: TParamSliderSForm;
    // Pop up form to choose parameter for slider.
    sliderParamAr: array of Integer;
    // holds parameter array index of parameter (p_vals) for each slider
    sliderPanelAr: array of TWebPanel; // Holds parameter sliders
    sliderPHighAr: array of Double; // High value for parameter slider
    sliderPLowAr: array of Double; // Low value for parameter slider
    sliderPTBarAr: array of TWebTrackBar;
    sliderPHLabelAr: array of TWebLabel; // Displays sliderPHighAr
    sliderPLLabelAr: array of TWebLabel; // Displays sliderPLowAr
    sliderPTBLabelAr: array of TWebLabel;
    // Displays slider param name and current value
    paramUpdated: Boolean; // true if a parameter has been updated.

    function ScreenToWorld(X, Y: Double): TPointF; // Network drawing panel
    function WorldToScreen(wx: Integer): Integer; // Network drawing panel
    procedure PingSBMLLoaded(); // Notify when done loading libSBML
    procedure getVals(newTime: Double; newVals: array of Double);
    // Get new values (species amt) from simulation run
    procedure processScan(t_new: Double; y_new: array of Double);

  end;

var
  mainForm: TMainForm;
  sbml_Text: String;
  sbmlmodel: SBMLhelpClass;
  numbRxns: Integer;
  sbmlModelLoaded: Boolean;
  solverUsed: ODEsolver;
  runTime: Double; // Total runtime of simulation (sec)
  runSim: TSimulationJS;
  stepSize: Double; // (msec) (integration step)
  pixelStep: Integer; // get rid of...
  pixelStepAr: array of Integer; // pixel equiv of time (integration) step
  currTime: Double;
  s_Vals: array of Double; // one to one correlation: s_Vals[n] = s_name's[n]
  s_names: array of String; // Use species ID as name
  p_Vals: array of Double; // Includes compartments
  p_names: array of String;
  online: Boolean; // Simulation running
  ODEready: Boolean; // TRUE: ODE solver is setup.

  spSelectform: TSpeciesSWForm; // display speciecs select to plot radio group

implementation

{$R *.dfm}

Uses uGraphUtils, uCreateNetworks, uLayout, uTestModel;

procedure TMainForm.addPlotButtonClick(Sender: TObject);
begin
  // Make runtime, stepsize, simulation buttons visible
  // TODO: need to check if model ready for simulation first.
  self.numbPlots := self.numbPlots + 1;
  rtLabel1.visible := true;
  rtLengthEdit1.visible := true;
  stepSizeLabel1.visible := true;
  stepSizeEdit1.visible := true;
  self.selectPlotSpecies(self.numbPlots);
end;

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  Showmessage('Version 0.1');
end;

procedure TMainForm.btnAddNodeClick(Sender: TObject);
begin
  controller.setAddNodeStatus;
end;

procedure TMainForm.btnAutoLayoutClick(Sender: TObject);
begin
  // showmessage (inttostr (networkPB1.Width) + ', ' + inttostr (networkPB1.Width));
  fruchterman_reingold(network, networkPB1.width, networkPB1.Height, 600, nil);
  network.centerNetwork(networkPB1.width, networkPB1.Height);
  networkPB1.Invalidate;
end;

procedure TMainForm.btnBiBiClick(Sender: TObject);
begin
  controller.setAddBiBiReaction;
end;

procedure TMainForm.btnBiUniClick(Sender: TObject);
begin
  controller.setAddBiUniReaction;
end;

procedure TMainForm.btnCenterClick(Sender: TObject);
var
  i: Integer;
begin
  network.centerNetwork(networkPB1.width, networkPB1.Height);
  // lb.Clear;
  for i := 0 to length(network.nodes) - 1 do
    begin
      // lb.Lines.Add (inttostr (trunc (network.nodes[i].state.x)) + ', ' + inttostr (trunc (network.nodes[i].state.x)));
    end;
  networkPB1.Invalidate;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  network.Clear;
  networkPB1.Invalidate;
  // self.testAddingModel();
  // self.GetSBMLInfo();
end;

procedure TMainForm.btnDrawClick(Sender: TObject);
var
  n1, n2, n3, n4: TNode;
begin
  n1 := controller.addNode('node1', 60, 200);
  n2 := controller.addNode('node2', 270, 270);
  n3 := controller.addNode('node3', 540, 80);
  n4 := controller.addNode('node4', 400, 500);

  controller.addReaction('r1', n1, n2);
  controller.addReaction('r2', n2, n3);
  controller.addReaction('r3', n3, n4);
  controller.addReaction('r4', n4, n2);

  networkPB1.Invalidate;
end;

procedure TMainForm.btnIdleClick(Sender: TObject);
begin
  controller.setSelectStatus;
end;

procedure TMainForm.btnNodeFillColorClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to length(network.nodes) - 1 do
    if network.nodes[i].selected then
      begin
        network.nodes[i].state.fillColor := btnNodeFillColor.color;
        networkPB1.Invalidate;
      end;
end;

procedure TMainForm.btnNodeOutlineColorClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to length(network.nodes) - 1 do
    if network.nodes[i].selected then
      begin
        network.nodes[i].state.outlineColor := btnNodeOutlineColor.color;
        networkPB1.Invalidate;
      end;
end;

procedure TMainForm.btnRandomNetworkClick(Sender: TObject);
var
  nNodes: Integer;
  probability: Double;
  n1, n2, n3, n4: TNode;
begin
  network.Clear;
  nNodes := 12;
  probability := 0.9;
  createRandomNetwork(network, nNodes, probability);

  networkPB1.Invalidate;
end;

procedure TMainForm.btnSimpleClick(Sender: TObject);
var s : string;
begin
  s := getTestModel;
  SBMLmodelMemo.Lines.Text := s;
  SBMLmodelMemo.visible := true;
  sbml_Text := s; // store the sbml model as text.
  // Check if sbmlmodel already created, if so, destroy before creating ?
  sbmlmodel := SBMLhelpClass.create();
  sbmlmodel.OnPing := PingSBMLLoaded; // *** Register the callback function
  sbmlmodel.readSBML(s); // Process text with libsbml.js
end;

procedure TMainForm.btnUniBiClick(Sender: TObject);
begin
  controller.setAddUniBiReaction;
end;

procedure TMainForm.btnUniUniClick(Sender: TObject);
begin
  controller.setAddUniUniReaction;
end;

procedure TMainForm.editNodeIdExit(Sender: TObject);
begin
  controller.setNodeId(editNodeId.Text);
  networkPB1.Invalidate;
end;

procedure TMainForm.SBMLloadButtonClick(Sender: TObject);
begin
  self.SBMLOpenDialog.execute();
end;

procedure TMainForm.SBMLOpenDialogChange(Sender: TObject);
begin
  if SBMLOpenDialog.Files.Count > 0 then
    SBMLOpenDialog.Files[0].GetFileAsText;
end;

procedure TMainForm.SBMLOpenDialogGetFileAsText(Sender: TObject;
  AFileIndex: Integer; AText: string);
begin
  SBMLmodelMemo.Lines.Text := AText;
  SBMLmodelMemo.visible := true;
  sbml_Text := AText; // store the sbml model as text.
  // Check if sbmlmodel already created, if so, destroy before creating ?
  sbmlmodel := SBMLhelpClass.create();
  sbmlmodel.OnPing := PingSBMLLoaded; // *** Register the callback function
  sbmlmodel.readSBML(AText); // Process text with libsbml.js
end;

procedure TMainForm.onLineSimButtonClick(Sender: TObject);
var
  i: Integer;
  yScaleWidth : integer;
begin
  if online = false then
    begin
      online := true;
      onLineSimButton.font.color := clgreen;
      onLineSimButton.ElementClassName := 'btn btn-success btn-sm';
      onLineSimButton.caption := 'Simulation: Online';
      simResultsMemo.visible := true;

      try
        runTime := strToFloat(rtLengthEdit1.Text);
      except
        on Exception: EConvertError do
          begin
            runTime := 10;
            self.rtLengthEdit1.Text := FloatToStr(runTime);
          end;
      end;
      if stepSize = 0 then
        stepSize := 0.1;

      for i := 0 to length (listOfPlots) - 1 do
          begin
          //listOfPlots[i].initGraph(0, 200, 0, maxYValueAr[i], 0,
          //  graphBitmapAr[i].width, 0, graphBitmapAr[i].Height,
          //  xscaleHeightAr[i], runTime, stepSize);

          yScaleWidth := 16; // Gap between the left edge and y axis

          listOfPlots[i].initGraph(0, 200, 0, 10, // The 10 is the max Y value in world coords
            0, self.plotsPBAr[i].width, 0, self.plotsPBAr[i].height,
            xscaleHeightAr[i], yscaleWidth, runTime, stepSize);

          // total steps: runTime/stepSize
          // Max viewable steps is PlotWebPB.width (1 pixel per step).
          pixelStepAr[i] := 0;
          if runTime / stepSize < self.plotsPBAr[i].width then
             pixelStepAr[i] := round(self.plotsPBAr[i].width * stepSize / runTime)
          else
             pixelStepAr[i] := 1;
          end;
      WebTimer1.enabled := true;
      self.updateSimulation();
    end
  else
    begin
      online := false;
      WebTimer1.enabled := false;
      onLineSimButton.font.color := clred;
      onLineSimButton.ElementClassName := 'btn btn-danger btn-sm';
      onLineSimButton.caption := 'Simulation: Offline';
    end;
end;

// Grab SBML model information when notified:
procedure TMainForm.PingSBMLLoaded();
begin
  sbmlModelLoaded := true;
  GetSBMLInfo();
end;

procedure TMainForm.plotEditLBClick(Sender: TObject);
begin
  if self.plotEditLB.ItemIndex = 0 then // change species to plot
    begin
      self.selectPlotSpecies(self.plotEditLB.tag);
    end;
  if self.plotEditLB.ItemIndex = 1 then // delete plot
    begin
      self.DeletePlot(self.plotEditLB.tag);
    end;
  // else ShowMessage('Cancel');
  self.plotEditLB.tag := 0;
  self.plotEditLB.visible := false;
  self.plotEditLB.Top := 40; // default
end;

procedure TMainForm.plotsPBArPaint(Sender: TObject);
var
  plot_i: Integer;
begin
  plot_i := (Sender as TWebPaintBox).tag;
  plotsPBAr[plot_i - 1].canvas.draw(0, 0, listOfPlots[plot_i - 1].bitmap);
end;

procedure TMainForm.networkPB1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Handle Ctrl-keys here
  if Key = VK_DELETE then
    begin
      controller.prepareUndo;
      controller.deleteSelectedItems;
      networkPB1.Invalidate;
      exit;
    end;

  if (Shift = [ssCtrl]) and (Upcase(Char(Key)) = 'Z') then
    begin
      controller.Undo;
      networkPB1.Invalidate;
    end;
end;

procedure TMainForm.networkPB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v: TPointF;
begin
  v := ScreenToWorld(X, Y);

  controller.OnMouseDown(Sender, Button, Shift, v.X, v.Y);
  networkPB1.Invalidate;
  if controller.selectedNode <> -1 then
    begin
      editNodeId.Text := controller.network.nodes
        [controller.selectedNode].state.id;
      pnlNodePanel.visible := true;
    end
  else
    pnlNodePanel.visible := false;
end;

procedure TMainForm.networkPB1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  v: TPointF;
begin
  v := ScreenToWorld(X, Y);

  controller.OnMouseMove(Sender, Shift, v.X, v.Y);
  networkPB1.Invalidate;
  xLbl.caption := 'X: ' + inttostr(trunc(v.X));
  yLbl.caption := 'Y: ' + inttostr(trunc(v.Y));
end;

procedure TMainForm.networkPB1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v: TPointF;
begin
  v := ScreenToWorld(X, Y);

  controller.OnMouseUp(Sender, Button, Shift, v.X, v.Y);
  networkPB1.Invalidate;
end;

procedure TMainForm.networkPB1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta < 0 then
    origin.Y := origin.Y + 20 // WheelDelta
  else
    origin.Y := origin.Y - 20; // WheelDelta

  if origin.Y < 0 then
    origin.Y := 0;

  Handled := true;
  networkPB1.Invalidate;
end;

procedure TMainForm.networkPB1Paint(Sender: TObject);
begin
  networkCanvas.paint;
  networkPB1.canvas.draw(0, 0, networkCanvas.bitmap);
end;

procedure TMainForm.ParamSliderBtnClick(Sender: TObject);
var
  i: Integer;
  odeFormat: TFormatODEs;
begin
  // TODO: Check if already 10 sliders, if so then showmessage( 'Only 10 parameter sliders allowed, edit existing one');
  // Fill out parameter list for model, if not done already:
  if length(p_Vals) < 1 then
    begin
      odeFormat := TFormatODEs.create(sbmlmodel);
      SetLength(p_Vals, length(odeFormat.get_pVals())); // Keep params for later
      SetLength(p_names, length(p_Vals));
      for i := 0 to length(odeFormat.get_pVals()) - 1 do
        begin
          p_Vals[i] := odeFormat.get_pVals[i];
          p_names[i] := odeFormat.get_paramsStr[i];
        end;
    end;
  self.selectParameter(length(sliderParamAr));
end;

procedure TMainForm.ParamSliderOnChange(Sender: TObject);
var
  i, p: Integer;
begin
  if Sender is TWebTrackBar then
    begin
      i := TWebTrackBar(Sender).tag;
      self.paramUpdated := true;
      p := self.sliderParamAr[i];
      // get slider parameter position in p_vals array
      p_Vals[p] := self.sliderPTBarAr[i].Position * 0.01 *
        (sliderPHighAr[i] - sliderPLowAr[i]); // Fix ?
      self.sliderPTBLabelAr[i].caption := p_names[self.sliderParamAr[i]] + ': '
        + FloatToStr(p_Vals[self.sliderParamAr[i]]);
      // console.log(' Updated param val: ',p_names[p],': ',p_vals[p]);
    end;

end;

procedure TMainForm.zoomTrackBarChange(Sender: TObject);
begin
  networkCanvas.scalingFactor := zoomTrackBar.Position / 10;
  networkPB1.Invalidate;
  zoomFactorLbl1.caption := FloatToStr(zoomTrackBar.Position / 10);
end;

function TMainForm.ScreenToWorld(X, Y: Double): TPointF;
begin
  result.X := (X + origin.X) / (zoomTrackBar.Position / 10);
  result.Y := (Y + origin.Y) / (zoomTrackBar.Position / 10);
end;

procedure TMainForm.netDrawScrollBarHorizValueChanged(Sender: TObject;
  Value: Double);
begin
  origin.X := Value;
  networkCanvas.origin.X := Value;
  networkPB1.Invalidate;
end;

procedure TMainForm.netDrawScrollBarVertValueChanged(Sender: TObject;
  Value: Double);
begin
  origin.Y := Value;
  networkCanvas.origin.Y := Value;
  networkPB1.Invalidate;
end;

procedure TMainForm.WebFormCreate(Sender: TObject);
begin
  // lb.clear;
  self.numbPlots := 0;
  self.numbSliders := 0;
  self.zoomTrackBar.left := 20;
  self.zoomTrackBar.Position := 10;
  origin.X := 0.0;
  origin.Y := 0.0;
  sbmlModelLoaded := false;
  online := false;
  onLineSimButton.font.color := clred;
  onLineSimButton.caption := 'Simulation: Offline';
  self.setODEsolver;
  stepSize := 0.1; // 100 msec
  // xscaleHeight  := round(0.15* plotPB1.Height);   // make %15 of total height  get rid of
  currentGeneration := 0;
  network := TNetwork.create('testNetwork');
  controller := TController.create(network);
  networkCanvas := TNetworkCanvas.create(network);
  controller.networkCanvas := networkCanvas;
  networkCanvas.bitmap.Height := networkPB1.Height;
  networkCanvas.bitmap.width := networkPB1.width;
  LeftWPanel.color := clWhite;

end;

procedure TMainForm.WebFormResize(Sender: TObject);
begin
  if networkCanvas = nil then // Resize may be called before Create
    begin
      network := TNetwork.create('testNetwork');
      controller := TController.create(network);
      networkCanvas := TNetworkCanvas.create(network);
    end;
  networkCanvas.bitmap.Height := networkPB1.Height;
  networkCanvas.bitmap.width := networkPB1.width;
  networkPB1.Invalidate;
end;

procedure TMainForm.WebTimer1Timer(Sender: TObject);
begin
  self.updateSimulation();
  if currTime > runTime then
    WebTimer1.enabled := false;
end;

function TMainForm.WorldToScreen(wx: Integer): Integer;
begin
  raise Exception.create('WorldtoScreen needs to be updated');
  result := trunc(wx * zoomTrackBar.Position / 10);
end;

// Add more model info as needed here:
procedure TMainForm.GetSBMLInfo();
var
  i: Integer;
  parInitval: array of Double; // param vals for ODE solver.
  varInitval: array of Double; // init values for ODE solver
  outputList: TStringList;
begin
  numbRxns := sbmlmodel.getNumReactions();
  paramSliderBtn.visible := true;
  // rxnsArray:= Copy(sbmlmodel.getReactions(),0,numbRxns);
  self.fillSpeciesArray();
end;

function TMainForm.setupSimulation(): String;
var
  simRTStr: String; // Output string for TWebMemo
  i: Integer;
  odeEqs: String;
  odeFormat: TFormatODEs;
begin
  ODEready := false;

  try
    self.WebTimer1.Interval := strtoint(stepSizeEdit1.Text);
    // Trailing blanks are not supported
  except
    on Exception: EConvertError do
      begin
        self.WebTimer1.Interval := 100; // default
        self.stepSizeEdit1.Text := inttostr(self.WebTimer1.Interval);
      end;
  end;

  stepSize := self.WebTimer1.Interval * 0.001; // 1 sec = 1000 msec
  simResultsMemo.Lines.Clear();
  odeFormat := TFormatODEs.create(sbmlmodel);

  if length(p_Vals) < 1 then
    begin
      SetLength(p_Vals, length(odeFormat.get_pVals())); // Keep params for later
      SetLength(p_names, length(p_Vals));
      for i := 0 to length(odeFormat.get_pVals()) - 1 do
        begin
          p_Vals[i] := odeFormat.get_pVals[i];
          p_names[i] := odeFormat.get_paramsStr[i];
        end;
    end;

  // Run Simulation using info from odeFormat:
  odeFormat.buildFinalEqSet();
  simRTStr := ' Time (s) '; // generate coloumn headers:
  SetLength(s_names, length(odeFormat.get_speciesStrAr()));
  for i := 0 to length(odeFormat.get_speciesStrAr()) - 1 do
    begin
      s_names[i] := odeFormat.get_speciesStrAr()[i]; // grab species names
      simRTStr := simRTStr + ', ' + s_names[i];
    end;
  simResultsMemo.Lines.Add(simRTStr);

  if solverUsed = LSODAS then
    self.startSimulation(odeFormat.getODEeqSet2(), odeFormat)
  else
    self.startSimulation(odeFormat.getODEeqSet(), odeFormat);
  // Result:= odeFormat.getODEeqSet();
end;

procedure TMainForm.SliderEditLBClick(Sender: TObject);
begin
  if self.SliderEditLB.ItemIndex = 0 then // change param for slider
    begin
      self.selectParameter(self.SliderEditLB.tag);
    end;
  if self.SliderEditLB.ItemIndex = 1 then // delete slider
    begin
      self.DeleteSlider(self.SliderEditLB.tag);
    end;
  // else ShowMessage('Cancel');
  self.SliderEditLB.tag := 0;
  self.SliderEditLB.visible := false;
  self.SliderEditLB.Top := 40; // default
end;

procedure TMainForm.splitterMoved(Sender: TObject);
begin
  networkCanvas.bitmap.Height := networkPB1.Height;
  networkCanvas.bitmap.width := networkPB1.width;
  networkPB1.Invalidate;
end;

procedure TMainForm.startSimulation(odeEqs: String; odeFormat: TFormatODEs);
begin
  runSim := TSimulationJS.create(runTime, stepSize, length(s_Vals), length(odeFormat.get_pVals()), odeEqs, solverUsed);
  runSim.OnUpdate := getVals; // register callback function.
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

procedure TMainForm.updateSimulation();
begin
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

// Get new values (species amt) from simulation run (ODE integrator)
procedure TMainForm.getVals(newTime: Double; newVals: array of Double);
var
  dataStr: String;
  i: Integer;
begin
  // Update table of data;
  dataStr := '';
  dataStr := floatToStrf(newTime, ffFixed, 4, 4) + ', ';
  SetLength(s_Vals, length(newVals));
  // TODO: should just be a check. s_Vals length set earlier?
  for i := 0 to length(newVals) - 1 do
    begin
      dataStr := dataStr + floatToStrf(newVals[i], ffExponent, 6, 2) + ', ';
      s_Vals[i] := newVals[i];
    end;
  simResultsMemo.Lines.Add(dataStr);
  processScan(newTime, s_Vals); // <------------
  currTime := newTime;
end;

procedure TMainForm.loadNetworkButtonClick(Sender: TObject);
begin
  self.NetworkJSONOpenDialog.execute();
end;

procedure TMainForm.mnuSaveClick(Sender: TObject);
var
  jstr, fileName: string;
begin
  fileName := InputBox('Save model to the downloads directory',
    'Enter File Name:', 'jstr.json');
  if fileName <> '' then
    begin
      jstr := controller.network.convertToJSON();
      Application.DownloadTextFile(jstr, fileName);
    end
  else
    Showmessage('Save cancelled');
end;

procedure TMainForm.mnuUndoClick(Sender: TObject);
begin
  controller.Undo;
  networkPB1.Invalidate;
end;

procedure TMainForm.NetworkJSONOpenDialogChange(Sender: TObject);
begin
  self.LoadJSONFile;
end;

procedure TMainForm.NetworkJSONOpenDialogGetFileAsText(Sender: TObject;
  AFileIndex: Integer; AText: string);
begin
  try
    controller.loadModel(AText);
  except
    on E: Exception do
      Showmessage(E.message);
  end;
  networkPB1.Invalidate;
end;

procedure TMainForm.LoadJSONFile;
var
  FFileName: string;
begin
  if Assigned(NetworkJSONOpenDialog.Files[0]) then
    begin
      NetworkJSONOpenDialog.Files[0].GetFileAsText;
      FFileName := NetworkJSONOpenDialog.Files[0].Name;
    end;
end;

procedure TMainForm.processScan(t_new: Double; y_new: array of Double);
var
  Y: array of Double;
  i: Integer;
  plot_y: array of Boolean;
  j: Integer;
begin
  inc(currentGeneration); // currentGeneration is 'time' (pixel number)
  SetLength(plot_y, length(y_new));
  for j := 0 to length(plotSpecies) - 1 do // cycle through all of the plots
    begin
      for i := 0 to length(y_new) - 1 do
        begin
          if plotSpecies[j][i] = '' then
            plot_y[i] := false
          else
            plot_y[i] := true;
        end;
      // Dynamically draw plots:
      listOfPlots[j].addPoint(currentGeneration, y_new,  plot_y, true, currTime);
      self.plotsPBAr[j].Canvas.draw (0, 0, listOfPlots[j].bitmap);
    end;
end;

procedure TMainForm.selectPlotSpecies(plotnumb: Integer);
// plot number to be added or modified

// Pass back to caller after closing popup:
  procedure AfterShowModal(AValue: TModalResult);
  var
    i: Integer;
    addingPlot: Boolean;
  begin
    if length(plotSpecies) < plotnumb then
      begin
        // Add a plot with species list
        addingPlot := true;
        SetLength(plotSpecies, plotnumb);
        SetLength(plotSpecies[plotnumb - 1], length(s_Vals));
      end
    else
      addingPlot := false;

    for i := 0 to plotSpeciesForm.SpPlotCG.Items.Count - 1 do
      begin
        if plotSpeciesForm.SpPlotCG.checked[i] then
          begin
            plotSpecies[plotnumb - 1][i] := plotSpeciesForm.SpPlotCG.Items[i];
          end
        else
          plotSpecies[plotnumb - 1][i] := '';
      end;
    if addingPlot then
      self.addPlot(); // <-- Add dynamically created plot at this point
  end;

// async called OnCreate for TSpeciesSWForm
  procedure AfterCreate(AForm: TObject);
  begin
    (AForm as TSpeciesSWForm).speciesList := s_names;
    (AForm as TSpeciesSWForm).fillSpeciesCG();
  end;

begin
  plotSpeciesForm := TSpeciesSWForm.CreateNew(@AfterCreate);
  plotSpeciesForm.Popup := true;
  plotSpeciesForm.PopupOpacity := 0.3;
  plotSpeciesForm.Border := fbDialogSizeable;
  plotSpeciesForm.caption := 'Species to plot:';

  plotSpeciesForm.ShowModal(@AfterShowModal);
end;

procedure TMainForm.addPlot(); // Add a plot

  procedure plotOnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    i: Integer; // grab plot which received event
  begin
    if (Button = mbRight) or (Button = mbLeft) then // Both for now.
      begin
        if Sender is TWebPaintBox then
          begin
            i := TWebPaintBox(Sender).tag;
            // assume only plot paintboxes in right panel.
            // ShowMessage('Paint box sent mouse msg:  '+ inttostr(i));
            self.EditPlotList(i);
          end;
      end;
  end;

begin
  setLength(self.xscaleHeightAr, length(self.xscaleHeightAr) + 1);
  setLength(self.plotsPBAr, length(self.plotsPBAr) + 1);
  setLength(self.maxYValueAr, length(self.maxYValueAr) + 1); // Screen Y height
  setLength(pixelStepAr, length(pixelStepAr) + 1);
  setLength(self.listOfPlots, length(listOfPlots) + 1); // ***

  self.listOfPlots[self.numbPlots - 1] := TPlotGraph.create;
  self.plotsPBAr[self.numbPlots - 1] := TWebPaintBox.create(self.RightWPanel);
  self.plotsPBAr[self.numbPlots - 1].parent := self.RightWPanel;
  self.plotsPBAr[self.numbPlots - 1].OnPaint := plotsPBArPaint;
  self.plotsPBAr[self.numbPlots - 1].OnMouseDown := plotOnMouseDown;

  configPbPlot(self.numbPlots,
    trunc(self.RightWPanel.width * PLOT_WIDTH_PERCENTAGE), self.RightWPanel.Height, self.plotsPBAr);

  self.xscaleHeightAr[self.numbPlots - 1] := round(0.15 * self.plotsPBAr[self.numbPlots - 1].Height);
  // make %15 of total height
  self.maxYValueAr[self.numbPlots - 1] := self.plotsPBAr[self.numbPlots - 1].Height; // // PaintBox dimension
  self.plotsPBAr[self.numbPlots - 1].Invalidate;
end;

// TODO still more cleanup .. reorder plots if middle one deleted ?
// Look at plotsPBAr[].Top value to determine order ? Still look at 'Tag' value for ordering?
procedure TMainForm.DeletePlot(plotn: Integer);
begin
  self.plotsPBAr[plotn - 1].free;
  delete(self.plotsPBAr, (plotn - 1), 1);
  delete(self.listOfPlots, (plotn - 1), 1);
  delete(self.maxYValueAr, (plotn - 1), 1);
  delete(self.plotSpecies, (plotn - 1), 1);
  delete(pixelStepAr, (plotn - 1), 1);
  delete(self.xscaleHeightAr, (plotn - 1), 1);
  self.numbPlots := self.numbPlots - 1;
  self.RightWPanel.Invalidate;
end;

procedure TMainForm.EditPlotList(plotn: Integer);
var
  plotXposition, plotYposition: Integer;
  editList: TStringList;
begin
  plotXposition := 40;
  plotYposition := self.plotsPBAr[plotn - 1].Top + 10;
  editList := TStringList.create();
  editList.Add('Change plot species.');
  editList.Add('Delete plot.');
  editList.Add('Cancel');
  self.plotEditLB.Items := editList;
  self.plotEditLB.Top := plotYposition;
  self.plotEditLB.left := plotXposition;
  self.plotEditLB.tag := plotn;
  self.plotEditLB.bringToFront;
  self.plotEditLB.visible := true;

end;

procedure TMainForm.EditSliderList(sn: Integer);
// delete, change param slider as needed.
var
  sliderXposition, sliderYposition: Integer;
  editList: TStringList;
begin
  // console.log('Edit Slider: slider #: ',sn);
  sliderXposition := 424;
  sliderYposition := self.sliderPanelAr[sn].Top + 10;
  editList := TStringList.create();
  editList.Add('Change slider parameter.');
  editList.Add('Delete slider.');
  editList.Add('Cancel');
  self.SliderEditLB.Items := editList;
  self.SliderEditLB.Top := sliderYposition;
  self.SliderEditLB.left := sliderXposition;
  self.SliderEditLB.tag := sn;
  self.SliderEditLB.bringToFront;
  self.SliderEditLB.visible := true;

end;

// TODO still more cleanup .. reorder sliders if middle one deleted ?
procedure TMainForm.DeleteSlider(sn: Integer);
begin
  // console.log('Delete Slider: slider #: ',sn);
  self.sliderPanelAr[sn].free;
  delete(self.sliderPanelAr, (sn), 1);
  delete(self.sliderPHLabelAr, (sn), 1);
  delete(self.sliderPLLabelAr, (sn), 1);
  delete(self.sliderPTBLabelAr, (sn), 1);
  delete(self.sliderPTBarAr, (sn), 1);
  delete(self.sliderPHighAr, (sn), 1);
  delete(self.sliderPLowAr, (sn), 1);
  self.RightWPanel.Invalidate;
end;

procedure TMainForm.setODEsolver();
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

// Get initial vals for Species from SBML model
// TODO: add code for nonSBML models.
procedure TMainForm.fillSpeciesArray();
var
  i: Integer;
begin
  if sbmlmodel.getSpeciesNumb() > 0 then // TODO: chk if sbml model first
    begin
      SetLength(s_Vals, sbmlmodel.getSpeciesNumb());
      SetLength(s_names, length(s_Vals));
      for i := 0 to sbmlmodel.getSpeciesNumb() - 1 do
        begin
          if sbmlmodel.getSBMLspecies(i).isSetInitialAmount() then
            s_Vals[i] := sbmlmodel.getSBMLspecies(i).getInitialAmount()
          else if sbmlmodel.getSBMLspecies(i).isSetInitialConcentration() then
            s_Vals[i] := sbmlmodel.getSBMLspecies(i).getInitialConcentration();
          s_names[i] := sbmlmodel.getSBMLspecies(i).getID();
          // Use species ID as name
        end;
    end;
end;

// Select parameter to use for slider
procedure TMainForm.selectParameter(sNumb: Integer);
var
  paramIndex: Integer; // param chosen in radiogroup
  // Pass back to caller after closing popup:
  procedure AfterShowModal(AValue: TModalResult);
  var
    i, pSliderNumb: Integer;
    addingSlider: Boolean;
  begin
    if length(self.sliderParamAr) < (sNumb + 1) then
      begin
        pSliderNumb := length(self.sliderParamAr); // same as self.numbParams
        // Add a slider
        addingSlider := true;
        SetLength(self.sliderParamAr, pSliderNumb + 1);
      end
    else
      addingSlider := false;
    console.log('Param index picked (chosenParam): ',
      sliderParamForm.chosenParam);
    self.sliderParamAr[sNumb] := sliderParamForm.chosenParam;
    if addingSlider then
      self.addParamSlider() // <-- Add dynamically created slider
    else
      self.SetSliderParamValues(sNumb, self.sliderParamAr[sNumb]);
    // update slider with new parameter:
  end;

// async called OnCreate for TParamSliderSForm
  procedure AfterCreate(AForm: TObject);
  begin
    (AForm as TParamSliderSForm).paramList := p_names;
    (AForm as TParamSliderSForm).fillParamRG();
  end;

begin
  sliderParamForm := TParamSliderSForm.CreateNew(@AfterCreate);
  sliderParamForm.Popup := true;
  sliderParamForm.PopupOpacity := 0.3;
  sliderParamForm.Border := fbDialogSizeable;
  sliderParamForm.caption := 'Parameter for slider:';
  sliderParamForm.ShowModal(@AfterShowModal);
end;

procedure TMainForm.addParamSlider();
// default TBar range: 0 to initVal*10
  procedure SliderOnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  var
    i: Integer; // grab plot which received event
  begin
    if (Button = mbRight) or (Button = mbLeft) then // Both for now.
      begin
        if Sender is TWebPanel then
          begin
            i := TWebPanel(Sender).tag;
            // assume only slider TWebpanel in right panel.
            // ShowMessage('WebPanel sent mouse msg:  '+ inttostr(i));
            self.EditSliderList(i);
          end;
      end;
  end;

// ###
var
  i, sliderTBarWidth, sliderPanelLeft, sliderPanelWidth: Integer;
begin
  // Left most position of the panel that holds the slider
  sliderPanelWidth := trunc((1 - PLOT_WIDTH_PERCENTAGE) * RightWPanel.width);
  sliderPanelLeft := (RightWPanel.width - sliderPanelWidth) - 6;
  // Width of the slider inside the panel
  // sliderTBarWidth:= trunc (0.8*sliderPanelWidth); // 90% of the panel

  i := length(self.sliderPanelAr);
  // array index for current slider to be added.
  SetLength(self.sliderPanelAr, i + 1);
  SetLength(self.sliderPHighAr, i + 1);
  SetLength(self.sliderPLowAr, i + 1);
  SetLength(self.sliderPTBarAr, i + 1);
  SetLength(self.sliderPHLabelAr, i + 1);
  SetLength(self.sliderPLLabelAr, i + 1);
  SetLength(self.sliderPTBLabelAr, i + 1);

  self.sliderPanelAr[i] := TWebPanel.create(self.RightWPanel);
  self.sliderPanelAr[i].parent := self.RightWPanel;
  self.sliderPanelAr[i].OnMouseDown := SliderOnMouseDown;

  configPSliderPanel(i, sliderPanelLeft, sliderPanelWidth, SLIDERPHEIGHT,
    self.sliderPanelAr);

  self.sliderPanelAr[i].tag := i; // keep track of slider index number.
  self.sliderPTBarAr[i] := TWebTrackBar.create(self.sliderPanelAr[i]);
  self.sliderPTBarAr[i].parent := self.sliderPanelAr[i];
  self.sliderPTBarAr[i].OnChange := self.ParamSliderOnChange;
  self.sliderPHLabelAr[i] := TWebLabel.create(self.sliderPanelAr[i]);
  self.sliderPHLabelAr[i].parent := self.sliderPanelAr[i];
  self.sliderPLLabelAr[i] := TWebLabel.create(self.sliderPanelAr[i]);
  self.sliderPLLabelAr[i].parent := self.sliderPanelAr[i];
  self.sliderPTBLabelAr[i] := TWebLabel.create(self.sliderPanelAr[i]);
  self.sliderPTBLabelAr[i].parent := self.sliderPanelAr[i];
  self.SetSliderParamValues(i, self.sliderParamAr[i]);

  configPSliderTBar(i, sliderPanelWidth, self.sliderPTBarAr,
    self.sliderPHLabelAr, self.sliderPLLabelAr, self.sliderPTBLabelAr);
end;

// Called when adding or updating a param slider. sn = slider number
procedure TMainForm.SetSliderParamValues(sn, paramForSlider: Integer);
var
  rangeMult: Integer;
begin
  rangeMult := 10; // default.
  self.sliderPTBLabelAr[sn].caption := p_names[self.sliderParamAr[sn]] + ': ' +
    FloatToStr(p_Vals[self.sliderParamAr[sn]]);
  self.sliderPLowAr[sn] := 0;
  self.sliderPLLabelAr[sn].caption := FloatToStr(self.sliderPLowAr[sn]);
  // console.log('SetSliderParamValues sn...',sn,' : ',p_names[self.sliderParamAr[sn]]);

  self.sliderPTBarAr[sn].Min := 0;
  self.sliderPTBarAr[sn].Position := trunc((1 / rangeMult) * 100);
  self.sliderPTBarAr[sn].Max := 100;
  if p_Vals[self.sliderParamAr[sn]] > 0 then
    begin
      self.sliderPHLabelAr[sn].caption :=
        FloatToStr(p_Vals[self.sliderParamAr[sn]] * rangeMult);
      self.sliderPHighAr[sn] := p_Vals[self.sliderParamAr[sn]] * rangeMult;
    end
  else
    begin
      self.sliderPHLabelAr[sn].caption := FloatToStr(100);
      self.sliderPHighAr[sn] := 100; // default if init param val <= 0.
    end;

end;

procedure TMainForm.testAddingModel(); // Test remove when needed.
var
  speciesAr: array of SBMLspecies;
  paramAr: array of SBMLparameter;
  comp: SBMLcompartment;
  rxnProdStoicAr: array of Double;
  rxnProdsAr: array of String;
  rxnReactsAr: array of String;
  rxnReactsStoicAr: array of Double;
begin
  sbmlmodel := SBMLhelpClass.create();

  SetLength(speciesAr, 3);
  speciesAr[0] := SBMLspecies.create('S1');
  speciesAr[0].setInitialConcentration(10);
  speciesAr[0].setCompartment('cell');
  speciesAr[1] := SBMLspecies.create('S2');
  speciesAr[1].setInitialConcentration(2);
  speciesAr[1].setCompartment('cell');
  speciesAr[2] := SBMLspecies.create('S3');
  speciesAr[2].setInitialConcentration(1);
  speciesAr[2].setCompartment('cell');
  sbmlmodel.addSBMLspecies(speciesAr[0]);
  sbmlmodel.addSBMLspecies(speciesAr[1]);
  sbmlmodel.addSBMLspecies(speciesAr[2]);

  comp := SBMLcompartment.create('cell');
  comp.setVolume(2);
  comp.setConstant(true);
  sbmlmodel.addSBMLcompartment(comp);

  SetLength(paramAr, 2);
  paramAr[0] := SBMLparameter.create('k1');
  paramAr[0].setValue(0.1);
  paramAr[1] := SBMLparameter.create('k2');
  paramAr[1].setValue(0.05);
  sbmlmodel.addSBMLParameter(paramAr[0]);
  sbmlmodel.addSBMLParameter(paramAr[1]);

  // Set up reactions:
  rxnProdsAr[0] := speciesAr[1].getID();
  rxnProdStoicAr[0] := 1;
  rxnReactsAr[0] := speciesAr[0].getID();
  rxnReactsStoicAr[0] := 1;
  sbmlmodel.addSBMLReaction('S1toS2', rxnProdsAr, rxnProdStoicAr, rxnReactsAr,
    rxnReactsStoicAr, 'k1*S1');

  rxnProdsAr[0] := speciesAr[2].getID();
  rxnProdStoicAr[0] := 1;
  rxnReactsAr[0] := speciesAr[1].getID();
  rxnReactsStoicAr[0] := 1;
  sbmlmodel.addSBMLReaction('S2toS3', rxnProdsAr, rxnProdStoicAr, rxnReactsAr,
    rxnReactsStoicAr, 'k2*S2');
  numbRxns := 2;
  sbmlModelLoaded := true;
  // addSBMLReaction(rxn name,products,prodStoich, reactants,reactStoich,kineticFormulaStr);

end;

end.
