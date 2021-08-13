unit ufMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, Types, WEBLib.Graphics,
  WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Dialogs, WEBLib.ExtCtrls,
  WEBLib.WebCtrls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.Buttons, Vcl.Imaging.pngimage,
  Vcl.Graphics,System.Generics.Collections,
  uControllerNetwork, uNetworkCanvas, uNetwork, Vcl.TMSFNCTypes, Vcl.TMSFNCUtils,
  Vcl.TMSFNCGraphics,
  Vcl.TMSFNCGraphicsTypes, Vcl.TMSFNCCustomControl, Vcl.TMSFNCScrollBar,
  Vcl.TMSFNCButton, Vcl.TMSFNCToolBar,
  uNetworkTypes, Vcl.Imaging.pngimage, WEBLib.Lists, Vcl.Forms, uModel,
  uSBMLClasses, uSimulation, uControllerMain,
  uODE_FormatUtility, uGraphP, Vcl.Menus, WEBLib.Menus, ufParamSelect,
  ufSpeciesSelect, uPlotLayout, uPlotActions,
  paramSlider, uParamSliderLayout, uSidewinderTypes, WEBLib.ComCtrls;

const
  SLIDERPHEIGHT = 50; // Param Sliders WebPanel height

const
  PLOT_WIDTH_PERCENTAGE = 0.6; // This means a plot extends to 60% of the right panel width
  NODE_TAB = 2; REACTION_TAB = 1; SIMULATION_TAB = 0;


type

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
    rtLengthEdit1: TWebEdit;
    rtLabel1: TWebLabel;
    stepSizeLabel1: TWebLabel;
    stepSizeEdit1: TWebEdit;
    ZoomCntrlPanel: TWebPanel;
    zoomCtlLabel: TWebLabel;
    zoomTrackBar: TWebTrackBar;
    addPlotButton: TWebButton;
    paramAddSliderBtn: TWebButton;
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
    RightWPanel: TWebPanel;   // Holds simulation tab.
    simResultsMemo: TWebMemo;
    plotEditLB: TWebListBox;
    SliderEditLB: TWebListBox;
    pnlCenter: TWebPanel;
    networkPB1: TWebPaintBox;
    netDrawScrollBarVert: TTMSFNCScrollBar;
    netDrawScrollBarHoriz: TTMSFNCScrollBar;
    splitter: TWebSplitter;
    btnSimple: TWebButton;
    SaveSBMLButton: TWebButton;
    SetUpSimButton: TWebButton;
    nodeConcLabel: TWebLabel;
    editNodeConc: TWebEdit;
    WebTabSet1: TWebTabSet;
    RNodeEditWPanel: TWebPanel;
    RRxnEditWPanel: TWebPanel;
    RxnRatePanel: TWebPanel;
    rateLawLabel: TWebLabel;
    rateLawEqLabel: TWebLabel;
    RxnSpStoichPanel: TWebPanel;
    RxnParamPanel: TWebPanel;
    RxnParamLabel: TWebLabel;
    RxnParamComboBox: TWebComboBox;
    RxnParamEdit: TWebEdit;
    RxnStoichLabel: TWebLabel;
    StoicReactantsLabel: TWebLabel;
    StoichProductsLabel: TWebLabel;

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
    procedure plotsPBListPaint(Sender: TObject);
    procedure addPlotButtonClick(Sender: TObject);
    procedure paramAddSliderBtnClick(Sender: TObject);
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
    procedure stepSizeEdit1Change(Sender: TObject);
    procedure SaveSBMLButtonClick(Sender: TObject);
    procedure SetUpSimButtonClick(Sender: TObject);
    procedure editNodeConcExit(Sender: TObject);
    procedure WebTabSet1Click(Sender: TObject);
    procedure RxnParamComboBoxChange(Sender: TObject);
    procedure RxnParamEditChange(Sender: TObject);
    procedure RxnParamComboBoxEnter(Sender: TObject);
    procedure RxnParamComboBoxClick(Sender: TObject);
    procedure RxnParamComboBoxExit(Sender: TObject);
    procedure editNodeConcChange(Sender: TObject);

  private
    numbPlots: Integer; // Number of plots displayed
    numbSliders: Integer; // Number of parameter sliders

    procedure InitSimResultsTable(); // Init simResultsMemo.
    procedure addPlot(); // Add a plot
    procedure selectPlotSpecies(plotnumb: Integer);
    procedure addParamSlider();
    procedure SetSliderParamValues(sn, paramForSlider: Integer);
    procedure selectParameter(sNumb: Integer); // Get parameter for slider
    procedure LoadJSONFile();
    procedure DeletePlot(plotIndex: Integer); // Index of plot to delete
    function  getEmptyPlotPosition(): Integer;
    function  getPlotPBIndex(plotTag: integer): Integer; // Return Plot index of tag.
    procedure EditPlotList(plotn: Integer);
    procedure updatePlots(); // Go through and remove species/plots no longer in model.
    procedure deletePlotSpecies(plotn: Integer); // Delete a species curve in a plot.
    // delete, change plot species, other added as needed using TWebListBox.
    procedure EditSliderList(sn: Integer);
    // delete, change param slider as needed using TWebListBox.
    procedure DeleteSlider(sn: Integer);
    procedure adjustRightTabWPanels(); // Adjust all right panels
                  // (node edit, rxn edit, simulation) to same width, height
    procedure updateRxnRatePanel(); // Refresh with current rxn info.
    procedure updateRxnParamPanel();
    procedure setRightPanels(); // set correct panel to be visible
    procedure setUpSimulationUI(); // Set up sim related buttons, plots, etc

  public
    network: TNetwork;
    networkController: TController;
    networkCanvas: TNetworkCanvas;
    origin: TPointF;
    fileName: string;
    maxYValueList: TList<Integer>; // max Y screen dimension for each plot
    currentGeneration: Integer; // Used by plots as current x axis point
    plotSpeciesForm: TSpeciesSWForm;
    plotSpecies: TList<TSpeciesList>; // species to graph for each plot
    plotsPBList: TList<TWebPaintBox>; // Plot paint boxes
    listOfPlots: TList<TPlotGraph>;   // Plotting class for each plot
    xscaleHeightList: TList<Integer>;
    // This is the space reserved for the x axis labeling of each plot
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
    mainController: TControllerMain;

    function ScreenToWorld(X, Y: Double): TPointF; // Network drawing panel
    function WorldToScreen(wx: Integer): Integer; // Network drawing panel
    procedure PingSBMLLoaded(newModel:TModel); // Notify when done loading or model changes
    procedure getVals(newTime: Double; newVals: array of Double);
    // Get new values (species amt) from simulation run

  end;

var
  mainForm: TMainForm;
  pixelStepList: TList<Integer>; // pixel equiv of time (integration) step
  spSelectform: TSpeciesSWForm; // display speciecs select to plot radio group

implementation

{$R *.dfm}

Uses uGraphUtils, uCreateNetworks, uLayout, uTestModel;

procedure TMainForm.addPlotButtonClick(Sender: TObject);
begin
  // Make runtime, stepsize, simulation buttons visible
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
  //model.testModelUpdate;
end;

procedure TMainForm.btnAddNodeClick(Sender: TObject);
begin
  networkController.setAddNodeStatus;
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
  networkController.setAddBiBiReaction;
end;

procedure TMainForm.btnBiUniClick(Sender: TObject);
begin
  networkController.setAddBiUniReaction;
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

end;

procedure TMainForm.btnDrawClick(Sender: TObject);
var
  n1, n2, n3, n4: TNode;
begin
  n1 := networkController.addNode('node1', 60, 200);
  n2 := networkController.addNode('node2', 270, 270);
  n3 := networkController.addNode('node3', 540, 80);
  n4 := networkController.addNode('node4', 400, 500);

  networkController.addReaction('r1', n1, n2);
  networkController.addReaction('r2', n2, n3);
  networkController.addReaction('r3', n3, n4);
  networkController.addReaction('r4', n4, n2);

  networkPB1.Invalidate;
end;

procedure TMainForm.btnIdleClick(Sender: TObject);
begin
  networkController.setSelectStatus;
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
  self.MainController.loadSBML(s);
 end;

procedure TMainForm.btnUniBiClick(Sender: TObject);
begin
  networkController.setAddUniBiReaction;
end;

procedure TMainForm.btnUniUniClick(Sender: TObject);
begin
  networkController.setAddUniUniReaction;
end;


procedure TMainForm.editNodeIdExit(Sender: TObject);
begin
  networkController.setNodeId(editNodeId.Text);
  networkPB1.Invalidate;
end;

procedure TMainForm.SaveSBMLButtonClick(Sender: TObject);
begin
  fileName := InputBox('Save model to the downloads directory',
    'Enter File Name:', 'newSBML.xml');
  if fileName <> '' then
    begin
      mainController.saveSBML(fileName);
    end
  else
    Showmessage('Save cancelled');
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
  // Check if sbmlmodel already created, if so, destroy before creating ?
  self.MainController.loadSBML(AText);
end;

procedure TMainForm.onLineSimButtonClick(Sender: TObject);
var
  i: Integer;
  yScaleWidth : integer;
begin
  if MainController.isOnline = false then
    begin
      MainController.setOnline(true);
      onLineSimButton.font.color := clgreen;
      onLineSimButton.ElementClassName := 'btn btn-success btn-sm';
      onLineSimButton.caption := 'Simulation: Online';
      simResultsMemo.visible := true;

      try
        MainController.SetRunTime( strToFloat(rtLengthEdit1.Text));
      except
        on Exception: EConvertError do
          begin
            MainController.SetRunTime(200); // Hard coded for now.
            self.rtLengthEdit1.Text := FloatToStr(MainController.getRunTime);
          end;
      end;
      if pixelStepList = nil then
        pixelStepList := TList<Integer>.create;
      for i := 0 to listOfPlots.Count - 1 do
        begin
          yScaleWidth := 16; // Gap between the left edge and y axis
          listOfPlots[i].initGraph(0, 200, 0, 10, // The 10 is the max Y value (yend) in world coords
            0, self.plotsPBList[i].width, 0, self.plotsPBList[i].height,
            xscaleHeightList.Items[i], yscaleWidth, MainController.getRunTime, MainController.getStepSize);

          // Max viewable steps is PlotWebPB.width (1 pixel per step).
          pixelStepList.Add(0);  // Default number.
          if MainController.getRunTime / MainController.getStepSize < self.plotsPBList[i].width then
             pixelStepList[i] := round(self.plotsPBList[i].width * MainController.getStepSize / MainController.getRunTime)
          else
             pixelStepList[i] := 1;
        end;
      self.InitSimResultsTable();  // Set table of Sim results.
      MainController.SetTimerEnabled(true); // Turn on web timer (Start simulation)
    end
  else
    begin
      MainController.setOnline(false);
      MainController.SetTimerEnabled(false); // Turn off web timer (Stop simulation)
      onLineSimButton.font.color := clred;
      onLineSimButton.ElementClassName := 'btn btn-danger btn-sm';
      onLineSimButton.caption := 'Simulation: Offline';
    end;
end;

procedure TMainForm.PingSBMLLoaded(newModel:TModel);
var
  i: Integer;
begin
  console.log(' TMainForm.PingSBMLLoaded');
  paramAddSliderBtn.visible := true;
  onLineSimButton.visible := true;
  addPlotButton.visible := true;
  // 1. Update current slider?
  //  2. updatePlots() <-- TODO: Check if plot species are no longer in model.
end;

procedure TMainForm.plotEditLBClick(Sender: TObject);
begin
  if self.plotEditLB.ItemIndex = 0 then // change species to plot
    begin
      self.selectPlotSpecies(self.plotEditLB.tag);
    end;
  if self.plotEditLB.ItemIndex = 1 then // delete plot
    begin
      self.DeletePlot(getPlotPBIndex(self.plotEditLB.tag));
    end;
  // else ShowMessage('Canceled');
  self.plotEditLB.tag := 0;
  self.plotEditLB.visible := false;
  self.plotEditLB.Top := 40; // default
end;

procedure TMainForm.plotsPBListPaint(Sender: TObject);
var
  plot_i: Integer;
begin
  plot_i := (Sender as TWebPaintBox).tag;
  plotsPBList.items[plot_i - 1].canvas.draw(0, 0, listOfPlots.Items[plot_i - 1].bitmap);
end;

procedure TMainForm.RxnParamComboBoxChange(Sender: TObject);  // NOT needed.
var i: integer;
begin
 console.log('TMainForm.RxnParamComboBoxChange');
 //i := self.RxnParamComboBox.ItemIndex;
 //self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
end;

procedure TMainForm.RxnParamComboBoxClick(Sender: TObject);
var i: integer;
begin
console.log('TMainForm.RxnParamComboBoxClick');
  i := self.RxnParamComboBox.ItemIndex;
  self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
end;

procedure TMainForm.RxnParamComboBoxEnter(Sender: TObject);
var i: integer;
begin
console.log('TMainForm.RxnParamComboBoxEnter');
 // i := self.RxnParamComboBox.ItemIndex;
 // self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
end;

procedure TMainForm.RxnParamComboBoxExit(Sender: TObject);
var i: integer;
begin
  console.log('TMainForm.RxnParamComboBoxExit');
 // self.RxnParamComboBox.invalidate;
 // i := self.RxnParamComboBox.ItemIndex;
 // self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
end;

procedure TMainForm.RxnParamEditChange(Sender: TObject);
var newVal: double;
begin
try
  begin
    newVal :=strtofloat(self.RxnParamEdit.text);
    self.networkController.network.reactions[networkController.selectedEdge].state.rateParams[self.RxnParamComboBox.ItemIndex].setValue(newVal);
  end;
except
  on Exception : EConvertError do
  begin
    ShowMessage(Exception.Message);
    self.RxnParamEdit.text := floattostr(self.networkController.network.reactions[networkController.selectedEdge].state.rateParams[self.RxnParamComboBox.ItemIndex].getValue());

  end;

end;

end;

procedure TMainForm.networkPB1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Handle Ctrl-keys here
  if Key = VK_DELETE then
    begin
      networkController.prepareUndo;
      networkController.deleteSelectedItems;
      networkPB1.Invalidate;
      exit;
    end;

  if (Shift = [ssCtrl]) and (Upcase(Char(Key)) = 'Z') then
    begin
      networkController.Undo;
      networkPB1.Invalidate;
    end;
end;

procedure TMainForm.networkPB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v: TPointF;
begin
  v := ScreenToWorld(X, Y);

  networkController.OnMouseDown(Sender, Button, Shift, v.X, v.Y);
  networkPB1.Invalidate;
  if networkController.selectedNode <> -1 then
    begin
      editNodeId.Text := networkController.network.nodes
        [networkController.selectedNode].state.id;
      editNodeConc.Text := networkController.network.nodes
        [networkCOntroller.selectedNode].state.conc.ToString;
      pnlNodePanel.visible := true;
      self.WebTabSet1.ItemIndex := NODE_TAB;
      self.RRxnEditWPanel.visible := false;
      self.RightWPanel.visible := false;
      self.RNodeEditWPanel.visible := true;
      self.RNodeEditWPanel.invalidate;
      self.WebTabSet1Click(nil); // View node edit tab
    end
  else if networkController.selectedEdge <>-1 then
    begin
      //console.log(' A reaction has been selected');
      self.updateRxnParamPanel;
      self.WebTabSet1.ItemIndex := REACTION_TAB;
      self.RightWPanel.visible := false;
      self.RNodeEditWPanel.visible := false;
      self.RRxnEditWPanel.visible := true;
      self.updateRxnRatePanel;
      self.RxnRatePanel.invalidate;

    end;

end;

procedure TMainForm.networkPB1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  v: TPointF;
begin
  v := ScreenToWorld(X, Y);

  networkController.OnMouseMove(Sender, Shift, v.X, v.Y);
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

  networkController.OnMouseUp(Sender, Button, Shift, v.X, v.Y);
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

procedure TMainForm.editNodeConcChange(Sender: TObject);   // Do not use.
begin
 // networkController.setNodeConc(editNodeConc.Text);
 // networkPB1.Invalidate;
end;

procedure TMainForm.editNodeConcExit(Sender: TObject);
begin
  networkController.setNodeConc(editNodeConc.Text);
  networkPB1.Invalidate;
end;

procedure TMainForm.paramAddSliderBtnClick(Sender: TObject);
var
  i: Integer;
begin
  // TODO: Check if already 5 sliders, if so then showmessage( 'Only 5 parameter sliders allowed, edit existing one');
  self.selectParameter(length(sliderParamAr));
end;

procedure TMainForm.ParamSliderOnChange(Sender: TObject);
var
  i, p: Integer;
  newPVal: double;
begin
  if Sender is TWebTrackBar then
    begin
      newPVal := 0;
      i := TWebTrackBar(Sender).tag;
      self.MainController.paramUpdated := true;
      p := self.sliderParamAr[i];
      newPVal := self.sliderPTBarAr[i].Position * 0.01 *
        (sliderPHighAr[i] - sliderPLowAr[i]);
      // get slider parameter position in p_vals array
      self.MainController.stopTimer;
      self.MainController.changeParameterVal(p, newPVal);
      self.MainController.startTimer;
      self.sliderPTBLabelAr[i].caption := self.MainController.getModel.getP_Names[self.sliderParamAr[i]] + ': '
        + FloatToStr(self.MainController.getModel.getP_Vals[self.sliderParamAr[i]]);

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
  self.numbPlots := 0;
  self.numbSliders := 0;
  self.zoomTrackBar.left := 20;
  self.zoomTrackBar.Position := 10;
  origin.X := 0.0;
  origin.Y := 0.0;
  self.network := TNetwork.create('testNetwork');
  self.networkController := TController.create(network);  // Move this inside of self.mainController
  self.networkCanvas := TNetworkCanvas.create(network);
  self.networkController.networkCanvas := networkCanvas;
  self.networkCanvas.bitmap.Height := networkPB1.Height;
  self.networkCanvas.bitmap.width := networkPB1.width;
  LeftWPanel.color := clWhite;
  self.WebTabSet1.ItemIndex := SIMULATION_TAB;
  self.RNodeEditWPanel.visible := false;
  self.RNodeEditWPanel.ElementBodyClassName := RightWPanel.ElementBodyClassName;
  self.RRxnEditWPanel.ElementBodyClassName := RightWPanel.ElementBodyClassName;
  self.RRxnEditWPanel.visible := false;
  self.adjustRightTabWPanels;

  self.mainController := TControllerMain.Create(self.networkController);
  self.mainController.setOnline(false);
  onLineSimButton.font.color := clred;
  onLineSimButton.caption := 'Simulation: Offline';
  self.mainController.setODEsolver;
  currentGeneration := 0;
  self.mainController.OnSBMLUpdate2 := self.PingSBMLLoaded;
   // Notification of changes:
  self.mainController.OnSimUpdate := self.getVals; // notify when new Sim results

end;

procedure TMainForm.WebFormResize(Sender: TObject);
begin
  if networkCanvas = nil then // Resize may be called before Create
    begin
      network := TNetwork.create('testNetwork');
      networkController := TController.create(network);
      networkCanvas := TNetworkCanvas.create(network);
    end;
  networkCanvas.bitmap.Height := networkPB1.Height;
  networkCanvas.bitmap.width := networkPB1.width;
  networkPB1.Invalidate;
end;


procedure TMainForm.WebTabSet1Click(Sender: TObject);
begin
console.log('TMainForm.WebTabSet1Click: ',inttostr(self.WebTabSet1.ItemIndex));
 self.setRightPanels;
end;

procedure TMainForm.setRightPanels();
begin
 { if self.WebTabSet1.ItemIndex = NODE_TAB then
  begin
    self.RightWPanel.visible := false;
    self.RRxnEditWPanel.visible := false;
    self.RNodeEditWPanel.visible := true;
    self.RightWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;
    self.RNodeEditWPanel.invalidate;
  end
  else if self.WebTabSet1.ItemIndex = REACTION_TAB then
  begin
    self.updateRxnParamPanel;
    self.RightWPanel.visible := false;
    self.RRxnEditWPanel.visible := true;
    self.RNodeEditWPanel.visible := false;
    self.RightWPanel.invalidate;
    self.RNodeEditWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;

    self.updateRxnRatePanel;
  end
  else }
   if self.WebTabSet1.ItemIndex = SIMULATION_TAB then
  begin
    self.RightWPanel.visible := true;
    self.RRxnEditWPanel.visible := false;
    self.RNodeEditWPanel.visible := false;
    self.RNodeEditWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;
    self.RightWPanel.invalidate;
  end;
end;

function TMainForm.WorldToScreen(wx: Integer): Integer;
begin
  raise Exception.create('WorldtoScreen needs to be updated');
  result := trunc(wx * zoomTrackBar.Position / 10);
end;


// set up Results table (simResultsMemo, WebMemo)  optional ?
procedure TMainForm.InitSimResultsTable();
var
  simRTStr: String; // Output string for TWebMemo
  i: Integer;

begin
  simResultsMemo.Lines.Clear();
  simRTStr := ' Time (s) '; // generate coloumn headers:

  for i := 0 to length(self.mainController.getModel.getS_Names) - 1 do
    begin
      simRTStr := simRTStr + ', ' + self.mainController.getModel.getS_Names()[i];
    end;
  simResultsMemo.Lines.Add(simRTStr);
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
  self.adjustRightTabWPanels; //TODO: Need to adjust right panels based on tab focus
  networkPB1.Invalidate;
end;


procedure TMainForm.stepSizeEdit1Change(Sender: TObject);
begin
  MainController.SetTimerInterval(strToInt(stepSizeEdit1.Text));
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
  for i := 0 to length(newVals) - 1 do
    begin
      dataStr := dataStr + floatToStrf(newVals[i], ffExponent, 6, 2) + ', ';
    end;
  simResultsMemo.Lines.Add(dataStr);
  inc(self.currentGeneration);

  uPlotActions.processScan(newTime, newVals,listOfPlots,self.plotsPBList, plotSpecies,
             currentGeneration );

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
      jstr := networkController.network.convertToJSON();
      Application.DownloadTextFile(jstr, fileName);
    end
  else
    Showmessage('Save cancelled');
end;

procedure TMainForm.mnuUndoClick(Sender: TObject);
begin
  networkController.Undo;
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
    networkController.loadModel(AText);
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


procedure TMainForm.selectPlotSpecies(plotnumb: Integer);
 // plotnumb: plot number, not index, to be added or modified

  // Pass back to caller after closing popup:
  procedure AfterShowModal(AValue: TModalResult);
  var
    i: Integer;
    addingPlot: Boolean;
  begin
    if self.plotSpecies = nil then
      self.plotSpecies := TList<TSpeciesList>.create;
    if self.plotSpecies.Count < plotnumb then
      begin
        // Add a plot with species list
        addingPlot := true;
        self.plotSpecies.Add(TSpeciesList.create);
      end
    else
      begin    // Changing species to plot, so clear out old entries:
        addingPlot := false;
        self.plotSpecies.Items[getPlotPBIndex(plotNumb)].Clear;
      end;

    for i := 0 to plotSpeciesForm.SpPlotCG.Items.Count - 1 do
      begin
        if plotSpeciesForm.SpPlotCG.checked[i] then
          begin
            if addingPlot then
              self.plotSpecies[plotnumb - 1].Add(plotSpeciesForm.SpPlotCG.Items[i])
            else
              self.plotSpecies[getPlotPBIndex(plotNumb)].Add(plotSpeciesForm.SpPlotCG.Items[i]);
          end
        else
          if addingPlot then
            self.plotSpecies.Items[plotnumb - 1].Add('')
          else self.plotSpecies.Items[getPlotPBIndex(plotNumb)].Add('');
      end;
    if addingPlot then
      self.addPlot(); // <-- Add dynamically created plot at this point
  end;

// async called OnCreate for TSpeciesSWForm
  procedure AfterCreate(AForm: TObject);
  begin
    (AForm as TSpeciesSWForm).speciesList := self.mainController.getModel.getS_names;
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
            self.EditPlotList(i);
          end;
      end;
  end;

var plotPositionToAdd: integer; // Add plot to next empty position.
    //newPlotPB: TWebPaintBox;
begin
  plotPositionToAdd := -1;
  plotPositionToAdd := self.getEmptyPlotPosition();
  if self.plotsPBList = nil then
    self.plotsPBList := TList<TWebPaintBox>.create;
  if self.xscaleHeightList = nil then
    self.xscaleHeightList := TList<Integer>.create;
  if self.listOfPlots = nil then
    self.listOfPlots := TList<TPlotGraph>.create;
  if self.maxYValueList = nil then
    self.maxYValueList := TList<Integer>.create;
  if pixelStepList = nil then
    pixelStepList := TList<Integer>.create;

  self.listOfPlots.Add(TPlotGraph.create);

  self.plotsPBList.Add(TWebPaintBox.create(self.RightWPanel));
  self.plotsPBList[self.numbPlots - 1].parent := self.RightWPanel;
  self.plotsPBList[self.numbPlots - 1].OnPaint := plotsPBListPaint;
  self.plotsPBList[self.numbPlots - 1].OnMouseDown := plotOnMouseDown;
  self.plotsPBList[self.numbPlots - 1].Tag := plotPositionToAdd;
  console.log('Position to add plot: ',plotPositionToAdd);

//  console.log('Adding plot, tag: ',self.plotsPBList[self.numbPlots - 1].Tag);
  configPbPlot(plotPositionToAdd, self.numbPlots,
    trunc(self.RightWPanel.width * PLOT_WIDTH_PERCENTAGE), self.RightWPanel.Height, self.plotsPBList);

  self.xscaleHeightList.Add( round(0.15 * self.plotsPBList[self.numbPlots - 1].Height) );
  // make %15 of total height
  self.maxYValueList.Add(self.plotsPBList[self.numbPlots - 1].Height); // PaintBox dimension
  self.plotsPBList[self.numbPlots - 1].Invalidate;

end;

function  TMainForm.getPlotPBIndex(plotTag: integer): Integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to self.numbPlots -1 do
  begin
    if self.plotsPBList[i].Tag = plotTag then
      Result := i;
  end;
end;

function TMainForm.getEmptyPlotPosition(): Integer;
var i, plotPosition, totalPlots: Integer;

begin
  plotPosition := 1;
  totalPlots := self.numbPlots;
  if self.numbPlots >1 then
  begin
    for i := 0 to totalPlots -2 do
    begin
      if self.plotsPBList[i].Tag = plotPosition then
        inc(plotPosition);
    end;
  end;

  Result := plotPosition;
end;


procedure TMainForm.DeletePlot(plotIndex: Integer);
var tempObj: TObject;
begin
  try
    begin
      try
        begin
        //console.log('Deleting plot with tag: ',self.plotsPBList[plotIndex].Tag);
          tempObj := self.plotsPBList[plotIndex];
          tempObj.Free;
          self.plotsPBList.Delete(plotIndex);
          self.xscaleHeightList.Delete(plotIndex);
          tempObj := self.listOfPLots[plotIndex];
          tempObj.Free;
          self.listOfPlots.Delete(plotIndex);
          self.maxYValueList.Delete(plotIndex);
          tempObj := self.plotSpecies[plotIndex];
          tempObj.Free;
          self.plotSpecies.Delete(plotIndex);
          pixelStepList.Delete(plotIndex);
          self.numbPlots := self.numbPlots - 1;
        end;
      finally
        self.RightWPanel.Invalidate;
      end;

    end;
  except
     on EArgumentOutOfRangeException do
      ShowMessage('Plot number not in array');

  end;

end;

procedure TMainForm.EditPlotList(plotn: Integer);
var
  plotXposition, plotYposition: Integer;
  plotIndex: Integer;
  editList: TStringList;
begin
  plotIndex := -1;
  plotIndex := getPlotPBIndex(plotn);
  plotXposition := 40;
  plotYposition := self.plotsPBList[plotIndex].Top + 10;
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

procedure TMainForm.updatePlots(); // Go through and remove species/plots no longer in model.
begin
  // TODO
  //  EditPlotList(plotn: Integer); <-- check each plot to see if need to remove plot species.
end;

procedure TMainForm.deletePlotSpecies(plotn: Integer); // Delete a species curve in a plot.
begin
  // TODO: Just delete plot, get new list of species from user and create new plot.
end;

procedure TMainForm.EditSliderList(sn: Integer);
// delete, change param slider as needed.
var
  sliderXposition, sliderYposition: Integer;
  editList: TStringList;
begin
  sliderXposition := 350;
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
    // update slider with new parameter.
  end;

// async called OnCreate for TParamSliderSForm
  procedure AfterCreate(AForm: TObject);
  begin
    (AForm as TParamSliderSForm).paramList := self.mainController.getModel.getP_Names;
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
            // ShowMessage('WebPanel sent mouse msg (addParamSlider):  '+ inttostr(i));
            self.EditSliderList(i);
          end;
      end;
  end;

// ***********************
var
  i, sliderTBarWidth, sliderPanelLeft, sliderPanelWidth: Integer;
begin
  // Left most position of the panel that holds the slider
  sliderPanelWidth := trunc((1 - PLOT_WIDTH_PERCENTAGE) * RightWPanel.width);
  sliderPanelLeft := (RightWPanel.width - sliderPanelWidth) - 6;
  // Width of the slider inside the panel

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
  pVal:Double;
  pName: String;
begin
  rangeMult := 10; // default. 10
  pName :=  self.mainController.getModel.getP_Names[self.sliderParamAr[sn]];
  pVal := self.mainController.getModel.getP_Vals[self.sliderParamAr[sn]];
  self.sliderPTBLabelAr[sn].caption := pName + ': ' + FloatToStr(pVal);
  self.sliderPLowAr[sn] := 0;
  self.sliderPLLabelAr[sn].caption := FloatToStr(self.sliderPLowAr[sn]);
  self.sliderPTBarAr[sn].Min := 0;
  self.sliderPTBarAr[sn].Position := trunc((1 / rangeMult) * 100);
  self.sliderPTBarAr[sn].Max := 100;
  if pVal > 0 then
    begin
      self.sliderPHLabelAr[sn].caption := FloatToStr(pVal * rangeMult);
      self.sliderPHighAr[sn] := pVal * rangeMult;
    end
  else
    begin
      self.sliderPHLabelAr[sn].caption := FloatToStr(100);
      self.sliderPHighAr[sn] := 100; // default if init param val <= 0.
    end;

end;


procedure TMainForm.SetUpSimButtonClick(Sender: TObject);
begin
  self.setUpSimulationUI(); // Clear out old plots, simulation, parameter sliders for new sim.

end;

procedure TMainForm.adjustRightTabWPanels(); // Adjust all right panels to same width, height
begin
  self.RNodeEditWPanel.Width := RightWPanel.Width;
  self.RNodeEditWPanel.Top := RightWPanel.Top;
  self.RNodeEditWPanel.Height := RightWPanel.Height;
  self.RNodeEditWPanel.Left := RightWPanel.Left;
  self.RRxnEditWPanel.Width := RightWPanel.Width;
  self.RRxnEditWPanel.Top := RightWPanel.Top;
  self.RRxnEditWPanel.Height := RightWPanel.Height;
  self.RRxnEditWPanel.Left := RightWPanel.Left;
  self.RNodeEditWPanel.invalidate;
  self.RRxnEditWPanel.invalidate;
end;

procedure TMainForm.updateRxnRatePanel(); // Refresh with current rxn info.
begin
   self.rateLawEqLabel.Caption := networkController.network.reactions[networkController.selectedEdge].state.rateLaw;
   self.RxnRatePanel.Width := self.rateLawEqLabel.Width + self.rateLawLabel.Width + 60;
   self.RxnRatePanel.invalidate;
   self.RightWPanel.visible := false;
   self.RRxnEditWPanel.visible := true;
   self.RNodeEditWPanel.visible := false;
   self.RightWPanel.invalidate;
   self.RNodeEditWPanel.invalidate;
   self.RRxnEditWPanel.invalidate;
end;

procedure TMainForm.updateRxnParamPanel();
var paramTStr: string;
    i: integer;
begin
  paramTStr:= '';
  self.RxnParamComboBox.clear;
  for i := 0 to networkController.network.reactions[networkController.selectedEdge].state.rateParams.count -1 do
  begin

      paramTStr := networkController.network.reactions[networkController.selectedEdge].state.rateParams.Items[i].getId;
      if paramTStr <> '' then
      begin
        self.RxnParamComboBox.AddItem( paramTStr,nil);
      end;

  end;
  self.RxnParamComboBox.ItemIndex := 0;
  self.RxnParamComboBox.invalidate;
end;

procedure TMainForm.setUpSimulationUI();
var i: integer;
begin
  self.currentGeneration := 0; // reset current x axis point (pixel)
   // delete existing plots
  if self.numbPlots >0 then
  begin
    if (self.plotsPBList.Count) > 0 then
    begin
    //  for i := 0 to self.plotsPBList.Count-1 do
      for i := self.plotsPBList.Count-1 downto 0 do
      begin
        self.DeletePlot(i);
      end;

      self.numbPlots := 0;
      self.WebTabSet1.ItemIndex := SIMULATION_TAB;
      self.setRightPanels;
      self.RightWPanel.invalidate;
    end;
  end;


  // delete existing param sliders.
  if self.sliderPanelAr <> nil then
  begin
    if length(self.sliderPanelAr) >0 then
    begin
      for i := length(self.sliderPanelAr) -1 downto 0 do
     // for i := 0 to length(self.sliderPanelAr) -1 do
      begin
        self.DeleteSlider(i);
      end;
      setLength(self.sliderPanelAr, 0);
    end;
  end;

  mainController.resetCurrTime;
  //mainController.stepSize := 0.1; // 100 msec
 // mainController.runTime := 500; // sec
  mainController.createModel;
  mainController.createSimulation;

end;

end.
