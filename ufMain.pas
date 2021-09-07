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
  {paramSlider,} uParamSliderLayout, uSidewinderTypes, WEBLib.ComCtrls;

const
  SLIDERPHEIGHT = 50; // Param Sliders WebPanel height
  DEFAULTSPECIESPLOTHT = 10; // Default y range for plotting species
  SLIDER_RANGE_MULT = 10;  // Default range multiplier for param slider
  PLOT_WIDTH_PERCENTAGE = 0.6; // This means a plot extends to 60% of the right panel width

type
  TPanelType = ( SIMULATION_PANEL, REACTION_PANEL, NODE_PANEL );
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
    rtLengthEdit1: TWebEdit;    // Run time length, Not used for now.
    rtLabel1: TWebLabel;        // Not used for now.
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
    WebPanel2: TWebPanel;  // ??
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
    RSimWPanel: TWebPanel;   // Holds simulation tab.
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
    procedure RPanelTabSetClick(Sender: TObject);
    procedure RxnParamComboBoxChange(Sender: TObject);
    procedure RxnParamComboBoxClick(Sender: TObject);
    procedure RxnParamComboBoxExit(Sender: TObject);
    procedure RxnParamEditExit(Sender: TObject);

  private
    numbPlots: Integer; // Number of plots displayed
    numbSliders: Integer; // Number of parameter sliders
    rightPanelType: TPanelType;
    networkUpdated: boolean; // Network has changed, update model, plots, etc when convenient.
    rxnReactStoichLabels: TList<TWebLabel>;
    rxnReactStoichEdits: TList<TWebEdit>;
    rxnProdStoichLabels: TList<TWebLabel>;
    rxnProdStoichEdits: TList<TWebEdit>;
    procedure InitSimResultsTable(); // Init simResultsMemo.
    procedure addPlot(yMax: double); // Add a plot, yMax: largest initial val of plotted species
    procedure resetPlots();  // Reset plots for new simulation.
    procedure selectPlotSpecies(plotnumb: Integer);

    procedure DeletePlot(plotIndex: Integer); // Index of plot to delete
    function  getEmptyPlotPosition(): Integer;
    function  getPlotPBIndex(plotTag: integer): Integer; // Return Plot index of tag.
    procedure EditPlotList(plotn: Integer);
    procedure updatePlots(); // Go through and remove species/plots no longer in model.
    procedure initializePlots();
    procedure initializePlot( n: integer);
    procedure drawPlotLegends();
    procedure deletePlotSpecies(plotn: Integer); // Delete a species curve in a plot.
             // delete, change plot species, other added as needed using TWebListBox.
    procedure addParamSlider();
    procedure SetSliderParamValues(sn, paramForSlider: Integer);
    procedure selectParameter(sNumb: Integer); // Get parameter for slider
    procedure LoadJSONFile();
    procedure EditSliderList(sn: Integer);
            // delete, change param slider as needed using TWebListBox.
    procedure DeleteSlider(sn: Integer);
    procedure adjustRightTabWPanels(); // Adjust all right panels
                  // (node edit, rxn edit, simulation) to same width, height
    procedure updateRxnRatePanel(); // Refresh with current rxn info.
    procedure updateRxnParamPanel();//        "
    procedure updateRxnStoichPanel(); //      "
    procedure setRightPanels(); // set correct panel to be visible
    procedure clearRxnStoichCoeffs(); // clear rxn stoichs when new reaction displayed.
    procedure clearRxnNodeRightPanels(); // clear references to rxn/nodes that have been deleted.
    procedure setUpSimulationUI(); // Set up sim related buttons, plots, etc
    procedure addRxnStoichEdit(spIndex: integer; rxnReactant: boolean);

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
    plotsPanelList: TList<TWebPanel>; // Panels in which each plot resides
    plotsPBList: TList<TWebPaintBox>; // Plot paint boxes
    listOfPlots: TList<TPlotGraph>;   // Plotting class for each plot
    listOfPlotLegendsPB: TList<TWebPaintBox>; // Paint box to hold plot legends
    listOfPlotLegendsBMap: TList<TBitMap>;
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
    function processPlotLegendPB(plotIndex: integer; plotLegendPB: TWebPaintBox): TWebPaintBox;
    function processPlotLegendBM(plotIndex: integer; plotLegendBitMap: TBitMap): TBitMap; // Draw plot legend
    procedure PingSBMLLoaded(newModel:TModel); // Notify when done loading or model changes
    procedure networkHasChanged(); // Notify when network has changed, may need to update model, plots, etc
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
 // rtLabel1.visible := true;  // Do not let user modify for now
 // rtLengthEdit1.visible := true; // Do not let user modify for now
  stepSizeLabel1.visible := true;
  stepSizeEdit1.visible := true;
  self.selectPlotSpecies(self.numbPlots);
end;

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  notifyUser('Version 0.1');
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
  if length(network.getCurrentState.savedNodes) = 0 then
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
  end
  else
    notifyUser('Network already exists in panel.');

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
//console.log('editNodeIdExit ');
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
    notifyUser('Save cancelled');
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
  yScaleWidth, newYMax : integer;
begin

  if MainController.isOnline = false then
    begin
      if self.networkUpdated = false then
      begin
        MainController.setOnline(true);
        onLineSimButton.font.color := clgreen;
        onLineSimButton.ElementClassName := 'btn btn-success btn-sm';
        onLineSimButton.caption := 'Simulation: Online';
        simResultsMemo.visible := true;

     // try
     //   MainController.SetRunTime( strToFloat(rtLengthEdit1.Text));
     // except
     //   on Exception: EConvertError do
      //    begin
            MainController.SetRunTime(200); // Hard coded for now.
            self.rtLengthEdit1.Text := FloatToStr(MainController.getRunTime);
       //   end;
      //end;

        if self.MainController.getCurrTime = 0  then
          self.InitSimResultsTable();  // Set table of Sim results.

        self.rightPanelType := SIMULATION_PANEL;
        self.setRightPanels;
        MainController.SetTimerEnabled(true); // Turn on web timer (Start simulation)
      end
      else
        begin
          notifyUser(' Model has changed, resetting simulation');
          //self.networkUpdated := false;
          self.setUpSimulationUI;
        end;
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

procedure TMainForm.initializePlots();
  var i: Integer;
     // yScaleWidth, newYMax : integer;
begin
  for i := 0 to listOfPlots.Count - 1 do
    begin
      self.initializePlot(i);
    end;
end;

procedure TMainForm.initializePlot( n: integer);
 var yScaleWidth, newYMax : integer;
begin
  if pixelStepList = nil then
    pixelStepList := TList<Integer>.create;
  yScaleWidth := 30; // Gap between the left edge and y axis ( for number labels)
       // adjust based on init max val (newYMax) ?
  if self.listOfPlots[n].getY_valsMax >0 then newYMax := round(self.listOfPlots[n].getY_valsMax)
    else newYMax := DEFAULTSPECIESPLOTHT;
  self.listOfPlots[n].resetGraph(self.listOfPlots[n].bitmap.canvas);
  self.listOfPlots[n].initGraph(0, 200, 0, newYMax, // The 10 is the max Y value (yend) in world coords
        0, self.plotsPBList[n].width, 0, self.plotsPBList[n].height,
        xscaleHeightList.Items[n], yscaleWidth, MainController.getRunTime, MainController.getStepSize);
  // Display the plot:
  uPlotActions.processScanOnePlot(0, mainController.getModel.getS_Vals,listOfPlots[n],self.plotsPBList[n], plotSpecies[n],
           0 {self.currentGeneration} );

  self.plotsPBList[n].invalidate;
  self.listOfPlotLegendsPB[n].Canvas.Draw(0,0,self.listOfPlotLegendsBMap[n]);

   // Max viewable steps is PlotWebPB.width (1 pixel per step).
  pixelStepList.Add(0);  // Default number.
  if MainController.getRunTime / MainController.getStepSize < self.plotsPBList[n].width then
    pixelStepList[n] := round(self.plotsPBList[n].width * MainController.getStepSize / MainController.getRunTime)
  else
    pixelStepList[n] := 1;
end;

procedure TMainForm.resetPlots();  // Reset plots for new simulation.
begin
  self.initializePlots;
end;

procedure TMainForm.PingSBMLLoaded(newModel:TModel);
var
  i: Integer;
begin
 // console.log(' TMainForm.PingSBMLLoaded');
  paramAddSliderBtn.visible := true;
  onLineSimButton.visible := true;
  addPlotButton.visible := true;
  
end;

procedure TMainForm.networkHasChanged();
begin
  self.networkUpdated := true;
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
  self.plotEditLB.Top := 40; // default    // Need to specify correct plotPanelList
end;

procedure TMainForm.plotsPBListPaint(Sender: TObject);
var
  plot_i: Integer;
begin
  plot_i := (Sender as TWebPaintBox).tag;
  plotsPBList.items[plot_i - 1].canvas.draw(0, 0, listOfPlots.Items[plot_i - 1].bitmap);
end;

procedure TMainForm.RxnParamComboBoxChange(Sender: TObject);  // NOT needed. ??
var i: integer;
begin
 //console.log('TMainForm.RxnParamComboBoxChange');
 i := self.RxnParamComboBox.ItemIndex;
 self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
end;

procedure TMainForm.RxnParamComboBoxClick(Sender: TObject);
var i: integer;
begin
//console.log('TMainForm.RxnParamComboBoxClick');
 // i := self.RxnParamComboBox.ItemIndex;
//  self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
//  self.RxnParamComboBox.invalidate;
end;


procedure TMainForm.RxnParamComboBoxExit(Sender: TObject);
var i: integer;
begin
 // console.log('TMainForm.RxnParamComboBoxExit');
 // i := self.RxnParamComboBox.ItemIndex;
 // self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[i].getValue);
end;



procedure TMainForm.RxnParamEditExit(Sender: TObject);
var newVal: double;
begin
  try
    begin
      newVal :=strtofloat(self.RxnParamEdit.text);
      self.networkController.network.reactions[networkController.selectedEdge].state.rateParams[self.RxnParamComboBox.ItemIndex].setValue(newVal);
      self.networkController.network.networkEvent;   // notify listener that network changed. TODO: Move to network class ( add add setValue to networkController)
    end;
  except
    on Exception : EConvertError do
    begin
    notifyUser(Exception.Message);
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
      clearRxnNodeRightPanels();  // Right panel no longer shows deleted obj
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
      self.rightPanelType := NODE_PANEL;
      self.RRxnEditWPanel.visible := false;
      self.RSimWPanel.visible := false;
      self.RNodeEditWPanel.visible := true;
      self.RNodeEditWPanel.invalidate;
      self.setRightPanels;
    end
  else if networkController.selectedEdge <>-1 then
    begin
     // console.log(' A reaction has been selected');
      self.rightPanelType := REACTION_PANEL;
      self.RSimWPanel.visible := false;
      self.RNodeEditWPanel.visible := false;
      self.RRxnEditWPanel.visible := true;
      self.updateRxnRatePanel;
      self.updateRxnStoichPanel;
      self.updateRxnParamPanel;
      self.RRxnEditWPanel.invalidate;
      self.setRightPanels;
    end
    else
      begin
        self.rightPanelType := SIMULATION_PANEL;
        self.setRightPanels;
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

procedure TMainForm.editNodeConcExit(Sender: TObject);
begin
//console.log('editNodeConcExit(');
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
  self.LeftWPanel.color := clWhite;
  self.rightPanelType := SIMULATION_PANEL;
  self.RNodeEditWPanel.visible := false;
  self.RNodeEditWPanel.ElementBodyClassName := RSimWPanel.ElementBodyClassName;
  self.RRxnEditWPanel.ElementBodyClassName := RSimWPanel.ElementBodyClassName;
  self.RRxnEditWPanel.visible := false;
  self.rxnReactStoichLabels := TList<TWebLabel>.create;
  self.rxnReactStoichEdits := TList<TWebEdit>.create;
  self.rxnProdStoichLabels := TList<TWebLabel>.create;
  self.rxnProdStoichEdits := TList<TWebEdit>.create;
  self.adjustRightTabWPanels;
  self.mainController := TControllerMain.Create(self.networkController);
  self.mainController.setOnline(false);
  onLineSimButton.font.color := clred;
  onLineSimButton.caption := 'Simulation: Offline';
  self.mainController.setODEsolver;
  self.networkUpdated := false;
  currentGeneration := 0;
  self.mainController.OnSBMLUpdate2 := self.PingSBMLLoaded;
  self.mainController.OnNetworkChange:= self.networkHasChanged;
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


procedure TMainForm.RPanelTabSetClick(Sender: TObject);
begin
//console.log('TMainForm.WebTabSet1Click: ',inttostr(self.RPanelTabSet.ItemIndex));
 //self.setRightPanels;
end;

procedure TMainForm.setRightPanels();
begin
 { if self.rightPanelType = NODE_PANEL then
  begin
    self.RSimWPanel.visible := false;
    self.RRxnEditWPanel.visible := false;
    self.RNodeEditWPanel.visible := true;
    self.RSimWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;
    self.RNodeEditWPanel.invalidate;
  end
  else if self.rightPanelType = REACTION_PANEL then
  begin
    self.updateRxnParamPanel;
    self.RSimWPanel.visible := false;
    self.RRxnEditWPanel.visible := true;
    self.RNodeEditWPanel.visible := false;
    self.RSimWPanel.invalidate;
    self.RNodeEditWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;

    self.updateRxnRatePanel;
  end
  else }
   if self.rightPanelType = SIMULATION_PANEL then
  begin
    self.RSimWPanel.visible := true;
    self.RRxnEditWPanel.visible := false;
    self.RNodeEditWPanel.visible := false;
    self.RNodeEditWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;
    self.RSimWPanel.invalidate;
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
 // console.log('splitterMoved....');
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
    notifyUser('Save cancelled');
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
      notifyUser(E.message);
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
    i: Integer; maxYVal: double; plotSp: string;
    addingPlot: Boolean;
  begin
    maxYVal := 0;
    plotSp := '';
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
        plotSp := '';
        if plotSpeciesForm.SpPlotCG.checked[i] then
          begin
            plotSp := self.mainController.getModel.getS_names[i];
            if self.mainController.getModel.getSBMLspecies(plotSp).isSetInitialAmount then
            begin
              if self.mainController.getModel.getSBMLspecies(plotSp).getInitialAmount > maxYVal then
                maxYVal := self.mainController.getModel.getSBMLspecies(plotSp).getInitialAmount;
            end
            else
              if self.mainController.getModel.getSBMLspecies(plotSp).getInitialConcentration > maxYVal then
                maxYVal := self.mainController.getModel.getSBMLspecies(plotSp).getInitialConcentration;

            if addingPlot then
              self.plotSpecies[plotnumb - 1].Add(plotSp)
            else
              self.plotSpecies[getPlotPBIndex(plotNumb)].Add(plotSp);
            //  self.plotSpecies[getPlotPBIndex(plotNumb)].Add(plotSpeciesForm.SpPlotCG.Items[i]);
          end
        else
          if addingPlot then
            self.plotSpecies.Items[plotnumb - 1].Add('')
          else self.plotSpecies.Items[getPlotPBIndex(plotNumb)].Add('');
      end;
    if maxYVal = 0 then
      maxYVal := DEFAULTSPECIESPLOTHT  // default for plot Y max
    else maxYVal := MaxYVal * 2.0;  // add 100% margin
    if addingPlot then
      self.addPlot(maxYVal) // <-- Add dynamically created plot at this point
    else
      self.listOfPlots[getPlotPBIndex(plotNumb)].setY_ValsMax( maxYVal);
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

procedure TMainForm.addPlot(yMax: double); // Add a plot

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
    plotWidth: integer;
  i: Integer;
begin
  plotWidth := 0;
  plotPositionToAdd := -1;
  plotPositionToAdd := self.getEmptyPlotPosition();
  if self.plotsPanelList = nil then
    self.plotsPanelList := TList<TWebPanel>.create;
  if self.plotsPBList = nil then
    self.plotsPBList := TList<TWebPaintBox>.create;
  if self.listOfPlotLegendsPB = nil then
    self.listOfPlotLegendsPB := TList<TWebPaintBox>.create;
  if self.listOfPlotLegendsBMap = nil then
    self.listOfPlotLegendsBMap := TList<TBitMap>.create;
  if self.xscaleHeightList = nil then
    self.xscaleHeightList := TList<Integer>.create;
  if self.listOfPlots = nil then
    self.listOfPlots := TList<TPlotGraph>.create;
  if self.maxYValueList = nil then
    self.maxYValueList := TList<Integer>.create;
  if pixelStepList = nil then
    pixelStepList := TList<Integer>.create;

  self.listOfPlots.Add(TPlotGraph.create);
  self.listOfPlots[self.numbPlots -1].setY_valsMax(yMax);
  // Put plot in a TWebPanel:
  self.plotsPanelList.Add(TWebPanel.create(self.RSimWPanel));
  self.plotsPanelList[self.numbPlots - 1].Parent := self.RSimWPanel;
  self.plotsPanelList[self.numbPlots - 1].Tag := plotPositionToAdd;
  self.plotsPBList.Add(TWebPaintBox.create(self.plotsPanelList[self.numbPlots-1]));
  self.plotsPBList[self.numbPlots - 1].parent := self.plotsPanelList[self.numbPlots-1];
  self.plotsPBList[self.numbPlots - 1].OnPaint := plotsPBListPaint;
  self.plotsPBList[self.numbPlots - 1].OnMouseDown := plotOnMouseDown;
  self.plotsPBList[self.numbPlots - 1].Tag := plotPositionToAdd;
  // Want all plots to have same width, not always the case if user deletes plot,
  // adjust right panel width:
  if self.numbPlots > 1 then
    plotWidth := self.plotsPanelList[0].Width + PLOT_WIDTH_OVERLAP
  else
    plotWidth := trunc(self.RSimWPanel.width * PLOT_WIDTH_PERCENTAGE);

//  console.log('Adding plot, tag: ',self.plotsPBList[self.numbPlots - 1].Tag);
  configPbPlot(plotPositionToAdd, self.numbPlots,
           plotWidth, self.RSimWPanel.Height, self.plotsPBList, self.plotsPanelList);

  self.xscaleHeightList.Add( round(0.15 * self.plotsPBList[self.numbPlots - 1].Height) );
  // make %15 of total height
  self.maxYValueList.Add(self.plotsPBList[self.numbPlots - 1].Height); // PaintBox dimension

  self.listOfPlotLegendsPB.Add(TWebPaintBox.create(self.plotsPanelList[self.numbPlots - 1]));
  self.listOfPlotLegendsBMap.Add(TBitMap.create);
  self.listOfPlotLegendsPB[self.numbPlots-1].Tag := plotPositionToAdd;
  self.listOfPlotLegendsPB[self.numbPlots-1] := self.processPlotLegendPB(self.numbPlots - 1, self.listOfPlotLegendsPB[self.numbPlots-1]);
  self.listOfPlotLegendsBMap[self.numbPlots-1] := self.processPlotLegendBM(self.numbPlots - 1, self.listOfPlotLegendsBMap[self.numbPlots-1]);
  self.initializePlot(self.numbPlots - 1);
  for i := 0 to self.numbPlots-1 do
    self.listOfPlotLegendsPB[i].Canvas.Draw(0,0,self.listOfPlotLegendsBMap[i]); // ? not sure why this has to be redrawn
  self.plotsPBList[self.numbPlots - 1].Invalidate;

end;

function TMainForm.processPlotLegendPB(plotIndex: integer; plotLegendPB: TWebPaintBox): TWebPaintBox;
var i, speciesPlot: integer;
begin
  speciesPlot := 0;
  speciesPlot := plotSpeciesForm.SpPlotCG.Items.Count; // max #, some will not be plotted.
  plotLegendPB.parent := self.plotsPanelList[plotIndex];
  plotLegendPB.top := 5;
  plotLegendPB.height := 10 * speciesPlot; // each row 5 'pixels' tall
  plotLegendPB.left := self.plotsPBList[plotIndex].width +10;
  result := plotLegendPB;
end;

function TMainForm.processPlotLegendBM(plotIndex: integer; plotLegendBitMap: TBitMap): TBitMap;
var i, speciesPlot: integer;
    spName: string;
begin
  speciesPlot := 0;
  speciesPlot := plotSpeciesForm.SpPlotCG.Items.Count; // max #, some will not be plotted.
  plotLegendBitMap := TBitMap.create;
  plotLegendBitMap.width := self.listOfPlotLegendsPB[plotIndex].width;
  plotLegendBitMap.height := self.listOfPlotLegendsPB[plotIndex].height;
  plotLegendBitMap.canvas.font.name := 'Courier New';
  plotLegendBitMap.canvas.font.size := 8;

  for i := 0 to speciesPlot -1 do
    begin
    spName := '';
    if plotSpeciesForm.SpPlotCG.checked[i] then
      begin
        spName := self.mainController.getModel.getS_names[i];
        if i <10 then
          plotLegendBitMap.canvas.pen.color := uGraphP.COLORS[i]
        else plotLegendBitMap.canvas.pen.color := uGraphP.COLORS[i mod length(uGraphP.COLORS)];
        plotLegendBitMap.canvas.moveto( 2, i*10+2 );
        plotLegendBitMap.canvas.lineto( 10, i*10+2);
        plotLegendBitMap.canvas.TextOut( 11, i*10, spName);
      end;
    end;
  result := plotLegendBitMap;
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

procedure TMainForm.drawPlotLegends();
var i: integer;
begin
  for i := 0 to self.numbPlots-1 do
    self.listOfPlotLegendsPB[i].Canvas.Draw(0,0,self.listOfPlotLegendsBMap[i]);
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
          tempObj := self.listOfPlotLegendsPB[plotIndex];
          tempObj.Free;
          self.listOfPlotLegendsPB.Delete(plotIndex);
          tempObj := self.listOfPlotLegendsBMap[plotIndex];
          tempObj.Free;
          self.listOfPlotLegendsBMap.Delete(plotIndex);
          tempObj := self.plotsPBList[plotIndex];
          tempObj.Free;
          self.plotsPBList.Delete(plotIndex);
          tempObj := self.plotsPanelList[plotIndex];
          tempObj.Free;
          self.plotsPanelList.Delete(plotIndex);
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
        self.RSimWPanel.Invalidate;
      end;

    end;
  except
     on EArgumentOutOfRangeException do
      notifyUser('Error: Plot number not in array');

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
//  plotYposition := self.plotsPBList[plotIndex].Top + 10;
  plotYposition := self.plotsPanelList[plotIndex].Top + 20;
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
  // TODO --> No noeed for this, just delete all plots and have user add them as necessary?
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
  self.RSimWPanel.Invalidate;
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
  //  console.log('Param index picked (chosenParam): ',
  //    sliderParamForm.chosenParam);
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
  sliderPanelWidth := trunc((1 - PLOT_WIDTH_PERCENTAGE) * RSimWPanel.width);
  sliderPanelLeft := (RSimWPanel.width - sliderPanelWidth) - 6;
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

  self.sliderPanelAr[i] := TWebPanel.create(self.RSimWPanel);
  self.sliderPanelAr[i].parent := self.RSimWPanel;
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
  self.drawPlotLegends(); // Not correct place for this?
end;

// Called when adding or updating a param slider. sn = slider number
procedure TMainForm.SetSliderParamValues(sn, paramForSlider: Integer);
var
  rangeMult: Integer;
  pVal:Double;
  pName: String;
begin
  rangeMult := SLIDER_RANGE_MULT; //10; // default.
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
  //if self.rightPanelType = SIMULATION_PANEL then
    //begin
      self.RNodeEditWPanel.Width := self.RSimWPanel.Width;
      self.RNodeEditWPanel.Top := self.RSimWPanel.Top;
      self.RNodeEditWPanel.Height := self.RSimWPanel.Height;
      self.RNodeEditWPanel.Left := self.RSimWPanel.Left;
      self.RRxnEditWPanel.Width := self.RSimWPanel.Width;
      self.RRxnEditWPanel.Top := self.RSimWPanel.Top;
      self.RRxnEditWPanel.Height := self.RSimWPanel.Height;
      self.RRxnEditWPanel.Left := self.RSimWPanel.Left;
      self.RSimWPanel.invalidate;
  {  end
  else if self.rightPanelType = NODE_PANEL then
    begin
      self.RSimWPanel.Width := self.RNodeEditWPanel.Width;
      self.RSimWPanel.Top := self.RNodeEditWPanel.Top;
      self.RSimWPanel.Height := self.RNodeEditWPanel.Height;
      self.RSimWPanel.Left := self.RNodeEditWPanel.Left;
      self.RSimWPanel.invalidate;
      self.RRxnEditWPanel.Width := self.RSimWPanel.Width;
      self.RRxnEditWPanel.Top := self.RSimWPanel.Top;
      self.RRxnEditWPanel.Height := self.RSimWPanel.Height;
      self.RRxnEditWPanel.Left := self.RSimWPanel.Left;
      self.RNodeEditWPanel.invalidate;
    end
    else if self.self.rightPanelType = REACTION_PANEL then
        begin
          self.RSimWPanel.Width := self.RRxnEditWPanel.Width;
          self.RSimWPanel.Top := self.RRxnEditWPanel.Top;
          self.RSimWPanel.Height := self.RRxnEditWPanel.Height;
          self.RSimWPanel.Left := self.RRxnEditWPanel.Left;
          self.RNodeEditWPanel.Width := self.RSimWPanel.Width;
          self.RNodeEditWPanel.Top := self.RSimWPanel.Top;
          self.RNodeEditWPanel.Height := self.RSimWPanel.Height;
          self.RNodeEditWPanel.Left := self.RSimWPanel.Left;
          self.RRxnEditWPanel.invalidate;
        end;  }

  self.RSimWPanel.invalidate;
  self.RNodeEditWPanel.invalidate;
  self.RRxnEditWPanel.invalidate;
end;

procedure TMainForm.updateRxnRatePanel(); // Refresh with current rxn info.
begin
  try
    self.rateLawEqLabel.Caption := networkController.network.reactions[networkController.selectedEdge].state.rateLaw; // use getRateLaw instead.
    self.RxnRatePanel.Width := self.rateLawEqLabel.Width + self.rateLawLabel.Width + 60;
    self.RxnRatePanel.invalidate;
    self.RSimWPanel.visible := false;
    self.RRxnEditWPanel.visible := true;
    self.RNodeEditWPanel.visible := false;
    self.RSimWPanel.invalidate;
    self.RNodeEditWPanel.invalidate;
    self.RRxnEditWPanel.invalidate;
  except
     on E: Exception do
      notifyUser(E.message);
  end;

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
    self.RxnParamComboBox.AddItem(networkController.network.reactions[networkController.selectedEdge].state.rateParams.Items[i].getId,nil);
  end;
  if self.rxnParamComboBox.Items.count >0 then
  begin
    self.rxnParamEdit.text := floattostr(networkController.network.reactions[networkController.selectedEdge].state.rateParams[0].getValue);
  end;
  self.RxnParamComboBox.invalidate;

end;

procedure TMainForm.updateRxnStoichPanel();
var i: integer;
begin
  self.clearRxnStoichCoeffs();
  for i := 0 to Length(networkController.network.reactions[networkController.selectedEdge].state.srcId)-1 do
  begin
    if networkController.network.reactions[networkController.selectedEdge].state.srcId[i] <> '' then
      self.addRxnStoichEdit(i, true);

  end;

  for i := 0 to Length(networkController.network.reactions[networkController.selectedEdge].state.destId)-1 do
  begin
    if networkController.network.reactions[networkController.selectedEdge].state.destId[i] <> '' then
    begin
      self.addRxnStoichEdit(i, false);
    end;
  end;

end;

procedure TMainForm.addRxnStoichEdit(spIndex: integer; rxnReactant: boolean);
// Handle updating the stoich for each species of a rxn. May be more than one src and dest species.
  procedure editRxnSpStoich(Sender: TObject; src: boolean);
  var newStoich: double;
  begin
  //console.log('addRxnStoichEdit');
  if Sender.classType = TWebEdit then
   begin
     try
     begin
       newStoich := strtofloat((Sender as TWebEdit).Text);
       if src = true then
         networkController.setReactionSpecStoich((Sender as TWebEdit).Tag, newStoich, src)
       else
         networkController.setReactionSpecStoich((Sender as TWebEdit).Tag, newStoich, src);
       self.updateRxnRatePanel;
     end;
     except
       on Exception: EConvertError do
      // ShowMessage(Exception.Message);
         notifyUser ('Stoichiometric coefficient must be a number');
     end;

   end;
  end;

  procedure editRxnSrcStoichExit(Sender: TObject);
  begin
    editRxnSpStoich(Sender, true);
  end;
  procedure editRxnDestStoichExit(Sender: TObject);
  begin
    editRxnSpStoich(Sender, false);
  end;
// ----------------------------------------------------
const EDITBOX_HT = 25;
var i: integer;
    newStoichLabel: TWebLabel;
    newStoichEdit: TWebEdit;
begin
    newStoichLabel := TWebLabel.create(self.RxnSpStoichPanel);
    newStoichLabel.parent := self.RxnSpStoichPanel;
    newStoichLabel.Left := 20;
    newStoichLabel.font.color := clWhite;
    newStoichLabel.font.size := 12;
    newStoichEdit := TWebEdit.create(self.RxnSpStoichPanel);
    newStoichEdit.parent := self.RxnSpStoichPanel;
    newStoichEdit.font.size := 12;
    newStoichEdit.Left := 100;
    newStoichEdit.Width := 50;
    newStoichEdit.Tag := spIndex;  // species index.

    if rxnReactant then
    begin
      if networkController.network.reactions[networkController.selectedEdge].state.srcId[spIndex] <> '' then
      begin
        newStoichLabel.caption := networkController.network.reactions[networkController.selectedEdge].state.srcId[spIndex];
        newStoichLabel.top := self.StoicReactantsLabel.top + self.StoicReactantsLabel.height + round(spIndex*EDITBOX_HT);
        rxnReactStoichLabels.add(newStoichLabel);
        newStoichEdit.Top := self.StoicReactantsLabel.top + self.StoicReactantsLabel.height + round(spIndex*EDITBOX_HT);

        newStoichEdit.onExit := editRxnSrcStoichExit;
        newStoichEdit.Text := networkController.network.reactions[networkController.selectedEdge].state.srcStoich[spIndex].toString();
        rxnReactStoichEdits.add(newStoichEdit);
      end;
    end
    else
    begin
      if networkController.network.reactions[networkController.selectedEdge].state.destId[spIndex] <> '' then
      begin
       newStoichLabel.caption := networkController.network.reactions[networkController.selectedEdge].state.destId[spIndex];
       newStoichLabel.top := self.StoichProductsLabel.top + self.StoichProductsLabel.height + round(spIndex*20);

       rxnProdStoichLabels.add(newStoichLabel);
       newStoichEdit.Top := self.StoichProductsLabel.top + self.StoichProductsLabel.height + round(spIndex*20);
       newStoichEdit.onExit := editRxnDestStoichExit;
       newStoichEdit.Text := networkController.network.reactions[networkController.selectedEdge].state.destStoich[spIndex].toString();
       rxnProdStoichEdits.add(newStoichEdit);
      end;
    end;


end;

procedure TMainForm.clearRxnStoichCoeffs();
var i: integer;
    tempObj: TObject;
begin
     // Delete all of Stoich labels and edit boxes:
    for i := self.rxnReactStoichLabels.count -1 downto 0 do
    begin
       tempObj := self.rxnReactStoichLabels[i];
       tempObj.Free;
       self.rxnReactStoichLabels.Delete(i);
    end;
    for i := self.rxnProdStoichLabels.count -1 downto 0 do
    begin
       tempObj := self.rxnProdStoichLabels[i];
       tempObj.Free;
       self.rxnProdStoichLabels.Delete(i);
    end;
     for i := self.rxnReactStoichEdits.count -1 downto 0 do
    begin
       tempObj := self.rxnReactStoichEdits[i];
       tempObj.Free;
       self.rxnReactStoichEdits.Delete(i);
    end;
    for i := self.rxnProdStoichEdits.count -1 downto 0 do
    begin
       tempObj := self.rxnProdStoichEdits[i];
       tempObj.Free;
       self.rxnProdStoichEdits.Delete(i);
    end;
end;

procedure TMainForm.clearRxnNodeRightPanels();
// When rxn or node deleted, may need to clear Right Panel to prevent reading undefined 'state'
begin
    self.rateLawEqLabel.Caption := '';
    self.RxnParamComboBox.clear;
    self.clearRxnStoichCoeffs();
    self.rightPanelType := SIMULATION_PANEL; // Right panel no longer shows deleted obj
    self.setRightPanels;
end;

procedure TMainForm.setUpSimulationUI();
var i: integer;
begin
  self.currentGeneration := 0; // reset current x axis point (pixel)
  if self.networkUpdated then // TODO: do not reset plots if species or param init vals have changed.
  begin
      // delete existing plots
    if self.numbPlots >0 then
    begin
      if (self.plotsPBList.Count) > 0 then
      begin
        for i := self.plotsPBList.Count-1 downto 0 do
        begin
          self.DeletePlot(i);
        end;
        self.numbPlots := 0;
      end;

    end;
    // delete existing param sliders.
    if self.sliderPanelAr <> nil then
    begin
      if length(self.sliderPanelAr) >0 then
      begin
        for i := length(self.sliderPanelAr) -1 downto 0 do
        begin
          self.DeleteSlider(i);
        end;
        setLength(self.sliderPanelAr, 0);
      end;
    end;
    mainController.createModel;
    self.networkUpdated := false;
    end;

  self.rightPanelType := SIMULATION_PANEL;
  self.setRightPanels;
  if self.mainController.getModel = nil then
  begin
    self.mainController.createModel;
    self.networkUpdated := false;
  end;
  self.mainController.createSimulation;
  if self.numbPlots >0 then
    self.resetPlots();
  self.RSimWPanel.invalidate;
end;

end.
