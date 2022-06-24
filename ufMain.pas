unit ufMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, Types, WEBLib.Graphics,
  WEBLib.Controls, StrUtils,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Dialogs, WEBLib.ExtCtrls,
  WEBLib.WebCtrls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.Buttons, Vcl.Imaging.pngimage,
  Vcl.Graphics,System.Generics.Collections,
  uControllerNetwork, uNetworkCanvas, uNetwork, Vcl.TMSFNCTypes, Vcl.TMSFNCUtils,
  Vcl.TMSFNCGraphics, Vcl.TMSFNCGraphicsTypes, Vcl.TMSFNCCustomControl, Vcl.TMSFNCScrollBar,
  uNetworkTypes, WEBLib.Lists, Vcl.Forms, uModel, uSBMLClasses, uSBMLClasses.rule, uSimulation,
  uControllerMain, uODE_FormatUtility, uGraphP, Vcl.Menus, WEBLib.Menus, ufVarSelect,
  uPlotPanel, uParamSliderLayout, uSidewinderTypes, WEBLib.ComCtrls, WEBLib.Miletus,
  WEBLib.JQCtrls, ufAssignments, ufSelectExample;

const SIDEWINDER_VERSION = 'Version 0.47 alpha';
      DEFAULT_RUNTIME = 10000;
      EDITBOX_HT = 25;
      ZOOM_SCALE = 20;
      MAX_STR_LENGTH = 50;  // Max User inputed string length for Rxn/spec/param id
      DEBUG = false; // true then show debug console output and any other debug related info

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
    btnOnLineSim: TWebButton;
    WebPanel1: TWebPanel;
    xLbl: TWebLabel;
    yLbl: TWebLabel;
    zoomLbl: TWebLabel;
    zoomFactorLbl1: TWebLabel;
    SBMLmodelMemo: TWebMemo;
    ZoomCntrlPanel: TWebPanel;
    zoomCtlLabel: TWebLabel;
    zoomTrackBar: TWebTrackBar;
    btnAddPlot: TWebButton;
    btnParamAddSlider: TWebButton;
    NetworkJSONOpenDialog: TWebOpenDialog;
    loadNetworkButton: TWebButton;
    SBMLOpenDialog: TWebOpenDialog;
    SBMLloadButton: TWebButton;
    pnlBase: TWebPanel;  // ??
    LeftWPanel: TWebPanel;
    btnUniUni: TWebSpeedButton;
    btnUniBi: TWebSpeedButton;
    btnBiUni: TWebSpeedButton;
    btnBiBi: TWebSpeedButton;
    btnIdle: TWebSpeedButton;
    btnAddNode: TWebSpeedButton;
    RSimWPanel: TWebPanel;
    plotEditLB: TWebListBox;
    SliderEditLB: TWebListBox;
    pnlCenter: TWebPanel;
    networkPB1: TWebPaintBox;
    netDrawScrollBarVert: TTMSFNCScrollBar;
    netDrawScrollBarHoriz: TTMSFNCScrollBar;
    splitter: TWebSplitter;
    SaveSBMLButton: TWebButton;
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
    SplitterPlotSlider: TWebSplitter;
    pnlSliderContainer: TWebPanel;
    pnlSimResultsFile: TWebPanel;
    btnSaveSimResults: TWebButton;
    lblFileName: TWebLabel;
    lblSimDataFileName: TWebLabel;
    btnStopSimSave: TWebButton;
    lblRxnId: TWebLabel;
    lblRxnIdString: TWebLabel;
    pnlPlotContainer: TWebPanel;
    simResultsMemo: TWebMemo;
    pnlNodePanel: TWebPanel;
    nodeOutlineLabel: TWebLabel;
    nodeFillLabel: TWebLabel;
    editNodeLabel: TWebLabel;
    nodeConcLabel: TWebLabel;
    btnNodeOutlineColor: TWebColorPicker;
    editNodeId: TWebEdit;
    btnNodeFillColor: TWebColorPicker;
    editNodeConc: TWebEdit;
    WebLabel1: TWebLabel;
    btnCloseNodePanel: TWebButton;
    btnResetSimSpecies: TWebButton;
    pnlMenu: TWebPanel;
    WebMainMenu: TWebMainMenu;
    File1: TMenuItem;
    mniNew: TMenuItem;
    mnuImportSBML: TMenuItem;
    edit1: TMenuItem;
    Help1: TMenuItem;
    mnuAbout: TMenuItem;
    Undo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    WebConsoleLog1: TWebConsoleLog;
    pnlReactionPanel: TWebPanel;
    btnCloseReactionEditPanel: TWebButton;
    N2: TMenuItem;
    mnuSaveJSon: TMenuItem;
    mnuOpenJson: TMenuItem;
    mnuSaveSBML: TMenuItem;
    lblReactionEditing: TWebLabel;
    WebLabel2: TWebLabel;
    edtReactionId: TWebEdit;
    btnReactionColor: TWebColorPicker;
    WebLabel3: TWebLabel;
    edtReactionWidth: TWebSpinEdit;
    WebLabel4: TWebLabel;
    btnParamReset: TWebButton;
    btnResetRun: TWebButton;
    checkBoxBoundarySp: TWebCheckBox;
    ButtonVarAssignments: TWebButton;
    pnlSimSpeedMult: TWebPanel;
    lblStepSize: TWebLabel;
    trackBarSimSpeed: TWebTrackBar;
    lblStepSizeMin: TWebLabel;
    lblStepSizeMax: TWebLabel;
    lblStepSizeVal: TWebLabel;
    pnlStepSize: TWebPanel;
    stepSizeLabel1: TWebLabel;
    stepSizeEdit1: TWebEdit;

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

    procedure btnOnLineSimClick(Sender: TObject);
    procedure runSim();
    procedure stopSim();
    procedure btnAddPlotClick(Sender: TObject);
    procedure btnParamAddSliderClick(Sender: TObject);
    procedure loadNetworkButtonClick(Sender: TObject);
    procedure NetworkJSONOpenDialogChange(Sender: TObject);
    procedure NetworkJSONOpenDialogGetFileAsText(Sender: TObject;
      AFileIndex: Integer; AText: string);
    procedure SBMLloadButtonClick(Sender: TObject);
    procedure SBMLOpenDialogChange(Sender: TObject);
    procedure SBMLOpenDialogGetFileAsText(Sender: TObject; AFileIndex: Integer;
      AText: string);
    procedure btnClearClick(Sender: TObject);
    procedure btnNodeFillColorSelect(Sender: TObject);
    procedure btnNodeOutlineColorSelect(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure ParamSliderOnChange(Sender: TObject);
    // User changes value of parameter
    procedure plotEditLBClick(Sender: TObject);
    procedure SliderEditLBClick(Sender: TObject);
    procedure splitterMoved(Sender: TObject);
    procedure btnSimpleClick(Sender: TObject);
    procedure SaveSBMLButtonClick(Sender: TObject);
    procedure resetInitValsButtonClick(Sender: TObject);
    procedure editNodeConcExit(Sender: TObject);
    procedure RPanelTabSetClick(Sender: TObject);
    procedure RxnParamComboBoxChange(Sender: TObject);
    procedure RxnParamComboBoxClick(Sender: TObject);
    procedure RxnParamComboBoxExit(Sender: TObject);
    procedure RxnParamEditExit(Sender: TObject);
    procedure SplitterPlotSliderMoved(Sender: TObject);
    procedure btnSaveSimResultsClick(Sender: TObject);
    procedure btnStopSimSaveClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure networkPB1DblClick(Sender: TObject);
    procedure btnCloseNodePanelClick(Sender: TObject);
    procedure btnCloseReactionEditPanelClick(Sender: TObject);
    procedure btnReactionColorSelect(Sender: TObject);
    procedure edtReactionWidthChange(Sender: TObject);
    procedure trackBarSimSpeedChange(Sender: TObject);
    procedure btnParamResetClick(Sender: TObject);
    procedure btnResetRunClick(Sender: TObject);
    procedure edtReactionIdExit(Sender: TObject);
    procedure stepSizeEdit1Exit(Sender: TObject);
    procedure checkBoxBoundarySpClick(Sender: TObject);
    procedure ButtonVarAssignmentsClick(Sender: TObject);
    procedure displayVarAssignments(rxnId: string);
    procedure splitterClick(Sender: TObject);

  private
    numbPlots: Integer; // Number of plots displayed
    numbSliders: Integer; // Number of parameter sliders
    stepSize: double; // default is 0.1
    rightPanelType: TPanelType;
    networkUpdated: boolean; // Network has changed, update model, plots, etc when convenient.
    rxnReactStoichLabels: TList<TWebLabel>;
    rxnReactStoichEdits: TList<TWebEdit>;
    rxnProdStoichLabels: TList<TWebLabel>;
    rxnProdStoichEdits: TList<TWebEdit>;
    saveSimResults: boolean;
    fSelectExample: TformExamples;
    procedure InitSimResultsTable(); // Init simResultsMemo.
    procedure addPlot(yMax: double); // Add a plot, yMax: largest initial val of plotted species
    procedure resetPlots();  // Reset plots for new simulation.
    procedure selectPlotSpecies(plotnumb: Integer);
    procedure addPlotAll(); // add plot of all species
    procedure deletePlot(plotIndex: Integer); // Index of plot to delete
    procedure deleteAllPlots();
    function  getEmptyPlotPosition(): Integer;
    function  getPlotPBIndex(plotTag: integer): Integer; // Return Plot index of tag.
    function  getSliderIndex(sliderTag: integer): Integer;
    procedure EditPlotList(plotn: Integer);
    procedure updatePlots(); // Go through and remove species/plots no longer in model. needed ?
    procedure initializePlots();
    procedure initializePlot( n: integer);
    procedure deletePlotSpecies(plotn: Integer); // Delete a species curve in a plot.
             // delete, change plot species, other added as needed using TWebListBox.
    procedure addParamSlider();
    procedure addAllParamSliders(); // add sliders without user intervention.
    procedure SetSliderParamValues(sn, paramForSlider: Integer);
    procedure resetSliderPositions(); // Reset param position to init model param value.
    procedure selectParameter(sNumb: Integer); // Get parameter for slider
    procedure LoadJSONFile();
    procedure EditSliderList(sn: Integer);
            // delete, change param slider as needed using TWebListBox.
    procedure resetBtnOnLineSim(); // reset to default look and caption of 'Start simulation'
    procedure deleteSlider(sn: Integer); // sn: slider index
    procedure deleteAllSliders();
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
    procedure refreshPlotAndSliderPanels();
    procedure refreshPlotPanels();
    procedure refreshSliderPanels();
    procedure clearNetwork(); // clear network canvas and delete all nodes, edges

    procedure enableEditNodePanel;
    procedure disableEditNodePanel;
    function enableStepSizeEdit(): boolean; // true: success
    function disableStepSizeEdit(): boolean;// true: success

    procedure enableEditReactionPanel;
    procedure disableEditReactionPanel;
  public
    network: TNetwork;
    networkController: TController;
    networkCanvas: TNetworkCanvas;
    origin: TPointF;
    fileName: string;
    currentGeneration: Integer; // Used by plots as current x axis point
    fPlotSpecies: TVarSelectForm;
    plotSpecies: TList<TSpeciesList>; // species to graph for each plot
    plotsPanelList: TList<TPlotPanel>; // Panels in which each plot resides
 //   fSliderParameter: TParamSliderSForm;// Pop up form to choose parameter for slider.
    fSliderParameter: TVarSelectForm;// Pop up form to choose parameter for slider.
    sliderParamAr: array of Integer;
    // holds parameter array index of parameter (p_vals) for each slider
    pnlSliderAr: array of TWebPanel; // Holds parameter sliders
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
    procedure networkHasChanged(sender: TObject); // Notify when network has changed, may need to update model, plots, etc
    procedure getVals( newTime: Double; newVals: TVarNameValList);// Get new values (species amt) from simulation run
    procedure generateAutoLayout(sender: TObject); // network needs a new layout generated ( fruchterman_reingold)

  end;

var
  mainForm: TMainForm;
 // spSelectform: TVarSelectForm; // display speciecs select to plot radio group

implementation

{$R *.dfm}

Uses uGraphUtils, uCreateNetworks, uLayout, uTestModel, uSelectedObjects;


procedure TMainForm.enableEditNodePanel;
begin
  self.editNodeId.Enabled := true;
  self.editNodeConc.Enabled := true;
  self.checkBoxBoundarySp.Enabled := true;
  self.btnNodeFillColor.Enabled := true;
  self.btnNodeOutlineColor.Enabled := true;
end;


procedure TMainForm.disableEditNodePanel;
begin
  self.editNodeId.Enabled := false;
  self.editNodeConc.Enabled := false;
  self.checkBoxBoundarySp.Enabled := false;
  self.btnNodeFillColor.Enabled := false;
  self.btnNodeOutlineColor.Enabled :=false;
end;

procedure TMainForm.enableEditReactionPanel;
begin
  edtReactionId.Enabled := true;
  btnReactionColor.Enabled := true;
  edtReactionWidth.Enabled := true;
end;

procedure TMainForm.disableEditReactionPanel;
begin
  edtReactionId.Enabled := false;
  btnReactionColor.Enabled := false;
  edtReactionWidth.Enabled := false;
end;


procedure TMainForm.btnAddPlotClick(Sender: TObject);
begin
  // Make runtime, stepsize, simulation buttons visible
  self.numbPlots := self.numbPlots + 1;
 // rtLabel1.visible := true;  // Run time: Do not let user modify for now
 // rtLengthEdit1.visible := true; // Do not let user modify for now
  stepSizeLabel1.visible := true;
  stepSizeEdit1.visible := true;
  self.selectPlotSpecies(self.numbPlots);
end;

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  notifyUser(SIDEWINDER_VERSION);
end;

procedure TMainForm.btnAddNodeClick(Sender: TObject);
begin
  networkController.setAddNodeStatus;
end;

procedure TMainForm.btnAutoLayoutClick(Sender: TObject);
begin
  // showmessage (inttostr (networkPB1.Width) + ', ' + inttostr (networkPB1.Width));
   self.generateAutoLayout(nil);
end;

procedure TMainForm.generateAutoLayout(sender: TObject);
var i, j : integer;
    cx, cy : double;
    pt : TPointF;
    srcNodes, destNodes : array of TNode;
    nodeid : string;
    index : integer;
begin
  fruchterman_reingold(network, networkPB1.width, networkPB1.Height, 600, nil);

  for i := 0 to length (network.reactions) - 1 do
      begin
      setlength (srcNodes, network.reactions[i].state.nReactants);
      for j := 0 to network.reactions[i].state.nReactants - 1 do
          begin
          nodeId := network.reactions[i].state.srcId[j];
          network.findNode (nodeId, index);
          srcNodes[j] := network.nodes[index];
          end;
      for j := 0 to network.reactions[i].state.nProducts - 1 do
          begin
          nodeId := network.reactions[i].state.destId[j];
          network.findNode (nodeId, index);
          destNodes[j] := network.nodes[index];
          end;
      network.computeAnyToAnyCoordinates (network.reactions[i], srcNodes, destNodes);
      end;

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
  self.clearNetwork;
end;

procedure TMainForm.btnDrawClick(Sender: TObject);
  procedure afterCreate(AForm: TObject);
  var exampleList: TStringList;
  begin
    exampleList := TStringList.Create;
    exampleList.Add('Simple network');
    exampleList.Add('Feedback example');
    exampleList.Add('Goldbeter 1990, calcium spike');
    exampleList.Add('Glycolysis');
    (AForm as TformExamples).rgSelectExample.Items := exampleList;
  end;

  procedure afterShowModal(AValue: integer);
  var i: integer;
      strModel: string;
  begin
    i := self.fSelectExample.indexExampleChosen;
    strModel := getTestModel(i);
    self.SBMLOpenDialogGetFileAsText(nil,0, strModel);
    if i = 3 then
      begin
      self.stepSizeEdit1.Text := inttostr(10); // glycolysis example
      self.stepSizeEdit1Exit(nil);
      end;
  end;
//var
  //n1, n2, n3, n4: TNode;
 // srcNodes, destNodes : array of TNode;
begin
  //setLength (srcNodes, 1); setLength (destNodes, 1);
  if length(network.getCurrentState.savedNodes) = 0 then
  begin
    // ****************************
    self.fSelectExample := TformExamples.CreateNew(@afterCreate);
    self.fSelectExample.Popup := true;

    self.fSelectExample.PopupOpacity := 0.3;
    self.fSelectExample.ShowModal(@AfterShowModal);
  end
  else
    notifyUser('Network already exists in panel.');
end;


procedure TMainForm.btnIdleClick(Sender: TObject);
begin
  networkController.setSelectStatus;
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

procedure TMainForm.btnSaveSimResultsClick(Sender: TObject);
var defaultName: string;
begin
  defaultName := 'simRun.txt';
  if self.lblSimDataFileName.Caption <> '' then
    defaultName := self.lblSimDataFileName.Caption;
  if self.saveSimResults = false then
    self.saveSimResults := true;

 fileName := InputBox('Save simulation results to the downloads directory',
    'Enter File Name:', defaultName);
  if fileName <> '' then
    begin
      self.lblSimDataFileName.visible := true;
      self.lblSimDataFileName.Caption := fileName;
      self.saveSimResults := true;
      self.btnStopSimSave.Visible := true;
    end
  else
    begin
      notifyUser('Save cancelled');
      self.saveSimResults := false;
    end;
end;

procedure TMainForm.btnSimpleClick(Sender: TObject);
var s : string;
begin
  s := getTestModel(0);
  if DEBUG then
    begin
    SBMLmodelMemo.Lines.Text := s;
    SBMLmodelMemo.visible := true;
    end;
  self.MainController.loadSBML(s);
 end;

procedure TMainForm.btnStopSimSaveClick(Sender: TObject);
begin
  self.saveSimResults := false;
  self.btnStopSimSave.Visible := false;
  self.lblSimDataFileName.visible := false;
end;

procedure TMainForm.btnUniBiClick(Sender: TObject);
begin
  networkController.setAddUniBiReaction;
end;

procedure TMainForm.btnUniUniClick(Sender: TObject);
begin
  networkController.setAddUniUniReaction;
end;


procedure TMainForm.ButtonVarAssignmentsClick(Sender: TObject);
var rxnId: string;
begin
// TODO Get a list of assignments for species and params used in reaction.
  rxnId := '';
  if networkController.selectedObjects.count > 0 then
    begin
      case networkController.selectedObjects[0].objType of
       // oNode     : begin
         //          networkController.selectedObjects[0].node.state.species;
         //          end;
       oReaction : begin
                   rxnId := networkController.selectedObjects[0].reaction.state.id;
                   self.displayVarAssignments(rxnId);
                   end;
      end;
    end;

end;

procedure TMainForm.displayVarAssignments(rxnId: string);

  // *****************************************
  procedure AfterCreate(AForm: TObject);
  var i, j, rxnIndex: integer;
      strListOfAssign: TStringList;
      curRule: TSBMLRule;
      curStr: string;
      foundVar: string;
  begin
    rxnIndex := 0;
   // strTitle := '';
    strListOfAssign := TStringList.create;
    if self.networkController.findReaction(rxnId, rxnIndex) then
      begin
        for i := 0 to self.network.getNumRules -1 do
          begin
            curRule := nil;
            curStr := '';
            foundVar := '';
            curRule := self.network.getRule(i);
            for j := 0 to self.network.reactions[rxnIndex].state.nReactants -1 do
              begin
              if curRule.getVariable = self.network.reactions[rxnIndex].state.srcId[j] then
                begin
                foundVar := self.network.reactions[rxnIndex].state.srcId[j]
                end;
              end;

            if foundVar = '' then
              begin
              for j := 0 to self.network.reactions[rxnIndex].state.nProducts -1 do
                begin
                if curRule.getVariable = self.network.reactions[rxnIndex].state.destId[j] then
                  begin
                  foundVar := self.network.reactions[rxnIndex].state.destId[j]
                  end;
                end;
              end;

            if foundVar = '' then
              begin
              for j := 0 to self.network.reactions[rxnIndex].state.rateParams.Count -1 do
                begin
                if curRule.getVariable = self.network.reactions[rxnIndex].state.rateParams[j].getId then
                  begin
                  foundVar := self.network.reactions[rxnIndex].state.rateParams[j].getId;
                  end;
                end;
              end;

            if (foundVar <> '') and (curRule.isAssignment) then
              begin
              curStr := foundVar + ' = ' + curRule.getFormula;
              strListOfAssign.Add(curStr);
              end;

          end;

        if strListOfAssign.Count <1 then strListOfAssign.Add('No variable assignments found.');

      end
    else strListOfAssign.Add('No reaction with that id found');

    //(AForm as TFormAssignments).Top := trunc(self.Height*0.2); // put popup %20 from top
    (AForm as TFormAssignments).listOfAssignments := strListOfAssign;
    (AForm as TFormAssignments).fillAssignmentList();
  end; // afterCreate
  /// **********************************************************************

var rxnIndex: integer;
    fShowAssignments: TFormAssignments;
    curRxnState: TReactionState;
    strTitle: string;
begin

  if self.networkController.findReaction(rxnId, rxnIndex) then
      begin
      curRxnState := self.network.reactions[rxnIndex].state;
      if self.network.getNumRules > 0 then
        begin
        strTitle := 'Reaction ' + self.network.reactions[rxnIndex].state.id + ' variable assignments:';
        fShowAssignments := TFormAssignments.CreateNew(@AfterCreate);
        fShowAssignments.Popup := true;
        fShowAssignments.ShowClose := true;
        fShowAssignments.PopupOpacity := 0.3;
        fShowAssignments.Border := fbDialogSizeable;
        fShowAssignments.Caption := strTitle;
        fShowAssignments.ShowModal({@AfterShowModal});
        end
      else notifyUser( ' No Assignments for variables used in reaction.' );
      end
  else notifyUser( ' No reaction with that id found.' );

end;

procedure TMainForm.editNodeIdExit(Sender: TObject);
var newId, cutNewId: string;
begin
//console.log('editNodeIdExit ');
  newId := editNodeId.Text;
  if length(newId) > MAX_STR_LENGTH then
    begin
      cutNewId := newId.Substring(0, MAX_STR_LENGTH);
      networkController.setNodeId(cutNewId);
      self.editNodeId.Text := cutNewId;
    end
  else networkController.setNodeId(newId);
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
  if DEBUG then
    begin
    SBMLmodelMemo.Lines.Text := AText;
    SBMLmodelMemo.visible := true;
    end;
  // Check if sbmlmodel already created, if so, destroy before creating ?
  self.deleteAllPlots;
  self.deleteAllSliders;
  self.resetBtnOnLineSim;
  self.btnResetSimSpecies.enabled := false;
  self.btnParamReset.enabled := false;
  self.btnResetRun.enabled := false;
  self.MainController.loadSBML(AText);
end;

procedure TMainForm.resetBtnOnLineSim();
begin
  self.btnOnLineSim.ElementClassName := 'btn btn-primary btn-sm';
  self.btnOnLineSim.caption := 'Start Simulation';
  self.btnAddPlot.Enabled := false;
  self.btnParamAddSlider.Enabled := false;
  self.enableStepSizeEdit;
  //onLineSimButton.Enabled := false;
end;

procedure TMainForm.btnOnLineSimClick(Sender: TObject);
var
  i: Integer;
  yScaleWidth, newYMax : integer;
begin
 if length(self.network.reactions) < 1 then     // Fix issue 1, 21dec16
   begin
   notifyUser(' No network or model created for simulation. ');
   exit;
   end;

 if MainController.isOnline = false then
   self.runSim
 else  // stop simulation
   self.stopSim;
end;

procedure TMainForm.runSim();
begin
  self.btnAddPlot.Enabled := true;
  self.btnParamAddSlider.Enabled := true;
  self.enableStepSizeEdit;
  if self.networkUpdated = true then
    begin
    self.setUpSimulationUI;
    self.btnOnLineSim.font.color := clgreen;
    self.btnOnLineSim.ElementClassName := 'btn btn-danger btn-sm';
    self.btnOnLineSim.caption := 'Simulation: Play';
     // add a default plot:
    if self.numbPlots < 1 then
      begin
     // if length( self.mainController.getModel.getS_Names ) < 10 then
        addPlotAll()
      end
    else self.btnAddPlotClick(nil);
      // add default param sliders:
    if self.numbSliders < 1 then
      begin
      //if length( self.mainController.getModel.getP_Names ) < 11 then
        //begin
      self.addAllParamSliders;
      if length( self.mainController.getModel.getP_Names ) < 11 then
        self.btnParamAddSlider.Enabled := false;
      end
    else self.btnParamAddSliderClick(nil);

    end;

  // ******************
  if self.mainController.IsModelLoaded then
    begin
      MainController.setOnline(true);
	    self.btnResetSimSpecies.Enabled := false;
      self.btnParamReset.Enabled := false;
      self.btnResetRun.Enabled := false;
      self.disableStepSizeEdit;
      self.btnOnLineSim.font.color := clred;
      self.btnOnLineSim.ElementClassName := 'btn btn-success btn-sm';
      self.btnOnLineSim.caption := 'Simulation: Pause';
      if DEBUG then
        simResultsMemo.visible := true;
      self.mainController.SetRunTime(DEFAULT_RUNTIME);
       // default timer interval is 100 msec:
      // multiplier default is 10, range 1 - 50
      self.mainController.SetTimerInterval(round(1000/self.trackBarSimSpeed.position));
      self.mainController.SetStepSize(self.stepSize);
       //  self.rtLengthEdit1.Text := FloatToStr(MainController.getRunTime);
      if self.mainController.getCurrTime = 0  then
        self.InitSimResultsTable();  // Set table of Sim results.
      self.rightPanelType := SIMULATION_PANEL;
      self.setRightPanels;
      MainController.SetTimerEnabled(true); // Turn on web timer (Start simulation)
      end
   else notifyUser(' No model created for simulation. ');
end;

procedure TMainForm.stopSim();
begin
   MainController.setOnline(false);
   self.btnResetSimSpecies.Enabled := true;
   self.btnParamReset.Enabled := true;
   self.btnResetRun.Enabled := true;
   self.enableStepSizeEdit;
   self.MainController.SetTimerEnabled(false); // Turn off web timer (Stop simulation)
   self.btnOnLineSim.font.color := clgreen;
   self.btnOnLineSim.ElementClassName := 'btn btn-danger btn-sm';
   self.btnOnLineSim.caption := 'Simulation: Play';
   if self.saveSimResults then
     begin
     self.mainController.writeSimData(self.lblSimDataFileName.Caption, self.simResultsMemo.Lines);
     end;
end;

procedure TMainForm.checkBoxBoundarySpClick(Sender: TObject);
begin
  networkController.setNodeBoundarySp(self.checkBoxBoundarySp.Checked);
  networkPB1.Invalidate;
end;

procedure TMainForm.clearNetwork();
begin
  self.mainController.clearModel;
  self.mainController.clearSim;
  self.deleteAllPlots;
  self.deleteAllSliders;
  self.networkController.clearNetwork;
  self.networkController.clearSelectedObjects;
  self.networkPB1.Invalidate;
  self.disableEditNodePanel;
  self.editNodeId.Text := '';
  self.editNodeConc.Text := '';
  self.edtReactionId.Text := '';
  self.resetBtnOnLineSim;
  self.btnResetSimSpecies.Enabled := false;
  self.btnParamReset.Enabled := false;
  self.btnResetRun.Enabled := false;
end;

procedure TMainForm.initializePlots();
  var i: Integer;
begin
  if plotsPanelList <> nil then
    begin
    for i := 0 to plotsPanelList.Count - 1 do
      begin
      self.initializePlot(i);
      end;
    end;
end;

procedure TMainForm.initializePlot( n: integer);
// var yScaleWidth, newYMax : integer;
begin
  try
    self.plotsPanelList[n].initializePlot( MainController.getRunTime,
                             MainController.getStepSize, self.plotSpecies[n]);
  except
    on E: Exception do
      notifyUser(E.message);
  end;

end;

procedure TMainForm.resetPlots();  // Reset plots for new simulation.
begin
  self.initializePlots;
end;

procedure TMainForm.PingSBMLLoaded(newModel:TModel);
var errList: string;
    i: integer;
begin
  if newModel.getNumSBMLErrors >0 then
    begin
    errList := '';
    for i := 0 to newModel.getNumSBMLErrors -1 do
      begin
      errList := errList + newModel.getSBMLErrorStrs()[i] + #13#10 ; // new line char
      end;
    errList := errList +  'Please fix or load a new model.';
    notifyUser(errList);
    clearNetwork();
    end
  else
  begin
    if newModel.getNumModelEvents > 0 then
      begin
      notifyUser(' SBML Events not supported at this time. Load a different SBML Model');
      clearNetwork();
      end
    else if newModel.getNumPiecewiseFuncs >0 then
      begin
      notifyUser(' SBML piecewise() function not supported at this time. Load a different SBML Model');
      clearNetwork();
      end
  {  else if newModel.getNumFuncDefs > 0 then
      begin
      notifyUser(' SBML FunctionDefinition not supported at this time. Load a different SBML Model' );
      clearNetwork();
      end; }

  end;
  if assigned(newModel.getSBMLLayout) then  // may want try/catch for layout not existing
    begin
    if newModel.getSBMLLayout <> nil then
      begin
    //console.log(' layout Width: ', trunc(newModel.getSBMLLayout.getDims.getWidth));
      self.networkPB1.Width := trunc(newModel.getSBMLLayout.getDims.getWidth);
      self.networkPB1.Height := trunc(newModel.getSBMLLayout.getDims.getHeight);
      end;
    end;

   // Loading new sbml model changes reaction network.
  self.networkPB1.invalidate;
  self.networkUpdated := true;
end;

procedure TMainForm.networkHasChanged(sender: TObject);
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
      self.deletePlot(getPlotPBIndex(self.plotEditLB.tag));
    end;
  // else ShowMessage('Canceled');
  self.plotEditLB.tag := 0;
  self.plotEditLB.visible := false;
  self.plotEditLB.Top := 40; // default    // Need to specify correct plotPanelList
end;

procedure TMainForm.RxnParamComboBoxChange(Sender: TObject);  // NOT needed. ??
var i: integer;
    paramInitAssign: string;
    curAssignRule: TSBMLRule;
begin
 //console.log('TMainForm.RxnParamComboBoxChange');
 paramInitAssign := '';
 curAssignRule := nil;
 self.rxnParamEdit.Enabled := true;
 i := self.RxnParamComboBox.ItemIndex;
 if self.network.getInitialAssignmentWithVarId(networkController.selectedObjects[0].reaction.state.rateParams[i].getId ) <> nil then
    paramInitAssign := self.network.getInitialAssignmentWithVarId(networkController.selectedObjects[0].reaction.state.rateParams[i].getId ).getFormula;
 if paramInitAssign = '' then
   begin  // If param has assignmentRule then do not allow editing.
   curAssignRule := self.networkController.network.getAssignmentRuleWithVarId(networkController.selectedObjects[0].reaction.state.rateParams[i].getId);
   if curAssignRule = nil then
     self.rxnParamEdit.text := floattostr(networkController.selectedObjects[0].reaction.state.rateParams[i].getValue)
   else self.rxnParamEdit.Enabled := false;


   end
 else self.rxnParamEdit.text := paramInitAssign;
 // TODO: What to do if someone removes initial Assignment?
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

  // TODO: CHeck if edited param has an initialAssignment, if it does, then delete it if user put in new value.
procedure TMainForm.RxnParamEditExit(Sender: TObject);
var newVal: double;
    paramId: string;
    reaction : TReaction;
begin
  reaction := self.networkController.selectedObjects[0].reaction;
  try
    begin
      newVal := strtofloat(self.RxnParamEdit.text);
      reaction.state.rateParams[self.RxnParamComboBox.ItemIndex].setValue(newVal);
      self.networkController.updateParamVal(reaction.state.rateParams[self.RxnParamComboBox.ItemIndex].getId, newVal);
      if self.network.getNumInitalAssignments > 0 then // Remove initial assignment:
        self.networkController.network.deleteInitialAssignment(reaction.state.rateParams[self.RxnParamComboBox.ItemIndex].getId);
    end;
  except
    on Exception : EConvertError do
    begin
    notifyUser(Exception.Message);
    self.RxnParamEdit.text := floattostr(reaction.state.rateParams[self.RxnParamComboBox.ItemIndex].getValue());
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
  if networkController.selectedObjects.count > 0 then
     begin
     case networkController.selectedObjects[0].objType of
       oNode     : begin enableEditNodePanel; disableEditReactionPanel; end;
       oReaction : begin enableEditReactionPanel; disableEditNodePanel end;
     else
       begin
       disableEditNodePanel;
       disableEditReactionPanel;
       end;
     end
     end
  else
    begin
    disableEditNodePanel;
    disableEditReactionPanel;
    end;

  networkPB1.Invalidate;
  if (networkController.selectedObjects.Count > 0) and (networkController.selectedObjects[0].objType = oNode) then
    begin
    if Assigned(networkController.selectedObjects[0].node.state) then
      begin
      self.editNodeId.Text := networkController.selectedObjects[0].node.state.species;
      self.editNodeConc.Text := networkCOntroller.selectedObjects[0].node.state.conc.ToString;
      self.checkBoxBoundarySp.Checked := networkCOntroller.selectedObjects[0].node.state.boundarySp;
      end;

    pnlNodePanel.visible := true;
    self.rightPanelType := NODE_PANEL;
    self.RRxnEditWPanel.visible := false;
    self.RSimWPanel.visible := false;
    self.RNodeEditWPanel.visible := true;
    self.RNodeEditWPanel.invalidate;
    self.setRightPanels;
    end
  else if (networkController.selectedObjects.Count > 0) and (networkController.selectedObjects[0].objType = oReaction) then
    begin
    self.rightPanelType := REACTION_PANEL;
    if Assigned(networkController.selectedObjects[0].reaction.state) then
      begin
      edtReactionId.Text := networkController.selectedObjects[0].reaction.state.id;
      btnReactionColor.Color := networkController.selectedObjects[0].reaction.state.fillColor;
      edtReactionWidth.Value := networkController.selectedObjects[0].reaction.state.thickness;
      end;
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
  networkController.setNodeConc(editNodeConc.Text);
  networkPB1.Invalidate;
end;

procedure TMainForm.btnParamAddSliderClick(Sender: TObject);
var
  i: Integer;
begin
  // TODO: Check if already 10 sliders, if so then showmessage( 'Only 10 parameter sliders allowed, edit existing one');
  self.selectParameter(length(sliderParamAr));
end;

procedure TMainForm.btnParamResetClick(Sender: TObject);
begin
  self.mainController.resetSimParamValues();
  self.resetSliderPositions();
end;

procedure TMainForm.resetSliderPositions();
var pVal: double; i: integer;
    pName: string;
begin
  for i := 0 to length(self.sliderPTBarAr) - 1 do
    begin
      pName :=  self.mainController.getModel.getP_Names[self.sliderParamAr[i]];
      pVal := self.mainController.getModel.getP_Vals[self.sliderParamAr[i]];
      self.sliderPTBLabelAr[i].caption := pName + ': ' + FloatToStr(pVal);
      self.sliderPTBarAr[i].Position := trunc((1 / SLIDER_RANGE_MULT) * 100);
    end;
end;

procedure TMainForm.ParamSliderOnChange(Sender: TObject);
var
  i, p: Integer;
  newPVal: double;
  isRunning: boolean;
begin
  if Sender is TWebTrackBar then
    begin
      newPVal := 0;
      isRunning := false; // simulation not active.
      i := TWebTrackBar(Sender).tag;
      self.MainController.paramUpdated := true;
      p := self.sliderParamAr[i];
      newPVal := self.sliderPTBarAr[i].Position * 0.01 *
        (sliderPHighAr[i] - sliderPLowAr[i]);
      // get slider parameter position in p_vals array
      self.sliderPTBLabelAr[i].Caption := floattostr(newPVal); // new
      if self.mainController.IsOnline then
        begin
          self.MainController.stopTimer;
          isRunning := true;
        end;
      self.MainController.changeSimParameterVal( p, newPVal );
      // recordValueEvent( self.MainController.getCurrTime, p, newPVal );
      if isRunning then self.MainController.startTimer;
      self.sliderPTBLabelAr[i].caption :=
           self.MainController.getModel.getP_Names[self.sliderParamAr[i]] + ': '
                                                         + FloatToStr(newPVal);
    end;
end;

procedure TMainForm.zoomTrackBarChange(Sender: TObject);
begin
  networkCanvas.scalingFactor := zoomTrackBar.Position / ZOOM_SCALE;
  networkPB1.Invalidate;
  zoomFactorLbl1.caption := FloatToStr(zoomTrackBar.Position / ZOOM_SCALE);
end;

function TMainForm.ScreenToWorld (X, Y: Double): TPointF;
begin
  result.X := (X + origin.X) / (zoomTrackBar.Position / ZOOM_SCALE);
  result.Y := (Y + origin.Y) / (zoomTrackBar.Position / ZOOM_SCALE);
end;

procedure TMainForm.netDrawScrollBarHorizValueChanged(Sender: TObject;
  Value: Double);
begin
  origin.X := Value;
  console.log('netDrawScrollBarHorizValue: ', Value);
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
  self.zoomTrackBar.Position := 20;
  self.zoomTrackBar.Min := 5;
  self.netDrawScrollBarHoriz.Value := 0;
  self.netDrawScrollBarVert.Value := 0;
  self.stepSize := 0.1;
  // This is to match the boostrap colors
  WebMainMenu.Appearance.BackgroundColor := RGB (50,56,62);

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
  self.mainController.setODEsolver;
  self.networkUpdated := false;
  self.saveSimResults := false;
  self.currentGeneration := 0;
  self.btnResetRun.Visible := true;
  self.btnParamReset.Visible := true;
  self.btnResetSimSpecies.Visible := true;
  self.btnResetRun.Enabled := false;
  self.btnParamReset.Enabled := false;
  self.btnResetSimSpecies.Enabled := false;
  self.btnParamAddSlider.Visible := true;
  self.btnAddPlot.Visible := true;
  self.btnParamAddSlider.Enabled := false;
  self.btnAddPlot.Enabled := false;
  self.enableStepSizeEdit;
  self.mainController.addSBMLListener( @self.PingSBMLLoaded );
  self.mainController.addNetworkListener( @self.networkHasChanged );
  self.mainController.addSimListener( @self.getVals ); // notify when new Sim results
  self.network.OnAutoLayoutEvent := self.generateAutoLayout;
  //self.checkBoxBoundarySp.Visible := false; // Do not make visible until reaction editing possible?

  if DEBUG then self.WebConsoleLog1.Visible := true
  else self.WebConsoleLog1.Visible := false;

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


procedure TMainForm.SplitterPlotSliderMoved(Sender: TObject);

begin
  self.refreshPlotAndSliderPanels();
end;

procedure TMainForm.refreshPlotAndSliderPanels();
begin
  self.refreshPlotPanels;
  self.refreshSliderPanels;

end;

procedure TMainForm.refreshPlotPanels;
var i: integer;
begin
 if assigned(self.plotsPanelList) then
   begin
     if self.plotsPanelList.count >0 then
     begin
     //console.log(' PlotWPanel width: ', plotsPanelList[0].plotWPanel.width, 'plot PB width: ', plotsPanelList[0].plotPB.width);
     for i := 0 to self.plotsPanelList.count -1 do
       begin
          plotsPanelList[i].setPlotPBWidth();
          plotsPanelList[i].initializePlot( MainController.getRunTime,
                             MainController.getStepSize, self.plotSpecies[i]);
          self.plotsPanelList[i].setPlotLegend(self.plotSpecies[i]);
       end;
     end;
   end;
end;

procedure TMainForm.refreshSliderPanels;
var i: integer;
begin
  if assigned(self.pnlSliderAr) then
    begin
    for i := 0 to Length(self.pnlSliderAr) - 1 do
      begin
      configPSliderPanel(i, 0, self.pnlSliderContainer.width, SLIDERPHEIGHT,
                         self.pnlSliderAr);
      configPSliderTBar(i, self.pnlSliderContainer.width, self.sliderPTBarAr,
             self.sliderPHLabelAr, self.sliderPLLabelAr, self.sliderPTBLabelAr);
      end;
    end;
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
  result := trunc(wx * zoomTrackBar.Position / ZOOM_SCALE);
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
      if not containsText(self.mainController.getModel.getS_Names()[i], '_Null') then // do not show null nodes
        simRTStr := simRTStr + ', ' + self.mainController.getModel.getS_Names()[i];
    end;
  simResultsMemo.Lines.Add(simRTStr);
 
end;

procedure TMainForm.SliderEditLBClick(Sender: TObject);
begin
  if self.SliderEditLB.ItemIndex = 0 then // change param for slider
    begin
      self.selectParameter(getSliderIndex(self.SliderEditLB.tag));
    end;
  if self.SliderEditLB.ItemIndex = 1 then // delete slider
    begin
      self.deleteSlider(getSliderIndex(self.SliderEditLB.tag));
    end;
  // else ShowMessage('Cancel');
  self.SliderEditLB.tag := -1;
  self.SliderEditLB.visible := false;
  self.SliderEditLB.Top := 40; // default
end;

procedure TMainForm.splitterClick(Sender: TObject);
begin
 // TODO   Popup with trackbar/slider to adjust self.splitter.left
 // self.splitterMoved(nil);
end;

procedure TMainForm.splitterMoved(Sender: TObject);
begin
  networkCanvas.bitmap.Height := networkPB1.Height;
  networkCanvas.bitmap.width := networkPB1.Width;
 // console.log('****splitter moved, networkPB1, height: ',networkPB1.Height,', width: ',networkPB1.Width);
  self.refreshPlotAndSliderPanels;

  self.adjustRightTabWPanels;
  networkPB1.Invalidate;
end;

procedure TMainForm.stepSizeEdit1Exit(Sender: TObject);
var newStep: integer;
    dblNewStep: double;
begin
  try
    dblNewStep := strToFloat(stepSizeEdit1.Text);
    if dblNewStep >0 then
      begin
      self.stepSize := dblNewStep * 0.001;
      self.mainController.SetStepSize(self.stepSize);
      end
    else notifyUser ('Step size must be a positive number integer');

  except
       on Exception: EConvertError do
         notifyUser ('Step size must be a positive number integer');
  end;

  if self.mainController.IsModelLoaded then
  begin
    self.mainController.createSimulation();
    if self.numbPlots >0 then
      self.initializePlots;
    self.currentGeneration := 0;
  end;
end;

procedure TMainForm.trackBarSimSpeedChange(Sender: TObject);
  var position: double;
begin
  position := self.trackBarSimSpeed.Position;
  self.lblStepSizeVal.Caption := floattostr( (position*0.1) ) + 'x';
  self.MainController.SetTimerInterval( round(1000/position) ); //timer interval does change. Speeds up/down sim
end;

// Get new values (species amt) from simulation run (ODE integrator)
procedure TMainForm.getVals( newTime: Double; newVals: TVarNameValList );
var
  dataStr: String;
  i: Integer;
  newValsAr: array of double;
  currentStepSize:double;
begin
  // Update table of data;
  newValsAr := newVals.getValAr;
  dataStr := '';
  dataStr := floatToStrf(newTime, ffFixed, 4, 4) + ', ';
  for i := 0 to length(newValsAr) - 1 do
    begin
      if not containsText(newVals.getNameVal(i).getId, '_Null') then // do not show null nodes
        begin
        if i = length(newValsAr)-1 then
          dataStr := dataStr + floatToStrf(newValsAr[i], ffExponent, 6, 2)
        else
          dataStr := dataStr + floatToStrf(newValsAr[i], ffExponent, 6, 2) + ', ';
        end;
    end;
  simResultsMemo.Lines.Add(dataStr);
  // Update plots:
  inc(self.currentGeneration);

  if plotsPanelList.count > 0 then
  begin
    for i := 0 to plotsPanelList.count -1 do
      begin
      plotsPanelList[i].processOneScan(newTime, newValsAr,self.plotSpecies[i],
             self.currentGeneration );
      end;
  end;
end;

procedure TMainForm.loadNetworkButtonClick(Sender: TObject);
begin
  self.NetworkJSONOpenDialog.execute();
  self.resetBtnOnLineSim;
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
  var errorMsg: string;
begin
  try
    errorMsg := '';
    self.clearNetwork;
    errorMsg := networkController.loadModel(AText);
  except
    on E: Exception do
      notifyUser(E.message);
  end;
  if errorMsg <> '' then
    notifyUser(errorMsg);
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

    for i := 0 to fPlotSpecies.SpPlotCG.Items.Count - 1 do
      begin
        plotSp := '';
        if fPlotSpecies.SpPlotCG.checked[i] then
          begin
            plotSp := self.mainController.getModel.getS_names[i];
          //  plotSp := self.mainController.getModel.getSBMLspecies(i).getID;
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
          end
        else
          if addingPlot then
            self.plotSpecies.Items[plotnumb - 1].Add('')
          else self.plotSpecies.Items[getPlotPBIndex(plotNumb)].Add('');
      end;

    //for i := 0 to Length(self.mainController.getModel.getSBMLspeciesAr) -1 do
    for i := 0 to length(self.mainController.getModel.getS_Names) -1 do
    begin
      if fPlotSpecies.SpPlotCG.Items.Count < (i +1) then
      begin
        if addingPlot then
            self.plotSpecies[plotnumb - 1].Add('')
        else self.plotSpecies[getPlotPBIndex(plotNumb)].Add('');
      end;
    end;

    if maxYVal = 0 then
      maxYVal := DEFAULTSPECIESPLOTHT  // default for plot Y max
    else maxYVal := MaxYVal * 2.0;  // add 100% margin
    if addingPlot then
      self.addPlot(maxYVal) // <-- Add dynamically created plot at this point
    else
      begin
        self.plotsPanelList[getPlotPBIndex(plotNumb)].plotGraph.setY_ValsMax( maxYVal);
        // Update plot legend:
         self.plotsPanelList[getPlotPBIndex(plotNumb)].setPlotLegend(self.plotSpecies.Items[getPlotPBIndex(plotNumb)]);
      end;

    self.refreshPlotAndSliderPanels;
  end;

// async called OnCreate for TVarSelectForm
  procedure AfterCreate(AForm: TObject);
  var i, lgth: integer;
     strList: array of String;
     curStr: string;
  begin
    lgth := 0;
   // TODO: Need Additional (non default) plots to allow plotting of boundary species.
   //  Need to look at plots, as new data is only plotted for species, change plot species array to handle boundary species.
   // for i := 0 to Length(self.mainController.getModel.getSBMLspeciesAr) -1 do
    for i := 0 to length(self.mainController.getModel.getS_Names) -1 do
    begin
      curStr := '';
      curStr := self.mainController.getModel.getS_names[i];
     { if self.mainController.getModel.getSBMLspecies(i).isSetIdAttribute then
         curStr := self.mainController.getModel.getSBMLspecies(i).getID
      else curStr := self.mainController.getModel.getSBMLspecies(i).getName; }

      if not curStr.Contains( NULL_NODE_TAG ) then
        begin
          lgth := Length(strList);
          setLength(strList, lgth + 1);
          strList[lgth] := curStr;
        end;
    end;
    (AForm as TVarSelectForm).Top := trunc(self.Height*0.2); // put popup %20 from top
    (AForm as TVarSelectForm).speciesList := strList;
    (AForm as TVarSelectForm).fillSpeciesCG();
  end;

begin
  fPlotSpecies := TVarSelectForm.CreateNew(@AfterCreate);
  fPlotSpecies.Popup := true;
  fPlotSpecies.ShowClose := false;
  fPlotSpecies.PopupOpacity := 0.3;
  //fPlotSpecies.Top := trunc(self.Height*0.2); // put popup %20 from top
  fPlotSpecies.Border := fbDialogSizeable;
  fPlotSpecies.caption := 'Species to plot:';
  fPlotSpecies.ShowModal(@AfterShowModal);
end;

procedure TMainForm.addPlotAll(); // add plot with all species
var i, numSpeciesToPlot: integer; maxYVal: double; plotSp: string;
begin
  maxYVal := 0;
  numSpeciesToPlot := 0;
  if self.plotSpecies = nil then
    self.plotSpecies := TList<TSpeciesList>.create;
  self.plotSpecies.Add(TSpeciesList.create);
  self.numbPlots := self.numbPlots + 1;
  numSpeciesToPlot := length(self.mainController.getModel.getS_Names);
  if numSpeciesToPlot > 8 then numSpeciesToPlot := 8;

  for i := 0 to numSpeciesToPlot -1 do
    begin
      plotSp := '';
      plotSp := self.mainController.getModel.getS_names[i];
      if ( plotSp.contains( NULL_NODE_TAG ) ) then plotSp := ''  // Null node
      else
      begin
        if self.mainController.getModel.getSBMLspecies(plotSp).isSetInitialAmount then
          begin
          if self.mainController.getModel.getSBMLspecies(plotSp).getInitialAmount > maxYVal then
            maxYVal := self.mainController.getModel.getSBMLspecies(plotSp).getInitialAmount;
          end
        else
          if self.mainController.getModel.getSBMLspecies(plotSp).getInitialConcentration > maxYVal then
            maxYVal := self.mainController.getModel.getSBMLspecies(plotSp).getInitialConcentration;
      end;
      self.plotSpecies[self.numbPlots - 1].Add(plotSp)

    end;
  for i := 0 to Length(self.mainController.getModel.getSBMLspeciesAr) -1 do
    begin
    //  if length(self.mainController.getModel.getS_Names) < (i +1) then
      if numSpeciesToPlot < (i +1) then
        begin
        self.plotSpecies[self.numbPlots - 1].Add('');
        end;
    end;

  if maxYVal = 0 then
    maxYVal := DEFAULTSPECIESPLOTHT  // default for plot Y max
  else maxYVal := maxYVal * 2.0;  // add 100% margin
  self.addPlot(maxYVal); // <-- Add dynamically created plot at this point
  self.refreshPlotAndSliderPanels;
end;

procedure TMainForm.addPlot(yMax: double); // Add a plot
var plotPositionToAdd: integer; // Add plot to next empty position.
    plotWidth: integer;
    newHeight: integer; i: Integer;
begin
  plotWidth := 0;
  plotPositionToAdd := -1;
  plotPositionToAdd := self.getEmptyPlotPosition();
  if self.plotsPanelList = nil then
    self.plotsPanelList := TList<TPlotPanel>.create;
  self.plotsPanelList.Add(TPlotPanel.create(pnlPlotContainer, plotPositionToAdd, yMax,
       self.mainController.getModel.getS_Names, self.mainController.getModel.getS_Vals));

  newHeight := 200;  // default
  if self.numbPlots > DEFAULT_NUMB_PLOTS then
  begin
    newHeight := round(self.pnlPlotContainer.Height/self.numbPlots);
  end;

  self.plotsPanelList[self.numbPlots - 1].OnPlotUpdate := self.editPlotList;
  self.initializePlot (self.numbPlots - 1);
  if self.numbPlots > DEFAULT_NUMB_PLOTS then
  begin  // Adjust plots to new height:
    for i := 0 to self.numbPlots - 1 do
      self.plotsPanelList[i].adjustPlotHeight(self.numbPlots, newHeight);

  end;
 end;

function  TMainForm.getPlotPBIndex(plotTag: integer): Integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to self.numbPlots -1 do
  begin
    if self.plotsPanelList[i].plotWPanel.Tag = plotTag then
      Result := i;
  end;
end;

function  TMainForm.getSliderIndex(sliderTag: integer): Integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to length(self.pnlSliderAr) -1 do
  begin
    if self.pnlSliderAr[i].Tag = sliderTag then
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
      if self.plotsPanelList[i].plotWPanel.Tag = plotPosition then
        inc(plotPosition);
    end;
  end;

  Result := plotPosition;
end;


procedure TMainForm.deletePlot(plotIndex: Integer);
var tempObj: TObject;
begin
  try
    begin
      try
        begin
          self.plotsPanelList[plotIndex].Destroy;
          tempObj := self.plotsPanelList[plotIndex];
          tempObj.Free;
          self.plotsPanelList.Delete(plotIndex);
          self.plotSpecies.Delete(plotIndex);
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

procedure TMainForm.deleteAllPlots();
var i: integer;
begin
  for i := self.numbPlots - 1 downto 1 do
    self.deletePlot(i);
  self.pnlPlotContainer.Invalidate;
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
  plotYposition := self.plotsPanelList[plotIndex].plotWPanel.Top + 20;
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
  // , just delete all plots and have user add them as necessary
  //  No need for this: EditPlotList(plotn: Integer);
end;

procedure TMainForm.deletePlotSpecies(plotn: Integer); // Delete a species curve in a plot.
begin
  // Not needed?: Just delete plot, get new list of species from user and create new plot.
end;

procedure TMainForm.EditSliderList(sn: Integer);
// delete, change param slider as needed. sn is slider index
var
  sliderXposition, sliderYposition: Integer;
  editList: TStringList;
begin
  sliderXposition := 350;
  sliderYposition := self.pnlSliderAr[sn].Top + 10;
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
procedure TMainForm.deleteSlider(sn: Integer); // sn: slider index
begin
  // console.log('Delete Slider: slider #: ',sn);
  self.pnlSliderAr[sn].free;
  delete(self.pnlSliderAr, (sn), 1);
  delete(self.sliderPHLabelAr, (sn), 1);
  delete(self.sliderPLLabelAr, (sn), 1);
  delete(self.sliderPTBLabelAr, (sn), 1);
  delete(self.sliderPTBarAr, (sn), 1);
  delete(self.sliderPHighAr, (sn), 1);
  delete(self.sliderPLowAr, (sn), 1);
  self.RSimWPanel.Invalidate;
end;

procedure TMainForm.deleteAllSliders();
var i: integer;
begin
  for I := Length(self.pnlSliderAr) -1 downto 0 do
    self.deleteSlider(i);
  self.pnlSliderContainer.Invalidate;
  self.numbSliders := 0;
end;

// Select parameter to use for slider
procedure TMainForm.selectParameter(sNumb: Integer); // snumb is slider index
var
  paramIndex: Integer; // param chosen in radiogroup
  // Pass back to caller after closing popup:
  procedure AfterShowModal(AValue: TModalResult);
  var
    i, sliderNumb:Integer;
    addingSlider: Boolean;
    sliderP: string;
  begin
    addingSlider := false;
    sliderNumb := 0;
    sliderP := '';
    if length(self.sliderParamAr) < (sNumb +1) then
      addingSlider := true
    else
      begin    // Changing species to plot, so clear out old param entries:
        addingSlider := false;
        self.sliderParamAr[getSliderIndex(sNumb)] := -1; // Clear slider position, not needed here ?
      end;

    for i := 0 to fSliderParameter.SpPlotCG.Items.Count - 1 do
      begin
        sliderP := '';
        if fSliderParameter.SpPlotCG.checked[i] then
          begin
            if addingSlider then
              SetLength(self.sliderParamAr, (sliderNumb + 1));    // add a slider
           // sliderP := self.mainController.getModel.getP_names[i];   // needed?
            self.sliderParamAr[sliderNumb] := i; // assign param indexto slider
            self.addParamSlider(); // <-- Add dynamically created slider
            inc(sliderNumb);
          end;

      end;


  end;

// async called OnCreate for TParamSliderSForm
  procedure AfterCreate(AForm: TObject);
  begin
    (AForm as TVarSelectForm).Top := trunc(self.Height*0.2); // put popup %20 from top
    (AForm as TVarSelectForm).speciesList := self.mainController.getModel.getP_Names;
    (AForm as TVarSelectForm).fillSpeciesCG();
  end;

begin
  fSliderParameter := TVarSelectForm.CreateNew(@AfterCreate);
  fSliderParameter.Popup := true;
  fSliderParameter.ShowClose := false;
  fSliderParameter.PopupOpacity := 0.3;
  fSliderParameter.Border := fbDialogSizeable;
  fSliderParameter.caption := 'Pick parameters for sliders:';
  fSliderParameter.ShowModal(@AfterShowModal);

end;


// *******************************************************

procedure TMainForm.addAllParamSliders();
var i, numParSliders: integer;
    sliderP: string;
begin
  numParSliders := 0;
  numParSliders := length(self.mainController.getModel.getP_Names);
  if numParSliders > 10 then numParSliders := 10;

  for i := 0 to numParSliders -1 do
    begin
    sliderP := '';
    SetLength(self.sliderParamAr, self.numbSliders + 1);    // add a slider
    //sliderP := self.mainController.getModel.getP_names[i];   // needed?
    self.sliderParamAr[self.numbSliders] := i; // assign param indexto slider
    inc(self.numbSliders);
    self.addParamSlider(); // <-- Add dynamically created slider

    end;

end;

procedure TMainForm.addParamSlider(); // assume slider index is at last position, otherwise it is just an edit.
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
  sliderPanelWidth :=  self.pnlSliderContainer.width;
  sliderPanelLeft := 0;    // not used anymore, just set to default

  // Width of the slider inside the panel

  i := length(self.pnlSliderAr);
  // array index for current slider to be added.
  SetLength(self.pnlSliderAr, i + 1);
  SetLength(self.sliderPHighAr, i + 1);
  SetLength(self.sliderPLowAr, i + 1);
  SetLength(self.sliderPTBarAr, i + 1);
  SetLength(self.sliderPHLabelAr, i + 1);
  SetLength(self.sliderPLLabelAr, i + 1);
  SetLength(self.sliderPTBLabelAr, i + 1);

  self.pnlSliderAr[i] := TWebPanel.create(self.pnlSliderContainer);
  self.pnlSliderAr[i].parent := self.pnlSliderContainer;
  self.pnlSliderAr[i].OnMouseDown := SliderOnMouseDown;

  configPSliderPanel(i, sliderPanelLeft, sliderPanelWidth, SLIDERPHEIGHT,
    self.pnlSliderAr);

  self.pnlSliderAr[i].tag := i; // keep track of slider index number.
  //self.pnlSliderAr[i].tag := i + 1 ; // keep track of slider position number.
  self.sliderPTBarAr[i] := TWebTrackBar.create(self.pnlSliderAr[i]);
  self.sliderPTBarAr[i].parent := self.pnlSliderAr[i];
  self.sliderPTBarAr[i].OnChange := self.ParamSliderOnChange;
  self.sliderPHLabelAr[i] := TWebLabel.create(self.pnlSliderAr[i]);
  self.sliderPHLabelAr[i].parent := self.pnlSliderAr[i];
  self.sliderPLLabelAr[i] := TWebLabel.create(self.pnlSliderAr[i]);
  self.sliderPLLabelAr[i].parent := self.pnlSliderAr[i];
  self.sliderPTBLabelAr[i] := TWebLabel.create(self.pnlSliderAr[i]);
  self.sliderPTBLabelAr[i].parent := self.pnlSliderAr[i];
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

          // Reset species amts to initial conditions:
procedure TMainForm.resetInitValsButtonClick(Sender: TObject);
begin
  self.mainController.resetSimSpeciesValues();
end;

procedure TMainForm.adjustRightTabWPanels(); // Adjust all right panels to same width, height
begin
  //if self.rightPanelType = SIMULATION_PANEL then
    //begin
 //   console.log('splitter moved, RSimWPanel, height: ',networkPB1.Height,', width: ',self.RSimWPanel.Width);
      self.RNodeEditWPanel.Width := self.RSimWPanel.Width;
      self.RNodeEditWPanel.Top := self.RSimWPanel.Top;
      self.RNodeEditWPanel.Height := self.RSimWPanel.Height;
      self.RNodeEditWPanel.Left := self.RSimWPanel.Left;
      self.RRxnEditWPanel.Width := self.RSimWPanel.Width;
      self.RRxnEditWPanel.Top := self.RSimWPanel.Top;
      self.RRxnEditWPanel.Height := self.RSimWPanel.Height;
      self.RRxnEditWPanel.Left := self.RSimWPanel.Left;
      self.RSimWPanel.invalidate;
 // console.log('After: splitter moved, RSimWPanel, height: ',self.RSimWPanel.Height,', width: ',self.RSimWPanel.Width);
 //  console.log('splitter moved, RNodeEditWPanel, height: ',self.RNodeEditWPanel.Height,', width: ',self.RNodeEditWPanel.Width);

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
    self.rateLawEqLabel.Caption := networkController.selectedObjects[0].reaction.state.rateLaw; // use getRateLaw instead.
    self.rateLawEqLabel.Hint := networkController.selectedObjects[0].reaction.state.rateLaw;
    self.lblRxnIdString.Caption := networkController.selectedObjects[0].reaction.state.id;
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
   // paramAssignStr: string;
    i: integer;
begin
  paramTStr:= '';
  self.RxnParamComboBox.clear;
  for i := 0 to networkController.selectedObjects[0].reaction.state.rateParams.count -1 do
  begin
    paramTStr := networkController.selectedObjects[0].reaction.state.rateParams.Items[i].getId;
    self.RxnParamComboBox.AddItem(networkController.selectedObjects[0].reaction.state.rateParams.Items[i].getId,nil);
  end;
  if self.rxnParamComboBox.Items.count >0 then
  begin
    self.rxnParamEdit.text := floattostr(networkController.selectedObjects[0].reaction.state.rateParams[0].getValue);
  end;
  self.RxnParamComboBox.invalidate;

end;

procedure TMainForm.updateRxnStoichPanel();
var i: integer;
begin
  self.clearRxnStoichCoeffs();
  for i := 0 to Length(networkController.selectedObjects[0].reaction.state.srcId)-1 do
  begin
    if networkController.selectedObjects[0].reaction.state.srcId[i] <> '' then
      self.addRxnStoichEdit(i, true);

  end;

  for i := 0 to Length(networkController.selectedObjects[0].reaction.state.destId)-1 do
  begin
    if networkController.selectedObjects[0].reaction.state.destId[i] <> '' then
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
      if networkController.selectedObjects[0].reaction.state.srcId[spIndex] <> '' then
      begin
        newStoichLabel.caption := networkController.selectedObjects[0].reaction.state.srcId[spIndex];
        newStoichLabel.top := self.StoicReactantsLabel.top + self.StoicReactantsLabel.height + round(spIndex*EDITBOX_HT);
        rxnReactStoichLabels.add(newStoichLabel);
        newStoichEdit.Top := self.StoicReactantsLabel.top + self.StoicReactantsLabel.height + round(spIndex*EDITBOX_HT);

        newStoichEdit.onExit := editRxnSrcStoichExit;
        if length(networkController.selectedObjects[0].reaction.state.srcStoich) > spIndex  then
          begin
          newStoichEdit.Text := networkController.selectedObjects[0].reaction.state.srcStoich[spIndex].toString();
          rxnReactStoichEdits.add(newStoichEdit);
          end;
      end;
    end
    else
    begin
      if networkController.selectedObjects[0].reaction.state.destId[spIndex] <> '' then
      begin
       newStoichLabel.caption := networkController.selectedObjects[0].reaction.state.destId[spIndex];
       newStoichLabel.top := self.StoichProductsLabel.top + self.StoichProductsLabel.height + round(spIndex*20);

       rxnProdStoichLabels.add(newStoichLabel);
       newStoichEdit.Top := self.StoichProductsLabel.top + self.StoichProductsLabel.height + round(spIndex*20);
       newStoichEdit.onExit := editRxnDestStoichExit;
       if length(networkController.selectedObjects[0].reaction.state.destStoich) > spIndex  then
         begin
         newStoichEdit.Text := networkController.selectedObjects[0].reaction.state.destStoich[spIndex].toString();
         rxnProdStoichEdits.add(newStoichEdit);
         end
      end;
    end;
end;

procedure TMainForm.btnNodeFillColorSelect(Sender: TObject);
var i : integer;
begin
  for i := 0 to length(network.nodes) - 1 do
    if network.nodes[i].selected then
       begin
       network.nodes[i].state.fillColor := btnNodeFillColor.color;
       end;
  networkPB1.Invalidate;
end;

procedure TMainForm.btnNodeOutlineColorSelect(Sender: TObject);
var i : integer;
begin
  for i := 0 to length(network.nodes) - 1 do
    if network.nodes[i].selected then
      begin
        network.nodes[i].state.outlineColor := btnNodeOutlineColor.color;
      end;
   networkPB1.Invalidate;
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

procedure TMainForm.mniNewClick(Sender: TObject);
begin
  self.clearNetwork;
end;

procedure TMainForm.networkPB1DblClick(Sender: TObject);
begin
  console.log ('double click')

end;

procedure TMainForm.setUpSimulationUI();
var i: integer;
begin
  btnParamAddSlider.Enabled := true;
  btnAddPlot.Enabled := true;

  self.currentGeneration := 0; // reset current x axis point (pixel)
  if self.networkUpdated then // TODO: do not reset plots if species or param init vals have changed.
  begin
      // delete existing plots
    if self.numbPlots >0 then
    begin
      if (self.plotsPanelList.Count) > 0 then
      begin
        for i := self.plotsPanelList.Count-1 downto 0 do
        begin
          self.DeletePlot(i);
        end;
        self.numbPlots := 0;
      end;

    end;
    // delete existing param sliders.
    if self.pnlSliderAr <> nil then
    begin
      if length(self.pnlSliderAr) >0 then
      begin
        for i := length(self.pnlSliderAr) -1 downto 0 do
        begin
          self.DeleteSlider(i);
        end;
        setLength(self.pnlSliderAr, 0);
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

procedure TMainForm.btnCloseNodePanelClick(Sender: TObject);
var minimizedHeight : integer;
begin
  minimizedHeight := 34;
  if pnlNodePanel.Height < minimizedHeight + 2 then
     begin
     pnlNodePanel.Height := 240;
     pnlReactionPanel.Top := pnlNodePanel.Top + pnlNodePanel.Height + 6;
     end
  else
    begin
    pnlNodePanel.Height := minimizedHeight;
    pnlReactionPanel.Top := pnlNodePanel.Top + pnlNodePanel.Height + 6;
    end;
end;

procedure TMainForm.btnCloseReactionEditPanelClick(Sender: TObject);
var minimizedHeight : integer;
begin
  minimizedHeight := 32;
  if pnlReactionPanel.Height < minimizedHeight + 2 then
     begin
     pnlReactionPanel.Height := 180
     end
  else
    begin
    pnlReactionPanel.Top := pnlNodePanel.Top + pnlNodePanel.Height + 6;
    pnlReactionPanel.Height := minimizedHeight;
    end;
end;

procedure TMainForm.btnReactionColorSelect(Sender: TObject);
var i : integer;
begin
  for i := 0 to length(network.reactions) - 1 do
    if network.reactions[i].selected then
       begin
       network.reactions[i].state.fillColor := btnReactionColor.color;
       end;
  networkPB1.Invalidate;
end;

procedure TMainForm.btnResetRunClick(Sender: TObject);
begin
  self.resetSliderPositions();
  self.enableStepSizeEdit;
  self.mainController.createSimulation();
  self.initializePlots;
  self.currentGeneration := 0;

end;

procedure TMainForm.edtReactionIdExit(Sender: TObject);
var newId, cutNewId: string;
begin
  newId := self.edtReactionId.Text;
  if length(newId) > MAX_STR_LENGTH then
    begin
      cutNewId := newId.Substring(0, MAX_STR_LENGTH);
      networkController.setReactionId(cutNewId);
      self.edtReactionId.Text := cutNewId;
    end
  else networkController.setReactionId(newId);
  networkPB1.Invalidate;
end;

procedure TMainForm.edtReactionWidthChange(Sender: TObject);
var i : integer;
begin
  for i := 0 to length(network.reactions) - 1 do
    if network.reactions[i].selected then
       begin
       network.reactions[i].state.thickness := edtReactionWidth.Value;
       end;
  networkPB1.Invalidate;
end;

function TMainForm.enableStepSizeEdit(): boolean; // true: success
begin
  self.stepSizeLabel1.Enabled := true;
  self.stepSizeEdit1.Enabled := true;
  Result := true;
end;
function TMainForm.disableStepSizeEdit(): boolean;// true: success
begin
  self.stepSizeLabel1.Enabled := false;
  self.stepSizeEdit1.Enabled := false;
  Result := true;

end;

end.
