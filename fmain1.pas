unit fmain1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, Types, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Dialogs, WEBLib.ExtCtrls, WEBLib.WebCtrls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.Buttons, Vcl.Imaging.pngimage,Vcl.Graphics,
  uController, uNetworkCanvas, uNetwork, VCL.TMSFNCTypes, VCL.TMSFNCUtils, VCL.TMSFNCGraphics,
  VCL.TMSFNCGraphicsTypes, VCL.TMSFNCCustomControl, VCL.TMSFNCScrollBar, VCL.TMSFNCButton, VCL.TMSFNCToolBar,
  uNetworkTypes, Vcl.Imaging.pngimage, WEBLib.Lists, Vcl.Forms, SBML.helper, Simulation,
  ODE_FormatUtility, GraphP, Vcl.Menus, WEBLib.Menus, paramSelectForm,speciesSelectForm, plotLayout,
  paramSlider, paramSliderLayout;

  const SLIDERPWIDTH= 195; SLIDERPHEIGHT= 50;  // Param Sliders WebPanel width/height
type
  TspeciesAr = array of String;
  TmainForm = class(TWebForm)
    TopWPanel: TWebPanel;
    LeftWPanel: TWebPanel;
    RightWPanel: TWebPanel;
    CenterWPanel: TWebPanel;
    networkPB1: TWebPaintBox;
    newNetworkBtn: TWebButton;
    networkSaveBtn: TWebButton;
    btnUniUni: TWebSpeedButton;
    btnUniBi: TWebSpeedButton;
    btnBiUni: TWebSpeedButton;
    btnBiBi: TWebSpeedButton;
    bottomWPanel: TWebPanel;
    btnIdle: TWebSpeedButton;
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
    zoomFactorLbl1: TWebLabel;
    lb: TWebMemo;
    btnNodeFillColor: TWebColorPicker;
    btnAddNode: TWebSpeedButton;
    netDrawScrollBarHoriz: TTMSFNCScrollBar;
    netDrawScrollBarVert: TTMSFNCScrollBar;        // Displays simulation results
    SBMLmodelMemo: TWebMemo;
    WebTimer1: TWebTimer;
    rtLengthEdit1: TWebEdit;
    rtLabel1: TWebLabel;
    stepSizeLabel1: TWebLabel;
    stepSizeEdit1: TWebEdit;
    pnlNodePanel: TWebPanel;
    nodeOutlineLabel: TWebLabel;
    nodeFillLabel: TWebLabel;
    editNodeLabel: TWebLabel;
    btnNodeOutlineColor: TWebColorPicker;
    editNodeId: TWebEdit;
    ZoomCntrlPanel: TWebPanel;
    zoomCtlLabel: TWebLabel;
    zoomTrackBar: TWebTrackBar;
    simResultsMemo: TWebMemo;
    addPlotButton: TWebButton;
    paramSliderBtn: TWebButton;
    NetworkJSONOpenDialog: TWebOpenDialog;
    loadNetworkButton: TWebButton;
    SBMLOpenDialog: TWebOpenDialog;
    SBMLloadButton: TWebButton;
    plotEditLB: TWebListBox;
    SliderEditLB: TWebListBox;

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
    procedure networkPB1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
    procedure ParamSliderOnChange(Sender: TObject);  // User changes value of parameter
    procedure plotEditLBClick(Sender: TObject);
    procedure SliderEditLBClick(Sender: TObject);  // User clicks choice from plot edit list


  private
    numbPlots: integer; // Number of plots displayed
    numbSliders: integer;  // Number of parameter sliders
    procedure GetSBMLInfo();  // Grab sbml model info.
    function setupSimulation():String; // returns eq list as String.
    procedure startSimulation(odeEqs: String;odeFormat: TFormatODEs);
    procedure updateSimulation();
    procedure addPlot(); // Add a plot
    procedure selectPlotSpecies(plotnumb: integer);
    procedure addParamSlider();
    procedure SetSliderParamValues(sn, paramForSlider: integer);
    procedure selectParameter(sNumb: integer); // Get parameter for slider
    procedure LoadJSONFile();
    procedure DeletePlot(plotn: integer);
    procedure EditPlotList(plotn: integer); // delete, change plot species, other added as needed using TWebListBox.
    procedure EditSliderList(sn: integer);  // delete, change param slider as needed using TWebListBox.
    procedure DeleteSlider(sn: integer);

  public
    network : TNetwork;
    controller : TController;
    networkCanvas : TNetworkCanvas;
    origin : TPointF;
    fileName : string;
    graphBitmap1: TBitmap;     // get rid of
    graphBitmapAr: array of TBitmap;  // bitmap for each plot
    maxYValueAr: array of integer; // max Y for each plot
    currentGeneration: integer;
    plotSpeciesForm: TSpeciesSWForm;
    plotSpecies: array of TspeciesAr; // species to graph for each plot
    plotsPBAr: array of TWebPaintbox;  // Plot paint boxes
    plotsG_Ar: array of TPlotGraph;     // Plotting class for each plot
    xscaleHeight: integer; { This is the space reserved for the x axis labelling, remove }
    xscaleHeightAr: array of integer;  //This is the space reserved for the x axis labelling of each plot
    sliderParamForm: TParamSliderSForm; // Pop up form to choose parameter for slider.
    sliderParamAr: array of integer;  // holds parameter array index of parameter (p_vals) for each slider
    sliderPanelAr: array of TWebPanel; // Holds parameter sliders
    sliderPHighAr: array of double;  // High value for parameter slider
    sliderPLowAr: array of double;  // Low value for parameter slider
    sliderPTBarAr: array of TWebTrackBar;
    sliderPHLabelAr: array of TWebLabel;  // Displays sliderPHighAr
    sliderPLLabelAr: array of TWebLabel;  // Displays sliderPLowAr
    sliderPTBLabelAr: array of TWebLabel; // Displays slider param name and current value
    paramUpdated: boolean;  // true if a parameter has been updated.

    function ScreenToWorld (x, y : double) : TPointF;   // Network drawing panel
    function WorldToScreen (wx : integer) : integer;    // Network drawing panel
    procedure PingSBMLLoaded(); // Notify when done loading libSBML
    procedure getVals(newTime:double; newVals: array of double); //Get new values (species amt) from simulation run
    procedure processScan(t_new:double; y_new: array of double);

  end;

var
  mainForm: TmainForm;
  sbml_Text: String;
  sbmlmodel: SBMLhelpClass;
  numbRxns: Integer;
  sbmlModelLoaded: Boolean;
  solverUsed: ODEsolver;
  runTime: double;      // Total runtime of simulation (sec)
  runSim: TSimulationJS;
  stepSize: double;     //(msec) (integration step)
  pixelStep: integer;   // get rid of...
  pixelStepAr: array of integer;   // pixel equiv of time (integration) step
  currTime: double;
  s_Vals: array of double; // one to one correlation: s_Vals[n] = s_names[n]
  s_names: array of String; // Use species ID as name
  p_Vals: array of double; // Includes compartments
  p_names: array of String;
  online: boolean; // Simulation running
  ODEready: boolean;     // TRUE: ODE solver is setup.

  spSelectform: TSpeciesSWForm;  // display speciecs select to plot radio group

implementation

{$R *.dfm}
Uses uGraphUtils, uCreateNetworks, uLayout;

procedure TmainForm.addPlotButtonClick(Sender: TObject);
begin
// Make runtime, stepsize, simulation buttons visible
  self.numbPlots:= self.numbPlots+1;
  rtLabel1.visible:= true;
  rtLengthEdit1.visible:= true;
  stepSizeLabel1.visible:= true;
  stepSizeEdit1.visible:= true;
  self.selectPlotSpecies(self.numbPlots);
end;

procedure TmainForm.btnAboutClick(Sender: TObject);
begin
  Showmessage ('Version 0.1');
end;

procedure TmainForm.btnAddNodeClick(Sender: TObject);
begin
  controller.setAddNodeStatus;
end;

procedure TmainForm.btnAutoLayoutClick(Sender: TObject);
begin
  //showmessage (inttostr (networkPB1.Width) + ', ' + inttostr (networkPB1.Width));
  fruchterman_reingold(network, networkPB1.width, networkPB1.Height, 600, nil);
  network.centerNetwork (networkPB1.Width, networkPB1.Height);
  networkPB1.Invalidate;
end;

procedure TmainForm.btnBiBiClick(Sender: TObject);
begin
  controller.setAddBiBiReaction;
end;

procedure TmainForm.btnBiUniClick(Sender: TObject);
begin
  controller.setAddBiUniReaction;
end;

procedure TmainForm.btnCenterClick(Sender: TObject);
var i : integer;
begin
  network.centerNetwork (networkPB1.Width, networkPB1.height);
  lb.Clear;
  for i := 0 to length (network.nodes) - 1 do
      begin
       lb.Lines.Add (inttostr (trunc (network.nodes[i].state.x)) + ', ' + inttostr (trunc (network.nodes[i].state.x)));
      end;
  networkPB1.Invalidate;
end;

procedure TmainForm.btnClearClick(Sender: TObject);
begin
  network.Clear;
  networkPB1.Invalidate;
end;

procedure TmainForm.btnDrawClick(Sender: TObject);
var n1, n2, n3, n4 : TNode;
begin
  n1 := controller.addNode ('node1', 60, 200);
  n2 := controller.addNode ('node2', 270, 270);
  n3 := controller.addNode ('node3', 540, 80);
  n4 := controller.addNode ('node4', 400, 500);

  controller.addReaction ('r1', n1, n2);
  controller.addReaction ('r2', n2, n3);
  controller.addReaction ('r3', n3, n4);
  controller.addReaction ('r4', n4, n2);

  networkPB1.Invalidate;
end;

procedure TmainForm.btnIdleClick(Sender: TObject);
begin
   controller.setSelectStatus;
end;

procedure TmainForm.btnNodeFillColorClick(Sender: TObject);
var i : integer;
begin
  for i := 0 to length (network.nodes) - 1 do
      if network.nodes[i].selected then
         begin
         network.nodes[i].state.fillColor := btnNodeFillColor.color;
         networkPB1.Invalidate;
         end;
end;

procedure TmainForm.btnNodeOutlineColorClick(Sender: TObject);
var i : integer;
begin
 for i := 0 to length (network.nodes) - 1 do
      if network.nodes[i].selected then
         begin
         network.nodes[i].state.outlineColor := btnNodeOutlineColor.color;
         networkPB1.Invalidate;
         end;
end;

procedure TmainForm.btnRandomNetworkClick(Sender: TObject);
var nNodes : integer;
    probability : double;
    n1, n2, n3, n4 : TNode;
begin
  network.clear;
  nNodes := 12;
  probability := 0.9;
  createRandomNetwork (network, nNodes, probability);

  networkPB1.Invalidate;
end;

procedure TmainForm.btnUniBiClick(Sender: TObject);
begin
   controller.setAddUniBiReaction;
end;

procedure TmainForm.btnUniUniClick(Sender: TObject);
begin
  controller.setAddUniUniReaction;
end;

procedure TmainForm.editNodeIdExit(Sender: TObject);
begin
  controller.setNodeId (editNodeId.Text);
  networkPB1.Invalidate;
end;

procedure TmainForm.SBMLloadButtonClick(Sender: TObject);
begin
  self.SBMLOpenDialog.execute();
end;

procedure TmainForm.SBMLOpenDialogChange(Sender: TObject);
begin
if SBMLOpenDialog.Files.Count > 0 then
  SBMLOpenDialog.Files[0].GetFileAsText;
end;

procedure TmainForm.SBMLOpenDialogGetFileAsText(Sender: TObject;
  AFileIndex: Integer; AText: string);
begin
  SBMLmodelMemo.Lines.Text := AText;
  SBMLmodelMemo.visible:= true;
  sbml_Text:= AText;     // store the sbml model as text.
   // Check if sbmlmodel already created, if so, destroy before creating ?
  sbmlmodel:= SBMLhelpClass.create();
  sbmlmodel.OnPing := PingSBMLLoaded;  // *** Register the callback function
  sbmlmodel.readSBML(AText); // Process text with libsbml.js
end;

procedure TmainForm.onLineSimButtonClick(Sender: TObject);
var i: integer;
begin
if online = false then
 begin
 online:= true;
 onLineSimButton.font.Color:= clgreen;
 onLineSimButton.caption:= 'Simulation: Online';
 simResultsMemo.visible:= true;

 try
     runTime:= strToFloat(rtLengthEdit1.Text);
   except
      on Exception : EConvertError do
      begin
        runTime:= 10;
        self.rtLengthEdit1.Text:= FloatToStr(runTime);
      end;
   end;
 if stepSize=0 then stepSize:=0.1;

 for i := 0 to length(graphBitmapAr)-1 do      // <-- for dynamically created plots
  begin
   plotsG_Ar[i].initGraph(0, 200, 0, maxYValueAr[i], 0, graphBitmapAr[i].width, 0, graphBitmapAr[i].height,
                 xscaleHeightAr[i], runTime, stepSize);

  //total steps: runTime/stepSize
  //  Max viewable steps is PlotWebPB.width (1 pixel per step).
    pixelStepAr[i]:=0;
    if runTime/stepSize < self.plotsPBAr[i].width then pixelStepAr[i]:= round(self.plotsPBAr[i].width*stepSize/runTime)
    else pixelStepAr[i]:= 1;
  end;
  WebTimer1.enabled:=true;
  self.updateSimulation();
 end
 else begin
   online:= false;
   WebTimer1.enabled:=false;
   onLineSimButton.font.Color:= clred;
   onLineSimButton.caption:= 'Simulation: Offline';
 end;
end;

// Grab SBML model information when notified:
procedure TmainForm.PingSBMLLoaded();
 begin
  sbmlModelLoaded:= true;
  GetSBMLInfo();
 end;

procedure TmainForm.plotEditLBClick(Sender: TObject);
begin
  if self.plotEditLB.ItemIndex = 0 then  // change species to plot
  begin
    self.selectPlotSpecies(self.plotEditLB.tag);
  end;
  if self.plotEditLB.ItemIndex = 1 then  // delete plot
  begin
    self.DeletePlot(self.plotEditLB.tag);
  end ;
  //else ShowMessage('Cancel');
  self.plotEditLB.tag:= 0;
  self.plotEditLB.visible:= false;
  self.plotEditLB.Top:= 40; // default
end;

procedure TmainForm.plotsPBArPaint(Sender: TObject);
var plot_i: integer;
begin
  plot_i:= (Sender as TWebPaintBox).Tag;
  plotsPBAr[plot_i-1].canvas.draw (0, 0, graphBitmapAr[plot_i-1]);
end;

procedure TmainForm.networkPB1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 // Handle Ctrl-keys here
  if Key = VK_DELETE then
     begin
     controller.prepareUndo;
     controller.deleteSelectedItems;
     networkPB1.invalidate;
     exit;
     end;

  if (Shift = [ssCtrl]) and (Upcase (Char (Key)) = 'Z') then
     begin
     controller.Undo;
     networkPB1.invalidate;
     end;
end;

procedure TmainForm.networkPB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var v : TPointF;
begin
   v := screenToWorld (x, y);

  controller.OnMouseDown(Sender, Button, Shift, v.x, v.y);
  networkPB1.invalidate;
  if controller.selectedNode <> -1 then
     begin
     editNodeId.text := controller.network.nodes[controller.selectedNode].state.id;
     pnlNodePanel.Visible := True;
     end
  else
     pnlNodePanel.Visible := False;
end;

procedure TmainForm.networkPB1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var v : TPointF;
begin
  v := screenToWorld (x, y);

  controller.OnMouseMove (Sender, Shift, v.x, v.y);
  networkPB1.Invalidate;
  xLbl.caption := 'X: ' + inttostr (trunc (v.x));
  yLbl.caption := 'Y: ' + inttostr (trunc (v.y));
end;

procedure TmainForm.networkPB1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var v : TPointF;
begin
  v := screenToWorld (x, y);

  controller.OnMouseUp (Sender, Button, Shift, v.x, v.y);
  networkPB1.invalidate;
end;

procedure TmainForm.networkPB1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if wheelDelta < 0 then
     origin.y := origin.y + 20  //WheelDelta
  else
     origin.y := origin.y - 20; //WheelDelta

  if origin.y < 0 then
     origin.y := 0;

  Handled := True;
  networkPB1.Invalidate;
end;

procedure TmainForm.networkPB1Paint(Sender: TObject);
begin
  networkCanvas.paint;
  networkPB1.canvas.draw (0, 0, networkCanvas.bitmap);
end;

procedure TmainForm.ParamSliderBtnClick(Sender: TObject);
var i:integer;
   odeFormat: TFormatODEs;
begin
// TODO: Check if already 10 sliders, if so then showmessage( 'Only 10 parameter sliders allowed, edit existing one');
  // Fill out parameter list for model, if not done already:
  if Length(p_Vals)<1 then
   begin
    odeFormat:= TFormatODEs.create(sbmlmodel);
    SetLength(p_Vals,Length(odeFormat.get_pVals()));  // Keep params for later
    SetLength(p_names,Length(p_vals));
    for i := 0 to Length(odeFormat.get_pVals())-1 do
    begin
       p_vals[i]:= odeFormat.get_pVals[i];
       p_names[i]:= odeFormat.get_paramsStr[i];
    end;
   end;
  self.selectParameter(Length(sliderParamAr));

end;


procedure TmainForm.ParamSliderOnChange(Sender: TObject);
var i,p: integer;
begin
  if sender is TWebTrackBar then
  begin
    i:= TWebTrackBar(Sender).Tag;
    self.paramUpdated:= true;
    p:= self.sliderParamAr[i]; // get slider parameter position in p_vals array
    p_vals[p]:= self.sliderPTBarAr[i].Position*0.01*(sliderPHighAr[i]-sliderPLowAr[i]); // Fix ?
    self.sliderPTBLabelAr[i].Caption:= p_names[self.sliderParamAr[i]]+ ': '+floattostr(p_vals[self.sliderParamAr[i]]);
  //  console.log(' Updated param val: ',p_names[p],': ',p_vals[p]);
  end;

end;


procedure TmainForm.zoomTrackBarChange(Sender: TObject);
begin
  networkCanvas.scalingFactor := zoomTrackBar.position/10;
  networkPB1.invalidate;
  ZoomFactorLbl1.caption := floattostr (zoomTrackBar.position/10);
end;


function TmainForm.ScreenToWorld (x, y : double) : TPointF;
begin
  result.x := (x + origin.x)/(zoomTrackBar.position/10);
  result.y := (y + origin.y)/(zoomTrackBar.position/10);
end;


procedure TmainForm.netDrawScrollBarHorizValueChanged(Sender: TObject; Value: Double);
begin
  origin.X := value;
  networkCanvas.origin.x := value;
  networkPB1.Invalidate;
end;


procedure TmainForm.netDrawScrollBarVertValueChanged(Sender: TObject; Value: Double);
begin
  origin.Y := value;
  networkCanvas.origin.y := value;
  networkPB1.Invalidate;
end;


procedure TmainForm.WebFormCreate(Sender: TObject);
begin
  lb.clear;
  self.numbPlots:= 0;
  self.numbSliders:= 0;
  self.zoomTrackBar.left := 20;
  self.zoomTrackBar.position := 10;
  origin.X := 0.0; origin.Y := 0.0;
  sbmlModelLoaded:= false;
  online:= false;
  onLineSimButton.font.Color:= clred;
  onLineSimButton.caption:= 'Simulation: Offline';
  self.setODEsolver;
  stepSize:= 0.1;   // 100 msec
  //xscaleHeight  := round(0.15* plotPB1.Height);   // make %15 of total height  get rid of
  CurrentGeneration := 0;
  network := TNetwork.Create ('testNetwork');
  controller := TController.Create (network);
  networkCanvas := TNetworkCanvas.Create (network);
  controller.networkCanvas := networkCanvas;
  networkCanvas.bitmap.Height := networkPB1.Height;
  networkCanvas.bitmap.Width := networkPB1.Width;
  leftWPanel.color := clWhite;

end;


procedure TmainForm.WebFormResize(Sender: TObject);
begin
if networkCanvas = nil then  // Resize may be called before Create
begin
  network := TNetwork.Create ('testNetwork');
  controller := TController.Create (network);
  networkCanvas := TNetworkCanvas.Create (network);
end;
  networkCanvas.bitmap.Height := networkPB1.height;
  networkCanvas.bitmap.Width := networkPB1.width;
  networkPB1.invalidate;
end;


procedure TmainForm.WebTimer1Timer(Sender: TObject);
begin
  self.updateSimulation();
   if currTime > runTime then
       WebTimer1.enabled:=false;
end;


function TmainForm.WorldToScreen (wx : integer) : integer;
begin
 raise Exception.Create ('WorldtoScreen needs to be updated');
 result := trunc (wx*zoomTrackBar.position/10);
end;


// Add more model info as needed here:
procedure TmainForm.GetSBMLInfo();
 var i: Integer;
     parInitval: array of double; // param vals for ODE solver.
     varInitval: array of double; // init values for ODE solver
     outputList: TStringList;
 begin
   numbRxns:= sbmlmodel.getrxnsnumb();
   paramSliderBtn.visible:= true;
   // rxnsArray:= Copy(sbmlmodel.getReactions(),0,numbRxns);
   self.fillSpeciesArray();
 end;


 function TmainForm.setupSimulation():String;
 var simRTStr: String;   // Output string for TWebMemo
     i: Integer;
     odeEqs: String;
     odeFormat: TFormatODEs;
 begin
   ODEready:= false;

   try
    self.WebTimer1.Interval:= strtoint(stepSizeEdit1.Text); // Trailing blanks are not supported
   except
      on Exception : EConvertError do
      begin
        self.WebTimer1.Interval:=100; // default
        self.stepSizeEdit1.Text:= IntToStr( self.WebTimer1.Interval );
      end;
   end;
   stepSize:= self.WebTimer1.Interval*0.001;  // 1 sec = 1000 msec
   simResultsMemo.Lines.Clear();

   odeFormat:= TFormatODEs.create(sbmlmodel);
   if Length(p_Vals)<1 then
   begin
    SetLength(p_Vals,Length(odeFormat.get_pVals()));  // Keep params for later
    SetLength(p_names,Length(p_vals));
    for i := 0 to Length(odeFormat.get_pVals())-1 do
    begin
      p_Vals[i]:= odeFormat.get_pVals[i];
      p_names[i]:= odeFormat.get_paramsStr[i];
    end;
   end;

    // Run Simulation using info from odeFormat:
   odeFormat.buildFinalEqSet();
   simRTStr:= ' Time (s) ';   // generate coloumn headers:
   setLength(s_names,Length(odeFormat.get_speciesStrAr()));
   for i := 0 to Length(odeFormat.get_speciesStrAr())-1 do
     begin
      s_names[i]:= odeFormat.get_speciesStrAr()[i];  // grab species names
      simRTStr:= simRTstr +', ' + s_names[i];
     end;
   simResultsMemo.Lines.Add( simRTStr);

   if SolverUsed = LSODAS then self.startSimulation(odeFormat.getODEeqSet2(),odeFormat)
   else  self.startSimulation(odeFormat.getODEeqSet(),odeFormat);//Result:= odeFormat.getODEeqSet();
 end;


 procedure TmainForm.SliderEditLBClick(Sender: TObject);
 begin
  if self.SliderEditLB.ItemIndex = 0 then  // change param for slider
  begin
    self.selectParameter(self.SliderEditLB.tag);
  end;
  if self.SliderEditLB.ItemIndex = 1 then  // delete slider
  begin
    self.DeleteSlider(self.SliderEditLB.tag);
  end ;
  //else ShowMessage('Cancel');
  self.SliderEditLB.tag:= 0;
  self.SliderEditLB.visible:= false;
  self.SliderEditLB.Top:= 40; // default
end;


procedure TmainForm.startSimulation(odeEqs: String; odeFormat: TFormatODEs);
 var i: Integer;
 begin
   runSim:= TSimulationJS.create(runTime, stepSize, Length(s_Vals),
                            Length(odeFormat.get_pVals()), odeEqs,solverUsed);
   runSim.OnUpdate:= getVals;  // register callback function.
   runSim.p:= odeFormat.get_pVals();
   currTime:= 0;
   if solverUsed= LSODAS then
      runSim.eval2(currTime, s_Vals)
   else runSim.eval(currTime, s_Vals);
   ODEready:= TRUE;
  // Debug:
  // printSpeciesParamsArr(odeFormat.get_sVals(), odeFormat.get_speciesStrAr());
  // printSpeciesParamsArr(runSim.p, odeFormat.get_paramsStr());
 end;


 procedure TmainForm.updateSimulation();
 begin
 stepSize:= self.WebTimer1.Interval * 0.001;  // 1 sec = 1000 msec
  if ODEready = TRUE then
    begin
    runSim.setStepSize(stepSize );
    if self.paramUpdated then
    begin
      runSim.p:= p_vals; // Update parameters ...
      self.paramUpdated:= false;
    end;
    // Get last time and s values and pass into eval2:
    if Length(s_Vals)>0 then
      begin
        if solverUsed = LSODAS then
          runSim.eval2(currTime,s_Vals)
        else runSim.eval(currTime, s_Vals);
      end;
    end
    // else error msg needed?
  else self.setupSimulation();
 end;


 // Get new values (species amt) from simulation run (ODE integrator)
 procedure TmainForm.getVals(newTime:double; newVals: array of double);
 var dataStr: String;
  i: Integer;
 begin
  // Update table of data;
    dataStr:= '';
    dataStr:= floatToStrf(newTime, ffFixed,4,4) + ', ';
    SetLength(s_Vals,Length(newVals));   // TODO: should just be a check. s_Vals length set earlier?
    for i := 0 to Length(newVals)-1 do
    begin
     dataStr:= dataStr + floatToStrf(newVals[i], ffExponent,6,2) + ', ';
     s_Vals[i]:= newVals[i];
    end;
    simResultsMemo.Lines.Add(dataStr);
    processScan(newTime,s_Vals);  //  <------------
    currTime:= newTime;
 end;


 procedure TmainForm.loadNetworkButtonClick(Sender: TObject);
begin
  self.NetworkJSONOpenDialog.execute();
end;


procedure TmainForm.mnuSaveClick(Sender: TObject);
var jstr, fileName : string;
begin
  fileName := InputBox('Save model to the downloads directory', 'Enter File Name:', 'jstr.json');
  if fileName <> '' then
     begin
     jstr := controller.network.convertToJSON();
     Application.DownloadTextFile(jstr, fileName);
     end
  else
     showmessage ('Save cancelled');
end;


procedure TmainForm.mnuUndoClick(Sender: TObject);
begin
  controller.undo;
  networkPB1.invalidate;
end;


procedure TmainForm.NetworkJSONOpenDialogChange(Sender: TObject);
begin
  self.LoadJSONFile;
end;


procedure TmainForm.NetworkJSONOpenDialogGetFileAsText(Sender: TObject;
  AFileIndex: Integer; AText: string);
begin
 try
     controller.loadModel (AText);
  except
    on E: exception do
       showmessage (e.message);
  end;
  networkPB1.Invalidate;
end;


procedure TmainForm.LoadJSONFile;
var FFileName : string;
begin
  if Assigned(NetworkJSONOpenDialog.Files[0]) then
  begin
    NetworkJSONOpenDialog.Files[0].GetFileAsText;
    FFileName := NetworkJSONOpenDialog.Files[0].Name;
    end;
end;


procedure TmainForm.processScan(t_new:double;y_new: array of double);
 var y: array of double;
  i: Integer;
  plot_y: array of boolean;
  j: Integer;
begin
  inc (CurrentGeneration);   // currentGeneration is 'time' (pixel number)
  setLength(plot_y,Length(y_new));
  for j := 0 to Length(plotSpecies)-1 do // cycle through all of the plots
  begin
    for i := 0 to Length(y_new)-1 do
    begin
     if plotSpecies[j][i] = '' then  plot_y[i]:= false
      else plot_y[i]:= true;
    end;
    // Dynamically draw plots:
  plotsG_Ar[j].addPoint (graphBitmapAr[j].canvas, currentGeneration, y_new, plot_y, True, currTime);
  self.plotsPBAr[j].canvas.draw (0, 0, graphBitmapAr[j])
  end;
end;


procedure TmainForm.selectPlotSpecies(plotnumb: integer);  // plot number to be added or modified
// Pass back to caller after closing popup:
  procedure AfterShowModal(AValue: TModalResult);
  var i:integer;
    addingPlot: boolean;
  begin
    if Length(plotSpecies)< plotnumb then
     begin
       // Add a plot with species list
      addingPlot:= true;
      setLength(plotSpecies,plotNumb);
      setLength(plotSpecies[plotNumb-1],length(s_Vals));
     end
    else addingPlot:= false;

    for i:=0 to plotSpeciesForm.SpPlotCG.Items.count -1 do
      begin
        if plotSpeciesForm.SpPlotCG.checked[i] then
        begin
           plotSpecies[plotNumb-1][i]:= plotSpeciesForm.SpPlotCG.Items[i];
        end
        else plotSpecies[plotNumb-1][i]:= '';
      end;
      if addingPlot then self.addPlot();   // <-- Add dynamically created plot at this point
  end;
// async called OnCreate for TSpeciesSWForm
  procedure AfterCreate(AForm: TObject);
  begin
    (AForm as TSpeciesSWForm).speciesList := s_names;
    (AForm as TSpeciesSWForm).fillSpeciesCG();
  end;
begin
  plotSpeciesForm := TSpeciesSWForm.CreateNew(@AfterCreate);
  plotSpeciesForm.Popup:= true;
  plotSpeciesForm.PopupOpacity:= 0.3;
  plotSpeciesForm.Border:= fbDialogSizeable;
  plotSpeciesForm.Caption:= 'Species to plot:';

  plotSpeciesForm.ShowModal(@AfterShowModal);
end;


procedure TmainForm.addPlot(); // Add a plot
  procedure plotOnMouseDown(sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var i:integer;   // grab plot which received event
  begin
    if (Button = mbRight) or (Button = mbLeft) then   // Both for now.
    begin
      if sender is TWebPaintBox then
      begin
       i:= TWebPaintBox(sender).tag;  // assume only plot paintboxes in right panel.
      // ShowMessage('Paint box sent mouse msg:  '+ inttostr(i));
       self.EditPlotList(i);
      end;
    end;
  end;

begin
 SetLength(graphBitmapAr,length(graphBitmapAr)+1); // want chk bitmaps = # plots?
 self.graphBitmapAr[self.numbPlots-1] := TBitmap.Create;
 SetLength(self.xscaleHeightAr,Length(self.xscaleHeightAr)+1);
 SetLength(self.plotsPBAr,Length(self.plotsPBAr)+1);
 SetLength(self.maxYValueAr,Length(self.maxYValueAr)+1);
 SetLength(pixelStepAr,Length(pixelStepAr)+1);
 SetLength(self.plotsG_Ar,Length(plotsG_Ar)+1);  // ***
 self.plotsG_Ar[self.numbPlots-1]:= TPlotGraph.create;
 self.plotsPBAr[self.numbPlots-1]:= TWebPaintBox.Create(self.RightWPanel);
 self.plotsPBAr[self.numbPlots-1].parent:=self.RightWPanel;
 self.plotsPBAr[self.numbPlots-1].OnPaint:= plotsPBArPaint;
 self.plotsPBAr[self.numbPlots-1].OnMouseDown:= plotOnMouseDown;

 configPbPlot(self.numbPlots,(self.RightWPanel.Width - SLIDERPWIDTH),self.RightWPanel.Height, self.plotsPBAr);
 self.xscaleHeightAr[self.numbPlots-1]:= round(0.15* self.plotsPBAr[self.numbPlots-1].Height);   // make %15 of total height
 self.maxYValueAr[self.numbPlots-1] := plotsPBAr[self.numbPlots-1].Height;  //  Adjust this..
 self.graphBitmapAr[self.numbPlots-1].width := self.plotsPBAr[self.numbPlots-1].Width;
 self.graphBitmapAr[self.numbPlots-1].height := self.plotsPBAr[self.numbPlots-1].Height;
 self.graphBitmapAr[self.numbPlots-1].canvas.brush.color := clWhite;
 self.graphBitmapAr[self.numbPlots-1].canvas.FillRect (rect (0, 0, self.plotsPBAr[self.numbPlots-1].width-1, self.plotsPBAr[self.numbPlots-1].height-1));
 self.plotsPBAr[self.numbPlots-1].invalidate;
end;


// TODO still more cleanup .. reorder plots if middle one deleted ?
// Look at plotsPBAr[].Top value to determine order ? Still look at 'Tag' value for ordering?
procedure TmainForm.DeletePlot(plotn: integer);
begin
 self.plotsPBAr[plotn-1].free;
 delete(self.plotsPBAr,(plotn-1),1);
 delete(self.graphBitmapAr,(plotn-1),1);
 delete(self.plotsG_Ar,(plotn-1),1);
 delete(self.maxYValueAr,(plotn-1),1);
 delete(self.plotSPecies,(plotn-1),1);
 delete(pixelStepAr,(plotn-1),1);
 delete(self.xscaleHeightAr,(plotn-1),1);
 self.numbPlots:= self.numbPlots - 1;
 self.RightWPanel.invalidate;
end;


procedure TmainForm.EditPlotList(plotn: integer);
var plotXposition, plotYposition: integer;
    editList: TStringList;
 begin
  plotXposition:= 40;
  plotYposition:= self.plotsPBAr[plotn-1].Top + 10;
  editList:= TStringList.create();
  editList.Add('Change plot species.');
  editList.Add('Delete plot.');
  editList.Add('Cancel');
  self.plotEditLB.Items:= editList;
  self.plotEditLB.Top:= plotYposition;
  self.plotEditLB.Left:= plotXposition;
  self.plotEditLB.Tag:= plotn;
  self.plotEditLB.bringToFront;
  self.plotEditLB.visible:= true;

 end;

procedure TmainForm.EditSliderList(sn: integer);  // delete, change param slider as needed.
var sliderXposition, sliderYposition: integer;
    editList: TStringList;
 begin
 //console.log('Edit Slider: slider #: ',sn);
  sliderXposition:= 424;
  sliderYposition:= self.sliderPanelAr[sn].Top + 10;
  editList:= TStringList.create();
  editList.Add('Change slider parameter.');
  editList.Add('Delete slider.');
  editList.Add('Cancel');
  self.SliderEditLB.Items:= editList;
  self.SliderEditLB.Top:= sliderYposition;
  self.SliderEditLB.Left:= sliderXposition;
  self.SliderEditLB.Tag:= sn;
  self.SliderEditLB.bringToFront;
  self.SliderEditLB.visible:= true;

 end;

 // TODO still more cleanup .. reorder sliders if middle one deleted ?
procedure TmainForm.DeleteSlider(sn: integer);
begin
//console.log('Delete Slider: slider #: ',sn);
 self.sliderPanelAr[sn].free;
 delete(self.sliderPanelAr,(sn),1);
 delete(self.sliderPHLabelAr,(sn),1);
 delete(self.sliderPLLabelAr,(sn),1);
 delete(self.sliderPTBLabelAr,(sn),1);
 delete(self.sliderPTBarAr,(sn),1);
 delete(self.sliderPHighAr,(sn),1);
 delete(self.sliderPLowAr,(sn),1);
 self.RightWPanel.invalidate;
end;

procedure TmainForm.setODEsolver();
  begin
  // If want choice then:
   // case ODEsolverWRadioGroup.ItemIndex of
   // 0: solverUsed:= EULER;
   // 1: solverUsed:= RK4;
   // 2: solverUsed:= LSODAS;
   // else solverUsed:= LSODAS;
   // end;
   solverUsed:= LSODAS;
  end;


  // Get initial vals for Species from SBML model
  // TODO: add code for nonSBML models.
  procedure TmainForm.fillSpeciesArray();
  var i:Integer;
  begin
    if sbmlmodel.getSpeciesNumb()>0  then   // TODO: chk if sbml model first
    begin
      setLength(s_Vals,sbmlmodel.getSpeciesNumb());
      setLength(s_names,length(s_Vals));
      for i:= 0 to sbmlmodel.getSpeciesNumb()-1 do
      begin
       if sbmlmodel.getSBMLspecies(i).isSetInitialAmount() then
            s_Vals[i]:= sbmlmodel.getSBMLspecies(i).getInitialAmount()
        else if sbmlmodel.getSBMLspecies(i).isSetInitialConcentration() then
            s_Vals[i]:= sbmlmodel.getSBMLspecies(i).getInitialConcentration();
        s_names[i]:=sbmlmodel.getSBMLspecies(i).getID();  // Use species ID as name
      end;
    end;
  end;

  // Select parameter to use for slider
  procedure TmainForm.SelectParameter(sNumb: integer);
  var paramIndex: integer; // param chosen in radiogroup
    // Pass back to caller after closing popup:
   procedure AfterShowModal(AValue: TModalResult);
    var i, pSliderNumb:integer;
      addingSlider: boolean;
    begin
      if Length(self.sliderParamAr)< (sNumb + 1) then
        begin
          pSliderNumb:= Length(self.sliderParamAr);  // same as self.numbParams
          // Add a slider
          addingSlider:= true;
          setLength(self.sliderParamAr,pSliderNumb+1);
        end
      else addingSlider:= false;
      console.log('Param index picked (chosenParam): ',sliderParamForm.chosenParam);
      self.sliderParamAr[sNumb]:= sliderParamForm.chosenParam;
      if addingSlider then self.addParamSlider()  // <-- Add dynamically created slider
      else self.SetSliderParamValues(sNumb, self.sliderParamAr[sNumb]);// update slider with new parameter:
    end;

// async called OnCreate for TParamSliderSForm
    procedure AfterCreate(AForm: TObject);
    begin
     (AForm as TParamSliderSForm).paramList := p_names;
     (AForm as TParamSliderSForm).fillParamRG();
    end;
  begin
    sliderParamForm := TParamSliderSForm.CreateNew(@AfterCreate);
    sliderParamForm.Popup:= true;
    sliderParamForm.PopupOpacity:= 0.3;
    sliderParamForm.Border:= fbDialogSizeable;
    sliderParamForm.Caption:= 'Parameter for slider:';
    sliderParamForm.ShowModal(@AfterShowModal);
  end;


  procedure TmainForm.addParamSlider();
  // default TBar range: 0 to initVal*10
    procedure SliderOnMouseDown(sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var i:integer;   // grab plot which received event
    begin
      if (Button = mbRight) or (Button = mbLeft) then   // Both for now.
      begin
        if sender is TWebPanel then
        begin
        i:= TWebPanel(sender).tag;  // assume only slider TWebpanel in right panel.
        // ShowMessage('WebPanel sent mouse msg:  '+ inttostr(i));
        self.EditSliderList(i);
        end;
      end;
    end;
  var i, sliderTBarWidth, sliderPanelLeft: integer;
  begin
    sliderPanelLeft:= 410;
    sliderTBarWidth:= 92;
    i:= Length(self.sliderPanelAr);  // array index for current slider to be added.
    SetLength(self.sliderPanelAr, i+1);
    SetLength(self.sliderPHighAr, i+1);
    SetLength(self.sliderPLowAr, i+1);
    SetLength(self.sliderPTBarAr, i+1);
    SetLength(self.sliderPHLabelAr, i+1);
    SetLength(self.sliderPLLabelAr, i+1);
    SetLength(self.sliderPTBLabelAr, i+1);
    self.sliderPanelAr[i]:= TWebPanel.Create(self.RightWPanel);
    self.sliderPanelAr[i].Parent:= self.RightWPanel;
    self.sliderPanelAr[i].OnMouseDown:= SliderOnMouseDown;
    configPSliderPanel(i, sliderPanelLeft, SLIDERPWIDTH, SLIDERPHEIGHT, self.sliderPanelAr);

    self.sliderPanelAr[i].Tag:= i;  // keep track of slider index number.
    self.sliderPTBarAr[i]:= TWebTrackBar.Create(self.sliderPanelAr[i]);
    self.sliderPTBarAr[i].Parent:= self.sliderPanelAr[i];
    self.sliderPTBarAr[i].OnChange:= self.ParamSliderOnChange;
    self.sliderPHLabelAr[i]:= TWebLabel.Create(self.sliderPanelAr[i]);
    self.sliderPHLabelAr[i].Parent:= self.sliderPanelAr[i];
    self.sliderPLLabelAr[i]:= TWebLabel.Create(self.sliderPanelAr[i]);
    self.sliderPLLabelAr[i].Parent:= self.sliderPanelAr[i];
    self.sliderPTBLabelAr[i]:= TWebLabel.Create(self.sliderPanelAr[i]);
    self.sliderPTBLabelAr[i].Parent:= self.sliderPanelAr[i];
    self.SetSliderParamValues(i, self.sliderParamAr[i]);
    configPSliderTBar(i,sliderTBarWidth, self.sliderPTBarAr,
               self.sliderPHLabelAr,self.sliderPLLabelAr,self.sliderPTBLabelAr);

  end;

  // Called when adding or updating a param slider. sn = slider number
  procedure TMainForm.SetSliderParamValues(sn, paramForSlider: integer);
  var rangeMult: integer;
  begin
    rangeMult:= 10; // default.
    self.sliderPTBLabelAr[sn].caption:= p_names[self.sliderParamAr[sn]]+ ': '+floattostr(p_vals[self.sliderParamAr[sn]]);
    self.sliderPLowAr[sn]:=0;
    self.sliderPLLabelAr[sn].caption:= floattostr(self.sliderPLowAr[sn]);
    //console.log('SetSliderParamValues sn...',sn,' : ',p_names[self.sliderParamAr[sn]]);

    self.sliderPTBarAr[sn].Min:= 0;
    self.sliderPTBarAr[sn].Position:= trunc((1/rangeMult)*100);
    self.sliderPTBarAr[sn].Max:= 100;
    if p_vals[self.sliderParamAr[sn]] > 0 then
    begin
        self.sliderPHLabelAr[sn].caption:= floattostr(p_vals[self.sliderParamAr[sn]]*rangeMult);
        self.sliderPHighAr[sn]:= p_vals[self.sliderParamAr[sn]]*rangeMult;
    end
    else begin
      self.sliderPHLabelAr[sn].caption:= floattostr(100);
      self.sliderPHighAr[sn]:= 100;   // default if init param val <= 0.
    end;

  end;

end.
