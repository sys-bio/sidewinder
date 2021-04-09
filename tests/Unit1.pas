unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  Types, WEBLib.StdCtrls, ODE_FormatUtility, LSODA.test, WEBLib.ExtCtrls, Vcl.Graphics,
  SBML.helper, SBML.model, Simulation, Graphp, Vcl.Menus, WEBLib.Menus, SBML.model.rule;

type

  TForm1 = class(TWebForm)
    SBMLrxns: TWebLabel;
    SBMLmodelWMemo: TWebMemo;
    SBMLFilePicker: TWebFilePicker;
    ReactionID: TWebComboBox;
    SBMLReactants: TWebListBox;
    WebLabel1: TWebLabel;
    SBMLProducts: TWebListBox;
    Products: TWebLabel;
    ModelNotes: TWebMemo;
    WebLabel2: TWebLabel;
    kinLaw: TWebMemo;
    speciesListBox: TWebListBox;
    speciesListLabel: TWebLabel;
    CompListBox: TWebListBox;
    CompLabel: TWebLabel;
    ParamListBox: TWebListBox;
    ParamLabel: TWebLabel;
    SimResultsWMemo: TWebMemo;
    WebLabel3: TWebLabel;
    RtLength: TWebEdit;
    stepsizeWEdit: TWebEdit;
    rtLenLabel: TWebLabel;
    ODEsolverWRadioGroup: TWebRadioGroup;
    steSizeWLabel: TWebLabel;
    onlineWebB: TWebButton;
    offlineWebB: TWebButton;
    WebTimer1: TWebTimer;
    ParamWTrackBar1: TWebTrackBar;
    ParamWLabel1: TWebLabel;
    PlotWebPB: TWebPaintBox;
    AssignRulesLB: TWebListBox;
    AssignRuleLabel: TWebLabel;



    procedure SBMLFilePickerChange(Sender: TObject);
    procedure SBMLFilePickerGetFileAsText(Sender: TObject; AFileIndex: Integer;
      AText: string);
    procedure fillReactionIDcombo();
    procedure fillSpeciesListBox();
    procedure fillCompListBox();
    procedure fillParamListBox();
    procedure fillAssignRulesListBox();
    procedure ReactionIDChange(Sender: TObject);
    procedure UpdateReactionList();  // Called when new reaction picked.
    procedure printSpeciesParamsArr(valsAr:array of double; valsStrArr: array of String ); //debug
    procedure ODEsolverWRadioGroupChange(Sender: TObject); // Debug: shows arrays versus sbml Ids.
    procedure setODEsolver();
    procedure RtLengthChange(Sender: TObject); // Chk ODEsolverWRadioGroup and update solver to use.
    function ConvertStrToFloat(numbStr:String): double;
    procedure stepsizeWEditChange(Sender: TObject);
    function setupSimulation():String; // returns eq list as String.
    procedure startSimulation(odeEqs: String;odeFormat: TFormatODEs);
    procedure updateSimulation();
    procedure onlineWebBClick(Sender: TObject);
    procedure offlineWebBClick(Sender: TObject);
    procedure WebTimer1Timer(Sender: TObject);
    procedure PlotWebPBPaint(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);

  private
    procedure GetSBMLInfo();

  public
    procedure Ping(); // Notify when done loading libSBML
   // Get new values (species amt) from simulation run.
    procedure getVals(newTime:double; newVals: array of double);
    graphBitmap: TBitmap;
    maxYValue: integer;
    currentGeneration: integer;
    t, dt: double;
    xscaleHeight: integer; { This is the space reserved for the x axis labelling }
    plotGraph1: TPlotGraph;
    procedure processScan(t_new:double; y_new: array of double);
  end;

var
   Form1: TForm1;
   sbml_Text: String;
   numbRxns: Integer;
   sbmlmodel: SBMLhelpClass;
   rxnsArray: array of SBMLReaction;
   solverUsed: ODEsolver;
   runTime: double;      // Total runtime of simulation (sec)
   runSim: TSimulationJS;
   stepSize: double;     //(msec) (integration step)
   pixelStep: integer;   // pixel equiv of time (integration) step
   odeEqSet2: String;    // LSODA eqs, testing.
   currTime: double;
   s_Vals: array of double;
   p_Vals: array of double;
   p_names: array of String;
   simDataY: array of double; // Hold data of one plot series
   simDataX: array of double; // Hold domain (time) of one plot series

   ODEready: boolean;     // TRUE: ODE solver is setup.

implementation

{$R *.dfm}

procedure TForm1.SBMLFilePickerChange(Sender: TObject);
begin
if SBMLFilePicker.Files.Count > 0 then
  SBMLFilePicker.Files[0].GetFileAsText;
end;

procedure TForm1.SBMLFilePickerGetFileAsText(Sender: TObject;
  AFileIndex: Integer; AText: string);
begin
   SBMLmodelWMemo.Lines.Text := AText;
   sbml_Text:= AText;     // store the sbml model as text.
   // Check if sbmlmodel already created, if so, destroy before creating ?
   sbmlmodel:= SBMLhelpClass.create();
   { *** Register the callback function *** }
   sbmlmodel.OnPing := Ping;
   sbmlmodel.readSBML(AText); // Process text with libsbml.js
end;

// Grab SBML model information when notified:
procedure TForm1.Ping();
 begin
  GetSBMLInfo();
 end;

procedure TForm1.PlotWebPBPaint(Sender: TObject);
begin
  PlotWebPB.canvas.draw (0, 0, graphBitmap);
end;

// Get new values (species amt) from simulation run (ODE integrator)
 procedure TForm1.getVals(newTime:double; newVals: array of double);
 var dataStr: String;
  i: Integer; //newPt:TPointF;
 begin
  // Update table of data;
    dataStr:= '';
    dataStr:= floatToStrf(newTime, ffFixed,4,4) + ', ';
    SetLength(s_Vals,Length(newVals));
    for i := 0 to Length(newVals)-1 do
    begin
     dataStr:= dataStr + floatToStrf(newVals[i], ffExponent,6,2) + ', ';
     s_Vals[i]:= newVals[i];
    end;
    SimResultsWMemo.Lines.Add(dataStr);
    // Add newTime and s_Vals to array of data points (type TPoint).

    SetLength(simDataY,Length(simDataY)+1);
    SetLength(simDataX,Length(simDataX)+1); // Should be the same size as simDataY
    simDataY[Length(simDataY)-1]:= s_Vals[0];// Only grab first state variable for now.
    simDataX[Length(simDataY)-1]:= newTime;

     processScan(newTime,s_Vals);  //  <------------
    currTime:= newTime;

 end;

 procedure TForm1.ODEsolverWRadioGroupChange(Sender: TObject);
begin
   self.setODEsolver();
end;

procedure TForm1.offlineWebBClick(Sender: TObject);
begin

    WebTimer1.enabled:=false;

end;

procedure TForm1.onlineWebBClick(Sender: TObject);
begin
if stepSize=0 then stepSize:=0.1;

   plotGraph1.initGraph (0, 200, 0, maxYValue, 0, graphBitmap.width, 0, graphBitmap.height,
                 xscaleHeight, runTime, stepSize);


 // total steps: runTime/stepSize
  // Max viewable steps is PlotWebPB.width (1 pixel per step).

  pixelStep:=0;
  if runTime/stepSize < self.PlotWebPB.width then pixelStep:= round(self.PlotWebPB.width*stepSize/runTime)
  else pixelStep:= 1;

    WebTimer1.enabled:=true;
    self.updateSimulation();
end;

// **************************************************
 procedure TForm1.GetSBMLInfo();
 var i: Integer;
     parInitval: array of double; // param vals for ODE solver.
     varInitval: array of double; // init values for ODE solver
     outputList: TStringList;
 begin
   numbRxns:= sbmlmodel.getrxnsnumb();
   SBMLrxns.Caption := 'Number of reactions: ' + IntToStr(numbRxns);
   ModelNotes.Lines.Text := sbmlmodel.getAnnotationStr();
   rxnsArray:= Copy(sbmlmodel.getReactions(),0,numbRxns);
   fillReactionIDcombo();
   fillSpeciesListBox();
   fillCompListBox();
   fillParamListBox();
   fillAssignRulesListBox();

 end;

 // *** Update species lists for current reaction:
 procedure TForm1.ReactionIDChange(Sender: TObject);
 begin
  self.UpdateReactionList();
 end;

 // *** Update species lists for current reaction:
 procedure TForm1.UpdateReactionList();
 var rxnproducts: Array of SBMLspeciesreference;
 var rxnreactants: Array of SBMLspeciesreference;
 var i: integer;
 var stoichStr: String;
 var spref: SBMLspeciesreference;
 var klaw: String;
 var reactArray, prodArray: Array of String;    // test

 begin
  klaw:= rxnsArray[ReactionID.itemIndex].kineticLawStr;
  kinLaw.Text:= klaw;

  rxnproducts:= rxnsArray[ReactionID.itemIndex].getrxnProducts();
  rxnreactants:= rxnsArray[ReactionID.itemIndex].getrxnReactants();
  //console.log( 'ReactionIDChange:Number of reactants:',Length(rxnreactants));
   // loop through all reactions, product species is the ODE for the equation
   // Test:    Reactant list not needed
  for i := 0 to length(rxnreactants)-1 do
    begin
     reactArray[i]:= rxnreactants[i].getSpecies();
    end;

   //  1. Get array of all species in model and their initial values (if given),
   //  2. Then pass into replaceStrNames the species array, product and reactant arrays,
   //     kinetic law and prefix.
  for i := 0 to length(rxnproducts)-1 do
    begin
      prodArray[i]:= rxnproducts[i].getSpecies();
      //console.log('ReactionIDChange: ** species: ', rxnproducts[i].getSpecies());
    end;
  // END of test

  // Clear listboxes of previous reaction species:
  SBMLReactants.Items.Clear();
  SBMLProducts.Items.Clear();
  for i := 0 to Length(rxnreactants)-1 do
    begin
    spref:= rxnreactants[i];
    if spref<> nil then
      begin
      if spref.isSetSpecies() then
        begin
        stoichStr:= rxnreactants[i].getStoichiometry().toString();
        //console.log( 'Reactant stoich value:',rxnreactants[i].getStoichiometry());
        SBMLReactants.Items.Add(rxnreactants[i].getSpecies()+' (' + stoichStr + ')');

        end;
      end;
    end;

  for i := 0 to Length(rxnproducts)-1 do
    begin
    spref:= rxnproducts[i];
    if spref<> nil then
      begin
      if spref.isSetSpecies() then
        begin
        stoichStr:= rxnproducts[i].getStoichiometry().toString();
        //console.log( 'Product stoich value:',rxnproducts[i].getStoichiometry());
        SBMLProducts.Items.Add(rxnproducts[i].getSpecies()+' (' + stoichStr + ')');
        end;
      end;

    end;
 end;


procedure TForm1.RtLengthChange(Sender: TObject);
var newRTstr:String;
begin
  newRTstr:= '';
  newRTstr:= RtLength.Text;
  runTime:= self.ConvertStrToFloat(newRTstr);
end;

function TForm1.ConvertStrToFloat(numbStr:String):double;
var newDbl: double;
begin
newDbl:=0;

  try
    newDbl := strtofloat(numbStr);    // Hexadecimal values are not supported
  except
    on Exception : EConvertError do
    begin
      newDbl:= 100;  // default
      console.log(Exception.Message);
    end;
  end;
  Result:=newDbl;
end;

// ReactionID: TWebComboBox;
 procedure TForm1.fillReactionIDcombo();
var
  i: Integer;
 begin
   for i := 0 to length(rxnsArray)-1 do
    begin
     ReactionID.AddItem(rxnsArray[i].getID(),nil);
    end;
    ReactionID.ItemIndex := 0;

    self.UpdateReactionList();
 end;

  procedure TForm1.fillSpeciesListBox();
  var i:Integer;
  var amt: String;
  begin
    for i:= 0 to sbmlmodel.getSpeciesNumb()-1 do
    begin
      if sbmlmodel.getSBMLspecies(i).isSetInitialAmount() then
          amt:= sbmlmodel.getSBMLspecies(i).getInitialAmount().toString()
      else if sbmlmodel.getSBMLspecies(i).isSetInitialConcentration() then
          amt:= sbmlmodel.getSBMLspecies(i).getInitialConcentration().toString();

      speciesListBox.AddItem(sbmlmodel.getSBMLspecies(i).getID()+' ('+amt+')',nil);
    end;

  end;


procedure TForm1.fillCompListBox();
  var i:Integer;
  var sz: String;
  begin
    for i:= 0 to sbmlmodel.getCompNumb()-1 do
    begin
      if sbmlmodel.getSBMLcompartment(i).isSetSize() then
        sz:= sbmlmodel.getSBMLcompartment(i).getSize().toString();
      compListBox.AddItem(sbmlmodel.getSBMLcompartment(i).getID()+' ('+sz+')',nil);
    end;
  end;

  procedure TForm1.fillParamListBox();
    var i:Integer;
    var pvalue: String;
  begin
      for i:= 0 to sbmlmodel.getParamNumb-1 do
    begin
    //console.log('fillParamListBox, param id: ',sbmlmodel.getSBMLparameter(i).getID());
      if sbmlmodel.getSBMLparameter(i).isSetValue() then
        pvalue:= sbmlmodel.getSBMLparameter(i).getValue().toString();
      paramListBox.AddItem(sbmlmodel.getSBMLparameter(i).getID()+' ('+pvalue+')',nil);
    end;
  end;

  procedure TForm1.fillAssignRulesListBox();
   var i:Integer;
       ruleValue: String;
       rules: array of TSBMLrule;
   begin
     ruleValue:= '';
     rules:= sbmlmodel.getSBMLmodelRules();
     for i := 0 to Length(rules)-1 do
     begin
       if rules[i].isAssignment then
       begin
         if rules[i].isParameter then ruleValue:= rules[i].getVariable()
         else if rules[i].isSpeciesConcentration() then ruleValue:= rules[i].getVariable();
         if rules[i].isSetFormula then
         begin
           ruleValue:= ruleValue + ' = ' +rules[i].getFormula();
         end;
         self.AssignRulesLB.AddItem(ruleValue, nil);
       end;
     end;

   end;

  // Debug:
  procedure TForm1.printSpeciesParamsArr(valsAr:array of double; valsStrArr: array of String );
  var i:Integer;
  begin
   // console.log(' Species/Param name:  Value');
    for i := 0 to Length(valsAr)-1 do
      begin
        console.log(valsStrArr[i],': ',valsAr[i]);
      end;
  end;

  procedure TForm1.setODEsolver();
  begin
    case ODEsolverWRadioGroup.ItemIndex of
    0: solverUsed:= EULER;
    1: solverUsed:= RK4;
    2: solverUsed:= LSODAS;
    else solverUsed:= LSODAS;
    end;
  end;


procedure TForm1.stepsizeWEditChange(Sender: TObject);
 var newRTstr:String;
 begin
    newRTstr:= '';
    newRTstr:= stepsizeWEdit.Text;
    stepSize:= self.ConvertStrToFloat(newRTstr)*0.001; // convert ms to sec
 end;

 // **************************************************************
 function TForm1.setupSimulation():String;
 var simRTStr: String;   // Output string for TWebMemo
     i: Integer;
     odeEqs: String;
     odeFormat: TFormatODEs;
 begin
   ODEready:= false;

   try
      runTime:= ConvertStrToFloat(RTLength.Text);
   except
      on Exception : EConvertError do
      begin
        runTime:= 10;
        self.RTLength.Text:= FloatToStr(runTime);
      end;
   end;

   try
    self.WebTimer1.Interval:= strtoint(stepsizeWEdit.Text); // Trailing blanks are not supported
   except
      on Exception : EConvertError do
      begin
        self.WebTimer1.Interval:=100; // default
        self.stepSizeWEdit.Text:= IntToStr( self.WebTimer1.Interval );
      end;
   end;
   stepSize:= self.WebTimer1.Interval*0.001;  // 1 sec = 1000 msec
   SimResultsWMemo.Lines.Clear();

   odeFormat:= TFormatODEs.create(sbmlmodel);
   //odeFormat.testStrReplace;
   SetLength(p_Vals,Length(odeFormat.get_pVals()));  // Keep params for later
   for i := 0 to Length(odeFormat.get_pVals())-1 do
   begin
      p_Vals[i]:= odeFormat.get_pVals[i];
   end;

    // Run Simulation using info from odeFormat:
   odeFormat.buildFinalEqSet();
   simRTStr:= ' Time (s) ';   // generate coloumn headers:
   for i := 0 to Length(odeFormat.get_speciesStrAr())-1 do
     begin
        simRTStr:= simRTstr +', ' + odeFormat.get_speciesStrAr()[i];
     end;
   SimResultsWMemo.Lines.Add( simRTStr);

   if SolverUsed = LSODAS then self.startSimulation(odeFormat.getODEeqSet2(),odeFormat)//Result:= odeFormat.getODEeqSet2()
   else  self.startSimulation(odeFormat.getODEeqSet(),odeFormat)//Result:= odeFormat.getODEeqSet();
 end;

 // **********************************************************************
 procedure TForm1.startSimulation(odeEqs: String; odeFormat: TFormatODEs);
 var i: Integer;
 begin
   runSim:= TSimulationJS.create(runTime, stepSize, Length(odeFormat.get_sVals()),
                            Length(odeFormat.get_pVals()), odeEqs,solverUsed);
   //runSim.testLSODA();
   runSim.OnUpdate:= getVals;  // register callback function.
   runSim.p:= odeFormat.get_pVals();
   currTime:= 0;
   if solverUsed= LSODAS then
    runSim.eval2(currTime,odeFormat.get_sVals())
   else runSim.eval(currTime, odeFormat.get_sVals());
   ODEready:= TRUE;

   // Debug:
  // printSpeciesParamsArr(odeFormat.get_sVals(), odeFormat.get_speciesStrAr());
  // printSpeciesParamsArr(runSim.p, odeFormat.get_paramsStr());

 end;

 procedure TForm1.updateSimulation();
var
  i: Integer;
 begin
 stepSize:= self.WebTimer1.Interval * 0.001;  // 1 sec = 1000 msec
  if ODEready = TRUE then
    begin
    runSim.setStepSize(stepSize );
    // Get last time and s values and pass into eval2:
    console.log(' Current time of sim: ',currTime);
    if Length(s_Vals)>0 then
      begin
        if solverUsed = LSODAS then
          runSim.eval2(currTime,s_Vals)
        else runSim.eval(currTime, s_Vals);

      end;
    end
    // else error msg?
  else self.setupSimulation();

 end;

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  stepSize:= 0.1;   // 100 msec
  maxYValue := PlotWebPB.Height;  //  Adjust this..
  plotGraph1:= TPlotGraph.Create;
  xscaleHeight  := round(0.15* PlotWebPB.Height);   // make %15 of total height
  CurrentGeneration := 0;
  { graphBitmap is an offscreen area for building the graph }
  graphBitmap := TBitmap.Create;
  graphBitmap.width := PlotWebPB.Width;
  graphBitmap.height := PlotWebPB.Height;
  graphBitmap.canvas.brush.color := clWhite;
  graphBitmap.canvas.FillRect (rect (0, 0, PlotWebPB.width-1, PlotWebPB.height-1));
end;

procedure TForm1.WebTimer1Timer(Sender: TObject);
begin
   self.updateSimulation();
   if currTime > runTime then
       WebTimer1.enabled:=false;
end;


procedure TForm1.processScan(t_new:double;y_new: array of double);
//var x, y, z : double;
var y: array of double;
  i: Integer;
  plot_y: array of boolean;

begin
  inc (CurrentGeneration);   // currentGeneration is 'time' (pixel number)

 // t := t + dt;
 // y[0]:= sin (t)*40 ;
 // y[1]:= cos (t)*40;
//  y[2]:= (2.3*y[0] + 6.7*cos (t*2)*10);
 // setLength(plot_y,Length(y));
  setLength(plot_y,Length(y_new));
  for i := 0 to Length(y_new)-1 do
  begin
    plot_y[i]:= true;
  end;
  //addPoint (graphBitmap.canvas, currentGeneration, trunc (x)+120, trunc (y)+120, trunc (z)+170, True, True, True, True);
  plotGraph1.addPoint (graphBitmap.canvas, currentGeneration, y_new, plot_y, True, currTime);
  PlotWebPB.canvas.draw (0, 0, graphBitmap);
end;



end.
