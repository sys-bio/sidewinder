unit uControllerNetwork;

interface

uses SysUtils, Classes, System.UITypes, contnrs, Types, WebLib.ExtCtrls,
  WebLib.Utils, WebLib.Buttons, WebLib.Graphics, WebLib.Controls,
  Vcl.StdCtrls, WebLib.StdCtrls, uNetwork, Dialogs, uSelectedObjects, Math,
  uNetworkCanvas, uModel, uNetworkToModel, uNetworkTypes;

const
  NOT_SELECTED = -1;

type
  TStackOfSavedStates = array of TNetworkSavedState;
  // TNetworkChangeEvent = procedure(updatedNetwork: TNetwork) of object; // Network has been changed.

  TMouseStatus = (sSelect, sAddNode, sAddUniUni, sAddUniBi, sAddBiUni, sAddBiBi,
    sMouseDown, sMoveCentroid, sMovingBezierHandle, sMoveNodeControlRectangle, sSelectingBox);

  TNetworkStack = class
    networkStack: TStackOfSavedStates;
    stackCounter: integer;

    procedure push(n: TNetworkSavedState);
    function pop: TNetworkSavedState;
    function ifEmpty: boolean;
    constructor Create;
  end;

  TController = class
    mStatus: TMouseStatus;
    srcNode, destNode: integer;
    selectedNode: integer;
    selectedReaction: integer;
    currentX, currentY: double;
    MouseX, MouseY: double;

    anyByAny_nReactants: integer;
    anyByAny_nProducts: integer;

    sourceNodeCounter, destNodeCounter: integer;

    sourceNodes: array of TNode;
    destNodes: array of TNode;

    network: TNetwork;
    undoStack: TNetworkStack;

    selectedObjects: TSelectedObjects;
    currentObject: TParent;

    mouseDownPressed: boolean;

    networkCanvas: TNetworkCanvas;

    procedure adjustMovedBezierHandle;
  public
    procedure loadModel(modelStr: string);
    procedure loadSBMLModel(newSBMLmodel: TModel);
    procedure setAddNodeStatus;
    procedure setAddUniUniReaction;
    procedure setAddUniBiReaction;
    procedure setAddBiUniReaction;
    procedure setAddBiBiReaction;

    procedure setSelectStatus;
    function addNode(Id: string; x, y: double): TNode; overload;
    procedure addNode(x, y: double); overload;
    procedure setNodeId(Id: string);
    procedure setNodeConc(conc: string);
    procedure addReaction(Id: string; srcNodes, destNodes: array of TNode);
    procedure setReactionSpecStoich(spIndex: integer; stoichVal: double; src: boolean);
    procedure prepareUndo;
    procedure undo;
    procedure deleteSelectedItems;

    procedure addAnyReactionMouseDown(Sender: TObject; x, y: double; nReactants, nProducts: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: double);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; x, y: double);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: double);
    function getRxnArrowPts(): array of TPointF;
    function createSBMLModel(currentModel: TModel): TModel;
    // Build SBML model based on current Network
    procedure SBMLUpdated(updatedModel: TModel);

    constructor Create(network: TNetwork);
  end;

implementation

Uses JS, Web, WebLib.JSON, uGraphUtils;

constructor TNetworkStack.Create;
begin
  stackCounter := -1;
end;

procedure TNetworkStack.push(n: TNetworkSavedState);
begin
  inc(stackCounter);
  setLength(networkStack, Length(networkStack) + 1);
  networkStack[stackCounter] := n;
end;

function TNetworkStack.pop: TNetworkSavedState;
begin
  if stackCounter <> -1 then
    begin
      result := networkStack[stackCounter];
      dec(stackCounter);
    end;
end;

function TNetworkStack.ifEmpty: boolean;
begin
  result := True;
  if stackCounter <> -1 then
    result := False;
end;

constructor TController.Create(network: TNetwork);
begin
  undoStack := TNetworkStack.Create;

  self.network := network;
  mStatus := sSelect;
  srcNode := NOT_SELECTED;
  destNode := NOT_SELECTED;
  sourceNodeCounter := -1;
  destNodeCounter := -1;
  selectedNode := -1;
  selectedReaction := -1;
  selectedObjects := TSelectedObjects.Create;
  mouseDownPressed := False;
end;

procedure TController.prepareUndo;
begin
  undoStack.push(network.getCurrentState);
end;

procedure TController.undo;
begin
  if not undoStack.ifEmpty then
    network.loadState(undoStack.pop);
end;

procedure TController.deleteSelectedItems;
var
  i, j: integer;
  alength: integer;
begin
  for i := 0 to Length(network.nodes) - 1 do
    begin
      if network.nodes[i].selected then
        begin
          if not network.hasReactions(network.nodes[i]) then
            begin
              alength := Length(network.nodes);
              for j := i + 1 to alength - 1 do
                network.nodes[j - 1] := network.nodes[j];
              setLength(network.nodes, alength - 1);
              network.networkEvent(nil);
              // notify listener that network changed.
              exit;
            end
          else
            showmessage('Delete connecting reactions first');
        end;
    end;

  for i := 0 to Length(network.reactions) - 1 do
    begin
      if network.reactions[i].selected then
        begin
          alength := Length(network.reactions);
          for j := i + 1 to alength - 1 do
            network.reactions[j - 1] := network.reactions[j];
          setLength(network.reactions, alength - 1);
          selectedReaction := -1;
          network.networkEvent(nil);
          exit;
        end;
    end;

end;

procedure TController.loadModel(modelStr: string);
begin
  network.loadModel(modelStr); // JSON format.
end;

procedure TController.loadSBMLModel(newSBMLmodel: TModel);
begin
  network.loadSBMLModel(newSBMLmodel);
end;

procedure TController.setSelectStatus;
begin
  mStatus := sSelect;
end;

procedure TController.setAddNodeStatus;
begin
  mStatus := sAddNode;
end;

procedure TController.setAddUniUniReaction;
begin
  mStatus := sAddUniUni;
  srcNode := NOT_SELECTED;
  destNode := NOT_SELECTED;
end;

procedure TController.setAddUniBiReaction;
begin
  mStatus := sAddUniBi;
  srcNode := NOT_SELECTED;
  destNode := NOT_SELECTED;
end;

procedure TController.setAddBiUniReaction;
begin
  mStatus := sAddBiUni;
  srcNode := NOT_SELECTED;
  destNode := NOT_SELECTED;
end;

procedure TController.setAddBiBiReaction;
begin
  mStatus := sAddBiBi;
  srcNode := NOT_SELECTED;
  destNode := NOT_SELECTED;
end;

procedure TController.addReaction(Id: string;
  srcNodes, destNodes: array of TNode);
var
  reactionIndex: integer;
begin
  prepareUndo;
  network.addAnyToAnyReaction (Id, srcNodes, destNodes, reactionIndex);
end;

function TController.addNode(Id: string; x, y: double): TNode;
var
  index: integer;
begin
  if network.findNode(Id, index) then
    raise Exception.Create('A node of that name already exists');

  prepareUndo;
  result := network.addNode(Id, x, y);
end;

procedure TController.addNode(x, y: double);
begin
  prepareUndo;
  network.addNode('node' + inttostr(Length(network.nodes) + 1), x, y);
end;

procedure TController.setNodeId(Id: string);
var
  index: integer;
begin
  if selectedNode = -1 then
    exit;

  prepareUndo;
  if network.findNode(Id, index) then
    begin
      showmessage('A node of that name already exists');
      exit;
    end;

  network.nodes[selectedNode].state.Id := Id;
  // Update reactions that have this node:
  network.updateReactions(network.nodes[selectedNode]);
  network.networkEvent(nil);
end;

procedure TController.setNodeConc(conc: string);
var
  index: integer;
  newConc: double;
begin
  if selectedNode = -1 then
    exit;

  prepareUndo;
  try
    newConc := StrToFloat(conc);
  except
    on Exception: EConvertError do
      // ShowMessage(Exception.Message);
      showmessage('Concentration must be a number');
  end;
  network.nodes[selectedNode].state.conc := newConc;
  network.networkEvent(nil);
end;

function TController.getRxnArrowPts(): array of TPointF;
begin
  result := self.networkCanvas.reactionRenderer.getRxnAPts;
end;

procedure TController.setReactionSpecStoich(spIndex: integer; stoichVal: double;
  src: boolean);
var
  i: integer;
begin
  if selectedReaction = -1 then
    exit;
  prepareUndo;
  try
    if src then
      begin
        if spIndex < Length(network.reactions[selectedReaction].state.srcStoich)
        then
          begin
            network.reactions[selectedReaction].state.srcStoich[spIndex] :=
              stoichVal;
            network.networkEvent(nil);
          end
        else
          showmessage('Source species index too large');
      end
    else // destination node:
      if spIndex < Length(network.reactions[selectedReaction].state.destStoich)
      then
        begin
          network.reactions[selectedReaction].state.destStoich[spIndex] :=
            stoichVal;
          network.networkEvent(nil);
        end
      else
        showmessage('Source destination index too large');
    network.reactions[selectedReaction].setRateRule;
  except
    on Exception do
      showmessage
        ('Error setting reaction Stoichiometric coefficient for a species.');
  end;
end;


procedure TController.addAnyReactionMouseDown(Sender: TObject; x, y: double;
  nReactants, nProducts: integer);
var
  index: integer;
begin
  (Sender as TPaintBox).cursor := crHandPoint;
  console.log(' TController.addAnyReactionMouseDown');
  anyByAny_nReactants := nReactants;
  anyByAny_nProducts := nProducts;

  if sourceNodeCounter = -1 then
    setLength(sourceNodes, nReactants);
  if destNodeCounter = -1 then
    setLength(destNodes, nProducts);

  if sourceNodeCounter < nReactants - 1 then
    begin
      if network.overNode(x, y, sourceNodes[sourceNodeCounter + 1]) then
        begin
          sourceNodeCounter := sourceNodeCounter + 1;
          // collect source node id
          sourceNodes[sourceNodeCounter].selected := True;
        end;
    end
  else
    begin
      if destNodeCounter < nProducts - 1 then
        begin
          // collect source node id
          if network.overNode(x, y, destNodes[destNodeCounter + 1]) then
            begin
              destNodeCounter := destNodeCounter + 1;
              destNodes[destNodeCounter].selected := True;
            end;
          if not(destNodeCounter < nProducts - 1) then
            begin
              prepareUndo;
              // network.AddAnyToAnyEdge(sourceNodes, destNodes, index);
              network.addAnyToAnyReaction ('J' + inttostr(Length(network.reactions)), sourceNodes, destNodes, index);
              sourceNodeCounter := -1;
              destNodeCounter := -1;
              network.UnSelectAll;
              // if Assigned (FModelChangeEvent) then
              // FModelChangeEvent (self, mcAddReaction, edgeIndex, edge.name);
            end;
        end;
    end;
end;

procedure TController.adjustMovedBezierHandle;
var bnx, bny, bn_1x, bn_1y, c1x, c1y : double;
    i : integer;
    reaction : TReaction;
    bezier : TBezierCurve;
begin
  console.log ('adjust....');
  reaction := network.reactions[ObjectInformation.reactionIndex];

  if ObjectInformation.isReactant then
     bezier := reaction.state.reactantReactionArcs[ObjectInformation.arcId]
  else
     bezier := reaction.state.productReactionArcs[ObjectInformation.arcId];

  if ObjectInformation.isReactant then
     begin
     for i := 0 to reaction.state.nReactants - 1 do
         reaction.state.reactantReactionArcs[i].h2 := bezier.h2;

     bnx := reaction.state.arcCenter.x;
     bny := reaction.state.arcCenter.y;
     bn_1x := reaction.state.reactantReactionArcs[0].h2.x;
     bn_1y := reaction.state.reactantReactionArcs[0].h2.y;

     for i := 0 to reaction.state.nProducts - 1 do
         begin
         reaction.state.productReactionArcs[i].h1.x := 2*bnx - bn_1x;
         reaction.state.productReactionArcs[i].h1.y := 2*bny - bn_1y;
         end;
     end
  else
     begin
     for i := 0 to reaction.state.nProducts - 1 do
         reaction.state.productReactionArcs[i].h1 := bezier.h1;

     bnx := reaction.state.arcCenter.x;
     bny := reaction.state.arcCenter.y;
     c1x := reaction.state.productReactionArcs[0].h1.x;
     c1y := reaction.state.productReactionArcs[0].h1.y;

     for i := 0 to reaction.state.nReactants - 1 do
         begin
         reaction.state.reactantReactionArcs[i].h2.x := 2*bnx - c1x;
         reaction.state.reactantReactionArcs[i].h2.y := 2*bny - c1y;
         end;
     end;
end;


procedure TController.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: double);
var
  index, objIndex: integer;
  arcId: integer;
  handleId: TCurrentSelectedBezierHandle;
  handleCoords: TPointF;
  isReactant : boolean;
  selectedNodeGrabRectangle : integer;
begin
  prepareUndo;
  console.log ('MOUSE DOWN');
  try
    case mStatus of
      sAddNode:
        begin
          addNode(x, y);
          exit;
        end;
       sAddUniUni:
        begin
          addAnyReactionMouseDown(Sender, x, y, 1, 1);
          exit;
        end;
      sAddUniBi:
        begin
          addAnyReactionMouseDown(Sender, x, y, 1, 2);
          exit;
        end;
      sAddBiUni:
        begin
          addAnyReactionMouseDown(Sender, x, y, 2, 1);
          exit;
        end;
      sAddBiBi:
        begin
          addAnyReactionMouseDown(Sender, x, y, 2, 2);
          exit;
        end;
      sSelect:
        begin
        console.log ('SELECT');
        //selectedNode := -1;
        //network.UnSelectAll;
        //selectedObjects.Clear;
        end;
    end;

    if network.overNode(x, y, index) <> nil then
       begin
        console.log ('MOUSEDONW: Overnode');
        currentObject := network.nodes[index];
        // In words: If shift not pressed and node not selected, then
        // unselect everything in preparation to select the node
        if not(ssShift in Shift) then
          begin
            if not currentObject.selected then
               begin
               network.UnSelectAll;
               selectedObjects.Clear;
               end;
          end;

        // Shift to select multiple objects
        // If the object is already selected and shift is held
        // down, then unselect the object
        if currentObject.selected and (ssShift in Shift) then
           begin
           currentObject.selected := False;
           selectedObjects.remove(currentObject);
           exit;
           end;

        // If the object is already in the selected list,
        // don't add it to the list again
        if not selectedObjects.isSelected(currentObject) then
           objIndex := selectedObjects.add(currentObject);

        mStatus := sMouseDown;
        selectedNode := index;
        selectedReaction := -1;
        network.nodes[index].selected := True;
        currentX := x;
        currentY := y;
        exit;
        end;

    if network.overNodeControlRectangle (x, y, selectedNode, selectedNodeGrabRectangle) then
       begin
       currentX := x - network.nodes[selectedNode].state.w;
       currentY := y - network.nodes[selectedNode].state.h;
       mStatus := sMoveNodeControlRectangle;
       console.log ('sMoveNodeControlRectangle: Selected Node = ', inttostr (selectedNode));
       console.log ('EXIT');
       exit;
       end;


    // selectedReaction is an input
    if network.overCentroid(x, y, selectedReaction) then
       begin
       currentX := x;
       currentY := y;
       mStatus := sMoveCentroid;
       console.log('Over centroid: Mouse Down');
       exit;
       end;

    // selectedReaction is an input here
    // isEeactant returns true if the located bezier is on the reactant side
    if network.overBezierHandle(x, y, selectedReaction, isReactant, arcId, handleId, handleCoords) then
        begin
        console.log('Mouse click over bezier handle');
        // CurrentObjectInfo holds the info on the handle but not the edge,
        // get the edge info from the last selected arc
        currentX := x - handleCoords.x; // * scalingFactor;
        currentY := y - handleCoords.y; // * scalingFactor;
        ObjectInformation.handleCoords := handleCoords;
        ObjectInformation.handleId := handleId;
        ObjectInformation.arcId := arcId;
        ObjectInformation.reactionIndex := selectedReaction;
        ObjectInformation.isReactant := isReactant;

        mStatus := sMovingBezierHandle;
        exit;
        end;

    console.log ('selectedReaction 1:=', selectedReaction);
    // selectedReaction is an output there
    if network.overReaction(x, y, selectedReaction, arcId) then
       begin
       //network.UnSelectAll;
       selectedNode := -1;
       console.log('Mouse click over reaction');
       network.reactions[selectedReaction].selected := True;
       console.log ('EXIT TNetwork.overReaction');
       exit;
       end;
    console.log ('selectedReaction 2:=', selectedReaction);

    console.log ('Click on white canvas');
    // Nothing has happened for clear everything
    mStatus := sSelect;
    network.UnSelectAll;
    selectedObjects.Clear;
    selectedNode := -1;
    selectedReaction := -1;
    mouseDownPressed := True;
    MouseX := x;
    MouseY := y;
  finally
    //(sender as TWebPaintbox).invalidate;
  end;
end;


procedure TController.OnMouseMove(Sender: TObject; Shift: TShiftState; x, y: double);
var
  dx, dy: double;
  index, i: integer;
  reaction : TReaction;
begin
  // Compute delta movement since last mouse move
  dx := (x - currentX);
  dy := (y - currentY);
  case mStatus of

    sMouseDown:
      begin
        for i := 0 to selectedObjects.Count - 1 do
          begin
            // if not selectedObjects[i].isPositionLocked then
            begin
              // Update the old node coords as we'll need them next time to compute the distance
              if selectedObjects[i].selected then
                begin
                  (selectedObjects[i] as TNode).state.x := (selectedObjects[i] as TNode).state.x + dx;
                  (selectedObjects[i] as TNode).state.y := (selectedObjects[i] as TNode).state.y + dy;

                  // Find out what compartment the node is now in and reassign compartment if necessary
                  // if Network.CompartmentList.IsInCompartment (SelectedObjectList[i].SubNode.x, SelectedObjectList[i].SubNode.y,
                  // compartment, Index) then
                  // SelectedObjectList[i].SubNode.ParentNode.compartment := Compartment;
                end;
            end;
            currentX := x;
            currentY := y;
          end;

        // network.nodes[selectedNode].state.x := network.nodes[selectedNode].state.x + dx;
        // network.nodes[selectedNode].state.y := network.nodes[selectedNode].state.y + dy;
        // currentX := x; currentY := y;
        exit;
      end;

    sMoveCentroid:
      begin
        if selectedReaction <> -1 then
          begin
            network.reactions[selectedReaction].adjustArcCentres(x, y);
            (Sender as TPaintBox).invalidate;
          end;
        exit;
      end;

    sMovingBezierHandle:
      begin
      if selectedReaction <> -1 then
         begin
         reaction := network.reactions[ObjectInformation.reactionIndex];
         // Each bezier arc has two handles, h1 and h2, update whichever is being moved
         //console.log ('handleId');
        //console.log (ObjectInformation.handleId);
         case ObjectInformation.handleId of
             0 : begin
                 if ObjectInformation.isReactant then
                    begin
                    reaction.state.reactantReactionArcs[ObjectInformation.arcId].h1.x := (x - CurrentX);// /scalingFactor;
                    reaction.state.reactantReactionArcs[ObjectInformation.arcId].h1.y := (y - CurrentY);// /scalingFactor;
                    end
                 else
                    begin
                    reaction.state.productReactionArcs[ObjectInformation.arcId].h1.x := (x - CurrentX);// /scalingFactor;
                    reaction.state.productReactionArcs[ObjectInformation.arcId].h1.y := (y - CurrentY);// /scalingFactor;
                    end;
                 end;
             1 : begin
                 if ObjectInformation.isReactant then
                    begin
                    reaction.state.reactantReactionArcs[ObjectInformation.arcId].h2.x := (x - CurrentX);// /scalingFactor;
                    reaction.state.reactantReactionArcs[ObjectInformation.arcId].h2.y := (y - CurrentY);// /scalingFactor;
                    end
                 else
                    begin
                    reaction.state.productReactionArcs[ObjectInformation.arcId].h2.x := (x - CurrentX);// /scalingFactor;
                    reaction.state.productReactionArcs[ObjectInformation.arcId].h2.y := (y - CurrentY);// /scalingFactor;
                    end
                 end
             else
              showmessage ('Currently Selected Handle has incorrect value:');
         end;
      adjustMovedBezierHandle;
      (Sender as TPaintBox).invalidate;
      end;

      end;

    sMoveNodeControlRectangle :
      begin
      network.nodes[selectedNode].state.w := (x - currentX);
      network.nodes[selectedNode].state.h := (y - currentY);
      (Sender as TPaintBox).invalidate;
      end;

    sSelect, sSelectingBox:
      begin
        if mouseDownPressed then
          begin
            networkCanvas.bolDrawSelectionBox := True;
            networkCanvas.selectionBoxPt.x := trunc(x); // Current coordinates
            networkCanvas.selectionBoxPt.y := trunc(y);

            // Original mousedown coordinate (See MouseDown)
            networkCanvas.MousePt.x := trunc(MouseX);
            networkCanvas.MousePt.y := trunc(MouseY);

            mStatus := sSelectingBox;
            (Sender as TPaintBox).invalidate;
          end;
      end;
  end;

  // Put halo around node is we're added reactions.
  if (mStatus in [sAddUniUni, sAddUniBi, sAddBiUni, sAddBiBi]) and
    (network.overNode(x, y, index) <> nil) then
    begin
      network.nodes[index].addReactionSelected := True;
    end
  else
    network.unReactionSelect;
end;


procedure TController.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: double);
var
  i: integer;
begin
  console.log ('MOUSE UP');
  if mStatus in [sMouseDown, sMoveCentroid, sMovingBezierHandle, sMoveNodeControlRectangle] then
     begin
     mStatus := sSelect;
     end;

  mouseDownPressed := False;
  networkCanvas.bolDrawSelectionBox := False;
  case mStatus of

    sSelectingBox:
      begin
        networkCanvas.paint; // (networkCanvas.origin, 1);
        mStatus := sSelect;
        network.UnSelectAll;
        selectedObjects.Clear;

        // Check for nodes
        for i := 0 to Length(network.nodes) - 1 do
          begin

            if network.nodes[i].IsInRectangle(networkCanvas.selectionBox) then
              begin
                currentObject := network.nodes[i];
                currentObject.selected := True;
                selectedObjects.add(currentObject);
              end;
          end;

       // Check for reactions
       for i := 0 to length (network.reactions) - 1 do
           if network.reactions[i].IsInRectangle (networkCanvas.selectionBox) then
              begin
              currentObject := network.reactions[i];
              currentObject.selected := True;
              selectedObjects.add(currentObject);
              //CurrentObjectInfo := TObjectInfo.Create (Network.EdgeList[i]);
              //CurrentObjectInfo.vx := selectionBox.Left; CurrentObjectInfo.vy := selectionBox.Top;
              //CurrentObjectInfo.Edge.selected := True;
              //SelectedObjectList.Add(CurrentObjectInfo);
              end;

      end;
  end;
  console.log ('Selected node = ', inttostr (selectednode));
end;


function TController.createSBMLModel(currentModel: TModel): TModel;
var
  builder: TNetworkToModel;
begin
  builder := TNetworkToModel.Create(currentModel, network,
    self.networkCanvas.bitmap.Width, self.networkCanvas.bitmap.Height,
    self.getRxnArrowPts());
  // builder.setRxnArrowPts( self.getRxnArrowPts()); // for SBML RenderLineEnding
  result := builder.getModel();

end;

procedure TController.SBMLUpdated(updatedModel: TModel);
begin
  // update Network to reflect updated model.

  if Length(network.nodes) < 1 then // Only update network if loaded from file.
    begin
      if updatedModel.getSpeciesNumb > 0 then
         console.log('Network: TController.SBMLUpdate, First species: ',
         updatedModel.getSBMLSpecies(0).getID);
      self.loadSBMLModel(updatedModel);
    end;
end;

end.
