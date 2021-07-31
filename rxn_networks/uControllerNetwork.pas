unit uControllerNetwork;

interface

uses SysUtils, Classes, System.UITypes, contnrs, Types, WebLib.ExtCtrls,
  WEBLib.Utils, WEBLib.Buttons, WEBLib.Graphics, WEBLib.Controls,
  Vcl.StdCtrls, WEBLib.StdCtrls, uNetwork, Dialogs, uSelectedObjects, Math,
  uNetworkCanvas,uModel, uNetworkToModel;

const
  NOT_SELECTED = -1;

type
  TStackOfSavedStates = array of TNetworkSavedState;
 // TNetworkChangeEvent = procedure(updatedNetwork: TNetwork) of object; // Network has been changed.

  TMouseStatus = (sSelect, sAddNode, sAddUniUni, sAddUniBi, sAddBiUni, sAddBiBi,
    sMouseDown, sMoveCentroid, sSelectingBox);

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
    selectedEdge: integer;
    currentX, currentY: double;
    MouseX, MouseY : double;

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

    networkCanvas : TNetworkCanvas;
  
    //sbmlModel: TModel;
  public
    procedure loadModel(modelStr: string);
    procedure setAddNodeStatus;
    procedure setAddUniUniReaction;
    procedure setAddUniBiReaction;
    procedure setAddBiUniReaction;
    procedure setAddBiBiReaction;

    procedure setSelectStatus;
    function  addNode(Id: string; x, y: double): TNode; overload;
    procedure addNode(x, y: double); overload;
    procedure setNodeId (id : string);
    procedure setNodeConc (conc : string);
    function  addReaction(Id: string; src, dest: TNode): integer;
    procedure prepareUndo;
    procedure undo;
    procedure deleteSelectedItems;

    procedure addUniUniReactionMouseDown(Sender: TObject; x, y: double);
    procedure addAnyReactionMouseDown(Sender: TObject; x, y: double; nReactants, nProducts: integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: double);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; x, y: double);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; x, y: double);
    function createSBMLModel(currentModel: TModel): TModel; // Build SBML model based on current Network
    procedure SBMLUpdated(updatedModel: TModel);

    constructor Create(network: TNetwork);
  end;

implementation

Uses JS, Web, WEBLib.JSON, uGraphUtils, uNetworkTypes;

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
  selectedEdge := -1;
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
      exit;
    end;
  end;

end;

procedure TController.loadModel(modelStr: string);
begin
  network.loadModel(modelStr); // JSON format.
end;

{procedure TController.networkUpdate(); // Notify listeners of change.
begin
    if Assigned(FNetworkUpdate) then
      FNetworkUpdate(self.network);
end;
  }
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

function TController.addReaction(Id: string; src, dest: TNode): integer;
begin
  prepareUndo;
  result := network.addUniUniReaction(Id, src, dest);
end;

function TController.addNode(Id: string; x, y: double): TNode;
var index : integer;
begin
  if network.findNode (Id, index) then
     raise Exception.Create('A node of that name already exists');

  prepareUndo;
  result := network.addNode(Id, x, y);
end;


procedure TController.addNode(x, y: double);
begin
  prepareUndo;
  network.addNode('node' + inttostr(Length(network.nodes) + 1), x, y);
end;


procedure TController.setNodeId (id : string);
var index : integer;
begin
  if selectedNode = -1 then
     exit;

  prepareUndo;
  if network.findNode (id, index) then
     begin
     showmessage ('A node of that name already exists');
     exit;
     end;

  network.nodes[selectedNode].state.id := Id;
end;

procedure TController.setNodeConc (conc : string);
var index : integer;
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
      ShowMessage ('Conc must be a number');
  end;
  network.nodes[selectedNode].state.conc := newConc;
end;

procedure TController.addUniUniReactionMouseDown(Sender: TObject; x, y: double);
var
  index: integer;
begin
  (Sender as TPaintBox).cursor := crHandPoint;

  if srcNode = NOT_SELECTED then // ie srcnode not chosen yet
  begin
    if network.overNode(x, y, index) <> nil then
    begin
      srcNode := index;
      destNode := NOT_SELECTED;
      network.nodes[index].addReactionSelected := True;
    end
    else
      mStatus := sSelect;
  end
  else if srcNode <> NOT_SELECTED then
  begin
    if network.overNode(x, y, index) <> nil then
    begin
      destNode := index;
      prepareUndo;
      network.addUniUniReaction('J' + inttostr(Length(network.reactions)),
        network.nodes[srcNode], network.nodes[destNode]);
      network.nodes[srcNode].addReactionSelected := False;
      srcNode := NOT_SELECTED;
      destNode := NOT_SELECTED;
      (Sender as TPaintBox).cursor := crDefault;
    end
    else
    begin
      mStatus := sSelect;
      srcNode := NOT_SELECTED;
      destNode := NOT_SELECTED;
    end;
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
      //  network.AddAnyToAnyEdge(sourceNodes, destNodes, index);
        network.AddAnyToAnyEdge('J' + inttostr(Length(network.reactions) ), sourceNodes, destNodes, index);
        sourceNodeCounter := -1;
        destNodeCounter := -1;
        network.UnSelectAll;
        // if Assigned (FModelChangeEvent) then
        // FModelChangeEvent (self, mcAddReaction, edgeIndex, edge.name);
      end;
    end;
  end;
end;



procedure TController.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: double);
var
  index, objIndex: integer;
begin
  try
    case mStatus of
      sAddNode:
        begin
          addNode(x, y);
          exit;
        end;

      sAddUniUni:
        begin
          addUniUniReactionMouseDown(Sender, x, y);
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
        end;
    end;

    if network.overNode(x, y, index) <> nil then
    begin
      currentObject := network.nodes[index];
      // In words: If shift not pressed and node not selected, then
      // unselect everything in preparation to select the node
      if not (ssShift in Shift) then
         begin
         if not currentObject.selected then
            begin
            network.unSelectAll;
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
      selectedEdge := -1;
      network.nodes[index].selected := True;
      currentX := x;
      currentY := y;
      exit;
    end;

    if network.overEdge(x, y, index) <> nil then
    begin
      network.UnSelectAll;
      selectedEdge := index;
      selectedNode := -1;
      console.log('mouse click over edge');
      network.reactions[index].selected := True;
      exit;
    end;

    if network.overCentroid(x, y, Index) <> nil then
    begin
      network.UnSelectAll;
      currentX := x;
      currentY := y;
      mStatus := sMoveCentroid;
      exit;
    end;

    mStatus := sSelect;
    network.UnSelectAll;
    selectedObjects.Clear;
    selectedNode := -1;
    selectedEdge := -1;
    mouseDownPressed := True;
    MouseX := x; MouseY := y;
  finally
    // (sender as TWebPaintbox).invalidate;
  end;
end;

procedure TController.OnMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: double);
var
  dx, dy: double;
  index, i: integer;
begin
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
              (selectedObjects[i] as TNode).state.x :=
                (selectedObjects[i] as TNode).state.x + dx;
              (selectedObjects[i] as TNode).state.y :=
                (selectedObjects[i] as TNode).state.y + dy;

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
        // Do we implement this, not sure?
        exit;
      end;

    sSelect,
    sSelectingBox:
      begin
        if mouseDownPressed then
           begin
           networkCanvas.bolDrawSelectionBox := True;
           networkCanvas.selectionBoxPt.x := trunc (x);  // Current coordinates
           networkCanvas.selectionBoxPt.y := trunc (y);
           networkCanvas.MousePt.x := trunc (MouseX);   // Original mousedown coordinate
           networkCanvas.MousePt.y := trunc (MouseY);
           mStatus := sSelectingBox;
           (Sender as TPaintBox).invalidate;
           end;
      end;
  end;

  // Put halo around node is we're added reactions.
  if (mStatus in [sAddUniUni, sAddUniBi, sAddBiUni, sAddBiBi]) and (network.overNode(x, y, index) <> nil) then
      begin
      network.nodes[index].addReactionSelected := True;
      end
  else
      network.unReactionSelect;
end;


procedure TController.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: double);
var i : integer;
begin
  if mStatus = sMouseDown then
     mStatus := sSelect;
  mouseDownPressed := False;
  networkCanvas.bolDrawSelectionBox := False;
  case mStatus of

    sSelectingBox:
        begin
        networkCanvas.paint;// (networkCanvas.origin, 1);
        mStatus := sSelect;
        network.UnSelectAll;
        selectedObjects.Clear;

        // Check for nodes
        for i := 0 to length (network.nodes) - 1 do
           begin
           if network.nodes[i].IsInRectangle (networkCanvas.selectionBox) then
              begin
              currentObject := network.nodes[i];
              currentObject.selected := True;
              selectedObjects.add (currentObject);
            end;
           end;
      end;
  end;
end;

function TController.createSBMLModel(currentModel: TModel): TModel;
var builder: TNetworkToModel;
begin
  builder := TNetworkToModel.create(currentModel, network);
  currentModel.SBML_UpdateEvent; // Notify listeners
  Result := builder.getModel();

end;

procedure TController.SBMLUpdated(updatedModel: TModel);
begin
  // TODO : update Network to reflect updated model.
   // Note: need to check that infinite loop created with
   //    network update causes model to update which causes network to update ...
  if updatedModel.getSpeciesNumb >0 then
    console.log('Network: TController.SBMLUpdate, First species: ',updatedModel.getSBMLSpecies(0).getID);

end;


end.
