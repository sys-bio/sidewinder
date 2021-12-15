unit uSelectedObjects;

interface

Uses SysUtils, Classes, uNetwork, Types, uNetworkTypes, System.Generics.Collections;

type
  TObjectType = (oNone, oNode, oReaction);

  TObjectInformation = class (TObject)

     objType : TObjectType;

     node : TNode;
     reaction : TReaction;
     handleCoords : TPointF;
     arcId : integer;   // Which bezier is it?
     handleId : integer;   // Which handle on the bezier is it?
     isReactant : boolean;
  end;

  TListOfSelectedObjects = TObjectList<TObjectInformation>;// array of TObjectInformation;

  TSelectedObjects = class
   private
     selectedObjects : TListOfSelectedObjects;
     function  getValue (index : integer) : TObjectInformation;
     procedure setValue (index : integer; obj : TObjectInformation);
   public
     function    isSelected (const obj : TObjectInformation; var index : integer) : boolean;
     function    add (const obj : TObjectInformation) : integer;
     procedure   remove (obj : TObjectInformation);
     procedure   clear;
     function    count : integer;

     property    items[index : integer] : TObjectInformation read getValue write setValue; default;

     constructor Create;
  end;


implementation

constructor TSelectedObjects.Create;
begin
  inherited;
  selectedObjects := TObjectList<TObjectInformation>.Create;
end;


function  TSelectedObjects.getValue (index : integer) : TObjectInformation;
begin
  result := selectedObjects[index];
end;


procedure TSelectedObjects.setValue (index : integer; obj : TObjectInformation);
begin
  selectedObjects[index] := obj;
end;


function TSelectedObjects.count : integer;
begin
  result := selectedObjects.Count;//  length (selectedObjects);
end;


function TSelectedObjects.isSelected (const obj : TObjectInformation; var index : integer) : boolean;
var i : integer;
    c : integer;
begin
  result := False;
  c := Count();
  for i := 0 to c - 1 do
      begin
      if (obj.node <> nil) and (obj.node = items[i].node) then
         begin
         result := True;
         index := i;
         exit;
         end;
      if (obj.reaction <> nil) and (Obj.reaction = items[i].reaction) then
         begin
         result := True;
         index := i;
         exit;
         end;
      //if (ObjectInfo.Compartment <> nil) and (ObjectInfo.Compartment = Items[i].Compartment) then
      //   begin
      //   result := True;
      //   index := i;
       //  exit;
       //  end;
      //if (ObjectInfo.NetObj <> nil) and (ObjectInfo.NetObj = Items[i].NetObj) then
      //   begin
      //   result := True;
      //   index := i;
      //   exit;
      //   end;
      end;
end;


function TSelectedObjects.add (const obj : TObjectInformation) : integer;
begin
 selectedObjects.Add (obj);
 //setLength(selectedObjects, length(selectedObjects) + 1);
 //selectedObjects[High(selectedObjects)] := obj;
 //result := length (selectedObjects) - 1;
end;


procedure TSelectedObjects.remove (obj : TObjectInformation);
var
  ALength: Cardinal;
  i, index : Cardinal;
begin
  index := -1;
  ALength := selectedObjects.Count;
  //ALength := Length(selectedObjects);
  // Find the object
  for i := 0 to ALength - 1 do
      if selectedObjects[i] = obj then
         begin
         index := i;
         break;
         end;
  if index = -1 then
     raise Exception.Create('Internal error, obj not found');

  selectedObjects.Remove (obj);
 // for i := index + 1 to ALength - 1 do
  //   selectedObjects[i - 1] := selectedObjects[i];
  //setLength(selectedObjects, ALength - 1);
end;


procedure TSelectedObjects.clear;
begin
  selectedObjects.Clear;
  //setLength (selectedObjects, 0);
end;

end.
