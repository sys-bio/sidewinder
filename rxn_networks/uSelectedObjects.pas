unit uSelectedObjects;

interface

Uses SysUtils, Classes, uNetwork;

type
  TListSelectedObjects = array of TParent;

  TSelectedObjects = class

   private
    selectedObjects : TListSelectedObjects;
    function  getValue (index : integer) : TParent;
    procedure setValue (index : integer; obj : TParent);
   public
    function    isSelected (obj : TParent) : boolean;
    function    add (obj : TParent) : integer;
    procedure   remove (obj : TParent);
    procedure   clear;
    function    count : integer;

    property    items[index : integer] : TParent read getValue write setValue; default;

    constructor Create;
  end;



implementation

constructor TSelectedObjects.Create;
begin
  inherited;
end;


function  TSelectedObjects.getValue (index : integer) : TParent;
begin
  result := selectedObjects[index];
end;


procedure TSelectedObjects.setValue (index : integer; obj : TParent);
begin
  selectedObjects[index] := obj;
end;


function TSelectedObjects.count : integer;
begin
  result := length (selectedObjects);
end;


function TSelectedObjects.isSelected (obj : TParent) : boolean;
begin
  result := obj.selected;
end;


function TSelectedObjects.add (obj : TParent) : integer;
begin
 setLength(selectedObjects, length(selectedObjects) + 1);
 selectedObjects[High(selectedObjects)] := obj;
end;


procedure TSelectedObjects.remove (obj : TParent);
var
  ALength: Cardinal;
  i, index : Cardinal;
begin
  index := -1;
  ALength := Length(selectedObjects);
  // Find the object
  for i := 0 to ALength - 1 do
      if selectedObjects[i] = obj then
         begin
         index := i;
         break;
         end;
  if index = -1 then
     raise Exception.Create('Internal error, obj not found');

  for i := index + 1 to ALength - 1 do
     selectedObjects[i - 1] := selectedObjects[i];
  setLength(selectedObjects, ALength - 1);
end;


procedure TSelectedObjects.clear;
begin
  setLength (selectedObjects, 0);
end;

end.
