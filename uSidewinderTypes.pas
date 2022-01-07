unit uSidewinderTypes;

interface
uses System.Generics.Collections, Vcl.Dialogs;

const
  SLIDERPHEIGHT = 50; // Param Sliders WebPanel height
  DEFAULTSPECIESPLOTHT = 10; // Default y range for plotting species
  SLIDER_RANGE_MULT = 10;  // Default range multiplier for param slider
  PLOT_WIDTH_PERCENTAGE = 0.6; // This means a plot extends to 60% of the right panel width

type
  TDoubleArray = array of double;
  TIntegerArray = array of integer;
  TSpeciesList = TList<String>;

  TVarNameVal = class  // store string/value pair.
    private
      fId: String;
      fVal: double;
    public
      constructor create() overload;
      constructor create(newId: String; newVal: double); overload;
      constructor create(newCpy: TVarNameVal); overload;
      property Id: String read fId write fId;
      property Val: double read fVal write fVal;
      function getId(): String;
      function getVal(): double;
      procedure setId(newId: String);
      procedure setVal(newVal: double);
  end;

  TVarNameValList = class  // store array of string/value pair.
    private
      idValList: TList<TVarNameVal>;
    public
      constructor create() overload;
      constructor create(newPair: TVarNameVal); overload;
      procedure copy( orig: TVarNameValList );
      function getNumPairs(): integer;
      procedure add(newPair: TVarNameVal);
      function delete(id: String): Boolean;
      function getNameVal(index: integer):TVarNameVal;
      procedure setVal( index: integer; newVal: double ); // set val at index
      function getNameValById(id: String):TVarNameVal;
      function getNameAr(): array of String;
      function getValAr(): array of double;
  end;

  // Utilities:
  procedure notifyUser( msg: string );

implementation
 constructor TVarNameVal.create(); overload;
 begin
   self.fId := '';
   self.fVal := 0;
 end;
 constructor TVarNameVal.create(newId: String; newVal: double); overload;
 begin
   self.fId := newId;
   self.fVal := newVal;
 end;

 constructor TVarNameVal.create(newCpy: TVarNameVal); overload;
 begin
   self.fId := newCpy.Id;
   self.fVal := newCpy.Val;
 end;

 function TVarNameVal.getId(): String;
 begin
   Result := self.fId;
 end;

 function TVarNameVal.getVal(): double;
 begin
   Result := self.fVal;
 end;

 procedure TVarNameVal.setId(newId: String);
 begin
   self.fId := newId;
 end;

 procedure TVarNameVal.setVal(newVal: double);
 begin
   self.fVal := newVal;
 end;

 constructor TVarNameValList.create(); overload;
 begin
   self.idValList := TList<TVarNameVal>.Create;
 end;

 constructor TVarNameValList.create(newPair: TVarNameVal); overload;
 begin
   self.idValList := TList<TVarNameVal>.Create;
   self.idValList.Add(newPair);
 end;
 procedure TVarNameValList.add(newPair: TVarNameVal);
 begin
   self.idValList.Add(newPair);
 end;

 procedure TVarNameValList.copy( orig: TVarNameValList );
 var i: integer;
    newNameVal: TVarNameVal;
 begin
   for i := 0 to orig.getNumPairs -1 do
     begin
     newNameVal := TVarNameVal.create(orig.getNameVal(i));
     self.idValList.Add(newNameVal);
     end;

 end;

 function TVarNameValList.getNumPairs(): integer;
 begin
   Result := self.idValList.Count;
 end;

 function TVarNameValList.delete(id: String): Boolean;
 var i: integer;
     found: Boolean;
 begin
   found := false;
   for i := 0 to self.idValList.Count -1 do
   begin
     if self.idValList.Items[i].getId = id then
     begin
       self.idValList.Delete(i);
       found := true;
     end;

   end;
   Result := found;
 end;

 function TVarNameValList.getNameVal(index: integer):TVarNameVal;
 begin
   if (self.idValList.count > index) and (index > -1) then
      Result := self.idValList[index];
 end;

 procedure TVarNameValList.setVal( index: integer; newVal: double );
 begin
   if (self.idValList.count > index) and (index > -1) then
     self.idValList[index].setVal(newVal);
 end;

 function TVarNameValList.getNameValById(id: String): TVarNameVal;
 var i: integer;
     found: Boolean;
 begin
   found := false;
   for i := 0 to self.idValList.Count -1 do
   begin
     if self.idValList.Items[i].getId = id then
     begin
       Result := self.idValList.Items[i];
       found := true;
     end;
   end;
   if found = false then Result := nil;
 end;

 function TVarNameValList.getNameAr(): array of String;
 var i: integer;
     idAr: array of String;
 begin
   setLength(idAr,self.idValList.Count);
   for i := 0 to Length(idAr) -1 do
     begin
       idAr[i] := self.idValList.Items[i].getId;
     end;
   Result := idAr;
 end;

 function TVarNameValList.getValAr(): array of double;
 var i: integer;
     valAr: array of double;
 begin
   setLength(valAr,self.idValList.Count);
   for i := 0 to Length(valAr) -1 do
     begin
       valAr[i] := self.idValList.Items[i].getVal;
     end;
   Result := valAr;
 end;



 procedure notifyUser( msg: string);
 begin
   ShowMessage(msg);
 end;

end.
