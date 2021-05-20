unit uSidewinderTypes;

interface
uses System.Generics.Collections;
type
  TDoubleArray = array of double;
  TIntegerArray = array of integer;
  TspeciesAr = array of String;

  TVarNameVal = class  // store string/value pair.
    private
      fId: String;
      fVal: double;
    public
      constructor create(); overload;
      constructor create(newId: String; newVal: double); overload;
      constructor create(newCpy: TVarNameVal); overload;
      property Id: String read fId write fId;
      property Val: double read fVal write fVal;
      function getId(): String;
      function getVal(): double;
      procedure setId(newId: String);
      procedure setVal(newVal: double);
  end;

  TVarNameValAr = class  // store array of string/value pair.
    private
      idValList: TList<TVarNameVal>;
    public
      constructor create(); overload;
      constructor create(newPair: TVarNameVal); overload;
      procedure add(newPair: TVarNameVal);
      function delete(id: String): Boolean;
      function getNameVal(id: String):TVarNameVal;
      function getNameAr(): array of String;
      function getValAr(): array of double;
  end;

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

 constructor TVarNameValAr.create();  overload;
 begin
   self.idValList := TList<TVarNameVal>.Create;
 end;

 constructor TVarNameValAr.create(newPair: TVarNameVal);  overload;
 begin
   self.idValList := TList<TVarNameVal>.Create;
   self.idValList.Add(newPair);
 end;
 procedure TVarNameValAr.add(newPair: TVarNameVal);
 begin
   self.idValList.Add(newPair);
 end;

 function TVarNameValAr.delete(id: String): Boolean;
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

 function TVarNameValAr.getNameVal(id: String): TVarNameVal;
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

 function TVarNameValAr.getNameAr(): array of String;
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

 function TVarNameValAr.getValAr(): array of double;
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


end.
