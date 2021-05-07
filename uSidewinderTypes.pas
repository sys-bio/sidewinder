unit uSidewinderTypes;

interface

type
  TDoubleArray = array of double;
  TIntegerArray = array of integer;
  TspeciesAr = array of String;

  TVarNameVal = class  // store string/value pair.
    private
      fId: String;
      fVal: double;
    public
      constructor create(newId: String; newVal: double); overload;
      constructor create(newCpy: TVarNameVal); overload;
      property Id: String read fId write fId;
      property Val: double read fVal write fVal;
  end;

  TVarNameValAr = class  // store array of string/value pair.
      // TODO
  end;

implementation
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
end.
