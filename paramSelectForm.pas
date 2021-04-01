unit paramSelectForm;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.StdCtrls, Vcl.StdCtrls;

type
  TParamSliderSForm = class(TWebForm)
    OkButton1: TWebButton;
    paramRG: TWebRadioGroup;

    procedure WebFormShow(Sender: TObject);
    procedure OkButton1Click(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure paramRGChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    var paramList: array of String;
        paramWForm: TParamSliderSForm;
        chosenParam:integer;
    procedure fillparamRG();
  end;

var
  ParamSliderSForm: TParamSliderSForm;

implementation

{$R *.dfm}


procedure TParamSliderSForm.paramRGChange(Sender: TObject);
begin
  chosenParam:=  paramRG.ItemIndex;
end;

procedure TParamSliderSForm.WebFormCreate(Sender: TObject);
begin
// needed?
end;

procedure TParamSliderSForm.WebFormShow(Sender: TObject);
begin
   // console.log('Species Select show() action');
end;

procedure TParamSliderSForm.OkButton1Click(Sender: TObject);
var lForm: TWebForm;
begin
  lForm := TWebForm((Sender as TWebButton).Parent);
  lForm.Close;
  lForm.Free;
end;

procedure TParamSliderSForm.fillparamRG();
var i:integer; pList: TStringList;
begin
pList:= TStringList.Create();
// Adjust checkgroup height as List may not fit with default height
 paramRG.Height:= 30*length(paramList);
for i := 0 to length(paramList)-1 do
    pList.Add('&nbsp; '+paramList[i]);
 paramRG.Items:= pList;
end;

end.
