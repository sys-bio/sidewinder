unit ufSelectExample;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.StdCtrls, Vcl.StdCtrls;


type
  TformExamples = class(TWebForm)
    rgSelectExample: TWebRadioGroup;
    btnClose: TWebButton;
    procedure btnCloseClick(Sender: TObject);
    procedure rgSelectExampleChange(Sender: TObject);
  private
    { Private declarations }
  public
  indexExampleChosen: integer;
  end;

var
  formExamples: TformExamples;

implementation

{$R *.dfm}

procedure TformExamples.btnCloseClick(Sender: TObject);
var lForm: TWebForm;
begin
  lForm := TWebForm((Sender as TWebButton).Parent);
  lForm.Close;
  lForm.Free;

end;

procedure TformExamples.rgSelectExampleChange(Sender: TObject);
begin
  console.log('change: ',rgSelectExample.ItemIndex );
  self.indexExampleChosen := rgSelectExample.ItemIndex;
end;

end.
