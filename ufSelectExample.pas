unit ufSelectExample;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.StdCtrls, Vcl.StdCtrls;


type
  TformExamples = class(TWebForm)
    rgSelectExample: TWebRadioGroup;
    procedure closeForm();
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

procedure TformExamples.closeForm();
var lForm: TWebForm;
begin
  self.close;
end;

procedure TformExamples.rgSelectExampleChange(Sender: TObject);
begin
 // console.log('change: ',rgSelectExample.ItemIndex );
  self.indexExampleChosen := rgSelectExample.ItemIndex;
  self.closeForm;
end;

end.
