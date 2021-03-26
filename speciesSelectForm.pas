unit speciesSelectForm;

// Contains a plot

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.ExtCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, Types;

type
  TSpeciesSWForm = class(TWebForm)
    okButton1: TWebButton;
    SpPlotCG: TWebCheckGroup;

    procedure plotFormCreate(Sender: TObject);
    procedure WebFormShow(Sender: TObject);
    procedure okButton1Click(Sender: TObject);
    procedure SpPlotCGCheckClick(Sender: TObject; AIndex: Integer);
  //
  private
    { Private declarations }
  public
    { Public declarations }
    var speciesList: array of String;
     PlotWForm: TSpeciesSWForm;
     procedure fillSpeciesCG();
  end;
// var  speciesList: array of String;

implementation

{$R *.dfm}

// Close Plot:
procedure TSpeciesSWForm.okButton1Click(Sender: TObject);
var lForm: TWebForm;
begin
  lForm := TWebForm((Sender as TWebButton).Parent);
  lForm.Close;
  lForm.Free;
end;

procedure TSpeciesSWForm.plotFormCreate(Sender: TObject);
begin
  //console.log('Species select form created');


end;





procedure TSpeciesSWForm.SpPlotCGCheckClick(Sender: TObject; AIndex: Integer);
begin
// TODO ??
end;

procedure TSpeciesSWForm.WebFormShow(Sender: TObject);
begin
   // console.log('Species Select show() action');
end;

procedure TSpeciesSWForm.fillSpeciesCG();
var i:integer; spList: TStringList;
begin
spList:= TStringList.Create();
// Adjust checkgroup height as List may not fit with default height
 SpPlotCG.Height:= 30*length(speciesList);
for i := 0 to length(speciesList)-1 do
    spList.Add('&nbsp; '+speciesList[i]);
 SpPlotCG.Items:= spList;
end;

end.
