unit ufVarSelect;

// Contains a plot

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.ExtCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, Types, VCL.TMSFNCTypes, VCL.TMSFNCUtils, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes;

type
  TVarSelectForm = class(TWebForm)
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
     speciesList: array of String;
     PlotWForm: TVarSelectForm;
     procedure fillSpeciesCG();
  end;

implementation

{$R *.dfm}


// Close Plot:
procedure TVarSelectForm.okButton1Click(Sender: TObject);
var lForm: TWebForm;
begin
  lForm := TWebForm((Sender as TWebButton).Parent);
  lForm.Close;
  lForm.Free;
end;

procedure TVarSelectForm.plotFormCreate(Sender: TObject);
begin
  //console.log('Species select form created');
end;


procedure TVarSelectForm.SpPlotCGCheckClick(Sender: TObject; AIndex: Integer);
begin
// TODO ??
end;

procedure TVarSelectForm.WebFormShow(Sender: TObject);
var i : integer;
begin
  for i := 0 to length(speciesList)-1 do
    if length(speciesList) <10 then
      SpPlotCG.Checked[i] := True
    else SpPlotCG.Checked[i] := false;
end;


procedure TVarSelectForm.fillSpeciesCG();
var i : integer;
begin
  // Adjust checkgroup height as List may not fit with default height
  SpPlotCG.Height := 30*length(speciesList);
  // okButton1.Top := SpPlotCG.Height + 30;
  for i := 0 to length(speciesList)-1 do
    SpPlotCG.Items.Add ('&nbsp; ' + speciesList[i]);
end;


end.
