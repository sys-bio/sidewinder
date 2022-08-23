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
  //  procedure WebFormShow(Sender: TObject);
    procedure okButton1Click(Sender: TObject);
    procedure SpPlotCGCheckClick(Sender: TObject; AIndex: Integer);
    function  setChkGrpWidth(): integer; // adjusts width based on longest string in speciesList
    procedure unCheckGroup();
    procedure checkGroup();
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

{procedure TVarSelectForm.WebFormShow(Sender: TObject);     // cannot check here...
var i : integer;
begin
  for i := 0 to length(speciesList)-1 do
    if length(speciesList) <10 then
      SpPlotCG.Checked[i] := True
    else SpPlotCG.Checked[i] := false;
end;         }

function  TVarSelectForm.setChkGrpWidth(): integer;
var i, maxLength: integer;
begin
  maxLength := 0;
  for i := 0 to length(self.speciesList) -1 do
    begin
    if length(self.speciesList[i]) > maxLength then
      maxLength := length(self.speciesList[i]);
    end;
  if maxLength < 10 then
    maxLength := 10;
  result := maxLength * 10;
end;

procedure TVarSelectForm.fillSpeciesCG();
var i : integer;
begin
  if length(speciesList) > 10 then
    self.Height := 20*length(speciesList);  //15

  // Adjust chkgrp height as List may not fit with default height:
  SpPlotCG.Height := 30*length(speciesList);
  self.SpPlotCG.Width := self.setChkGrpWidth ;// Adjust chkgrp width to fit longest string
  // okButton1.Top := SpPlotCG.Height + 30;
  for i := 0 to length(speciesList)-1 do
    SpPlotCG.Items.Add ('&nbsp; ' + speciesList[i]);
end;

procedure TVarSelectForm.unCheckGroup();
var i: integer;
begin
  for i := 0 to length(speciesList)-1 do
    SpPlotCG.Checked[i] := false;
end;

procedure TVarSelectForm.checkGroup();
var i: integer;
begin
  for i := 0 to length(speciesList)-1 do
    SpPlotCG.Checked[i] := true;
end;

end.
