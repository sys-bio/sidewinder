unit ufYAxisMinMaxEdit;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  uSidewinderTypes;

type
  TUpdateYMaxMinValsEvent = procedure( newYMin, newYMax: double) of object;
  TFYAxisMinMaxEdit = class(TWebForm)
    editYMax: TWebEdit;
    editYMin: TWebEdit;
    lblYMax: TWebLabel;
    lblYMin: TWebLabel;
    lblYMaxMin: TWebLabel;
    procedure editYMaxChange(Sender: TObject);
    procedure editYMinChange(Sender: TObject);
    procedure editYMinExit(Sender: TObject);
    procedure getChanges();
    procedure WebFormClose(Sender: TObject; var Action: TCloseAction);
    procedure WebFormCreate(Sender: TObject);
  private
    fUpdateYMaxMinEvent: TUpdateYMaxMinValsEvent;
  public
    procedure setDefaultYMinMax(min, max: double);
    property OnUpdateYMaxMinValsEvent: TUpdateYMaxMinValsEvent read fUpdateYMaxMinEvent write fUpdateYMaxMinEvent;
  end;

var
  FYAxisMinMaxEdit: TFYAxisMinMaxEdit;

implementation

{$R *.dfm}

procedure TFYAxisMinMaxEdit.setDefaultYMinMax(min, max: double);
begin
  self.editYMin.Text := min.toString;
  self.editYMax.Text := max.ToString;
end;

procedure TFYAxisMinMaxEdit.editYMaxChange(Sender: TObject);
begin
  self.getChanges
end;

procedure TFYAxisMinMaxEdit.editYMinChange(Sender: TObject);
begin
  self.getChanges;
end;

procedure TFYAxisMinMaxEdit.editYMinExit(Sender: TObject);
begin
  self.getChanges;
end;

procedure TFYAxisMinMaxEdit.getChanges;
var yMax, yMin: double;
    sOrigMin, sOrigMax: string;
begin
  try
    try
      begin
      yMax := strToFloat(self.editYMax.Text);
      sOrigMax := self.editYMax.Text;
      yMin := strToFloat(self.editYMin.Text);
      sOrigMin := self.editYMin.Text;
      end;

    except
      on Exception : EConvertError do
      begin
      notifyUser(Exception.Message);
      yMax := 10; // default
      yMin := 0;
      self.editYMin.Text := sOrigMin; //'0';
      self.editYMax.Text := sOrigMax; //'10';
      end;
    end;

  finally
    if Assigned(fUpdateYMaxMinEvent) then
      fUpdateYMaxMinEvent(yMin, yMax);
  end;

end;

procedure TFYAxisMinMaxEdit.WebFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  self.getChanges
end;

procedure TFYAxisMinMaxEdit.WebFormCreate(Sender: TObject);
begin
  self.lblYMaxMin.font.Size := 18;
end;

end.