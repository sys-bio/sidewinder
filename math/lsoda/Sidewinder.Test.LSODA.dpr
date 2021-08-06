program Sidewinder.Test.LSODA;

uses
  Vcl.Forms,
  WEBLib.Forms,
  ufTestLSODA in 'ufTestLSODA.pas' {Form1: TWebForm} {*.html},
  LSODA.test in 'LSODA.test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
