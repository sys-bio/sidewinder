unit ufNodeFrame;
   // Currently not in use. May not be needed.
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TFrame1 = class(TFrame)
    WebButton1: TWebButton;
    WebEdit1: TWebEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
