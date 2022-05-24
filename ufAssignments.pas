unit ufAssignments;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections;

type
  TFormAssignments = class(TWebForm)
     lbSBMLAssigments: TWebListBox;
   private

   public
     listOfAssignments: TStringList;
     strCaption: string;
  //   constructor create( );  TODO: move code from ufMain to here.
     procedure fillAssignmentList();
  end;

var
  FormAssignments: TFormAssignments;

implementation

{$R *.dfm}
 {constructor TFormAssignments.create();
 begin

 end;
 }
 procedure TFormAssignments.fillAssignmentList();
 var i: integer;
 begin
 self.Caption := self.strCaption;
  { for i := 0 to self.listOfAssignments.count -1 do
     begin
     if self.lbSBMLAssigments.Width < length(self.listOfAssignments[i])  then
       begin
       self.Width := length(self.listOfAssignments[i])*2 + 2;
       self.lbSBMLAssigments.Width := self.width;
       end;
     end; }
   self.lbSBMLAssigments.Items := listOfAssignments;
 end;

end.