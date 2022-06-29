unit ufAssignments;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  System.Generics.Collections, uNetwork, uSBMLClasses.rule;

type
  TFormAssignments = class(TWebForm)
     lbSBMLAssignments: TWebListBox;
   private

   public
     listOfAssignments: TStringList;
     strCaption: string;
  //   procedure buildList(newRxn: TReactionState; assignmentList: TList<TSBMLRule> );  //TODO: move code from ufMain to here.
     procedure fillAssignmentList();
  end;

var
  FormAssignments: TFormAssignments;

implementation

{$R *.dfm}

 {procedure TFormAssignments.buildList(newRxn: TReactionState; assignmentList: TList<TSBMLRule> );
 var i, j, rxnIndex: integer;
      strListOfAssign: TStringList;
      curRule: TSBMLRule;
      curStr: string;
      foundVar: string;
 begin
  rxnIndex := 0;
  strListOfAssign := TStringList.create;
    for i := 0 to assignmentList.Count -1 do
        begin
          curRule := nil;
          curStr := '';
          foundVar := '';
          curRule := assignmentList[i];
          for j := 0 to newRxn.nReactants -1 do
            begin
            if curRule.getVariable = newRxn.srcId[j] then
              begin
              foundVar := newRxn.srcId[j]
              end;
            end;

          if foundVar = '' then
            begin
            for j := 0 to newRxn.nProducts -1 do
              begin
              if curRule.getVariable = newRxn.destId[j] then
                begin
                foundVar := newRxn.destId[j]
                end;
              end;
            end;

          if foundVar = '' then
            begin
            for j := 0 to newRxn.rateParams.Count -1 do
              begin
              if curRule.getVariable = newRxn.rateParams[j].getId then
                begin
                foundVar := newRxn.rateParams[j].getId;
                end;
              end;
            end;

          if (foundVar <> '') and (curRule.isAssignment) then
            begin
            curStr := foundVar + ' = ' + curRule.getFormula;
            strListOfAssign.Add(curStr);
            end;

        end;

    if strListOfAssign.Count <1 then strListOfAssign.Add('No variable assignments found.');
   //self.listOfAssignments := strListOfAssign;
    self.lbSBMLAssignments.Items := strListOfAssign;
   // self.fillAssignmentList();
 end;
    }
 procedure TFormAssignments.fillAssignmentList();
 var i, newHeight: integer;
 begin
   newHeight := self.listOfAssignments.Count *30 + 20;
   self.Height := newHeight + 50;
   self.lbSBMLAssignments.Height := newHeight;
   self.lbSBMLAssignments.Items := self.listOfAssignments;
 end;

end.