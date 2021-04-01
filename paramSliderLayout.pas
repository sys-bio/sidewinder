unit paramSliderLayout;

interface
uses System.SysUtils, System.Classes, JS, Web, Vcl.Controls,WEBLib.StdCtrls, WEBLib.ExtCtrls,WEBLib.Dialogs ;

procedure configPSliderPanel(sliderNumb: integer; sPLeft, sliderPanelWidth, sliderPanelHeight: integer;
  newSPanelAr: array of TWebPanel);

procedure configPSliderTBar(sliderNumb, sliderWidth: integer;  newSBarAr: array of TWebTrackBar;
        newSHLabelAr, newSLLabelAr, newSTBLabelAr: array of TWebLabel);

implementation
procedure configPSliderPanel(sliderNumb: integer; sPLeft, sliderPanelWidth, sliderPanelHeight: integer;
  newSPanelAr: array of TWebPanel);
  begin
   newSPanelAr[sliderNumb].visible:= true;
   newSPanelAr[sliderNumb].Top:= sliderPanelHeight*sliderNumb + 3;
   newSPanelAr[sliderNumb].Left:= sPLeft;
   newSPanelAr[sliderNumb].Height:= sliderPanelHeight;
   newSPanelAr[sliderNumb].Width:= sliderPanelWidth ;
  end;


procedure configPSliderTBar(sliderNumb, sliderWidth: integer;  newSBarAr: array of TWebTrackBar;
        newSHLabelAr, newSLLabelAr, newSTBLabelAr: array of TWebLabel);
  begin
    newSBarAr[sliderNumb].visible:= true;
    newSBarAr[sliderNumb].Tag:= sliderNumb;  // keep track of slider index number.
    newSBarAr[sliderNumb].Left:= 25;
    newSBarAr[sliderNumb].Top:= 27;
    newSBarAr[sliderNumb].Width:= 94;
    newSBarAr[sliderNumb].Height:= 20;
    newSHLabelAr[sliderNumb].visible:= true;
    newSHLabelAr[sliderNumb].Tag:= sliderNumb;
    newSHLabelAr[sliderNumb].Top:= 30;
    newSHLabelAr[sliderNumb].Left:= 125;
    newSLLabelAr[sliderNumb].visible:= true;
    newSLLabelAr[sliderNumb].Tag:= sliderNumb;
    newSLLabelAr[sliderNumb].Top:= 30;
    newSLLabelAr[sliderNumb].Left:= 8;
    newSTBLabelAr[sliderNumb].visible:= true;
    newSTBLabelAr[sliderNumb].Tag:= sliderNumb;
    newSTBLabelAr[sliderNumb].Left:= 48;
    newSTBLabelAr[sliderNumb].Top:= 3;
  end;

end.
