unit paramSliderLayout;

interface
uses System.SysUtils, System.Classes, JS, Web, Vcl.Controls,WEBLib.StdCtrls, WEBLib.ExtCtrls,WEBLib.Dialogs ;

procedure configPSliderPanel(sliderNumber, sPLeft, sliderPanelWidth, sliderPanelHeight: integer;
  newSPanelAr: array of TWebPanel);

procedure configPSliderTBar(sliderNumber, sliderPanelWidth : integer;  newSBarAr: array of TWebTrackBar;
        newSHLabelAr, newSLLabelAr, newSTBLabelAr: array of TWebLabel);


implementation

procedure configPSliderPanel(sliderNumber, sPLeft, sliderPanelWidth, sliderPanelHeight: integer;
  newSPanelAr: array of TWebPanel);
  begin
   newSPanelAr[sliderNumber].visible := true;
   newSPanelAr[sliderNumber].Top := sliderPanelHeight*sliderNumber + 3;
   newSPanelAr[sliderNumber].Left := sPLeft;
   newSPanelAr[sliderNumber].Height := sliderPanelHeight;
   newSPanelAr[sliderNumber].Width := sliderPanelWidth - 6; // -6 to move it in from the extreme right edge
  end;


// Define the sliders inside the panel that holds the sliders
procedure configPSliderTBar(sliderNumber, sliderPanelWidth : integer;  newSBarAr: array of TWebTrackBar;
        newSHLabelAr, newSLLabelAr, newSTBLabelAr: array of TWebLabel);
var sliderTBarWidth : integer;
  begin
    // Width of the slider inside the panel
    sliderTBarWidth:= trunc (0.8*sliderPanelWidth); // 80% of the panel's width

    // This defines the location of the slider itself (not the position of the panel)
    newSBarAr[sliderNumber].visible:= True;
    newSBarAr[sliderNumber].Tag:= sliderNumber;  // keep track of slider index number.
    newSBarAr[sliderNumber].Left:= 20;
    newSBarAr[sliderNumber].Top:= 27;
    newSBarAr[sliderNumber].Width:= sliderTBarWidth;
    newSBarAr[sliderNumber].Height:= 20;

    // Value positioned on the right-side of slider
    newSHLabelAr[sliderNumber].visible:= True;
    newSHLabelAr[sliderNumber].Tag:= sliderNumber;
    newSHLabelAr[sliderNumber].Top:= 30;
    newSHLabelAr[sliderNumber].Left:= sliderPanelWidth - trunc (0.1*sliderPanelWidth);

    // Value positioned on the left-side of slider
    newSLLabelAr[sliderNumber].visible:= True;
    newSLLabelAr[sliderNumber].Tag:= sliderNumber;
    newSLLabelAr[sliderNumber].Top:= 30;
    newSLLabelAr[sliderNumber].Left:= 4;

    // parameter label and current value
    newSTBLabelAr[sliderNumber].visible:= True;
    newSTBLabelAr[sliderNumber].Tag:= sliderNumber;
    newSTBLabelAr[sliderNumber].Left:= 48;
    newSTBLabelAr[sliderNumber].Top:= 3;
  end;

end.
