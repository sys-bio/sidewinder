unit uLayout;

interface

Uses Classes, Graphics, uNetwork, Dialogs;

type
  TCallBack = procedure of object;

  procedure fruchterman_reingold(network : TNetwork; width, height : integer; iterations : integer; callBack : TCallBack);


//To call it:

// fruchterman_reingold(network, paintbox.width, paintbox.Height, 600, nil);


implementation

Uses sysutils, math, ufMain;

const
  enumber = 2.7182818284590452353;

var N : double = 1000;
    k : double = 5;
    P : double = 0.05;


// attractive force
function f_attractive (d, k : double) : double;
begin
    result := d*d/k;
end;

// repulsive force
function f_repulsive (d, k : double) : double;
begin
    result := k*k/d;
end;


// Translated from Annastasia Deckard's C# code of autolayout in SBW

procedure fruchterman_reingold(network : TNetwork; width, height : integer; iterations : integer; callBack : TCallBack);
var W, L, area : double;
    k, dt, t, x, y : double;
    dx, dy, ddx, ddy, delta, disp, d: double;
    v, u, m, i, n : integer;
    tempinit, alpha : double;
    finished : boolean;
    iter : integer;
    maxIter : integer;
begin
  finished := False;
  iter := 0;
  maxIter := 60; //600;
  W := width;
  L := height;
  area := W*L;
  //k := 120;
  k := sqrt( area/( 5 *length(network.nodes) ) ); // python code: https://gist.github.com/mmisono/8972731
  iterations := trunc ((130 * ln(length (network.nodes) + 2)));

  dt := 1/(iterations + 1);
  tempinit := 1000 * ln(length (network.reactions) + 2);
  alpha := ln(tempinit) - ln(0.25);

    while not finished do
         begin
         t := tempinit * Math.Power(enumber, -alpha * time);
         if Assigned (callBack) then
            callBack;
         //sleep (10);

         // calculate repulsive forces
         for v := 0 to length(network.nodes) - 1 do
             begin
             if network.hasReactions (network.nodes[v]) then
                begin
                network.nodes[v].dx := 0;
                network.nodes[v].dy := 0;
                for u := 0 to length(network.nodes) - 1 do
                    begin
                    if network.nodes[v] <> network.nodes[u] then
                       begin
                       dx := network.nodes[v].state.getCenter().x - network.nodes[u].state.getCenter().x;
                       dy := network.nodes[v].state.getCenter().y - network.nodes[u].state.getCenter().y;
                       delta := sqrt(dx*dx+dy*dy);
                       if delta <> 0 then
                          begin
                          d := f_repulsive (delta, k)/delta;
                          network.nodes[v].dx := network.nodes[v].dx + dx*d;
                          network.nodes[v].dy := network.nodes[v].dy + dy*d;
                          //showmessage (floattostr (network.nodes[v].dx));
                          end;
                       end;
                    end;
                end;
             end;

          // calculate attractive forces
         for m := 0 to length (network.reactions) - 1 do
             begin
             for i := 0 to network.reactions[m].state.nReactants -1 do
               begin
               for n := 0 to network.reactions[m].state.nProducts -1 do
                 begin
                 dx := network.reactions[m].state.srcPtr[i].state.x - network.reactions[m].state.destPtr[n].state.x;
                 dy := network.reactions[m].state.srcPtr[i].state.y - network.reactions[m].state.destPtr[n].state.y;
                 delta := sqrt(dx*dx+dy*dy);
               //showmessage ('dx = ' + floattostr (dx) + ' dy = ' + floattostr (dy) + ' delta = ' + floattostr (delta));
                 if delta <> 0 then
                   begin
                   d := f_attractive (delta,k)/delta;
                  //showmessage ('d = ' + floattostr (d));
                   ddx := dx*d;
                   ddy := dy*d;
                   network.reactions[m].state.srcPtr[i].dx := network.reactions[m].state.srcPtr[i].dx + (-ddx);
                   network.reactions[m].state.destPtr[n].dx := network.reactions[m].state.destPtr[n].dx + (+ddx);

                   network.reactions[m].state.srcPtr[i].dy := network.reactions[m].state.srcPtr[i].dy + (-ddy);
                   network.reactions[m].state.destPtr[n].dy := network.reactions[m].state.destPtr[n].dy + (+ddy);
                   //showmessage (floattostr (network.reactions[m].state.srcPtr[0].dx));
                   end;
                 end;
               end;

             {
             dx := network.reactions[m].state.srcPtr[0].state.x - network.reactions[m].state.destPtr[0].state.x;
             dy := network.reactions[m].state.srcPtr[0].state.y - network.reactions[m].state.destPtr[0].state.y;
             delta := sqrt(dx*dx+dy*dy);
             //showmessage ('dx = ' + floattostr (dx) + ' dy = ' + floattostr (dy) + ' delta = ' + floattostr (delta));
             if delta <> 0 then
                begin
                d := f_attractive (delta,k)/delta;
                //showmessage ('d = ' + floattostr (d));
                ddx := dx*d;
                ddy := dy*d;
                network.reactions[m].state.srcPtr[0].dx := network.reactions[m].state.srcPtr[0].dx + (-ddx);
                network.reactions[m].state.destPtr[0].dx := network.reactions[m].state.destPtr[0].dx + (+ddx);

                network.reactions[m].state.srcPtr[0].dy := network.reactions[m].state.srcPtr[0].dy + (-ddy);
                network.reactions[m].state.destPtr[0].dy := network.reactions[m].state.destPtr[0].dy + (+ddy);
                //showmessage (floattostr (network.reactions[m].state.srcPtr[0].dx));
                end;
                }
            end;


         //showmessage ('t = ' + floattostr (t));
				 // Adjust Coordinates
         for v := 0 to length(network.nodes) - 1 do
				     begin
             if network.hasReactions (network.nodes[v]) then
                begin
                //if not network.nodes[v].locked then
                   begin
                   dx := network.nodes[v].dx;
                   dy := network.nodes[v].dy;
                   disp := sqrt(dx*dx+dy*dy);
                  // showmessage ('d 2 = ' + floattostr (disp));
                   if (disp <> 0) then
                     begin
                     network.nodes[v].state.x := 0 + (network.nodes[v].state.x + ((dx/disp) * t)); // divide by d is okay
                     network.nodes[v].state.y := 0 + trunc (network.nodes[v].state.y + ((dy/disp) * t));

                     end;
                   end;
                end;
				     end;

         inc (iter);
         if ((abs(dx) < 0.4) and (abs (dy) < 0.4)) or (iter > maxIter) then
            finished := True;

         // cooling
         t := t - dt;
         end;
    dx := 0;
         //showmessage ('x = ' + floattostr (network.nodes[v].state.x));
    // Hack to keep nodes in canvas until investigated in more detail:
    for v := 0 to length(network.nodes) -1 do
      begin
      if network.nodes[v].state.y <0 then network.nodes[v].state.y := v*2;
      end;
end;


end.

