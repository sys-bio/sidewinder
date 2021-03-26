unit uCreateNetworks;

interface

Uses SysUtils, uNetwork, Dialogs;

procedure createRandomNetwork (network : TNetwork; nNodes : integer; probability : double);

implementation

Uses uMath;

procedure createRandomNetwork (network : TNetwork; nNodes : integer; probability : double);
var i, j : integer;
    alength : integer;
begin
  //setSeed (1237);
  for i := 0 to nNodes - 1 do
      network.addNode ('node' + inttostr (i), myRandomInt (500), myRandomInt (660));

  for i := 0 to nNodes - 1 do
      for j := 0 to nNodes - 1 do
          if myRandom() > probability then
             begin
             if i <> j then
                network.addUniUniReaction ('J' + inttostr (i) + inttostr (j), network.nodes[i], network.nodes[j]);
             end;

  for i := nNodes - 1 downto 0 do
      if not network.hasReactions(network.nodes[i]) then
         begin
         alength := Length(network.nodes);
         for j := i + 1 to alength - 1 do
             network.nodes[j - 1] := network.nodes[j];
         setLength (network.nodes, alength - 1);
         end;
end;


end.
