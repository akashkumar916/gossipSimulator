%%%-------------------------------------------------------------------
%%% @author akashkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 8:50 PM
%%%-------------------------------------------------------------------
-module('GetTopology').
-author("akashkumar").

%% API
-export([st/3,init/3,selectTopology/2,selectAlgorithm/3,createFull/1,createLine/1,create2D/1,createImperfect3D/1]).
-export([updateIn2D_3D/6,updateAdjInLine/3,updateAdjIn2D_3D/5,updateInLine/3,updateInLine/3,make1DGrid/1,make2DGrid/3,updateInFull/3,find_X/2,find_XY/3]).

-records(actorDetails,{
%for every actors these details is associated - { PID, S, WEIGHT, X-AXIS,Y-AXIS, PING COUNT,NEIGHBOURS LIST}
    pid, s, w, x,y, pscount, adjacenylist
}).

-record(actorDetails, {nth}).
-record(allActor,{actorDetails}).


st(ListOfPids,Topology,Algorithm) ->
  Pid = spawn(?MODULE, init, [ListOfPids,Topology,Algorithm]),
  register(?MODULE, Pid).


init(ListOfPids,Topology,Algorithm) ->
  NumberOfActor=length(ListOfPids),
  selectTopology(ListOfPids,Topology),


selectTopology(ListOfPids, Topology)->
  case Topology of
    "full" ->createFull(ListOfPids);
    "2D" ->create2D(ListOfPids);
    "line" ->createLine(ListOfPids);
    "imperfect3d" ->createImperfect3D(ListOfPids)
  end.



% methods below
%finding the coordinates in the adjacency List
find_XY([T], X,Y) ->
  if (X == #actorDetails.x and Y == #actorDetails.y) -> {actorDetails} end,
  find_XY([_|T],X,Y).

find_X([T], X) ->
  if (X==#actorDetails.x) -> {T} end,
  find_X([_|T],X).


%Updating the adjacency per topology
updateAdjIn2D_3D(Actorgraph, X, Y, X_, Y_)->
  T=find_XY(Actorgraph,X,Y),
  T1=find_XY(Actorgraph,X_,Y_),
  List=#T.adjacenylist,
  List2=append([T1|List]),
  Actornode=#T{Adjacenylist=List2}.


updateAdjInLine(Actorgraph,Pos, Node)->
  T=find_X(Actorgraph,Pos),
  T1=find_X(Actorgraph,Node),
  List = #T1.adjacenylist,
  List2=append([T1|List]),
  Actornode=#T{Adjacenylist=List2}.



updateInFull(Actorarray,S,N) when S<N ->
  Start = 0.
  for(Start,N,S,Actorarray) when Start < N ->
    if Start /= S -> update1D(Actorarray,Start,S)
    end,
    for(Start+1,N,S,Actorarray),

  for(Start,N,S,Actorarray),
  updateFull(Actorarray,S+1,N).



updateInLine(Actorgraph,Start,Size) ->
  if
    Start -1 > 0->
      update1D(Start-1,Start);
    Start+1 < Size ->
      update1D(Start+1,Start)
  end,
  if
    Start < Size ->
      line(Actorgraph,Start+1,Size);
    true -> []
  end.


updateIn2D_3D(Actorgraph,StartRow, StartColumn, Row, Col, Diagonal) ->
  if
    StartRow < Row and StartColumn < Col ->
      if StartRow-1 > 0 ->
        updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow-1, StartColumn) end,
      if (StartRow + 1) < Row ->
        updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow+1, StartColumn) end,
      if (StartColumn-1) > 0 ->
        updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow, StartColumn-1)end,
      if (StartColumn-1) <  Col ->
        updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow, StartColumn+1) end,
      if
        Diagonal == true ->
          if (StartRow-1) > 0 and (StartColumn-1) > 0 ->
            updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow-1, StartColumn-1) end,
          if (StartRow+1) < Row and (StartColumn+1) < Col ->
            updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow+1, StartColumn+1) end,
          if (StartColumn+1) < Col and (StartRow-1) >0  ->
            updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow-1, StartColumn+1) end,
          if (StartColumn-1) >0 and (StartRow+1) < Row ->
            updateAdjIn2D_3D(Actorgraph,StartRow, StartColumn, StartRow+1, StartColumn-1) end,
          %In case of Imperfect 3D (all eight neighbours )- 2D + 1 random node
          X = rand:uniform(Row),
          Y = rand:uniform(Col),
          updateAdjIn2D_3D(Actorgraph, StartRow, StartColumn, X,Y)
      end
  end,
  if
    StartColumn < Col ->
      loop( Actorgraph, StartRow, StartColumn+1, Row, Col,Diagonal);

    StartRow < Row->
      loop( Actorgraph, StartRow+1, StartColumn, Row, Col, Diagonal)

  end.


%Topology
createFull(ListOfPids) ->
  Actors=make1DGrid(ListOfPids),
  updateFull(Actors,0,length(Actors)).


createLine(ListOfPids)->
  Actors =make1DGrid(ListOfPids),
  updateline(Actors,0,length(Actors)).


create2D(ListOfPids)->
  NumberOfActor= length(ListOfPids),
  Rows = 2,
  Columns = NumberOfActor/2,
  SqrtOfActor = sqrt(NumberOfActor),
  if SqrtOfActor * SqrtOfActor == NumberOfActor->
    Rows = SqrtOfActor,
    Columns = SqrtOfActor
  end,
  Actorarray = make2DGrid(ListOfPids,Rows,Columns),
  %diagonal sending as false
  updateIn2D_3D(Actorarray,0,0,Rows,Columns,false).


createImperfect3D(ListOfPids)->
  NumberOfActor= length(ListOfPids),
  Rows = 2,
  Columns = NumberOfActor/2,
  SqrtOfActor = sqrt(NumberOfActor),
  if SqrtOfActor * SqrtOfActor == NumberOfActor->
    Rows = SqrtOfActor,
    Columns = SqrtOfActor
  end,
  Actorarray = make2DGrid(ListOfPids,Rows,Columns),
  %diagonally sending as true plus one random node
  updateIn2D_3D(Actorarray,0,0,Rows,Columns,true).


make2DGrid(ListOfPid,Rows, Cols)->
  S=0,
  C=0,
  I=0,
  Allactor=[],
  for(S,C,Rows, Cols) when S< Rows ->
    for(S,C,Rows, Cols) when C < Cols ->
    %intialization state of an actor
      {Actordetails=#actorDetails{[nth(I,ListOfPid)],0,0,1,S,C,[]}},
      Allactor=append(Actordetails,[AllActor]),
      I=I+1,
      for(S,C+1,Rows, Cols),
      for(S+1,C,Rows, Cols),
      %for(S,C,Rows,Cols),
  Allactor = #allActor{Allactor},
  {Allactor}.


make1DGrid(ListOfPid)->
  AllActor=length(ListOfPid),
  S=0,
  I=0,
  Allactor=[].
  for(S,AllActor,I,ListOfPid) when S < AllActor ->
    Pid=nth(I,ListOfPid),
    %intialization state of an actor
    {Actordetails=#actorDetails{Pid,0,0,1,S,0,[]}},
    AllactorList=append(Actordetails,[AllActor]),
    I=I+1,
    for(S+1,AllActor,I,ListOfPid),
    %for(0,0,Rows,Cols),
  Allactor = #allActor.actorDetails{AllActorList}.