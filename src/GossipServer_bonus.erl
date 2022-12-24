
%%%-------------------------------------------------------------------
%%% @author ayushkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2022 7:17 AM
%%%-------------------------------------------------------------------
-module('GossipServer_bonus').
-author("ayushkumar").

%% API
-export([start/5, start_all_actor_nodes/4, stop/0, init/6, supervisor_listener_loop/2, stop_all_actor_nodes/1, initialize_epidemic/2]).
-record(activeNodes, {activeActorNodes}).
-record(adjacencyList, {nodeToNbrListMap}).
-record(nodeToNodeNumMap, {nodeToNodeNumMap}).
-record(nodeToDoneMap, {nodeToDoneMap}).
-define(MaxPropagationsPerNode, 10).

start(NumNodes, Topology, Algorithm, ShouldTestFaultTol, NumNodesToKill) ->
  Message = "test",
  Pid = spawn(?MODULE, init, [NumNodes, Topology, Algorithm, Message, ShouldTestFaultTol, NumNodesToKill]),
  register(?MODULE, Pid).

stop() ->
  self() ! terminate.

init(NumNodes, Topology, Algorithm, Message, ShouldTestFaultTol, NumNodesToKill) ->
  ActiveNodes = #activeNodes{activeActorNodes = []},
  NodeToNodeNumMap = #nodeToNodeNumMap{nodeToNodeNumMap = maps:new()},
  NodeToDoneMap = #nodeToDoneMap{nodeToDoneMap = maps:new()},
  start_all_actor_nodes(NumNodes, Algorithm, ActiveNodes, NodeToNodeNumMap),
  #adjacencyList{nodeToNbrListMap = 'Util':get_adjacency_list_from_topology(ActiveNodes#activeNodes.activeActorNodes, Topology)},
  statistics(runtime),
  statistics(wall_clock),
  initialize_epidemic(Algorithm, Message),
  supervisor_listener_loop(Algorithm, NodeToDoneMap),
  if ShouldTestFaultTol == true ->
    killNActorsRandomly()
  end.

supervisor_listener_loop(Algo, #nodeToDoneMap{nodeToDoneMap = NodeToDoneMap}) ->
  receive
    {'EXIT', PidJustDied, _} ->
      error_logger:error_msg("actor node process died, respawning"),
      NumberOfNodeToBeSpawned = maps:get(PidJustDied, #nodeToNodeNumMap.nodeToNodeNumMap, 1),
      {ok, RespawnedPid} = 'GossipActor':start_link(Algo, NumberOfNodeToBeSpawned, ?MaxPropagationsPerNode, self()),
      #adjacencyList{nodeToNbrListMap = 'Util':replace_died_with_respawned_node(PidJustDied, RespawnedPid, #adjacencyList.nodeToNbrListMap)},
      #nodeToNodeNumMap{nodeToNodeNumMap = maps:put(RespawnedPid, NumberOfNodeToBeSpawned, #nodeToNodeNumMap.nodeToNodeNumMap)};
    {giveNbrList, AskerPid} ->
      AskerPid ! {yourNbrs, maps:get(AskerPid, #adjacencyList.nodeToNbrListMap, [])};
    {iamdone, FinishedPid} ->
      io:format("Actor completed their work"),
      #nodeToDoneMap{nodeToDoneMap = maps:put(FinishedPid, true, #nodeToDoneMap.nodeToDoneMap)},
      if map_size(#nodeToDoneMap.nodeToDoneMap) == map_size(#adjacencyList.nodeToNbrListMap) -> self() ! terminate
      end;
    terminate ->
      io:format("supervisor stopping all nodes now"),
      stop_all_actor_nodes(maps:values(#adjacencyList.nodeToNbrListMap)),
      io:format("supervisor stopping itself"),
      {_, Time1} = statistics(runtime),
      {_, Time2} = statistics(wall_clock),
      U1 = Time1 * 1000,
      U2 = Time2 * 1000,
      io:format("Code time=~p (~p) microseconds~n",
        [U1,U2]),
      exit(normal)
  end,
  supervisor_listener_loop(Algo, NodeToDoneMap).

start_all_actor_nodes(
    NumNodesToSpawn,
    Algo,
    #activeNodes{activeActorNodes = ActiveActorNodes},
    #nodeToNodeNumMap{nodeToNodeNumMap = NodeToNodeNumMap}) when NumNodesToSpawn > 0 ->
  process_flag(trap_exit, true),
  {ok, NodePid} = 'GossipActor':start_link(Algo, NumNodesToSpawn, ?MaxPropagationsPerNode, self()),
  UpdatedActiveNodes = lists:append(ActiveActorNodes, NodePid),
  UpdatedNodeToNodeNumMap = maps:put(NodePid, NumNodesToSpawn, NodeToNodeNumMap),
  start_all_actor_nodes(NumNodesToSpawn-1, Algo,  #activeNodes{activeActorNodes = UpdatedActiveNodes}, #nodeToNodeNumMap{nodeToNodeNumMap = UpdatedNodeToNodeNumMap}).

stop_all_actor_nodes([]) -> {};
stop_all_actor_nodes([CurrPid | Rest]) ->
  CurrPid ! terminate,
  stop_all_actor_nodes([Rest]).

initialize_epidemic(Algorithm, Message) ->
  ChooseOneOutOf = maps:values(#adjacencyList.nodeToNbrListMap),
  FirstPid = lists:nth(rand:uniform(length(ChooseOneOutOf)), ChooseOneOutOf),
  case Algorithm of
    "Gossip"  -> FirstPid ! {receivedGossip, Message};
    "PushSum" -> FirstPid ! {receivedPushSum, 1/2, 1/2};
    _ -> FirstPid ! {receivedGossip, Message}
  end.

killNActorsRandomly() ->
  ChooseOneOutOf = maps:values(#adjacencyList.nodeToNbrListMap),
  CurrPid = lists:nth(rand:uniform(length(ChooseOneOutOf)), ChooseOneOutOf),
  CurrPid ! terminate.

