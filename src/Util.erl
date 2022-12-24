%%%-------------------------------------------------------------------
%%% @author ayushkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 3:29 AM
%%%-------------------------------------------------------------------
-module('Util').
-author("ayushkumar").

%% API
-export([get_random_pid_from_nbrs/1, get_adjacency_list_from_topology/2, replace_died_with_respawned_node/3]).


get_random_pid_from_nbrs(Neighbors) ->
  lists:nth(rand:uniform(length(Neighbors)), Neighbors).

get_adjacency_list_from_topology(ActiveNodes, Topology) -> maps:new().

replace_died_with_respawned_node(DiedPid, RespawnedPid, Map) ->
  NeighborNodes = maps:get(DiedPid, Map, []),
  if length(NeighborNodes) == 0 -> Map;
    true ->
      RemovedMap = maps:remove(DiedPid, Map),
      UpdatedMap = maps:put(RespawnedPid, NeighborNodes, RemovedMap),
      UpdatedMap
  end.


%%Map = #{"counter" => 1},
%%Fun = fun(V) -> V + 1 end,
%%maps:update_with("counter",Fun,Map).
%%#{"counter" => 2}