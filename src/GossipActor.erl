%%%-------------------------------------------------------------------
%%% @author ayushkumar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 4:33 PM
%%%-------------------------------------------------------------------
-module('GossipActor').
-author("ayushkumar").

%% API
-export([start_link/4, stop/0, init/4, gossip_main_loop/4, start_self_gossip_propagator_loop/2,
  propagate_gossip_message_to_random/2, pushsum_main_loop/3, pushsum_propagator/2, propagate_pushsum_to_random/3,
  inform_supervisor_about_updation/1, inform_supervisor_about_completion/1, ask_supervisor_nbrs/1]).


-record(pushSumState, {sum, weight}).
-record(gossipState, {message}).
-record(numPropogationsDone, {num}).
-record(timesTerminationConditionHit, {times}).

start_link(Algo, NodeNum, MaxPropagationsPerNode, SupervisorPid) ->
  Pid = spawn_link(?MODULE, init, [Algo, NodeNum, MaxPropagationsPerNode, SupervisorPid]),
  register(?MODULE, Pid),
  {ok, Pid}.

stop() ->
  ?MODULE ! terminate.

init(Algo, NodeNum, MaxPropagationsPerNode, SupervisorPid) ->
  PropsState = #numPropogationsDone{num = MaxPropagationsPerNode},
  TimesTerminationConditionHit = #timesTerminationConditionHit{times = 0},
  case Algo of
    "Gossip" -> State = #gossipState{message = null}, gossip_main_loop(State, TimesTerminationConditionHit, SupervisorPid, false);
    "PushSum" -> State = #pushSumState{sum = NodeNum, weight = 1}, pushsum_main_loop(State, TimesTerminationConditionHit, SupervisorPid);
    _-> State = #gossipState{message = null}, gossip_main_loop(State, TimesTerminationConditionHit, SupervisorPid, false)
  end.
%=======================================================GOSSIP========================================================
gossip_main_loop(
    #gossipState{message = Message} = GossipState,
    #timesTerminationConditionHit{times = Times} = TimesTerminationConditionHit,
    SupervisorPid,
    IsSenderLoopActive) ->
  receive
    {receivedGossip, Message} ->
      inform_supervisor_about_updation(SupervisorPid),
      #timesTerminationConditionHit{times = Times + 1},
      if #timesTerminationConditionHit.times >= 10 -> inform_supervisor_about_completion(SupervisorPid)
      end,
      if IsSenderLoopActive == false ->
        IsSenderLoopActive = true,
        start_self_gossip_propagator_loop(#gossipState, SupervisorPid)
      end;
    terminate ->
      io:format("gossip actor stopped"), exit(normal)
  end,
  gossip_main_loop(#gossipState, #timesTerminationConditionHit, SupervisorPid, IsSenderLoopActive).

start_self_gossip_propagator_loop(
    #gossipState{message = Message} = GossipState,
    SupervisorPid) ->
  receive {yourNbrs, Message, Neighbors} ->
    propagate_gossip_message_to_random(Message, Neighbors)
  end,
  start_self_gossip_propagator_loop(GossipState,  SupervisorPid).

propagate_gossip_message_to_random(Message, Nbrs) ->
  RandomNbr = 'Util':get_random_pid_from_nbrs(Nbrs),
  RandomNbr ! {receivedGossip, Message}.

%===========================================================PUSH_SUM=====================================================

pushsum_main_loop(
    #pushSumState{sum = Sum, weight = Weight} = PushSumState,
    #timesTerminationConditionHit{times = Times} = TimesTerminationConditionHit,
    SupervisorPid) ->
  receive
    {receivedPushSum, SumToBeAdded, WeightToBeAdded} ->
      inform_supervisor_about_updation(SupervisorPid),
      NewSum = Sum + SumToBeAdded, NewWeight = Weight + WeightToBeAdded,
      OldSumEstimate = Sum/Weight, NewSumEstimate = NewSum/NewWeight,
      #pushSumState{sum = NewSum, weight = NewWeight},
      if
        abs(OldSumEstimate - NewSumEstimate) < (1/10000000000) -> #timesTerminationConditionHit{times = Times + 1};
        true -> #timesTerminationConditionHit{times = 0}
      end,
      if #timesTerminationConditionHit.times >= 3 -> inform_supervisor_about_completion(SupervisorPid)
      end,
      pushsum_propagator(#pushSumState, SupervisorPid);
    terminate ->
      io:format("pushsum actor stopped"), exit(normal)
  end,
  pushsum_main_loop(#pushSumState, #timesTerminationConditionHit, SupervisorPid).

pushsum_propagator(
    #pushSumState{sum = Sum, weight = Weight},
    SupervisorPid) ->
    ask_supervisor_nbrs(SupervisorPid),
    receive {yourNbrs, Neighbors} ->
      propagate_pushsum_to_random(Sum/2, Weight/2, Neighbors)
  end.

propagate_pushsum_to_random(Sum, Weight, Nbrs) ->
  RandomNbr = 'Util':get_random_pid_from_nbrs(Nbrs),
  RandomNbr ! {receivedPushSum, Sum, Weight}.

%=======================================================================================================================

inform_supervisor_about_updation(SupervisorPid) ->
  SupervisorPid ! {nodeGotTheUpdate, self()}.

inform_supervisor_about_completion(SupervisorPid) ->
  SupervisorPid ! {iamdone, self()}.

ask_supervisor_nbrs(SupervisorPid) ->
  SupervisorPid ! {giveNbrList, self()}.


%%propagate_message_to_all(Message, []) -> {}.
%%propagate_message_to_all(Message, [CurrNbr|Rest]) ->
%%  CurrNbr ! {receivedGossip, Message},
%%  propagate_message_to_all(Message, Rest).