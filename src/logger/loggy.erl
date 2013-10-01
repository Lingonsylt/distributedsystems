-module(loggy).
-export([start/1, stop/1]).
start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).
stop(Logger) ->
  Logger ! stop.
init(Nodes) ->
  loop(1, node_datas(Nodes, [])).

node_datas([], NodeDatas) ->
  NodeDatas;
node_datas([Node|Nodes], NodeDatas) ->
  node_datas(Nodes, [{Node, {0, []}}|NodeDatas]).

advance_time(CurrentTime, []) ->
  CurrentTime + 1;
advance_time(CurrentTime, [{Node, {MaxTime, NodeData}}|NodeDatas]) ->
  if MaxTime >= CurrentTime ->
    advance_time(CurrentTime, NodeDatas);
  true ->
    CurrentTime
  end.

log_outstanding(_, [], NodeDatasAccum) ->
  NodeDatasAccum;
log_outstanding(CurrentTime, [{Node, {MaxTime, Messages}}|NodeDatas], NodeDatasAccum) ->
  % io:format("log_outstanding->NodeDatasAccum: ~p~n", [NodeDatasAccum]),
  log_outstanding(CurrentTime, NodeDatas, [{Node, {MaxTime, log_outstanding_messages(Node, CurrentTime, Messages, [])}}|NodeDatasAccum]).

log_outstanding_messages(_, _, [], MessagesAccum) ->
  MessagesAccum;
log_outstanding_messages(Node, CurrentTime, [{Time, Message}|Messages], MessagesAccum) ->
  case Time =< CurrentTime of
    true ->
      log(Node, Time, Message),
      log_outstanding_messages(Node, CurrentTime, Messages, MessagesAccum);
    false ->
      log_outstanding_messages(Node, CurrentTime, Messages, MessagesAccum ++ [{Time, Message}])
  end.

loop(CurrentTime, NodeDatas) ->
  CurrentTime2 = advance_time(CurrentTime, NodeDatas),
  % io:format("advance_time(~p, ~p)~n", [CurrentTime2, NodeDatas]),
  NodeDatas2 = log_outstanding(CurrentTime2, NodeDatas, []),
  % io:format("log_outstanding(~p, ~p)~n", [CurrentTime2, NodeDatas2]),
  receive
    {log, From, Time, Message} ->
      NodeDatas3 = add_message(From, Time, Message, NodeDatas2),
      % io:format("add_message(~p)~n", [NodeDatas3]),
      % log(From, Time, Msg),
      loop(CurrentTime2, NodeDatas3);
    stop ->
      ok                                                             2
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [From, Time, Msg]).

get_max(Node, NodeData) ->
  case lists:keyfind(Node, 1, NodeData) of
    false ->
      0;
    {N, _} ->
      N
  end.

get_node(Node, NodeData) ->
  case lists:keyfind(Node, 1, NodeData) of
    false ->
      0;
    {Time, _} ->
      Time
  end.

add_message(Node, Time, Message, NodeDatas) ->
  case lists:keyfind(Node, 1, NodeDatas) of
    false ->
      error;
    {_, {OldMaxTime, OldNodeData}} ->
      % io:format("{OldMaxTime: ~p, OldNodeData: ~p}~n", [OldMaxTime, OldNodeData]),
      NodeData2 = lists:keydelete(Node, 1, NodeDatas),
      case Time > get_max(Node, NodeData2) of
        true ->
          [{Node, {Time, OldNodeData ++ [{Time, Message}]}}|NodeData2];
        false ->
          [{Node, {OldMaxTime, OldNodeData ++ [{Time, Message}]}}|NodeData2]
      end
  end.