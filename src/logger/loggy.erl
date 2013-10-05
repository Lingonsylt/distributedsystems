-module(loggy).
-export([start/1, stop/1]).
start(Nodes) ->
  register(error_checker, spawn(fun() -> run_error_check() end)),
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
advance_time(CurrentTime, [{_, {MaxTime, _}}|NodeDatas]) ->
  if MaxTime >= CurrentTime ->
    advance_time(CurrentTime, NodeDatas);
  true ->
    CurrentTime
  end.

log_outstanding(_, [], NodeDatasAccum) ->
  NodeDatasAccum;
log_outstanding(CurrentTime, [{Node, {MaxTime, Messages}}|NodeDatas], NodeDatasAccum) ->
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
  NodeDatas2 = log_outstanding(CurrentTime2, NodeDatas, []),
  receive
    {log, From, Time, Message} ->
      NodeDatas3 = add_message(From, Time, Message, NodeDatas2),
      loop(CurrentTime2, NodeDatas3);
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [From, Time, Msg]),
  error_checker ! {From, Time, Msg}.

run_error_check() ->
  run_error_check([]).

run_error_check(Data) ->
  receive
    {From, Time, {received, {Msg,Id}}} ->
      case error_check(Id, Data) of
        {error, true} ->
          io:format("ERROR: ~w ~w ~p received before it was sent!~n", [From, Time, {received, {Msg,Id}}]);
        {error, false} ->
          true
      end,
      run_error_check(Data);
    {_, _, {sending, {_,Id}}} ->
      run_error_check([Id|Data])
  end.

error_check(_, []) ->
  {error, true};
error_check(ReceivedId, [SentId|Data]) ->
  case ReceivedId == SentId of
    true ->
      {error, false};
    false ->
      error_check(ReceivedId, Data)
  end.

get_max(Node, NodeData) ->
  case lists:keyfind(Node, 1, NodeData) of
    false ->
      0;
    {N, _} ->
      N
  end.

add_message(Node, Time, Message, NodeDatas) ->
  case lists:keyfind(Node, 1, NodeDatas) of
    false ->
      error;
    {_, {OldMaxTime, OldNodeData}} ->
      NodeData2 = lists:keydelete(Node, 1, NodeDatas),
      case Time > get_max(Node, NodeData2) of
        true ->
          [{Node, {Time, OldNodeData ++ [{Time, Message}]}}|NodeData2];
        false ->
          [{Node, {OldMaxTime, OldNodeData ++ [{Time, Message}]}}|NodeData2]
      end
  end.