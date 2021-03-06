-module(gms2).
-author("lingon").
-define(timeout, 500).
-define(arghh, 100).

%% API
-export([start/1, start/2, run/0]).

run() ->
  %{ok, LeaderPid} = start(leader),
  %{ok, Slave1Pid} = start(slave1, LeaderPid),
  timer:sleep(1000).

start(Id) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
  leader(Id, Master, [], [Master]).

start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      erlang:monitor(process, Leader),
      io:format("gms ~w: joined group~n", [Id]),
      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, Msg}, Slaves),
      %io:format("gms ~w: received {mcast, ~w}~n", [Id, Msg]),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("gms ~w: forward join from ~w to master~n", [Id, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
    stop ->
      ok;
    Error ->
      io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
  end.

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      %io:format("gms ~w: received {mcast, ~w}~n", [Id, Msg]),
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("gms ~w: forward join from ~w to leader~n", [Id, Peer]),
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->
      Master ! Msg,
      %io:format("gms ~w: deliver msg ~w~n", [Id, Msg]),
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader|Slaves2], Group2} ->
      io:format("gms ~w: received view ~w~n", [Id, Group2]),
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      io:format("gms ~w: leader detected dead! starting election!~n", [Id]),
      election(Id, Master, Slaves, Group);
    stop ->
      ok;
    Error ->
      io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
  end.

election(Id, Master, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      io:format("gms ~w: elected as leader!~n", [Id]),
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, Rest, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Rest, Group)
  end.

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).
