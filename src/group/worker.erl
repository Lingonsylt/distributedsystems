-module(worker).
-export([start/5, start/2, run/0, run/1]).
-define(change, 20).
-define(color, {0,0,0}).

run()->
  run(2).

run(N) ->
  start(1, gms3, 1, 500, {callhome, self()}),
  receive
    {hello, Leader} ->
      start_client(N-1, Leader),
      Leader
  end.

start_client(N, Leader) ->
  case N of
    0 ->
      ok;
    _ ->
      start(N+1, gms3, 1, Leader, 500),
      N2 = N - 1,
      start_client(N2, Leader)
  end.

start(Id, Leader)->
  start(Id, gms3, Id, Leader, 500).

start(Id, Module, Rnd, Sleep, {callhome, Pid}) ->
  spawn(fun() -> init(Id, Module, Rnd, Sleep, {callhome, Pid}) end);
start(Id, Module, Rnd, Peer, Sleep) ->
  spawn(fun() -> init(Id, Module, Rnd, Peer, Sleep) end).

init(Id, Module, Rnd, Sleep, {callhome, Pid}) ->
  {ok, Cast} = apply(Module, start, [Id]),
  Color = ?color,
  Pid ! {hello, Cast},
  init_cont(Id, Rnd, Cast, Color, Sleep);
init(Id, Module, Rnd, Peer, Sleep) ->
  {ok, Cast} = apply(Module, start, [Id, Peer]),
  {ok, Color} = join(Id, Cast),
  init_cont(Id, Rnd, Cast, Color, Sleep).

join(Id, Cast) ->
  receive
    {view, _} ->
      Ref = make_ref(),
      Cast ! {mcast, {state_request, Ref}},
      state(Id, Ref);
    {error, Reason} ->
      {error, Reason}
  end.

state(Id, Ref) ->
  receive
    {state_request, Ref} ->
      receive
        {state, Ref, Color} ->
          {ok, Color}
      end;
    _Ignore ->
      state(Id, Ref)
end.

init_cont(Id, Rnd, Cast, Color, Sleep) ->
  random:seed(Rnd, Rnd, Rnd),
  Title = "Worker: " ++ integer_to_list(Id),
  Gui = gui:start(Title, self()),
  Gui ! {color, Color},
  worker(Id, Cast, Color, Gui, Sleep),
  Cast ! stop,
  Gui ! stop.

worker(Id, Cast, Color, Gui, Sleep) ->
  Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
  receive
    {change, N} ->
      Color2 = change_color(N, Color),
      Gui ! {color, Color2},
      worker(Id, Cast, Color2, Gui, Sleep);
    {state_request, Ref} ->
      Cast ! {mcast, {state, Ref, Color}},
      worker(Id, Cast, Color, Gui, Sleep);
    {state, _, _} ->
      worker(Id, Cast, Color, Gui, Sleep);
    {join, Peer, Gms} ->
      Cast ! {join, Peer, Gms},
      worker(Id, Cast, Color, Gui, Sleep);
    {view, _} ->
      worker(Id, Cast, Color, Gui, Sleep);
    stop ->
      ok;
    Error ->
      io:format("strange message: ~w~n", [Error]),
      worker(Id, Cast, Color, Gui, Sleep)
  after Wait ->
    Cast ! {mcast, {change, random:uniform(?change)}},
    worker(Id, Cast, Color, Gui, Sleep)
end.

%% rotate RGB and add N
change_color(N, {R,G,B}) ->
  {G, B, ((R+N) rem 256)}.