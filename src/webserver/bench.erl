%% Copyright
-module(bench).

%% API
-export([bench/2,run/1]).

run([Host, PortString|_]) ->
  bench(Host, list_to_integer(PortString)).

bench(Host, Port) ->
  Start = now(),
  Runs = 100,
  run(Runs, Host, Port),
  Finish = now(),
  Time = timer:now_diff(Finish, Start)/1000,
  io:format("~p runs in ~p millis, ~p millis/run", [Runs, Time, Time/Runs]).

run(N, Host, Port) ->
  if
    N == 0 ->
      ok;
    true ->
      request(Host, Port),
      run(N-1, Host, Port)
  end.

request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("test: error: ~w~n", [Error])
  end.