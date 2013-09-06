%% Copyright
-module(rudy).
-author("Anton Blomberg").

%% API
-export([start/3,stop/0,run/1,server_handle/2,ver/0,server_start/3,server_parse/2]).

run([PortString,WorkerNode,ServerNode|_]) ->
  start(list_to_integer(PortString), list_to_atom(WorkerNode), list_to_atom(ServerNode));
run([PortString,WorkerNode|_]) ->
  start(list_to_integer(PortString), list_to_atom(WorkerNode), false);
run([PortString|_]) ->
  start(list_to_integer(PortString), false, false).


start(Port, WorkerNode, ServerNode) ->
  io:format("Spawning server on port: ~p (server: ~p, workers: ~p) ...~n", [Port, ServerNode, WorkerNode]),

  if ServerNode =/= false ->
    io:format("Ping server: ~p~n", [net_adm:ping(ServerNode)]),
    io:format("Reloading code: ~p (~p)~n", [[c:nl(rudy), c:nl(http)], nodes()]),
    Server = spawn(ServerNode, rudy, server_start, [Port, ServerNode, WorkerNode]);
  true ->
    Server = spawn(rudy, server_start, [Port, ServerNode, WorkerNode])
  end,
  timer:sleep(infinity).

ver() ->
  "4".

stop() ->
  exit(whereis(rudy), "time to die").

server_start(Port, ServerNode, WorkerNode) ->
  if WorkerNode =/= false ->
    io:format("Ping worker ~p: ~p~n", [WorkerNode, net_adm:ping(WorkerNode)]),
    io:format("Reloading code: ~p (~p)~n", [[c:nl(rudy), c:nl(http)], nodes()]);
  true ->
    0
  end,
  {ok, ServerSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  io:format("Listening for connections on ~p...~n", [Port]),
  server_accept(ServerSock, ServerNode, WorkerNode).

server_accept(ServerSock, ServerNode, WorkerNode) ->
  {ok, Sock} = gen_tcp:accept(ServerSock),
  if ServerNode =/= false ->
    Accepter = spawn(ServerNode, rudy, server_handle, [Sock,WorkerNode]);
  true ->
    Accepter = spawn(rudy, server_handle, [Sock, WorkerNode])
  end,

  server_accept(ServerSock, ServerNode, WorkerNode).

server_handle(Sock, WorkerNode) ->
  {ok, Bin} = do_recv(Sock, []),
  if WorkerNode =/= false ->
    spawn(WorkerNode, rudy, server_parse, [self(), Bin]);
  true ->
    spawn(rudy, server_parse, [self(), Bin])
  end,

  receive
    {response, Data} ->
      gen_tcp:send(Sock, Data),
      ok = gen_tcp:close(Sock)
  end.

server_parse(Pid, Data) ->
  RequestData = binary_to_list(Data),
  Request = http:parse_request(RequestData),
  timer:sleep(40),
  Pid ! {response, reply(Request)}.

reply({{get, URI, _}, _, _}) ->
  http:ok("Hello erlang!").

do_recv(Sock, ReceivedData) ->
  if byte_size(ReceivedData) >= 4 ->
    LastFourBytes = binary:part(ReceivedData, byte_size(ReceivedData), -4);
  true ->
    LastFourBytes = <<"">>
  end,

  if LastFourBytes =:= <<"\r\n\r\n">> ->
    {ok, ReceivedData};
  true ->
    case gen_tcp:recv(Sock, 0) of
      {ok, NewReceivedData} ->
        do_recv(Sock, list_to_binary([ReceivedData, NewReceivedData]));
      {error, closed} ->
        io:format("Request error: connection closed!"),
        {ok, list_to_binary(ReceivedData)}
    end
  end.