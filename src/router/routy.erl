-module(routy).

-export([start/1, stop/1, sprint/2, bootstrap/0]).
start(Name) ->
  register(Name, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

bootstrap() ->
  timer:sleep(200),
  start(stockholm),
  start(lund),
  start(uppsala),
  start(orebro),
  start(goteborg),

  timer:sleep(200),
  stockholm ! {add, lund, {lund, 'sweden@lingon'}},
  lund ! {add, stockholm, {stockholm, 'sweden@lingon'}},


  stockholm ! {add, uppsala, {uppsala, 'sweden@lingon'}},
  uppsala ! {add, stockholm, {stockholm, 'sweden@lingon'}},

  uppsala ! {add, orebro, {orebro, 'sweden@lingon'}},
  orebro ! {add, uppsala, {uppsala, 'sweden@lingon'}},

  orebro ! {add, goteborg, {goteborg, 'sweden@lingon'}},
  goteborg ! {add, orebro, {orebro, 'sweden@lingon'}},

  lund ! {add, goteborg, {goteborg, 'sweden@lingon'}},
  goteborg ! {add, lund, {lund, 'sweden@lingon'}},

  % sprint(lund, status),
  % sprint(stockholm, status),

  timer:sleep(200),
  stockholm ! broadcast,
  lund ! broadcast,
  uppsala ! broadcast,
  orebro ! broadcast,
  goteborg ! broadcast,

  timer:sleep(300),
  stockholm ! update,
  timer:sleep(100),
  lund ! update,
  timer:sleep(100),
  uppsala ! update,
  timer:sleep(100),
  orebro ! update,
  timer:sleep(100),
  goteborg ! update,

  timer:sleep(200),
  sprint(stockholm, status),
  sprint(lund, status),
  sprint(uppsala, status),
  sprint(orebro, status),
  sprint(goteborg, status),

  stockholm ! {send, goteborg, hejfransthlm},
  timer:sleep(100),
  goteborg ! {send, stockholm, hejfrangbg},
  timer:sleep(100),
  orebro ! {send, stockholm, hejfranorebro},
  timer:sleep(100),
  orebro ! {send, lund, hejfranorebro}.
  % lund ! {send, stockholm, hopp}.
  % timer:sleep(500),
  % lund ! {send, stockholm, hej}.

sprint(Pid, Message) ->
  Pid ! {Message, self()},
  receive
    Response ->
      io:format("~p~n", [Response])
  end.

init(Name) ->
  io:format("Starting routy...~n"),
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process,Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      % io:format("~w: adding links: ~p~n", [Name, intf:list(Intf1)]),
      Map1 = map:update(Name, intf:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      Map1 = map:update(Name, intf:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w: exit recived from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      Map1 = map:update(Name, intf:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);
    {links, Node, R, Links} ->
      % io:format("~w: received links from ~p with N ~p, hist: ~p~n", [Name, Node, R, Hist]),
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          % io:format("~w: received NEW links from ~p to ~p~n", [Name, Node, Links]),
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;
    update ->
      io:format("~w: update...~n", [Name]),
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);
    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);
    {route, Name, From, Message} ->
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);
    {route, To, From, Message} ->
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          io:format("~w: routing message (~w) from ~p to ~p through ~p~n", [Name, Message, From, To, Gw]),
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);
    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);
    stop ->
      ok
  end.
