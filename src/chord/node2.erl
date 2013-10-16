-module(node2).
-define(Timeout, 500).
-define(Stabilize, 500).

%% API
-export([start/1, start/2, test/0]).

start(Id) ->
  start(Id, nil).
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

send_probe(Pid) ->
  timer:sleep(2000),
  Pid ! probe,
  send_probe(Pid).

add_pair(Node, Key, Value) ->
  spawn(fun() ->
    Qref = make_ref(),
    Node ! {add, Key, Value, Qref, self()},
    receive
      {Qref, ok} ->
        io:format("Added (~p,~p) to ~p~n", [Key, Value, Node])
    end
  end).

lookup_key(Node, Key) ->
  spawn(fun() ->
    Qref = make_ref(),
    Node ! {lookup, Key, Qref, self()},
    receive
      {Qref, Result} ->
        io:format("Lookup for ~p on ~p: ~p~n", [Key, Node, Result])
    end
  end).

test() ->
  % random:seed(erlang:now()),
  Key1 = key:generate(),
  Key2 = key:generate(),
  Key3 = key:generate(),
  Key4 = key:generate(),
  Node1 = start(key:generate()),
  Node2 = start(key:generate(), Node1),
  add_pair(Node2, Key1, msg1),
  Node3 = start(key:generate(), Node2),
  spawn(fun() -> send_probe(Node1) end),
  timer:sleep(1000),
  Node4 = start(key:generate(), Node1),
  add_pair(Node3, Key2, msg2),
  Node5 = start(key:generate(), Node3),
  add_pair(Node1, Key3, msg3),
  timer:sleep(1000),
  add_pair(Node5, Key4, msg4),
  timer:sleep(2500),
  lookup_key(Node5, Key1),
  lookup_key(Node3, Key2),
  lookup_key(Node4, Key3),
  lookup_key(Node5, Key4).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

node(Id, Predecessor, Successor, Store) ->
  receive
    % A peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    % A new node informs us of its existence
    {notify, New} ->
      {Pred, Store2} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Store2);
    % A predecessor needs to know our predecessor
    {request, Peer} ->
      % io:format("~p: got request for predecessor: ~p~n", [Id, Predecessor]),
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    % Our successor informs us about its predecessor
    {status, Pred} ->
      % io:format("~p: response with current predecessor: ~p~n", [Id, Pred]),
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);

    % Run stabilization
    stabilize ->
      Succ = stabilize(Successor), % of
      node(Id, Predecessor, Succ, Store);

    {add, Key, Value, Qref, Client} ->
      %       add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);
    % probe stuff
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);

    Unexpected ->
      io:format("Unexpected message: ~p~n", [Unexpected])
  end.

handover(Store, Nkey, Npid) ->
  {Leave, Keep} = storage:split(Nkey, Store),
  Npid ! {handover, Leave},
  Keep.

add(Key, Value, Qref, Client, Id, Predecessor, {_, Spid}, Store) ->
  case Predecessor of
    nil ->
      Store2 = storage:add(Key, Value, Store),
      Client ! {Qref, ok},
      Store2;
    {Pkey, _} ->
      case key:between(Key, Pkey, Id) of
        true ->
          Store2 = storage:add(Key, Value, Store),
          Client ! {Qref, ok},
          Store2;
        false ->
          Spid ! {add, Key, Value, Qref, Client},
          Store
      end
  end.



lookup(Key, Qref, Client, Id, Predecessor, Successor, Store) ->
  case Predecessor of
    nil ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    {Pkey, _} ->
      case key:between(Key, Pkey, Id) of
        true ->
          Result = storage:lookup(Key, Store),
          Client ! {Qref, Result};
        false ->
          {_, Spid} = Successor,
          Spid ! {lookup, Key, Qref, Client}
      end
    end.

create_probe(Id, {Skey, Spid}) ->
  io:format("Sending probe to from ~p to ~p~n", [Id, Skey]),
  Spid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
  timer:sleep(500),
  % io:format("~p: Forwarding probe originating from ~p to ~p~n", [Id, Ref, Skey]),
  Spid ! {probe, Ref, [Id|Nodes], T}.

remove_probe(T, Nodes) ->
  io:format("Received probe after ~p seconds, going through ~p~n", [T, Nodes]).

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

stabilize({Skey, Spid}) ->
  Spid ! {request, self()},
  {Skey, Spid}.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
  % We're the second node entering the ring
  % Why can this happen?
    nil ->
      % io:format("~p: notifying ~p that we're its new predecessor (nil)~n", [Id, Skey]),
      Spid ! {notify, {Id, self()}},
      Successor;

  % We're stabilized, our successors predecessor is us
    {Id, _} ->
      Successor;

  % We're the second node entering the ring
    {Skey, _} ->
      % io:format("~p: notifying ~p that we're its new predecessor (second)~n", [Id, Skey]),
      Spid ! {notify, {Id, self()}},
      Successor;

  % There is already another predecessor
    {Pkey, Ppid} ->
      case key:between(Pkey, Id, Skey) of
      % The predecessor of our successor is in between us and our successor. Try stepping backwards in the ring
        true ->
          % io:format("~p: stepping backards to new successor ~p~n", [Id, Pred]),
          Pred;

      % The predecessor of our successor is behind us, notify our successor that we're its new predecessor
        false ->
          % io:format("~p: notifying ~p that we're its new predecessor (~p in between ~p and ~p)~n", [Id, Skey, Id, Pkey, Skey]),
          Spid ! {notify, {Id, self()}},
          Successor

      end
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      % io:format("~p: updating predecessor ~p (nil)~n", [Id, {Nkey, Npid}]),
      Keep = handover(Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          % io:format("~p: updating predecessor ~p (~p in between ~p and ~p)~n", [Id, {Nkey, Npid}, Nkey, Pkey, Id]),
          Keep = handover(Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          % io:format("~p: not updating predecessor ~p~n", [Id, {Nkey, Npid}]),
          {Predecessor, Store}
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).