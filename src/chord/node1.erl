-module(node1).
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

test() ->
  Node1 = start(key:generate()),
  Node2 = start(key:generate(), Node1),
  %Node3 = start(key:generate(), Node2),
  spawn(fun() -> send_probe(Node1) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
  receive
    % A peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    % A new node informs us of its existence
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    % A predecessor needs to know our predecessor
    {request, Peer} ->
      io:format("~p: got request for predecessor: ~p~n", [Id, Predecessor]),
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    % Our successor informs us about its predecessor
    {status, Pred} ->
      io:format("~p: response with current predecessor: ~p~n", [Id, Pred]),
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);

    % Run stabilization
    stabilize ->
      Succ = stabilize(Successor), % of
        %{new_successor, NewSuccessor} ->
        %  io:format("NewSuccessor: ~p~n", [Successor]),
        %  stabilize(NewSuccessor),
        %  node(Id, nil, NewSuccessor);
        %_ ->
      node(Id, Predecessor, Succ);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor);

    Unexpected ->
      io:format("Unexpected message: ~p~n", [Unexpected])
  end.

create_probe(Id, {Skey, Spid}) ->
  io:format("Sending probe to from ~p to ~p~n", [Id, Skey]),
  Spid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
  timer:sleep(500),
  io:format("~p: Forwarding probe originating from ~p to ~p~n", [Id, Ref, Skey]),
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
      io:format("~p: notifying ~p that we're its new predecessor (nil)~n", [Id, Skey]),
      Spid ! {notify, {Id, self()}},
      Successor;

    % We're stabilized, our successors predecessor is us
    {Id, _} ->
      Successor;

    % We're the second node entering the ring
    {Skey, _} ->
      io:format("~p: notifying ~p that we're its new predecessor (second)~n", [Id, Skey]),
      Spid ! {notify, {Id, self()}},
      Successor;

    % There is already another predecessor
    {Pkey, Ppid} ->
      case key:between(Pkey, Id, Skey) of
        % The predecessor of our successor is in between us and our successor. Try stepping backwards in the ring
        true ->
          io:format("~p: stepping backards to new successor ~p~n", [Id, Pred]),
          Pred;

        % The predecessor of our successor is behind us, notify our successor that we're its new predecessor
        false ->
          io:format("~p: notifying ~p that we're its new predecessor (~p in between ~p and ~p)~n", [Id, Skey, Id, Pkey, Skey]),
          Spid ! {notify, {Id, self()}},
          Successor

  end
end.

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      io:format("~p: updating predecessor ~p (nil)~n", [Id, {Nkey, Npid}]),
      {Nkey, Npid};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          io:format("~p: updating predecessor ~p (~p in between ~p and ~p)~n", [Id, {Nkey, Npid}, Nkey, Pkey, Id]),
          {Nkey, Npid};
      false ->
        io:format("~p: not updating predecessor ~p~n", [Id, {Nkey, Npid}]),
        Predecessor
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).