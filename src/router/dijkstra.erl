%% Copyright
-module(dijkstra).
-export([table/2, route/2, test/0]).

%% API
-export([]).

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false ->
      0;
    {Node, N, Gateway} ->
      N
  end.

sort_nodes(Nodes) ->
  lists:sort(
    fun({Node1, N1, Gateway1}, {Node2, N2, Gateway2}) ->
      N1 =< N2
    end, Nodes).

replace(Node, N, Gateway, Sorted) ->
  CleanedSorted = lists:keydelete(Node, 1, Sorted),
  sort_nodes([{Node, N, Gateway}|CleanedSorted]).

update(Node, N, Gateway, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false ->
      Sorted;
    {_, ExistingN, _} ->
      if N < ExistingN ->
        replace(Node, N, Gateway, Sorted);
      true ->
        Sorted
      end
  end.

iterate([], _, Table) ->
  Table;
iterate([{_, inf, _}|_], _, Table) ->
  Table;
iterate(Sorted, Map, Table) ->
  % io:format("Sorted: ~p~n", [Sorted]),
  [{Node, N, Gateway}|SortedTail] = Sorted,
  io:format("~p hops to ~p~n", [N, Node]),
  Links = map:reachable(Node, Map),
  UnSorted = lists:foldl(
    fun(Link, Accum) ->
      update(Link, N+1, Gateway, Accum)
    end, SortedTail, Links),
  iterate(sort_nodes(UnSorted), Map, [{Node, Gateway}|Table]).

table(Gateways, Map) ->
  Sorted = sort_nodes(lists:foldl(
    fun(Node, Accum) ->
      case lists:member(Node, Gateways) of
        true ->
          [{Node, 0, Node}|Accum];
        false ->
          [{Node, inf, unknown}|Accum]
      end
    end, [], map:all_nodes(Map))),
  % io:format("gateways: ~p, map: ~p, sorted nodes: ~p, all_nodes:~p~n", [Gateways, Map, Sorted, map:all_nodes(Map)]),
  iterate(Sorted, Map, []).

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {Node, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
  end.


test() ->
  Update = [],
  Update = update(london, 2, amsterdam, []),
  Update2 = [{london,2,paris}],
  Update2 = update(london, 2, amsterdam, [{london,2,paris}]),
  Update3 = [{london, 1, stockholm}, {berlin, 2, paris}],
  Update3 = update(london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]),

  Iterate = [{berlin,paris}, {paris, paris}],
  Iterate = iterate([{paris, 0, paris}, {berlin, inf, unknown}], [{paris, [berlin]}], []),

  Table = [{berlin,madrid},{rome,paris},{paris,paris}, {madrid,madrid}],
  Table = table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]).

