-module(map).
-export([update/3, reachable/2, all_nodes/1, new/0, test/0]).

new() ->
  [].

update(Node, Links, Map) ->
  case lists:keyfind(Node, 1, Map) of
    false ->
      [{Node, Links}|Map];
    _ ->
      CleanedMap = lists:keydelete(Node, 1, Map),
      [{Node, Links}|CleanedMap]
  end.

reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    false ->
      [];
    {Node, Links} ->
      Links
  end.

all_nodes(Map) ->
  lists:foldl(
    fun({Node, Links}, Accum) ->
      case lists:member(Node, Accum) of
        true ->
          Accum1 = Accum;
        false ->
          Accum1 = [Node|Accum]
      end,
      lists:foldl(
        fun(Link, LinkAccum) ->
          case lists:member(Link, LinkAccum) of
            true ->
              LinkAccum;
            false ->
              [Link|LinkAccum]
          end
        end, Accum1, Links)
    end, [], Map).

test() ->
  Map = [],
  Map = new(),
  Map2 = [{berlin,[london,paris]}],
  Map2 = update(berlin, [london, paris], Map),
  Reachable = [london,paris],
  Reachable = reachable(berlin, Map2),
  Reachable2 = [],
  Reachable2 = reachable(london, Map2),
  AllNodes = [berlin,london,paris],
  AllNodes = all_nodes(Map2),
  Map3 = [{berlin, [madrid]}],
  Map3 = update(berlin, [madrid], Map2).

