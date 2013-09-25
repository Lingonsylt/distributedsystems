%% Copyright
-module(hist).
-author("lingon").

%% API
-export([new/1, update/3]).

new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    {Node, CurrentN} when N > CurrentN ->
      {new, [{Node, N}|lists:keydelete(Node, 1, History)]};
    {_, _} ->
      old;
    false ->
      {new, [{Node, N}|History]}
  end.