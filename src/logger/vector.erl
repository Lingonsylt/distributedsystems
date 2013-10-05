%%%-------------------------------------------------------------------
%%% @author lingon
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2013 10:51 AM
%%%-------------------------------------------------------------------
-module(vector).
-author("lingon").

%% API
-export([increment_vector/2, max_vectors/2]).

increment_vector(TimeVector, Id) ->
  % # TimeVector[Id] += 1
  lists:foldr(
    fun(Item, {I, Accum}) ->
      case Item == Id of
        true ->
          Item2 = Item + 1;
        false ->
          Item2 = Item
      end,
      {I + 1, [Item2|Accum]} end, {0, []}, TimeVector).

max_vectors(TimeVector, OtherTimeVector) ->
  lists:foldl(
    fun({Left, Right}, Accum) ->
      Result = max(Left, Right),
      Accum ++ [Result] end, [], lists:zip(TimeVector, OtherTimeVector)).