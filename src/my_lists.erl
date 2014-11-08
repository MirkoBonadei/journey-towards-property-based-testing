-module(my_lists).
-export([sum/1]).

sum(L) ->
  sum_acc(L, 0).

%% private functions
sum_acc([], Acc) ->
  Acc;
sum_acc([H|T], Acc) ->
  sum_acc(T, Acc + H).
