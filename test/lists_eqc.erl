-module(lists_eqc).
-include_lib("eqc/include/eqc.hrl").
-export([prop_delete/0]).

prop_delete() ->
  ?FORALL(
    {X, Xs},
    {int(), list(int())},
    not lists:member(X, lists:delete(X, Xs))).
