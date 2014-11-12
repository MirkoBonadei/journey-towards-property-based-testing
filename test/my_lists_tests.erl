-module(my_lists_tests).
-include_lib("triq/include/triq.hrl").
-define(NUMBER_OF_GENERATED_TESTS, 100).
-export([
         sum_test/0
         %%prop_sum_monotonicity/0
         %%prop_sum_identity/0,
         %%prop_sum_base_case/0
]).

sum_test() ->
  triq:check(prop_sum_monotonicity()).

%% my_lists:sum/1
prop_sum_monotonicity() ->
    ?FORALL({L, NonNegInteger}, {list(pos_integer()), pos_integer()}, 
            begin
              L1 = [NonNegInteger|L],
              %%my_lists:sum(L) =< my_lists:sum(L1)
              1 = 0
            end).

%% prop_sum_identity() ->
%%   eqc:numtests(?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL({L}, {list(nat())}, 
%%             begin
%%               L1 = [0|L],
%%               eqc:equals(my_lists:sum(L), my_lists:sum(L1))
%%             end)).

%% prop_sum_base_case() ->
%%   eqc:numtests(?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL({Int}, {int()}, 
%%             begin
%%               L = [Int],
%%               eqc:equals(my_lists:sum(L), Int)
%%             end)).
