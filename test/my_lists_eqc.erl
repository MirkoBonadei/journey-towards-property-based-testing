-module(my_lists_eqc).
-include_lib("eqc/include/eqc.hrl").
-define(NUMBER_OF_GENERATED_TESTS, 200).
-export([
         %% sum/1
         prop_sum_monotonicity/0,
         prop_sum_identity/0,
         prop_sum_base_case/0
]).

%% my_lists:sum/1
prop_sum_monotonicity() ->
  numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({L, PositiveInteger}, {list(nat()), nat()}, 
            begin
              L1 = [PositiveInteger|L],
              my_lists:sum(L) =< my_lists:sum(L1)
            end)).

prop_sum_identity() ->
  numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({L}, {list(nat())}, 
            begin
              L1 = [0|L],
              eqc:equals(my_lists:sum(L), my_lists:sum(L1))
            end)).

prop_sum_base_case() ->
  numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({Int}, {int()}, 
            begin
              L = [Int],
              eqc:equals(my_lists:sum(L), Int)
            end)).
