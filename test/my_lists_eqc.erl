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
  eqc:numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({L, NonNegInteger}, {list(nat()), nat()}, 
            begin
              L1 = [NonNegInteger|L],
              my_lists:sum(L) =< my_lists:sum(L1)
            end)).

prop_sum_identity() ->
  eqc:numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({L}, {list(nat())}, 
            begin
              L1 = [0|L],
              eqc:equals(my_lists:sum(L), my_lists:sum(L1))
            end)).

prop_sum_base_case() ->
  eqc:numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL({Int}, {int()}, 
            begin
              L = [Int],
              eqc:equals(my_lists:sum(L), Int)
            end)).
