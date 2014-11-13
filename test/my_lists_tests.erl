-module(my_lists_tests).
-include_lib("triq/include/triq.hrl").

%% my_lists:sum/1 properties
prop_sum_monotonicity() ->
    ?FORALL({L, NonNegInteger}, {list(int()), pos_integer()},
            begin
              L1 = [NonNegInteger|L],
              my_lists:sum(L) =< my_lists:sum(L1)
            end).

prop_sum_identity() ->
    ?FORALL({L}, {list(pos_integer())}, 
            begin
              L1 = [0|L],
              my_lists:sum(L) =:= my_lists:sum(L1)
            end).

prop_sum_base_case() ->
    ?FORALL({Int}, {int()}, 
            begin
              L = [Int],
              my_lists:sum(L) =:= Int
            end).
