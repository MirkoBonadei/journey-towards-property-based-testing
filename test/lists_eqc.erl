-module(lists_eqc).
-include_lib("eqc/include/eqc.hrl").

%% The goal of this exercise is to discover properties that must be satisfied 
%% by all the functions defined in the Erlang lists module. 
%% But this is not enough... 
%% The module my_lists should define algorithms to make these properties pass 
%% so that the one of the properties must be that the function my_lists:F/n 
%% should behave equally to lists:F/n. 

-export([
         prop_append_two_lists_results_in_list_with_length_equal_to_the_length_of_the_two_lists/0
]).
-define(NUMBER_OF_GENERATED_TESTS, 2000).

%% Here is what the Erlang documentation says about the lists:append/2 function:
%%
%% append(List1, List2) -> List3
%%
%% Types:
%% List1 = List2 = List3 = [T]
%% T = term()
%%
%% Returns a new list List3 which is made from the elements of List1 followed by 
%% the elements of List2. For example:
%% 
%% > lists:append("abc", "def").
%% "abcdef"
%%
%% lists:append(A, B) is equivalent to A ++ B.
%%
%% Properties:
%% 
%% - the number of the elements of the result must be equal to the sum of the 
%%   number of the element of the two lists;
%% - TODO: find others

prop_append_two_lists_results_in_list_with_length_equal_to_the_length_of_the_two_lists() ->
  numtests(?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {List1, List2},
      {list(int()), list(int())},
      begin
        Length1 = erlang:length(List1),
        Length2 = erlang:length(List2),
        ExpectedLength = Length1 + Length2,
        ResultLength = erlang:length(my_lists:append(List1, List2)),

        ResultLength =:= ExpectedLength
      end)).
