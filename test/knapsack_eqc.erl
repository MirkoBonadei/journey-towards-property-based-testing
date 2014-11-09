-module(knapsack_eqc).
-include_lib("eqc/include/eqc.hrl").
-define(NUMBER_OF_GENERATED_TESTS, 200).
-export([
         prop_output_must_be_subset_of_the_input/0,
         thing/0
]).

%% Generators

%% A "thing" is something we can put into the knapsack. I have decided to use Erlang 
%% maps to model this abstraction.
%% A "thing" is something with a weight and a value, in erlang:
%% #{ w => nat(), v => nat()}
thing() ->
  ?LET(
     {Weight, Value}, 
     {?SUCHTHAT(W, nat(), W > 0), nat()}, 
     #{ w => Weight, v => Value}).


%% Properties

prop_output_must_be_subset_of_the_input() ->
  ok.

