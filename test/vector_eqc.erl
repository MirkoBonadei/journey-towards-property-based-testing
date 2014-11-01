-module(vector_eqc).
-include_lib("eqc/include/eqc.hrl").

%% This module is my exercise to implement vector calculus in Erlang defining
%% all the well known properties of the operations of the vector algebra.
%% In other words I am making some exercises on the shoulders of the giants...

-export([
         prop_vector_addition_is_the_addition_of_relative_components/0,
         prop_vector_addition_is_commutative/0,
         prop_vector_addition_is_associative/0,
         prop_zero_vector_is_the_addition_identity_element/0
%%         prop_inverse_element_of_the_addition/0
]).
-define(NUMBER_OF_GENERATED_TESTS, 2000).

prop_vector_addition_is_the_addition_of_relative_components() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, B},
      {vector(2, nat()), vector(2, nat())},
      begin
        [Xa, Ya] = A,
        [Xb, Yb] = B,
        [Xc, Yc] = vector:add(A, B),
        Xc =:= Xa + Xb,
        Yc =:= Ya + Yb
      end)).

prop_vector_addition_is_commutative() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, B},
      {vector(2, nat()), vector(2, nat())},
      vector:add(A, B) =:= vector:add(B, A))).

prop_vector_addition_is_associative() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, B, C},
      {vector(2, nat()), vector(2, nat()), vector(2, nat())},
      vector:add(A, vector:add(B, C)) =:= vector:add(vector:add(A, B), C))).

prop_zero_vector_is_the_addition_identity_element() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, ZeroVector},
      {vector(2, nat()), vector(2, 0)},
      vector:add(A, ZeroVector) =:= A)).

%% TODO: to write this property I need the scalar multiplication to generate the
%% inverse element of the addition

%% prop_inverse_element_of_the_addition() ->
%%   numtests(
%%     ?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL(
%%       )).
