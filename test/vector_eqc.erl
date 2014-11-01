-module(vector_eqc).
-include_lib("eqc/include/eqc.hrl").
-define(NUMBER_OF_GENERATED_TESTS, 2000).

%% This module is my exercise to implement vector calculus in Erlang defining
%% all the well known properties of the operations of the vector algebra.
%% 
%% There are two points to understand in property based testing:
%% 1- How to write good properties
%% 2- How to write good generators
%%
%% Here I try to exploit the experience I already have on linear algebra, and 
%% see how the properties I have studied on the books can be applied to write a 
%% module that have to deal with the basic algebraic operations on vectors.
%%
%% For the moment there is more focus on properties then on generators and it is
%% for this reason that I have decided to generate (for now) vectors from the 
%% two dimensional vector space over the real numbers (from now R^2).

-export([
         %% addition properties
         prop_vector_addition_is_the_addition_of_relative_components/0,
         prop_vector_addition_is_commutative/0,
         prop_vector_addition_is_associative/0,
         prop_zero_vector_is_the_addition_identity_element/0
%%         prop_inverse_element_of_the_addition/0,
         %% scalar multiplication properties
%%         prop_vector_scalar_mul_is_the_mul_of_vector_components_for_the_scalar/0
]).

%% Given two vectors A and B which belong to R^2
%% And A = (a1, a2)
%% And B = (b1, b2) 
%% Then C = A + B 
%% And C = (a1 + b1, a2 + b2)
prop_vector_addition_is_the_addition_of_relative_components() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, B},
      {vector(2, real()), vector(2, real())},
      begin
        [Xa, Ya] = A,
        [Xb, Yb] = B,
        [Xc, Yc] = vector:add(A, B),
        eqc:equals(Xc, Xa + Xb),
        eqc:equals(Yc, Ya + Yb)
      end)).

%% Given two vectors A and B which belong to R^2
%% Then A + B = B + A 
prop_vector_addition_is_commutative() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, B},
      {vector(2, real()), vector(2, real())},
      eqc:equals(vector:add(A, B), vector:add(B, A)))).

%% Given two vectors A and B which belong to R^2
%% Then A + (B + C) = (A + B) + C 
prop_vector_addition_is_associative() ->
  %% in this test I had a problem with floating point number comparison,
  %% eqc:equals/2 was too strict since it probably uses =:= and I was 
  %% getting errors such the one below only after 10/15 tests:
  %% > prop_vector_addition_is_associative: ...............Failed! After 16 tests.
  %% > {[-1.3333333333333333,1.0],[-0.4,2.5],[1.0,0.0]}
  %% > [-0.7333333333333333,3.5] /= [-0.7333333333333334,3.5]
  %% > Shrinking................(16 times)
  %% > {[0.3333333333333333,0.0],[1.0,0.0],[1.0,0.0]}
  %% > [2.3333333333333335,0.0] /= [2.333333333333333,0.0]
  %% This is why I have decided to make a fuzzy match between vector components. 
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, B, C},
      {vector(2, real()), vector(2, real()), vector(2, real())},
      %% eqc:equals(vector:add(A, vector:add(B, C)), vector:add(vector:add(A, B), C)))).
      begin
        [LeftX, LeftY] = vector:add(A, vector:add(B, C)),
        [RightX, RightY] = vector:add(vector:add(A, B), C),
        fuzzy_match(LeftX, RightX, 1),
        fuzzy_match(LeftY, RightY, 1)
      end)).

%% Given a vector A which belongs to R^2
%% And the zero vector 0 [which in our case is (0, 0)]
%% Then A + 0 = A 
prop_zero_vector_is_the_addition_identity_element() ->
  numtests(
    ?NUMBER_OF_GENERATED_TESTS,
    ?FORALL(
      {A, ZeroVector},
      {vector(2, real()), vector(2, 0)},
      eqc:equals(vector:add(A, ZeroVector), A))).

%% TODO: to write this property I need the scalar multiplication to generate the
%% inverse element of the addition

%% prop_inverse_element_of_the_addition() ->
%%   numtests(
%%     ?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL(
%%       )).

%% private functions
fuzzy_match(A,B,L) ->
  <<AT:L/binary, _/binary>> = <<A/float>>,
  <<BT:L/binary, _/binary>> = <<B/float>>,
  eqc:equals(AT, BT).
