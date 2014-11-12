%% -module(vector_eqc).
%% -include_lib("eqc/include/eqc.hrl").
%% -define(NUMBER_OF_GENERATED_TESTS, 2000).

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
%% two dimensional vector space over the real numbers (from now R^2)[*].
%%
%% [*] NB: I have to switch from real numbers to integer numbers (aka the ring Z) 
%%         because I have some problems with floating point arithmetics in Erlang 
%%         and at the moment I am not able to keep the associative property with 
%%         real numbers. It is a problem which goes beyond the property based 
%%         testing and I am going to address it in the next days.
%%         So that, close an eye on the fact that we have not a vector space in 
%%         these examples, because scalars have to be any field and the integer
%%         numbers does not form a field but only a ring.

%% -export([
         %% addition properties
         %% prop_vector_addition_is_commutative/0,
         %% prop_vector_addition_is_associative/0,
         %% prop_zero_vector_is_the_addition_identity_element/0
%%         prop_inverse_element_of_the_addition/0,
         %% scalar multiplication properties
%%         prop_vector_scalar_mul_is_the_mul_of_vector_components_for_the_scalar/0
%% ]).

%% TODO: Is it possible to generate vector spaces with random dimension?

%% Given two vectors A and B which belong to Z^2
%% Then A + B = B + A 
%% prop_vector_addition_is_commutative() ->
%%   numtests(
%%     ?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL(
%%       {A, B},
%%       {vector(2, int()), vector(2, int())},
%%       eqc:equals(vector:add(A, B), vector:add(B, A)))).

%% Given two vectors A and B which belong to Z^2
%% Then A + (B + C) = (A + B) + C 
%% prop_vector_addition_is_associative() ->
  %% REPORT OF THE FLOATING POINT ARITHMETIC PROBLEM:
  %%
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
  %% numtests(
  %%   ?NUMBER_OF_GENERATED_TESTS,
  %%   ?FORALL(
  %%     {A, B, C},
  %%     {vector(2, int()), vector(2, int()), vector(2, int())},
  %%     eqc:equals(vector:add(A, vector:add(B, C)), vector:add(vector:add(A, B), C)))).

%% Given a vector A which belongs to Z^2
%% And the zero vector 0 [which in our case is (0, 0)]
%% Then A + 0 = A 
%% prop_zero_vector_is_the_addition_identity_element() ->
%%   numtests(
%%     ?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL(
%%       {A, ZeroVector},
%%       {vector(2, int()), vector(2, 0)},
%%       eqc:equals(vector:add(A, ZeroVector), A))).

%% TODO: to write this property I need the scalar multiplication to generate the
%% inverse element of the addition

%% prop_inverse_element_of_the_addition() ->
%%   numtests(
%%     ?NUMBER_OF_GENERATED_TESTS,
%%     ?FORALL(
%%       )).

