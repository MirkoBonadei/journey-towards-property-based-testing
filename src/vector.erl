-module(vector).
-export([add/2]).

add(A, B) ->
  add_helper(A, B, []).

add_helper([], [], Acc) ->
  lists:reverse(Acc);
add_helper([HeadA|TailA], [HeadB| TailB], Acc) ->
  add_helper(TailA, TailB, [HeadA + HeadB|Acc]).

