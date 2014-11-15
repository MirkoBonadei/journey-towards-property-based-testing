-module(clock).
-export([hms2s/1, s2hms/1]).

-type time() :: {0..23, 0..59, 0..59}.

-spec hms2s(time()) -> non_neg_integer().
hms2s({H, M, S}) -> 
  (H * 60 * 60) + (M * 60) + S.

-spec s2hms(non_neg_integer()) -> time().
s2hms(Seconds) ->
  H = Seconds div 3600,
  Minutes = Seconds rem 3600,
  M = Minutes div 60,
  S = Minutes rem 60,
  {H, M, S}.
