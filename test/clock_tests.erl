-module(clock_tests).
-include_lib("triq/include/triq.hrl").

%% Generator (or domain since we are using Triq)
%% A sample:
%% [{52,23,54},
%%  {2,28,48},
%%  {4,33,22},
%%  {10,35,2},
%%  {39,34,32},
%%  {8,53,20},
%%  {10,4,31},
%%  {49,5,52},
%%  {6,40,50},
%%  {16,52,12},
%%  {47,8,19}]
time_dom() ->
  {choose(0, 23), choose(0, 59), choose(0, 59)}.

%% Properties
prop_to_seconds_conversion_always_greater_than_0() ->
  ?FORALL(
     {H, M, S},
     time_dom(),
       clock:hms2s({H, M, S}) >= 0).

prop_to_seconds_conversion_inverse() ->
  ?FORALL(
     {H, M, S},
     time_dom(),
         {H, M, S} =:= clock:s2hms(clock:hms2s({H, M, S}))).
