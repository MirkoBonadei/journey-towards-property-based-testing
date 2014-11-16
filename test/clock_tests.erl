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

%% Generates a time between 00:00:00 and 3:09:09.
%% It olny needs to create small deltas without having to use 
%% random:uniform/1 inside the property because in that case 
%% Quickcheck does not shrinks it. (used below)
delta_dom() ->
  {choose(0, 3), choose(0, 9), choose(0, 9)}.  

%% Properties
prop_time_to_seconds_conversion_always_greater_than_0() ->
  ?FORALL(
    {H, M, S},
    time_dom(),
       clock:hms2s({H, M, S}) >= 0).

prop_an_hour_weights_3600_seconds() ->
  ?FORALL(
    {H, M, S},
    time_dom(),
  ?IMPLIES(H < 23,
       begin
         Seconds1 = clock:hms2s({H, M, S}),
         Seconds2 = clock:hms2s({H + 1, M, S}),
         Seconds1 =:= Seconds2 - 3600
       end)).

prop_a_minute_weights_60_seconds() ->
  ?FORALL(
    {H, M, S},
    time_dom(),
  ?IMPLIES(M < 59,
      begin
         Seconds1 = clock:hms2s({H, M, S}),
         Seconds2 = clock:hms2s({H, M + 1, S}),
         Seconds1 =:= Seconds2 - 60
      end)).

prop_a_second_weights_1_second() ->
  ?FORALL(
    {H, M, S},
    time_dom(),
  ?IMPLIES(S < 59,
      begin
         Seconds1 = clock:hms2s({H, M, S}),
         Seconds2 = clock:hms2s({H, M, S + 1}),
         Seconds1 =:= Seconds2 - 1
      end)).

%% This property is difficult to write because I are not managing 
%% time overflow. We start from 00:00:00 and stop to 23:59:59, so that 
%% I have to find a trusted delta to sum to the original time.
prop_time_to_seconds_is_monotonically_increasing() ->
  ?FORALL(
    [{H, M, S}, {Hdelta, Mdelta, Sdelta}],
    [time_dom(), delta_dom()],
  ?IMPLIES(H < 20,
  ?IMPLIES(M < 50,
  ?IMPLIES(S < 50,
    begin
      Time2 = {H + Hdelta, M + Mdelta, S + Sdelta},
      clock:hms2s({H, M, S}) =< clock:hms2s(Time2)
    end)))).

prop_time_to_seconds_conversion_inverse() ->
  ?FORALL(
     {H, M, S},
     time_dom(),
         {H, M, S} =:= clock:s2hms(clock:hms2s({H, M, S}))).
