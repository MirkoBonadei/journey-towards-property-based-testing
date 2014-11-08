-module(clock_eqc).
-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

prop_time_to_seconds() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M < 60,
    ?IMPLIES(S < 60,
             clock:hms2s({H,M,S}) == (H*60*60)+(M*60)+S)))).

prop_time_is_an_integer() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M < 60,
    ?IMPLIES(S < 60,
             is_integer(clock:hms2s({H,M,S})))))).

% is there a way to extract the IMPLIES? They're always the same
prop_time_can_only_be_positive() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M < 60,
    ?IMPLIES(S < 60,
             clock:hms2s({H,M,S}) >= 0)))).

prop_time_cannot_exceed_a_day() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M < 60,
    ?IMPLIES(S < 60,
             clock:hms2s({H,M,S}) < (24*60*60))))).

% I get lots of x, I think because I'm too selective with the values
% and lots of them are discarded
% probably needs a custom generator?
prop_time_monotonically_increases() ->
    ?FORALL({H1,M1,S1,H2,M2,S2},{nat(),nat(),nat(),nat(),nat(),nat()},
    ?IMPLIES(H1 < 24,
    ?IMPLIES(M1 < 60,
    ?IMPLIES(S1 < 60,
    ?IMPLIES(H2 < 24,
    ?IMPLIES(M2 < 60,
    ?IMPLIES(S2 < 60,
    ?IMPLIES(H1 =< H2,
    ?IMPLIES(M1 =< M2,
    ?IMPLIES(S1 =< S2,
             clock:hms2s({H1,M1,S1}) =< clock:hms2s({H2,M2,S2}))))))))))).

% could to the same with minutes and seconds,
% but it may not make sense as a property
% but it's more refined than the previous one
% Will also need the converse: > with M > 60
prop_time_hours_count_as_60_times_minutes_less_than_or_equal() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M < 60,
    ?IMPLIES(S < 60,
             clock:hms2s({H,M,S}) =< clock:hms2s({H+1,0,S}))))).

% cannot generate values, it seems
% "Gave up! Passed only 0 tests"
prop_time_hours_count_as_60_times_minutes_greater_than_or_equal() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M >= 60,
    ?IMPLIES(S < 60,
             clock:hms2s({H,M,S}) >= clock:hms2s({H+1,0,S}))))).
