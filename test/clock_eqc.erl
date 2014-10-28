-module(clock_eqc).
-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

prop_time_to_seconds() ->
    ?FORALL({H,M,S},{nat(),nat(),nat()},
    ?IMPLIES(H < 24,
    ?IMPLIES(M < 60,
    ?IMPLIES(S < 60,
             clock:hms2s({H,M,S}) == (H*60*60)+(M*60)+S)))).
