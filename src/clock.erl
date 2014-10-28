-module(clock).
-export([hms2s/1]).

hms2s({H,M,S}) -> H*60*60 + M*60 + S.
