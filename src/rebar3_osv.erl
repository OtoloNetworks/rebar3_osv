-module(rebar3_osv).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_prv_osv:init(State),
    {ok, State1}.
