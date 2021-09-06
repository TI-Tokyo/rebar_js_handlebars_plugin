-module(rebar_js_handlebars_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_js_handlebars_plugin_prv:init(State),
    {ok, State1}.
