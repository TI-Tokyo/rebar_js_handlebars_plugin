-module(rebar_js_handlebars_plugin_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar_js_handlebars_plugin).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, false},
            {deps, ?DEPS},
            {example, "rebar3 rebar_js_handlebars_plugin"},
            {opts, []},
            {short_desc, "Concatenates js assets into bundle.js"},
            {desc, "A rebar3 plugins to concatenate JS files into a bundle"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running js_handlebars...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         AppDir = rebar_app_info:dir(AppInfo),
         AppOpts = rebar_app_info:opts(AppInfo),

         Opts = rebar_opts:get(AppOpts, js_handlebars, []),
         Templates = option(templates, Opts),
         OutDir = filename:join([AppDir, option(out_dir, Opts)]),
         DocRoot = filename:join([AppDir, option(doc_root, Opts)]),
         Targets = [{normalize_path(Destination, OutDir),
                     lists:zip(Sources, normalize_paths(Sources, DocRoot)), Opts} || {Destination, Sources} <- Templates],
         build_each(Targets)
     end || AppInfo <- Apps],
    {ok, State}.

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root)  -> "priv/assets/javascripts";
default(out_dir)   -> "priv/www/javascripts";
default(target)    -> "Ember.TEMPLATES";
default(compiler)  -> "Ember.Handlebars.compile";
default(source_ext)-> ".hbs";
default(templates) -> [].

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


handlebars(Name, Body, Target, Compiler) ->
    Targeted = iolist_to_binary([Target, "['", Name, "']"]),
    Compiled = iolist_to_binary([Compiler, "('", Body, "');\n"]),
    iolist_to_binary([Targeted, " = ", Compiled]).

read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
           Binary;
        {error, Reason} ->
           rebar_api:error("Reading asset ~s failed: ~p", [File, Reason]),
           rebar_utils:abort()
    end.

normalize_paths(Paths, Basedir) ->
    lists:foldr(fun(X, Acc) ->
                [normalize_path(X, Basedir) | Acc] end, [], Paths).
normalize_path(Path, Basedir) ->
    filename:join([Basedir, Path]).

template_name(Source, SourceExt) ->
    filename:rootname(Source, SourceExt).

build_each([]) ->
    ok;
build_each([{Destination, Sources, Options} | Rest]) ->
    rebar_api:debug("building ~p from ~p, options ~p", [Destination, Sources, Options]),
    Target = option(target, Options),
    Compiler = option(compiler, Options),
    SourceExt = option(source_ext, Options),
    Contents = [handlebars(template_name(SrcName, SourceExt),
                           re:replace(read(SrcFile), "\n", "", [global]), Target, Compiler) || {SrcName, SrcFile} <- Sources],
    Concatenated = rebar_js_concatenator_plugin:concatenate(Contents),
    case file:write_file(Destination, Concatenated, [write]) of
        ok ->
            io:format("Compiled handlebars asset ~s~n", [Destination]);
        {error, Reason} ->
            rebar_api:error("Handlebars compliation of ~s failed: ~p", [Destination, Reason]),
            rebar_utils:abort()
    end,
    build_each(Rest).

%% delete_each([]) ->
%%     ok;
%% delete_each([First | Rest]) ->
%%     case file:delete(First) of
%%         ok ->
%%             ok;
%%         {error, enoent} ->
%%             ok;
%%         {error, Reason} ->
%%             rebar_api:error("Failed to delete ~s: ~p", [First, Reason])
%%     end,
%%     delete_each(Rest).
