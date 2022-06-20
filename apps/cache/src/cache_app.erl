%%%-------------------------------------------------------------------
%% @doc cache public API
%% @end
%%%-------------------------------------------------------------------

-module(cache_app).

-behaviour(application).

-ignore_xref(cache_app).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([start_phase/3]).

%% application
%% @doc Starts the application
start() ->
  application:ensure_all_started(cache).

%% @doc Stops the application
stop() ->
  application:stop(cache).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  cache_sup:start_link().

%% @private
stop(_State) ->
  ok = cowboy:stop_listener(cache_http).

start_phase(start_trails_http, _StartType, []) ->
    {ok, Port} = application:get_env(cache, http_port),
    Trails = trails:trails([cache_rest_handler,
                          cowboy_swagger_handler]),
    trails:store(Trails),
    Dispatch      = trails:single_host_compile(Trails),
    RanchOptions  = [{port, Port}],
    CowboyOptions = #{env => #{dispatch => Dispatch}
                   , compress => true
                   , timeout  => 12000},
    cache_tbl = ets:new(cache_tbl, [set, public, named_table, {keypos,1}, {read_concurrency, true}]),	
    {ok, _} = cowboy:start_clear(cache_http, RanchOptions, CowboyOptions),
    ok.
