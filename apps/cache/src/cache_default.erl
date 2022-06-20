-module(cache_default).

-behaviour(cowboy_rest).

-export([init/2,
        rest_init/2,
        content_types_accepted/2,
        content_types_provided/2,
        forbidden/2,
        resource_exists/2,
        allowed_methods/2,
        known_methods/2]).

%% cowboy
init(Req, _Opts) ->
  {cowboy_rest, Req, #{}}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

content_types_accepted(#{method := Method} = Req, State) ->
  io:format("content_types_accepted|Method:~p ~n", [Method]),
  {[{'*', handle_rest}], Req, State}.

content_types_provided(#{method := Method} = Req, State) ->
  io:format("content_types_provided|Method:~p|Req:~p ~n", [Method, Req]),
  {[{<<"text/plain">>, handle_rest}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

known_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.


