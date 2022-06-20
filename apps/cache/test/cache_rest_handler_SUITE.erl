-module(cache_rest_handler_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([post_cache/1, get_cache/1, delete_cache/1]).

all() ->
    [post_cache, get_cache, delete_cache].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    application:stop(gun).

get_cache(_Config) ->
    {ok, ConnPid} = gun:open("localhost", 8000),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:request(ConnPid, <<"GET">>, "/cache", 
                    [{<<"accept">>, <<"*/*">>},
                    {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
                    "key=name"),
    {response, nofin, 200, _} = gun:await(ConnPid, StreamRef).

post_cache(_Config) ->
    {ok, ConnPid} = gun:open("localhost", 8000),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:request(ConnPid, <<"POST">>, "/cache", 
                    [{<<"accept">>, <<"*/*">>}, 
                    {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
                    "key=name&value=hasitha"),              
    {response, fin, 204, _} = gun:await(ConnPid, StreamRef).

delete_cache(_Config) ->
    {ok, ConnPid} = gun:open("localhost", 8000),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:request(ConnPid, <<"DELETE">>, "/cache", 
                    [{<<"accept">>, <<"*/*">>},
                    {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
                    "key=name"),
    {response, fin, 204, _} = gun:await(ConnPid, StreamRef).



