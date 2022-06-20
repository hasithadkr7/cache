-module(cache_rest_handler).

-ignore_xref(cache_rest_handler).

-behaviour(cowboy_rest).

-include_lib("mixer/include/mixer.hrl").
-mixin([
    {cache_default,
        [ init/2,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2,
          allowed_methods/2,
          known_methods/2
    ]}
]).

-export([handle_rest/2, delete_resource/2]).

-behaviour(trails_handler).
-export([trails/0]).

-type options() :: #{path => string()}.
-type state() :: #{opts => options()}.

trails() ->
    Key = 
        #{name => <<"key">>,
        description => <<"Chache item's key">>,
        in => <<"body">>,
        required => true,
        type => string,
        schema =>
            #{type => string,
                example => <<"key=name">>
            }
    },
    CacheItem = 
        #{name => <<"cache">>,
        description => <<"Chache item's key">>,
        in => <<"body">>,
        required => true,
        schema =>
            #{type => string,
                example => <<"key=name&value=data">>
            }
    },
    Metadata = #{
        get =>
            #{tags => [],
                description => "Retrieve cached value",
                parameters => [Key],
                responses => #{
                    <<"200">> => #{
                        description => <<"success">>,
                        content => #{
                        'application/json' =>
                            #{item => #{
                                key => string,
                                value => string}
                            }
                        }
                    },
                    <<"400">> => #{
                        description => <<"bad request">>
                    },
                    <<"404">> => #{
                        description => <<"value not found">>
                    }
                }
            },
        post =>
            #{tags => [],
                description => "Insert or update cache",
                parameters => [CacheItem],
                responses => #{
                    <<"204">> => #{
                        description => <<"success">>
                    },
                    <<"400">> => #{
                        description => <<"bad request">>
                    }
                }
            },
        delete =>
            #{tags => [],
                description => "Delete cached item with key",
                parameters => [Key],
                responses => #{
                    <<"204">> => #{
                        description => <<"success">>
                    },
                    <<"400">> => #{
                        description => <<"bad request">>
                    }
                }
            }
    },
    [trails:trail("/cache", cache_rest_handler, [], Metadata)].


-spec handle_rest(cowboy_req:req(), state()) -> {iodata(), cowboy_req:req(), state()}.
%%@doc Handle GET requests
handle_rest(#{method := <<"GET">>} = Request, State) ->
    io:format("Inside------------|GET|Request:~p ~n", [Request]),
    {ok, Body, _} = read_body(Request, <<>>),
    case decode_post_body(Body) of
        [{"key", Key}] ->
            case cache_db:get_cache(Key) of
                {error, not_found} ->
                    send_response(404, Request);
                {ok, Value} ->
                    send_response(200, Request, Key, Value)
            end;
        [] ->
            send_response(400, Request, "error", "Bad Request")
    end,
    {[<<"OK">>,[]], Request, State};

handle_rest(#{method := <<"POST">>} = Request, State) ->
%%@doc Handle POST requests
    io:format("Inside------------|POST|Request:~p ~n", [Request]),
    {ok, Body, _} = read_body(Request, <<>>),
    case decode_post_body(Body) of
        [{"key", Key},{"value", Value}] ->
            cache_db:insert_or_update_cache(Key, Value),
            send_response(204, Request);
        [ _] ->
            send_response(400, Request, "error", "Bad Request");
        [] ->
            send_response(400, Request, "error", "Bad Request")
    end,
    {true, Request, State};

handle_rest(Request, State) ->
    Response = send_response(405, Request, "error", "Method Not Allowed"),
    {Response, Request, State}.

-spec delete_resource(cowboy_req:req(), state()) -> { boolean(), cowboy_req:req(), state()}.
%%@doc Handle DELETE requests
delete_resource(Request, State) ->
    io:format("Inside------------|DELETE|Request:~p ~n", [Request]),
    {ok, Body, _} = read_body(Request, <<>>),
    case decode_post_body(Body) of
        [{"key", Key}] ->
            cache_db:delete_cache(Key),
            send_response(204, Request);
        [] ->
            send_response(400, Request, "error", "Bad Request")
    end,
    {true, Request, State}.


-spec decode_post_body(Body:: binary()) -> DecodedList :: [{term(),term()}].
%%@doc decode body binary string
decode_post_body(Body) ->
    KeyValList = string:split(binary_to_list(Body),"&",all),
    format_key_value(KeyValList, []).

-spec format_key_value(KeyValueStrList:: list(), EmptyTupleList :: list()) -> TupleList :: list().
%%@doc seperate key value pairs and data formatting
format_key_value([], FormattedList) ->
    lists:reverse(FormattedList);
format_key_value([KeyValueStr|Rest], FormattedList) ->
    case string:split(KeyValueStr, "=", all) of
        [Key, Value] ->
            case catch list_to_integer(Value) of
                {'EXIT', _} ->
                    format_key_value(Rest, [{Key, Value}|FormattedList]);
                IntVal ->
                    format_key_value(Rest, [{Key, IntVal}|FormattedList])
            end;
        _ ->
            format_key_value([], FormattedList)
    end.


create_json_obj(Key, Value)->
    lists:concat(["{\"",Key,"\":",Value,"}"]).

send_response(StatusCode, Request) ->
    cowboy_req:reply(StatusCode, Request).

send_response(StatusCode, Request, Key, Value) ->
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, create_json_obj(Key, Value), Request).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> 
            {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> 
            read_body(Req, << Acc/binary, Data/binary >>)
    end.



