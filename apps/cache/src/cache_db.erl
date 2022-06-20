-module(cache_db).

-export([insert_or_update_cache/2, 
        get_cache/1, 
        delete_cache/1]).

insert_or_update_cache(Key, Value) ->
    ets:insert(cache_tbl, {Key, Value}).

get_cache(Key) ->
    case ets:lookup(cache_tbl, Key) of
		[] ->
			{error, not_found};
		[{Key, Value}] ->
			{ok, Value}
	end.

delete_cache(Key) ->
    ets:delete(cache_tbl, Key).

