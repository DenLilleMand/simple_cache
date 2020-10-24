% A module to encapsulate your key-to-pid mapping
-module(sc_store).

-export([init/0, insert/2, delete/1, lookup/1]).

-define(TABLE_ID, ?MODULE).

% This would maybe be implemented with a map
% that maps some Key => Pid, and that same Pid => Key.
% So that looking up both is some O(logn) time

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Pid}] -> {ok, Pid};
        []           -> {error, not_found}
end.

delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).


