% The user API; the application's face to the outside
-module(simple_cache).

-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    case sc_store:lookup(Key) of
        {ok, Pid} -> sc_element:replace(Pid, Value);
        {error, _} ->
            {ok, Pid} = sc_element:create(Value),
            sc_store:insert(Key, Pid)
    end.

% lookup uses some exception API, because we use gen_server,
% any call to gen_server:call(<deleted pid>, ...) will throw an
% exception. That means that we don't timeout in the case where
% some process has looked up the key + Pid just as we terminate
% the process. In that case we just return a not_found. And how
% could the client know about those race conditions anyway, doesn't
% matter really. Replace being called in this same case does not matter,
% because async functions just always return ok regardless of the result
lookup(Key) ->
    try
        {ok, Pid} = sc_store:lookup(Key),
        {ok, Value} = sc_element:fetch(Pid),
        {ok, Value}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

delete(Key) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_element:delete(Pid);
        {error, _Reason} ->
            ok
    end.


