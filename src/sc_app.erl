% The application behaviour implementation
-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    % this initialization will normally happen
    % in supervisor (although it is frowned upon)
    sc_store:init(),
    case sc_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.


stop(_State) -> ok.

