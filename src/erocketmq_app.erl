%%%-------------------------------------------------------------------
%% @doc erocketmq public API
%% @end
%%%-------------------------------------------------------------------

-module(erocketmq_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erocketmq_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
