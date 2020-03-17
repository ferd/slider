%%%-------------------------------------------------------------------
%% @doc slider public API
%% @end
%%%-------------------------------------------------------------------

-module(slider_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    slider:setup(),
    slider_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
