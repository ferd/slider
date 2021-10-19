%%% Provide an abstraction for all slide types to have the following calls:
%%%
%%%  init()
%%%    |
%%%    v
%%% [unloaded] --prepare--> [loaded] --display--> [displayed]
%%%    | ^-----cleanup------'    ^-----standby-----'
%%%    |
%%%  terminate()
%%%
-module(slider_fsm).
-behaviour(gen_statem).
-define(VIA(Name), {via, gproc, {n, l, Name}}).
-record(data, {mod :: module(),
               data :: data()}).
-type data() :: term().

-callback init(wx:wx_object(), slider:slide_cfg()) -> {ok, data()}.
-callback terminate(data()) -> term().
-callback prepare(data()) -> data().
-callback display(data()) -> data().
-callback standby(data()) -> data().
-callback cleanup(data()) -> data().

-export([start_link/3, prepare/1, display/1, standby/1, cleanup/1, resize/1]).
-export([init/1, callback_mode/0, terminate/3,
         unloaded/3, loaded/3, displayed/3]).

%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%
start_link(Name, Parent, Opts) ->
    Env = wx:get_env(),
    gen_statem:start_link(?VIA(Name), ?MODULE, {Env, Parent, Opts}, []).

prepare(Name) ->
    gen_statem:cast(?VIA(Name), prepare).

display(Name) ->
    gen_statem:call(?VIA(Name), display, timer:seconds(30)).

standby(Name) ->
    gen_statem:cast(?VIA(Name), standby).

cleanup(Name) ->
    gen_statem:cast(?VIA(Name), cleanup).

resize(Name) ->
    gen_statem:call(?VIA(Name), resize).

%%%%%%%%%%%%%%%%%
%%% BEHAVIOUR %%%
%%%%%%%%%%%%%%%%%
init({Env, Parent, {Type,Opts}}) ->
    process_flag(trap_exit, true),
    wx:set_env(Env),
    {ok, Data} = Type:init(Parent, Opts),
    {ok, unloaded, #data{mod=Type, data=Data}}.

callback_mode() ->
    [state_functions].

unloaded(cast, prepare, Data=#data{mod=Type, data=CbData}) ->
    NewCbData = Type:prepare(CbData),
    {next_state, loaded, Data#data{data=NewCbData}};
unloaded({call, From}, resize, Data) ->
    {next_state, loaded, Data,
     [{reply, From, ok}]}.

loaded({call, From}, display, Data=#data{mod=Type, data=CbData}) ->
    NewCbData = Type:display(CbData),
    {next_state, displayed, Data#data{data=NewCbData},
     [{reply, From, ok}]};
loaded(cast, cleanup, Data=#data{mod=Type, data=CbData}) ->
    NewCbData = Type:cleanup(CbData),
    {next_state, unloaded, Data#data{data=NewCbData}};
loaded({call, From}, resize, Data=#data{mod=Type, data=CbData}) ->
    TmpCbData = Type:cleanup(CbData),
    NewCbData = Type:prepare(TmpCbData),
    {next_state, loaded, Data#data{data=NewCbData},
     [{reply, From, ok}]}.


displayed(cast, standby, Data=#data{mod=Type, data=CbData}) ->
    NewCbData = Type:standby(CbData),
    {next_state, loaded, Data#data{data=NewCbData}};
displayed({call, From}, resize, Data=#data{mod=Type, data=CbData}) ->
    ClearCbData = Type:standby(CbData),
    TmpCbData = Type:cleanup(ClearCbData),
    PrepCbData = Type:prepare(TmpCbData),
    NewCbData = Type:display(PrepCbData),
    {next_state, displayed, Data#data{data=NewCbData},
     [{reply, From, ok}]};
displayed(info, {'_wxe_error_',_,Call,Args}, _Data) ->
    io:format("Wx Error: ~p ~p~n", [Call, Args]),
    keep_state_and_data.

terminate(_Reason, displayed, #data{mod=Type, data=CbData}) ->
    ClearCbData = Type:standby(CbData),
    Type:cleanup(ClearCbData),
    ok;
terminate(_Reason, loaded, #data{mod=Type, data=CbData}) ->
    Type:cleanup(CbData),
    ok;
terminate(_Reason, unloaded, _) ->
    ok.
