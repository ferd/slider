-module(slider_notes).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").

-record(state, {parent, wx, notes}).

-export([start_link/2, display/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Parent, Notes) ->
    Env = wx:get_env(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Env, Parent, Notes}, []).

display(N) ->
    gen_server:call(?MODULE, {display, N}).

init({Env, Parent, Notes}) ->
    process_flag(trap_exit, true),
    wx:set_env(Env),
    Font = wxFont:new(
        20,
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{encoding, ?wxFONTENCODING_UNICODE}]
    ),
    Style = wxTextAttr:new(),
    wxTextAttr:setFont(Style, Font),

    NotesMap = maps:from_list(Notes),
    First = maps:get(1, NotesMap, ""),
    {W,H} = wxFrame:getSize(Parent),
    Wx = wxTextCtrl:new(Parent, ?wxID_ANY, [
        {style, ?wxTE_MULTILINE bor ?wxTE_READONLY},
        {size, {W,H}}
    ]),
    wxTextCtrl:setDefaultStyle(Wx, Style),
    wxTextCtrl:setValue(Wx, First),
    wxTextAttr:destroy(Style),
    wxFont:destroy(Font),
    {ok, #state{parent = Parent, wx = Wx, notes = NotesMap}}.

handle_call({display, N}, _From, S=#state{wx=Wx, notes=Notes, parent=P}) ->
    wxTextCtrl:setSize(Wx, wxFrame:getSize(P)),
    wxTextCtrl:setValue(Wx, maps:get(N, Notes, "")),
    {reply, ok, S}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, #state{wx=Wx}) ->
    wxTextCtrl:destroy(Wx).
