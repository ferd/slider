-module(slider).
-export([setup/0]).
%% TODO: check all files before start

-type slide() :: {slider_base, #{
                    title := unicode:chardata(),
                    subtitle := unicode:chardata(),
                    background := file:filename_all(),
                    font => unicode:chardata(),
                    text_color => color(),
                    %% They're thousandths in 1..1000 for screen height!
                    title_size => 1..1000,
                    subtitle_size => 1..1000
                  }}.
-type color() :: {0..255, 0..255, 0..255}.
-export_type([slide/0, color/0]).

-include_lib("wx/include/wx.hrl").
-define(SLIDE_ID, 1).
-define(PREVIEW_ID, 2).
-define(NOTE_ID, 3).
-define(LEFT_ARROW, 314).
-define(RIGHT_ARROW, 316).

setup() ->
    spawn(fun() -> setup_() end).

setup_() ->
    Server = wx:new(),
    %Resolution = {1280, 720},
    Resolution = {1024, 576},
    SlideFrame = wxFrame:new(
        Server, ?SLIDE_ID, "Slides",
        [{size, Resolution},
         {style, ?wxMINIMIZE_BOX bor ?wxMAXIMIZE_BOX bor ?wxCLOSE_BOX bor
          ?wxRESIZE_BORDER bor ?wxFRAME_TOOL_WINDOW bor ?wxWANTS_CHARS}]
    ),
    NoteFrame = wxFrame:new(Server, ?NOTE_ID, "Notes"),
    %% used to get key bindings
    wxPanel:new(SlideFrame),
    wxPanel:new(NoteFrame),
    SetBase = application:get_env(slider, set, code:priv_dir(slider)),
    file:set_cwd(SetBase),
    [SetFile|_] = filelib:wildcard(filename:join([SetBase, "*.set"])),
    Set = slider_parser:file(SetFile),
    [slider_fsm:start_link(N, SlideFrame, Slide) || {N, Slide, _Note} <- Set],
    Frames = [SlideFrame, NoteFrame],
    show(Frames),
    wxFrame:connect(SlideFrame, size),
    wxFrame:connect(SlideFrame, char_hook),
    wxFrame:connect(NoteFrame, char_hook),
    %wxFrame:connect(SlideFrame, char),
    %wxFrame:connect(SlideFrame, key_down),
    %wxFrame:connect(SlideFrame, key_up),
    [ID|IDs] = [N || {N, _Slide, _Note} <- Set],
    slider_fsm:prepare(1),
    slider_fsm:display(1),
    IDs =/= [] andalso slider_fsm:prepare(2),
    layout(Frames),
    slider_notes:start_link(NoteFrame, [{N, Note} || {N, _Slide, Note} <- Set]),
    evt_loop(undefined, {{[], ID, IDs}, Frames}),
    ok.

evt_loop(undefined, Slides) ->
    receive
        Evt ->
            evt_loop(Evt, Slides)
    end;
evt_loop(Prev, Slides) ->
    receive
        Evt -> evt_loop(Evt, Slides)
    after 0 ->
        NewSlides = handle_evt(Prev, Slides),
        evt_loop(undefined, NewSlides)
    end.

handle_evt(#wx{id=?SLIDE_ID, event=#wxSize{size={_W, _H}}}, Slides) ->
    resize(Slides),
    Slides;
handle_evt(#wx{id=_, event=#wxKey{keyCode=?LEFT_ARROW}}, Slides) ->
    shift_left(Slides);
handle_evt(#wx{id=_, event=#wxKey{keyCode=?RIGHT_ARROW}}, Slides) ->
    shift_right(Slides);
handle_evt(#wx{id=_, event=#wxKey{keyCode=_}}, Slides) ->
    %% ignore this key binding
    Slides;
handle_evt(Other, Slides) ->
    io:format("unknown event ~p~n", [Other]),
    Slides.

resize({{[], Current, []}, Frames}) ->
    slider_fsm:resize(Current),
    layout(Frames);
resize({{[Prev|_], Current, []}, Frames}) ->
    slider_fsm:resize(Prev),
    slider_fsm:resize(Current),
    layout(Frames);
resize({{[], Current, [Next|_]}, Frames}) ->
    slider_fsm:resize(Next),
    slider_fsm:resize(Current),
    layout(Frames);
resize({{[Prev|_], Current, [Next|_]}, Frames}) ->
    slider_fsm:resize(Next),
    slider_fsm:resize(Prev),
    slider_fsm:resize(Current),
    layout(Frames).

shift_left({{[], _, _}, _} = State) ->
    State;
shift_left({{[Prev|Ps], Current, []}, Frames}) ->
    slider_fsm:standby(Current),
    case Ps of
        [NewPrev|_] -> slider_fsm:prepare(NewPrev);
        _ -> ok
    end,
    slider_fsm:display(Prev),
    slider_notes:display(Prev),
    layout(Frames),
    {{Ps, Prev, [Current]}, Frames};
shift_left({{[Prev|Ps], Current, [Next|Ns]}, Frames}) ->
    slider_fsm:standby(Current),
    slider_fsm:cleanup(Next),
    case Ps of
        [NewPrev|_] -> slider_fsm:prepare(NewPrev);
        _ -> ok
    end,
    slider_fsm:display(Prev),
    slider_notes:display(Prev),
    layout(Frames),
    {{Ps, Prev, [Current,Next|Ns]}, Frames}.

shift_right({{_, _, []}, _} = State) ->
    State;
shift_right({{[], Current, [Next|Ns]}, Frames}) ->
    slider_fsm:standby(Current),
    case Ns of
        [NewNext|_] -> slider_fsm:prepare(NewNext);
        _ -> ok
    end,
    slider_fsm:display(Next),
    slider_notes:display(Next),
    layout(Frames),
    {{[Current], Next, Ns}, Frames};
shift_right({{[Prev|Ps], Current, [Next|Ns]}, Frames}) ->
    slider_fsm:standby(Current),
    slider_fsm:cleanup(Prev),
    case Ns of
        [NewNext|_] -> slider_fsm:prepare(NewNext);
        _ -> ok
    end,
    slider_fsm:display(Next),
    slider_notes:display(Next),
    layout(Frames),
    {{[Current,Prev|Ps], Next, Ns}, Frames}.

show(List) ->
    [wxFrame:show(Obj) || Obj <- List],
    ok.

layout(List) ->
    [wxFrame:layout(Obj) || Obj <- List],
    ok.

