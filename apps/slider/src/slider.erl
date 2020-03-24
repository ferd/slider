-module(slider).
-export([setup/0]).
%% TODO: check all files before start

-type slide() :: {slider_base, base()}
               | {slider_code, code()}
               | {slider_image, image()}.
-type slide_cfg() :: base() | code() | image(). % simpler typing for callbacks
-type base() :: #{
                    title := unicode:chardata(),
                    subtitle := unicode:chardata(),
                    background := file:filename_all(),
                    font := unicode:chardata(),
                    text_color := color(),
                    title_size := font_size(),
                    subtitle_size := font_size()
                 }.
-type code() :: #{
                    title := unicode:chardata(),
                    title_font := unicode:chardata(),
                    source := unicode:chardata(),
                    source_font := unicode:chardata(),
                    background := file:filename_all(),
                    text_color := color(),
                    title_size := font_size(),
                    source_size := font_size()
                 }.
-type image() :: #{
                    title := unicode:chardata(),
                    background := file:filename_all(),
                    image := file:filename_all(),
                    font := unicode:chardata(),
                    text_color := color(),
                    title_size := font_size()
                 }.
-type color() :: {0..255, 0..255, 0..255}.
-type font_size() :: 1..1000. % in 1/1000th of screen height, to allow auto-scaling
-export_type([slide/0, slide_cfg/0, base/0, code/0, color/0, image/0]).

-include_lib("wx/include/wx.hrl").
-define(SLIDE_ID, 1).
-define(PREVIEW_ID, 2).
-define(NOTE_ID, 3).
-define(LEFT_ARROW, 314).
-define(RIGHT_ARROW, 316).

setup() ->
    spawn(fun() -> setup_() end).

-spec setup_() -> no_return().
setup_() ->
    Server = wx:new(),
    Resolution = case os:getenv("RESOLUTION") of
        false ->
            {1280, 720};
            %{1024, 576},
        StrRes ->
            [WStr, HStr] = string:lexemes(StrRes, "x"),
            {list_to_integer(WStr), list_to_integer(HStr)}
    end,
    SlideFrame = wxFrame:new(
        Server, ?SLIDE_ID, "Slides",
        [{size, Resolution},
         {style, (?wxDEFAULT_FRAME_STYLE bor ?wxWANTS_CHARS)
                 band (bnot ?wxSYSTEM_MENU)}]
    ),
    NoteFrame = wxFrame:new(Server, ?NOTE_ID, "Notes"),
    %% used to get key bindings
    transparent_pane() andalso wxPanel:new(SlideFrame),
    wxPanel:new(NoteFrame),

    file:set_cwd(os:getenv("DECK_CWD", "./")),
    SetFile = case filelib:wildcard("*.set") of
        [CwdFile|_] ->
            CwdFile;
        [] ->
            SetBase = application:get_env(slider, set, code:priv_dir(slider)),
            file:set_cwd(SetBase),
            [CfgFile|_] = filelib:wildcard(filename:join([SetBase, "*.set"])),
            CfgFile
    end,
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
    evt_loop(undefined, {{[], ID, IDs}, Frames}).

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

-ifdef(TRANSPARENT_PANE).
transparent_pane() -> true.
-else.
transparent_pane() -> false.
-endif.
