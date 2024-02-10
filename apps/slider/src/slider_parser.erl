-module(slider_parser).
-export([file/1, string/1]).

file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> string(unicode:characters_to_list(Bin));
        {error, enoent} -> {error, {not_found, Path}}
    end.

string(Str) ->
    defaults(string:lexemes(Str, [$\n, "\r\n"])).

defaults(["=== defaults ==="|Rest]) ->
    defaults(Rest, #{}).

defaults(["=== slides ===" | Lines], Defaults) ->
    slides(Lines, 1, Defaults, #{}, []);
defaults(["//"++_ | Lines], Map) ->
    defaults(Lines, Map);
defaults([Line|Lines], Map) ->
    [KS, VS] = string:split(Line, ": "),
    K = list_to_atom(lists:append(string:replace(KS, " ", "_", all))),
    defaults(Lines, handle(K, VS, Map)).

slides([], _N, _Defaults, _Opt, Acc) ->
    lists:reverse(Acc);
slides(["---" | Lines], N, Defaults, Opt, Acc) ->
    {Notes, Rest} = gather_notes(Lines),
    SlideOpts = maps:merge(maps:without([type], Defaults), maps:without([type], Opt)),
    Slide = maps:get(type, Opt, slider_base),
    SlideDefaults = slide_defaults(Slide),
    NewAcc = [{N, {Slide, maps:merge(SlideDefaults, SlideOpts)}, Notes} | Acc],
    slides(Rest, N+1, Defaults, #{}, NewAcc);
slides(["//"++_ | Lines], N, Defaults, Opt, Acc) ->
    slides(Lines, N, Defaults, Opt, Acc);
slides([Line | Lines], N, Defaults, Opt, Acc) ->
    case string:split(Line, ": ") of
        [KS, VS] ->
            K = list_to_atom(lists:append(string:replace(KS, " ", "_", all))),
            slides(Lines, N, Defaults, handle(K, VS, Opt), Acc);
        _ ->
            error({invalid_config, {slide, N}, Line})
    end.

handle(Key, [$#,R1,R2,G1,G2,B1,B2], Map) ->
    Map#{Key => {list_to_integer([R1,R2], 16),
                 list_to_integer([G1,G2], 16),
                 list_to_integer([B1,B2], 16)}};
handle(type, Str, Map) ->
    Map#{type => list_to_atom("slider_" ++ Str)};
handle(Atom, Str, Map) ->
    try list_to_integer(Str) of
        Int -> Map#{Atom => Int}
    catch
        error:badarg ->
            Map#{Atom => Str}
    end.

gather_notes(Lines) ->
    gather_notes(Lines, []).

gather_notes([], Acc) ->
    {lists:reverse(Acc), []};
gather_notes(["==="|Lines], Acc) ->
    {lists:reverse(Acc), Lines};
gather_notes(["//"++_|Lines], Acc) ->
    gather_notes(Lines, Acc);
gather_notes([Line|Lines], Acc) ->
    gather_notes(Lines, [io_lib:format("~ts~n", [Line]) | Acc]).

slide_defaults(slider_base) ->
    #{title => "", subtitle => "",
      font => "Tisa OT", text_color => {16#00, 16#00, 16#00},
      title_size => 80, subtitle_size => 55};
slide_defaults(slider_image) ->
    #{title => "",
      font => "Tisa OT", text_color => {16#00, 16#00, 16#00},
      title_size => 80};
slide_defaults(slider_code) ->
    #{title => "", source => "",
      title_font => "Tisa OT", text_color => {16#00, 16#00, 16#00},
      title_size => 80, source_size => 34,
      source_font => "Fira Code"}.


