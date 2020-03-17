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
defaults([Line|Lines], Map) ->
    [KS, VS] = string:split(Line, ": "),
    K = list_to_atom(lists:append(string:replace(KS, " ", "_", all))),
    defaults(Lines, handle(K, VS, Map)).

slides([], _N, _Defaults, _Opt, Acc) ->
    lists:reverse(Acc);
slides(["---" | Lines], N, Defaults, Opt, Acc) ->
    {Notes, Rest} = gather_notes(Lines),
    SlideOpts = maps:merge(Defaults, maps:without([type], Opt)),
    Slide = maps:get(type, Opt, slider_base),
    SlideDefaults = slide_defaults(Slide),
    NewAcc = [{N, {Slide, maps:merge(SlideDefaults, SlideOpts)}, Notes} | Acc],
    slides(Rest, N+1, Defaults, #{}, NewAcc);
slides([Line | Lines], N, Defaults, Opt, Acc) ->
    [KS, VS] = string:split(Line, ": "),
    K = list_to_atom(lists:append(string:replace(KS, " ", "_", all))),
    slides(Lines, N, Defaults, handle(K, VS, Opt), Acc).

handle(text_color, [$#,R1,R2,G1,G2,B1,B2], Map) ->
    Map#{text_color => {list_to_integer([R1,R2], 16),
                        list_to_integer([G1,G2], 16),
                        list_to_integer([B1,B2], 16)}};
handle(title_size, Str, Map) ->
    Map#{title_size => list_to_integer(Str)};
handle(subtitle_size, Str, Map) ->
    Map#{subtitle_size => list_to_integer(Str)};
handle(Atom, Str, Map) ->
    Map#{Atom => Str}.

gather_notes(Lines) ->
    gather_notes(Lines, []).

gather_notes([], Acc) ->
    {lists:reverse(Acc), []};
gather_notes(["==="|Lines], Acc) ->
    {lists:reverse(Acc), Lines};
gather_notes([Line|Lines], Acc) ->
    gather_notes(Lines, [io_lib:format("~ts~n", [Line]) | Acc]).

slide_defaults(slider_base) ->
    #{title => "", subtitle => "",
      font => "Tisa OT", text_color => {16#00, 16#00, 16#00},
      title_size => 80, subtitle_size => 55}.


