-module(slider_parser_SUITE).
-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [load].

load(_Config) ->
    Cfg = filename:join([code:priv_dir(slider), "demo.set"]),
    ?assertMatch(
       [{1,
         {slider_base,
         #{title := "This is one of the slides", subtitle := "with a subtitle",
           background := "sloths.jpg",
           font := "Tisa OT", text_color := {16#FF, 16#FF, 16#FF},
           title_size := 80, subtitle_size := 55}},
         _},
        {2,
         {slider_base,
          #{title := "Another slide", subtitle := "with sub-titles",
            background := "fire.png",
            font := "Tisa OT", text_color := {16#FF, 16#FF, 16#FF},
            title_size := 80, subtitle_size := 55}},
         _},
        {3,
         {slider_base,
          #{title := "Third slide", subtitle := "Third subtitles",
            background := "control.jpg",
            font := "Tisa OT", text_color := {16#66, 16#FF, 16#66},
            title_size := 80, subtitle_size := 55}},
         _},
        {4,
         {slider_base,
          #{title := "Here we go!", subtitle := "To the moon! 🌕",
            background := "wind.jpg",
            font := "Tisa OT", text_color := {16#00, 16#00, 16#00},
            title_size := 80, subtitle_size := 55}},
         _},
        {5,
         {slider_code,
          #{title := "Rebalance", title_font := "Tisa OT", title_size := 80,
            text_color := {16#FF, 16#FF, 16#FF}, background := "sloths.jpg",
            source := "rebalance1.erl", source_font := "Fira Code",
            source_size := 24}},
         _},
        {6, _Python, _},
        {7, _Ruby, _},
        {8, _Elixir, _},
        {9, _Go, _},
        {10,
         {slider_image,
          #{title := "vertical image", font := "Tisa OT",
            title_size := 80, text_color := {16#FF, 16#FF, 16#FF},
            image := "sloths.jpg", background := "fire.png"}},
         _},
        {11,
         {slider_image,
          #{title := "horizontal image", font := "Tisa OT",
            title_size := 80, text_color := {16#FF, 16#FF, 16#FF},
            image := "fire.png", background := "sloths.jpg"}},
         _}
       ],
       slider_parser:file(Cfg)
    ),
    %% Comments are taken out
    {2, {_Type, Map}, Notes} = lists:keyfind(2, 1, slider_parser:file(Cfg)),
    ?assertEqual(nomatch, re:run(Notes, "//", [multiline])),
    ?assertEqual(undefined, maps:get('//text_color', Map, undefined)),
    ?assertEqual(undefined, maps:get('//subtitle', Map, undefined)),
    ok.
