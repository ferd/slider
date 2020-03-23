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
         _}
       ],
       slider_parser:file(Cfg)
    ),
    ok.
