-module(slider_code).
-behaviour(slider_fsm).
-include_lib("wx/include/wx.hrl").

-export([init/2, terminate/1, prepare/1, display/1, standby/1, cleanup/1]).

%% For wx-2.9 usage, from erlang's debugger
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

init(Parent, Config) ->
    {ok, Config#{parent => Parent}}.

terminate(_) ->
    ok.

prepare(Config = #{parent := Parent,
                   background := Path,
                   title_font := FontName,
                   title_size := TS,
                   source := SrcFile,
                   source_size := SS,
                   source_font := SrcFontName}) ->
    {W, H} = resolution(Parent),

    TitleFont = wxFont:new(
        slider_utils:check_font_size(trunc(H*(TS/1000))),
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, FontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    SrcFont = wxFont:new(
        slider_utils:check_font_size(trunc(H*(SS/1000))),
        ?wxFONTFAMILY_TELETYPE,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, SrcFontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    {ok, Code} = file:read_file(SrcFile),
    VBox = wxBoxSizer:new(?wxVERTICAL),

    Img = wxImage:new(Path, [{type, slider_utils:bitmap_type(Path)}]),
    Cropped = slider_utils:crop(Img),
    wxImage:rescale(Cropped, W, H, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
    Bmp = wxBitmap:new(Cropped),
    wxImage:destroy(Cropped),
    wxImage:destroy(Img),
    Config#{fonts => {TitleFont, SrcFont},
            bg_bitmap => Bmp,
            code => Code,
            vbox => VBox}.

cleanup(Config = #{fonts := {TitleFont, SrcFont},
                   bg_bitmap := Bmp,
                   vbox := _VBox}) ->
    wxFont:destroy(SrcFont),
    wxFont:destroy(TitleFont),
    wxBitmap:destroy(Bmp),
    maps:without([fonts, bg_bitmap, vbox, code], Config).


display(Config = #{parent := Parent,
                   title := TitleTxt,
                   text_color := TxtColor,
                   fonts := {TitleFont, SrcFont},
                   bg_bitmap := Bmp,
                   code := CodeTxt,
                   vbox := VBox}) ->

    Bg = wxStaticBitmap:new(Parent, ?wxID_ANY, Bmp),

    {Width, Height} = resolution(Parent),

    wxFrame:setSizer(Parent, VBox), % Parent will now clean up the VBox
    wxBoxSizer:addSpacer(VBox, trunc(Height/20)),

    Title = wxStaticText:new(Parent, ?wxID_ANY, TitleTxt,
                             [{style, ?wxTE_CENTRE bor ?wxTE_MULTILINE}]),
    wxStaticText:setFont(Title, TitleFont),
    wxStaticText:setForegroundColour(Title, TxtColor),

    CodeW = Width-trunc(Width/100)*5,
    CodeH = Height-trunc(Height/20)*5,
    Code = wxStyledTextCtrl:new(Parent, [{size, {CodeW, CodeH}}]),
    wxStyledTextCtrl:styleClearAll(Code),
    wxStyledTextCtrl:styleSetFont(Code, ?wxSTC_STYLE_DEFAULT, SrcFont),
    wxStyledTextCtrl:setMarginType(Code, 0, ?wxSTC_MARGIN_NUMBER),
    wxStyledTextCtrl:setMarginWidth(Code, 0, 0),
    wxStyledTextCtrl:setUseHorizontalScrollBar(Code, false),
    wxStyledTextCtrl:setUseVerticalScrollBar(Code, false),
    wxStyledTextCtrl:setText(Code, CodeTxt),
    set_styles(Code, SrcFont, filename:extension(maps:get(source, Config))),

    {TitleW, _} = wxStaticText:getBestSize(Title),
    TPad = max(0, trunc((Width - TitleW)/2)),
    HTBox = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:addSpacer(HTBox, TPad),
    wxBoxSizer:add(HTBox, Title, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

    %{SubTitleW, _} = wxStaticText:getBestSize(SubTitle),
    CodePad = max(0, trunc((Width - CodeW)/2)),
    HCBox = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:addSpacer(HCBox, CodePad),
    wxBoxSizer:add(HCBox, Code, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

    wxBoxSizer:add(VBox, HTBox, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),
    wxBoxSizer:add(VBox, HCBox, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

    Config#{wx_title => Title,
            wx_code => Code,
            wx_bg => Bg}.

standby(Config = #{wx_title := Title,
                   wx_bg := Bg,
                   wx_code := Code,
                   vbox := _VBox}) ->
    wxStaticText:destroy(Title),
    wxStyledTextCtrl:destroy(Code),
    wxStaticBitmap:destroy(Bg),
    NewVBox = wxBoxSizer:new(?wxVERTICAL), % re-create for state to be right
    maps:without([wx_title, wx_code, wx_bg],
                 Config#{vbox => NewVBox}).


resolution(Parent) ->
    wxFrame:getSize(Parent).

set_styles(Code, Font, Ext) ->
    Lang = language(Ext),
    wxStyledTextCtrl:setLexer(Code, lexer(Lang)),
    lists:foreach(fun({Style, Color}) ->
        wxStyledTextCtrl:styleSetFont(Code, Style, Font),
        wxStyledTextCtrl:styleSetForeground(Code, Style, Color)
    end, styles(Lang)),
    wxStyledTextCtrl:setKeyWords(Code, 0, keywords(Lang)).

language(".erl") -> erlang.

lexer(erlang) -> ?wxSTC_LEX_ERLANG.

styles(erlang) ->
    [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
     {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
     {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
     {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
     {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
     {?wxSTC_ERLANG_STRING,   {170,45,132}},
     {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
     {?wxSTC_ERLANG_ATOM,     {0,0,0}},
     {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
     {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
     {?wxSTC_ERLANG_MACRO,    {40,144,170}},
     {?wxSTC_ERLANG_RECORD,   {40,100,20}},
     {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
     {?wxSTC_ERLANG_NODE_NAME,{0,0,0}},
     %% Optional 2.9 stuff
     {?wxSTC_ERLANG_COMMENT_FUNCTION, {160,53,35}},
     {?wxSTC_ERLANG_COMMENT_MODULE, {160,53,35}},
     {?wxSTC_ERLANG_COMMENT_DOC, {160,53,35}},
     {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160,53,35}},
     {?wxSTC_ERLANG_ATOM_QUOTED, {0,0,0}},
     {?wxSTC_ERLANG_MACRO_QUOTED, {40,144,170}},
     {?wxSTC_ERLANG_RECORD_QUOTED, {40,100,20}},
     {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0,0,0}},
     {?wxSTC_ERLANG_BIFS, {130,40,172}},
     {?wxSTC_ERLANG_MODULES, {64,102,244}},
     {?wxSTC_ERLANG_MODULES_ATT, {64,102,244}}
    ].

keywords(erlang) ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
         "end","fun","if","let","of","receive","when","bnot","not",
         "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).
