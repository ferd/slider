-module(slider_code).
-behaviour(slider_fsm).
-include_lib("wx/include/wx.hrl").
-include("slider_scheme.hrl").

-export([init/2, terminate/1, prepare/1, display/1, standby/1, cleanup/1]).

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
    wxStyledTextCtrl:setTabWidth(Code, 4),
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

language(".erl") -> erlang;
language(".ex") -> elixir;
language(".rb") -> ruby;
language(".go") -> go;
language(".c") -> c;
language(".h") -> c;
language(".cpp") -> c;
language(".py") -> python.

lexer(erlang) -> ?wxSTC_LEX_ERLANG;
lexer(elixir) -> ?wxSTC_LEX_RUBY; % not supported out of the box
lexer(ruby) -> ?wxSTC_LEX_RUBY;
lexer(go) -> ?wxSTC_LEX_CPP; % not supported out of the box
lexer(c) -> ?wxSTC_LEX_CPP;
lexer(python) -> ?wxSTC_LEX_PYTHON.

styles(erlang) ->
    [{?wxSTC_ERLANG_DEFAULT,           ?CLR_DEFAULT},
     {?wxSTC_ERLANG_COMMENT,           ?CLR_COMMENT},
     {?wxSTC_ERLANG_VARIABLE,          ?CLR_VARIABLE},
     {?wxSTC_ERLANG_NUMBER,            ?CLR_NUMBER},
     {?wxSTC_ERLANG_KEYWORD,           ?CLR_KEYWORD},
     {?wxSTC_ERLANG_STRING,            ?CLR_STRING},
     {?wxSTC_ERLANG_OPERATOR,          ?CLR_OPERATOR},
     {?wxSTC_ERLANG_ATOM,              ?CLR_DEFAULT},
     {?wxSTC_ERLANG_FUNCTION_NAME,     ?CLR_FUNCTION},
     {?wxSTC_ERLANG_CHARACTER,         ?CLR_CHAR},
     {?wxSTC_ERLANG_MACRO,             ?CLR_MACRO},
     {?wxSTC_ERLANG_RECORD,            ?CLR_RECORD},
     {?wxSTC_ERLANG_SEPARATOR,         ?CLR_DEFAULT},
     {?wxSTC_ERLANG_NODE_NAME,         ?CLR_DEFAULT},
     %% Optional 2.9 stuff
     {?wxSTC_ERLANG_COMMENT_FUNCTION,  ?CLR_COMMENT},
     {?wxSTC_ERLANG_COMMENT_MODULE,    ?CLR_COMMENT},
     {?wxSTC_ERLANG_COMMENT_DOC,       ?CLR_COMMENT},
     {?wxSTC_ERLANG_COMMENT_DOC_MACRO, ?CLR_COMMENT},
     {?wxSTC_ERLANG_ATOM_QUOTED,       ?CLR_DEFAULT},
     {?wxSTC_ERLANG_MACRO_QUOTED,      ?CLR_MACRO},
     {?wxSTC_ERLANG_RECORD_QUOTED,     ?CLR_RECORD},
     {?wxSTC_ERLANG_NODE_NAME_QUOTED,  ?CLR_DEFAULT},
     {?wxSTC_ERLANG_BIFS,              ?CLR_KEYWORD},
     {?wxSTC_ERLANG_MODULES,           ?CLR_FUNCTION},
     {?wxSTC_ERLANG_MODULES_ATT,       ?CLR_FUNCTION}
    ];
styles(elixir) ->
    styles(ruby);
styles(ruby) ->
    [{?wxSTC_RB_DEFAULT,      ?CLR_DEFAULT},
     {?wxSTC_RB_ERROR,        ?CLR_DEFAULT},
     {?wxSTC_RB_COMMENTLINE,  ?CLR_COMMENT},
     {?wxSTC_RB_POD,          ?CLR_DEFAULT},
     {?wxSTC_RB_NUMBER,       ?CLR_NUMBER},
     {?wxSTC_RB_WORD,         ?CLR_VARIABLE},
     {?wxSTC_RB_STRING,       ?CLR_STRING},
     {?wxSTC_RB_CHARACTER,    ?CLR_CHAR},
     {?wxSTC_RB_CLASSNAME,    ?CLR_FUNCTION},
     {?wxSTC_RB_DEFNAME,      ?CLR_FUNCTION},
     {?wxSTC_RB_OPERATOR,     ?CLR_OPERATOR},
     {?wxSTC_RB_IDENTIFIER,   ?CLR_DEFAULT},
     {?wxSTC_RB_REGEX,        ?CLR_MACRO},
     {?wxSTC_RB_GLOBAL,       ?CLR_VARIABLE},
     {?wxSTC_RB_SYMBOL,       ?CLR_VARIABLE},
     {?wxSTC_RB_MODULE_NAME,  ?CLR_FUNCTION},
     {?wxSTC_RB_INSTANCE_VAR, ?CLR_VARIABLE},
     {?wxSTC_RB_CLASS_VAR,    ?CLR_VARIABLE},
     {?wxSTC_RB_BACKTICKS,    ?CLR_MACRO},
     {?wxSTC_RB_DATASECTION,  ?CLR_STRING},
     {?wxSTC_RB_HERE_DELIM,   ?CLR_STRING},
     {?wxSTC_RB_HERE_Q,       ?CLR_STRING},
     {?wxSTC_RB_HERE_QQ,      ?CLR_STRING},
     {?wxSTC_RB_HERE_QX,      ?CLR_STRING},
     {?wxSTC_RB_STRING_Q,     ?CLR_STRING},
     {?wxSTC_RB_STRING_QQ,    ?CLR_STRING},
     {?wxSTC_RB_STRING_QX,    ?CLR_STRING},
     {?wxSTC_RB_STRING_QR,    ?CLR_STRING},
     {?wxSTC_RB_STRING_QW,    ?CLR_STRING},
     {?wxSTC_RB_WORD_DEMOTED, ?CLR_VARIABLE},
     {?wxSTC_RB_STDIN,        ?CLR_MACRO},
     {?wxSTC_RB_STDOUT,       ?CLR_MACRO},
     {?wxSTC_RB_STDERR,       ?CLR_MACRO},
     {?wxSTC_RB_UPPER_BOUND,  ?CLR_VARIABLE}];
styles(go) ->
    styles(c);
styles(c) -> % handle the whole c-style family
    [{?wxSTC_C_DEFAULT,                ?CLR_DEFAULT},
     {?wxSTC_C_COMMENT,                ?CLR_COMMENT},
     {?wxSTC_C_COMMENTLINE,            ?CLR_COMMENT},
     {?wxSTC_C_COMMENTDOC,             ?CLR_COMMENT},
     {?wxSTC_C_NUMBER,                 ?CLR_NUMBER},
     {?wxSTC_C_WORD,                   ?CLR_DEFAULT},
     {?wxSTC_C_STRING,                 ?CLR_STRING},
     {?wxSTC_C_CHARACTER,              ?CLR_CHAR},
     {?wxSTC_C_UUID,                   ?CLR_STRING},
     {?wxSTC_C_PREPROCESSOR,           ?CLR_MACRO},
     {?wxSTC_C_OPERATOR,               ?CLR_OPERATOR},
     {?wxSTC_C_IDENTIFIER,             ?CLR_VARIABLE},
     {?wxSTC_C_STRINGEOL,              ?CLR_CHAR},
     {?wxSTC_C_VERBATIM,               ?CLR_STRING},
     {?wxSTC_C_REGEX,                  ?CLR_MACRO},
     {?wxSTC_C_COMMENTLINEDOC,         ?CLR_COMMENT},
     {?wxSTC_C_WORD2,                  ?CLR_DEFAULT},
     {?wxSTC_C_COMMENTDOCKEYWORD,      ?CLR_COMMENT},
     {?wxSTC_C_COMMENTDOCKEYWORDERROR, ?CLR_COMMENT},
     {?wxSTC_C_GLOBALCLASS,            ?CLR_RECORD},
     {?wxSTC_C_STRINGRAW,              ?CLR_STRING},
     {?wxSTC_C_TRIPLEVERBATIM,         ?CLR_STRING},
     {?wxSTC_C_HASHQUOTEDSTRING,       ?CLR_STRING},
     {?wxSTC_C_PREPROCESSORCOMMENT,    ?CLR_MACRO},
     {?wxSTC_C_PREPROCESSORCOMMENTDOC, ?CLR_MACRO},
     {?wxSTC_C_USERLITERAL,            ?CLR_CHAR},
     {?wxSTC_C_TASKMARKER,             ?CLR_DEFAULT},
     {?wxSTC_C_ESCAPESEQUENCE,         ?CLR_CHAR}];
styles(python) ->
    [{?wxSTC_P_DECORATOR,    ?CLR_MACRO},
     {?wxSTC_P_WORD2,        ?CLR_KEYWORD},
     {?wxSTC_P_STRINGEOL,    ?CLR_DEFAULT},
     {?wxSTC_P_COMMENTBLOCK, ?CLR_COMMENT},
     {?wxSTC_P_IDENTIFIER,   ?CLR_VARIABLE},
     {?wxSTC_P_OPERATOR,     ?CLR_OPERATOR},
     {?wxSTC_P_DEFNAME,      ?CLR_FUNCTION},
     {?wxSTC_P_CLASSNAME,    ?CLR_FUNCTION},
     {?wxSTC_P_TRIPLEDOUBLE, ?CLR_STRING},
     {?wxSTC_P_TRIPLE,       ?CLR_STRING},
     {?wxSTC_P_WORD,         ?CLR_KEYWORD},
     {?wxSTC_P_CHARACTER,    ?CLR_CHAR},
     {?wxSTC_P_STRING,       ?CLR_STRING},
     {?wxSTC_P_NUMBER,       ?CLR_NUMBER},
     {?wxSTC_P_COMMENTLINE,  ?CLR_COMMENT},
     {?wxSTC_P_DEFAULT,      ?CLR_DEFAULT}
    ].

keywords(Lang) ->
    L = keywords_(Lang),
    lists:flatten([K ++ " " || K <- L] ++ [0]).

keywords_(erlang) ->
    ["after","begin","case","try","cond","catch","andalso","orelse",
     "end","fun","if","let","of","receive","when","bnot","not",
     "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"];
keywords_(python) ->
    ["False", "await", "else", "import", "pass", "None", "break",
     "except", "in", "raise", "True", "class", "finally", "is", "return",
     "and", "continue", "for", "lambda", "try", "as", "def", "from",
     "nonlocal", "while", "assert", "del", "global", "not", "with",
     "async", "elif", "if", "or", "yield"];
keywords_(elixir) ->
    ["true", "false", "nil", "when", "and", "or", "not", "in",
     "def", "defmodule", "if", "quote", "case", "with",
     "fn", "do", "end", "catch", "rescue", "after", "else"];
keywords_(ruby) ->
    ["BEGIN", "END", "alias", "and", "begin", "break", "case",
     "class", "def", "module", "next", "nil", "not", "or", "redo",
     "rescue", "retry", "return", "elsif", "end", "false",
     "ensure", "for", "if", "true", "undef", "unless",
     "do", "else", "super", "then", "until", "when", "while",
     "defined?", "self"];
keywords_(go) ->
    ["break", "default", "func", "interface", "select", "case", "defer",
     "go", "map", "struct", "chan", "else", "goto", "package", "switch",
     "const", "fallthrough", "if", "range", "type", "continue", "for",
     "import", "return", "var"];
keywords_(c) ->
    ["auto", "break", "case", "char", "const", "continue", "default", "do",
     "double", "else", "enum", "extern", "float", "for", "goto", "if",
     "inline", "int", "long", "register", "restrict", "return", "short",
     "signed", "sizeof", "static", "struct", "switch", "typedef", "union",
     "unsigned", "void", "volatile", "while"].
