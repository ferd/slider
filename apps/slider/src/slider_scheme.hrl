%% Syntax highlight color scheme

-define(CLR_DEFAULT, {0,0,0}).
-define(CLR_COMMENT, {160,53,35}).
-define(CLR_VARIABLE, {150,100,40}).
-define(CLR_NUMBER, {5,5,100}).
-define(CLR_KEYWORD, {130,40,172}).
-define(CLR_STRING, {170,45,132}).
-define(CLR_OPERATOR, {30,0,0}).
-define(CLR_FUNCTION, {64,102,244}).
-define(CLR_CHAR, {236,155,172}).
-define(CLR_MACRO, {40,144,170}).
-define(CLR_RECORD, {40,100,20}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%% for C-style langs
-ifndef(wxSTC_C_STRINGRAW).
-define(wxSTC_C_STRINGRAW, 20).
-define(wxSTC_C_TRIPLEVERBATIM, 21).
-define(wxSTC_C_HASHQUOTEDSTRING, 22).
-define(wxSTC_C_PREPROCESSORCOMMENT, 23).
-define(wxSTC_C_PREPROCESSORCOMMENTDOC, 24).
-define(wxSTC_C_USERLITERAL, 25).
-define(wxSTC_C_TASKMARKER, 26).
-define(wxSTC_C_ESCAPESEQUENCE, 27).
-endif.
