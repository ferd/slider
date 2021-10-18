-module(slider_base).
-behaviour(slider_fsm).
-include_lib("wx/include/wx.hrl").

-export([init/2, terminate/1, prepare/1, display/1, standby/1, cleanup/1]).

init(Parent, Config) ->
    {ok, Config#{parent => Parent}}.

terminate(_) ->
    ok.

prepare(Config = #{parent := Parent,
                   background := Path,
                   font := FontName,
                   title_size := TS,
                   subtitle_size := STS}) ->
    {W, H} = resolution(Parent),

    TitleFont = wxFont:new(
        slider_utils:check_font_size(trunc(H*(TS/1000))),
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, FontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    SubTitleFont = wxFont:new(
        slider_utils:check_font_size(trunc(H*(STS/1000))),
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, FontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    VBox = wxBoxSizer:new(?wxVERTICAL),

    Img = wxImage:new(Path, [{type, slider_utils:bitmap_type(Path)}]),
    Cropped = slider_utils:crop(Img),
    wxImage:rescale(Cropped, W, H, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
    Bmp = wxBitmap:new(Cropped),
    wxImage:destroy(Img),
    Config#{fonts => {TitleFont, SubTitleFont},
            bg_bitmap => Bmp,
            vbox => VBox}.

cleanup(Config = #{fonts := {TitleFont, SubTitleFont},
                   bg_bitmap := Bmp,
                   vbox := _VBox}) ->
    wxFont:destroy(SubTitleFont),
    wxFont:destroy(TitleFont),
    wxBitmap:destroy(Bmp),
    maps:without([fonts, bg_bitmap, vbox], Config).


display(Config = #{parent := Parent,
                   title := TitleTxt,
                   subtitle := SubTitleTxt,
                   text_color := TxtColor,
                   fonts := {TitleFont, SubTitleFont},
                   bg_bitmap := Bmp,
                   vbox := VBox}) ->

    Bg = wxStaticBitmap:new(Parent, ?wxID_ANY, Bmp),

    {Width, Height} = resolution(Parent),

    wxFrame:setSizer(Parent, VBox), % Parent will now clean up the VBox
    wxBoxSizer:addSpacer(VBox, trunc(2*Height/5)),

    Title = wxStaticText:new(Parent, ?wxID_ANY, TitleTxt,
                             [{style, ?wxTE_CENTRE bor ?wxTE_MULTILINE}]),
    wxStaticText:setFont(Title, TitleFont),
    wxStaticText:setForegroundColour(Title, TxtColor),

    SubTitle = wxStaticText:new(Parent, ?wxID_ANY, SubTitleTxt,
                                [{style, ?wxTE_CENTRE bor ?wxTE_MULTILINE}]),
    wxStaticText:setFont(SubTitle, SubTitleFont),
    wxStaticText:setForegroundColour(SubTitle, TxtColor),

    {TitleW, _} = wxStaticText:getBestSize(Title),
    TPad = max(0, trunc((Width - TitleW)/2)),
    HTBox = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:addSpacer(HTBox, TPad),
    wxBoxSizer:add(HTBox, Title, [{flag, ?wxEXPAND}]),

    {SubTitleW, _} = wxStaticText:getBestSize(SubTitle),
    STPad = max(0, trunc((Width - SubTitleW)/2)),
    HSTBox = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:addSpacer(HSTBox, STPad),
    wxBoxSizer:add(HSTBox, SubTitle, [{flag, ?wxEXPAND}]),

    wxBoxSizer:add(VBox, HTBox, [{flag, ?wxEXPAND}]),
    wxBoxSizer:add(VBox, HSTBox, [{flag, ?wxEXPAND}]),

    Config#{wx_title => Title,
            wx_subtitle => SubTitle,
            wx_bg => Bg}.

standby(Config = #{wx_title := Title,
                   wx_subtitle := SubTitle,
                   wx_bg := Bg,
                   vbox := _VBox}) ->
    wxStaticText:destroy(Title),
    wxStaticText:destroy(SubTitle),
    wxStaticBitmap:destroy(Bg),
    NewVBox = wxBoxSizer:new(?wxVERTICAL), % re-create for state to be right
    maps:without([wx_title, wx_subtitle, wx_bg],
                 Config#{vbox => NewVBox}).


resolution(Parent) ->
    wxFrame:getSize(Parent).
