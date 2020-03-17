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
        check_font_size(trunc(H*(TS/1000))),
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, FontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    SubTitleFont = wxFont:new(
        check_font_size(trunc(H*(STS/1000))),
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, FontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    VBox = wxBoxSizer:new(?wxVERTICAL),

    Img = wxImage:new(Path, [{type, bitmap_type(Path)}]),
    Cropped = crop(Img),
    wxImage:rescale(Cropped, W, H, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
    Bmp = wxBitmap:new(Cropped),
    wxImage:destroy(Cropped),
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

    {_Width, Height} = resolution(Parent),

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

    wxBoxSizer:add(VBox, Title, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),
    wxBoxSizer:add(VBox, SubTitle, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

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

bitmap_type(Path) ->
    case filename:extension(Path) of
        ".jpg" -> ?wxBITMAP_TYPE_JPEG;
        ".jpeg" -> ?wxBITMAP_TYPE_JPEG;
        ".png" -> ?wxBITMAP_TYPE_PNG
    end.

crop(Img) ->
    W = wxImage:getWidth(Img),
    H = wxImage:getHeight(Img),
    case largest_resolution({W,H}, {16,9}) of
        {ok, Resolution = {NewW, NewH}} ->
            {TopX, TopY} = center_corner({W,H}, Resolution),
            wxImage:getSubImage(Img, {TopX, TopY, NewW, NewH});
        {error, no_resolution} ->
            Img
    end.

largest_resolution({X,Y}, Ratio) ->
    try
        [throw({RX, RY})
         || {RX,RY} <- resolutions(Ratio),
            RX =< X, RY =< Y],
        {error, no_resolution}
    catch
        R -> {ok, R}
    end.

center_corner({AW,AH}, {BW, BH}) ->
    {if AW - BW < 2 -> 0;
        true -> trunc((AW-BW)/2)
     end,
     if AH - BH < 2 -> 0;
        true -> trunc((AH-BH)/2)
     end}.

resolutions({16,9}) ->
    [{15360, 8640}, % 16K UHD
     {7680, 4320}, % 8K UHD
     {5120, 2880}, % 5K
     {4096, 2304},
     {3840, 2160}, % 4K
     {3200, 1800}, % QHD+
     {2880, 1620},
     {2560, 1440}, % QHD
     {2048, 1152},
     {1920, 1080}, % Full HD
     {1600, 900},  % HD+
     {1366, 768},  % WXGA
     {1280, 720},  % HD
     {1024, 576}].

check_font_size(70) -> 69; % 70 is a magic value that means "default" ?!
check_font_size(S) -> S.
