-module(slider_image).
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
                   image := ImgPath,
                   title_size := TS}) ->
    {W, H} = resolution(Parent),

    TitleFont = wxFont:new(
        slider_utils:check_font_size(trunc(H*(TS/1000))),
        ?wxFONTFAMILY_DEFAULT,
        ?wxFONTWEIGHT_NORMAL,
        ?wxFONTSTYLE_NORMAL,
        [{face, FontName},
         {encoding, ?wxFONTENCODING_UNICODE}]
    ),
    VBox = wxBoxSizer:new(?wxVERTICAL),

    Img = wxImage:new(Path, [{type, slider_utils:bitmap_type(Path)}]),
    slider_utils:crop(Img),
    wxImage:rescale(Img, W, H, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
    Bmp = wxBitmap:new(Img),
    wxImage:destroy(Img),

    FgImg = wxImage:new(ImgPath, [{type, slider_utils:bitmap_type(ImgPath)}]),
    ImgW = wxImage:getWidth(FgImg),
    ImgH = wxImage:getHeight(FgImg),
    MaxImgW = W-trunc(W/100)*5,
    MaxImgH = H-trunc(H*(3/10)),
    if ImgW =< MaxImgW, ImgH =< MaxImgH ->
        ok
     ; ImgW =< MaxImgW, ImgH > MaxImgH ->
        Ratio = MaxImgH/ImgH,
        wxImage:rescale(FgImg, trunc(ImgW * Ratio), MaxImgH,
                        [{quality, ?wxIMAGE_QUALITY_HIGH}])
     ; ImgW > MaxImgW, ImgH =< MaxImgH ->
        Ratio = MaxImgW/ImgW,
        wxImage:rescale(FgImg, MaxImgW, trunc(ImgH * Ratio),
                        [{quality, ?wxIMAGE_QUALITY_HIGH}])
     ; MaxImgH/ImgH =< MaxImgW/ImgW ->
        Ratio = MaxImgH/ImgH,
        wxImage:rescale(FgImg, trunc(ImgW * Ratio), MaxImgH,
                        [{quality, ?wxIMAGE_QUALITY_HIGH}])
     ; true ->
        Ratio = MaxImgW/ImgW,
        wxImage:rescale(FgImg, MaxImgW, trunc(ImgH * Ratio),
                        [{quality, ?wxIMAGE_QUALITY_HIGH}])
    end,
    FgBmp = wxBitmap:new(FgImg),
    wxImage:destroy(FgImg),

    Config#{fonts => TitleFont,
            bg_bitmap => Bmp,
            fg_bitmap => FgBmp,
            vbox => VBox}.

cleanup(Config = #{fonts := TitleFont,
                   bg_bitmap := Bmp,
                   fg_bitmap := FgBmp,
                   vbox := _VBox}) ->
    wxFont:destroy(TitleFont),
    wxBitmap:destroy(Bmp),
    wxBitmap:destroy(FgBmp),
    maps:without([fonts, bg_bitmap, vbox], Config).


display(Config = #{parent := Parent,
                   title := TitleTxt,
                   text_color := TxtColor,
                   fonts := TitleFont,
                   bg_bitmap := Bmp,
                   fg_bitmap := FgBmp,
                   vbox := VBox}) ->

    Bg = wxStaticBitmap:new(Parent, ?wxID_ANY, Bmp),

    {Width, Height} = resolution(Parent),

    wxFrame:setSizer(Parent, VBox), % Parent will now clean up the VBox
    TitleSpacerSize = trunc(Height/20),
    wxBoxSizer:addSpacer(VBox, TitleSpacerSize),

    Title = wxStaticText:new(Parent, ?wxID_ANY, TitleTxt,
                             [{style, ?wxTE_CENTRE bor ?wxTE_MULTILINE}]),
    wxStaticText:setFont(Title, TitleFont),
    wxStaticText:setForegroundColour(Title, TxtColor),

    {TitleW, TitleH} = wxStaticText:getBestSize(Title),
    TPad = max(0, trunc((Width - TitleW)/2)),
    HTBox = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:addSpacer(HTBox, TPad),
    wxBoxSizer:add(HTBox, Title, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

    TitlePad = TitleSpacerSize + TitleH,
    FgX = max(0, trunc((Width - wxBitmap:getWidth(FgBmp))/2)),
    FgY = max(0, TitlePad + trunc(((Height-TitlePad) - wxBitmap:getHeight(FgBmp))/3)),
    Fg = wxStaticBitmap:new(Parent, ?wxID_ANY, FgBmp, [{pos, {FgX, FgY}}]),

    wxBoxSizer:add(VBox, HTBox, [{flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

    Config#{wx_title => Title,
            wx_fg => Fg,
            wx_bg => Bg}.

standby(Config = #{wx_title := Title,
                   wx_bg := Bg,
                   wx_fg := Fg,
                   vbox := _VBox}) ->
    wxStaticText:destroy(Title),
    wxStaticBitmap:destroy(Fg),
    wxStaticBitmap:destroy(Bg),
    NewVBox = wxBoxSizer:new(?wxVERTICAL), % re-create for state to be right
    maps:without([wx_title, wx_fg, wx_bg],
                 Config#{vbox => NewVBox}).


resolution(Parent) ->
    wxFrame:getSize(Parent).

