-module(slider_utils).
-export([bitmap_type/1, crop/1, check_font_size/1, largest_resolution/2]).
-include_lib("wx/include/wx.hrl").

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

check_font_size(70) -> 69; % 70 is a magic value that means "default" ?!
check_font_size(S) -> S.

largest_resolution({X,Y}, Ratio) ->
    try
        [throw({RX, RY})
         || {RX,RY} <- resolutions(Ratio),
            RX =< X, RY =< Y],
        {error, no_resolution}
    catch
        R -> {ok, R}
    end.
%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

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


