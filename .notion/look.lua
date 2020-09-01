-- look_clean.lua drawing engine configuration file for Notion.

if not gr.select_engine("de") then return end

local active_bg_color = "#7c6f64"
local active_border_color = "#ebdbb2"
local active_shadow_color = "#504945"

local inactive_bg_color = "#504945"
local inactive_highlight_color = "#bdae93"
local inactive_shadow_color = "#282828"
local inactive_fg_color = "#bdae93"

local sel_inactive_bg_color = "#3c3836"
local sel_inactive_highlight_color = "#a89984"
local sel_inactive_shadow_color = "#1d2021"
local sel_inactive_fg_color = "#a89984"

de.reset()

de.defstyle("*", {
    shadow_colour = "grey",
    highlight_colour = "grey",
    background_colour = inactive_bg_color,
    foreground_colour = inactive_fg_color,
    padding_pixels = 1,
    highlight_pixels = 1,
    shadow_pixels = 1,
    border_style = "elevated",
    font = "xft:Iosevka:style=Regular:pixelsize=14",
    text_align = "center",
})

de.defstyle("tab", {
    border_style = "ridge",
    de.substyle("active-selected", {
        font = "xft:Iosevka:style=Heavy:pixelsize=14",
        shadow_colour = active_shadow_color,
        highlight_colour = active_border_color,
        background_colour = active_bg_color,
        foreground_colour = active_border_color,
    }),
    de.substyle("active-unselected", {
        shadow_colour = inactive_shadow_color,
        highlight_colour = inactive_highlight_color,
        background_colour = inactive_bg_color,
        foreground_colour = inactive_fg_color,
    }),
    de.substyle("inactive-selected", {
        shadow_colour = sel_inactive_shadow_color,
        highlight_colour = sel_inactive_highlight_color,
        background_colour = sel_inactive_bg_color,
        foreground_colour = sel_inactive_fg_color,
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = inactive_shadow_color,
        highlight_colour = inactive_highlight_color,
        background_colour = inactive_bg_color,
        foreground_colour = inactive_fg_color,
    }),
    text_align = "center",
})

de.defstyle("input", {
    background_colour = inactive_bg_color,
    foreground_colour = inactive_fg_color,
    de.substyle("*-cursor", {
        background_colour = inactive_fg_color,
        foreground_colour = inactive_bg_color,
    }),
    de.substyle("*-selection", {
        foreground_colour = active_border_color,
    }),
    font = "xft:Iosevka:style=Regular:pixelsize=14",
})

dopath("lookcommon_clean")
dopath("lookcommon_clean_frame")

de.defstyle("tab-menuentry", {
    padding_pixels = 7,
    shadow_colour = "gray",
    highlight_colour = "gray",
    border_style = "elevated",
    font = "xft:Iosevka:style=Regular:pixelsize=14",
})

de.defstyle("tab-menuentry-big", {
    padding_pixels = 7,
    shadow_colour = "gray",
    highlight_colour = "gray",
    border_style = "elevated",
    font = "xft:Iosevka:style=Regular:pixelsize=14",
})

de.defstyle("frame", {
    based_on = "*",
    border_style = "elevated",
    padding_colour    = "#505050",
    background_colour = "#000000",
    foreground_colour = "#ffffff",
    padding_pixels = 2,
    highlight_pixels = 1,
    shadow_pixels = 1,
})

de.defstyle("frame-ionframe", {
    based_on = "frame",
    border_style = "inlaid",
    padding_pixels = 1,
    spacing = 1,
})

de.defstyle("frame-floatframe", {
    based_on = "frame",
    border_style = "ridge",
})

gr.refresh()

