-- look_clean.lua drawing engine configuration file for Notion.

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    shadow_colour = "grey",
    highlight_colour = "grey",
    background_colour = "#b57615",
    foreground_colour = "#282828",
    padding_pixels = 1,
    highlight_pixels = 1,
    shadow_pixels = 1,
    border_style = "elevated",
    font = "-*-terminus-medium-r-*-*-*-140-*-*-*-*-paratype-*",
    text_align = "center",
})

de.defstyle("tab", {
    font = "-*-terminus-medium-r-*-*-*-140-*-*-*-*-paratype-*",
    de.substyle("active-selected", {
        shadow_colour = "white",
        highlight_colour = "white",
        background_colour = "#d79921",
        foreground_colour = "#282828",
    }),
    de.substyle("active-unselected", {
        shadow_colour = "grey",
        highlight_colour = "grey",
        background_colour = "#b57615",
        foreground_colour = "#fbf1c7",
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "grey",
        highlight_colour = "grey",
        background_colour = "#b57615",
        foreground_colour = "#fbf1c7",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "grey",
        highlight_colour = "grey",
        background_colour = "#b57615",
        foreground_colour = "#fbf1c7",
    }),
    text_align = "center",
})

de.defstyle("input", {
    foreground_colour = "white",
    de.substyle("*-cursor", {
        background_colour = "#d79921",
        foreground_colour = "#282828",
    }),
    de.substyle("*-selection", {
        background_colour = "#b57615",
        foreground_colour = "#282828",
    }),
    font = "-*-terminus-medium-r-*-*-*-140-*-*-*-*-paratype-*",
})

dopath("lookcommon_clean")
dopath("lookcommon_clean_frame")

de.defstyle("tab-menuentry-big", {
    padding_pixels = 7,
    font = "-*-terminus-medium-r-*-*-*-140-*-*-*-*-paratype-*",
})

de.defstyle("frame", {
    based_on = "*",
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

