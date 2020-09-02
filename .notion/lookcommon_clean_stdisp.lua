-- Common stdisp configuration for the "clean styles"

de.defstyle("stdisp", {
    border_sides = "tb",
    border_style = "elevated",

    shadow_pixels = 0,
    highlight_pixels = 0,
    text_align = "left",
    transparent_background = true,
    background_colour = "#000000",
    foreground_colour = "grey",

    de.substyle("important", {
        foreground_colour = "green",
    }),

    de.substyle("critical", {
        foreground_colour = "red",
    }),
})
