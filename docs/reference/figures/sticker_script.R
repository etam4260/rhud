library(hexSticker)

# Build for the hud logo sticker
# Make sure the working directory is in ./hudr
# Check this with getwd()
sticker("./man/figures/urban_city.png", package = "rhud", p_size=20, s_x=1, s_y=.85,
        s_width=.65, s_height= 1, h_fill = "#87cefa", h_color = "white",
        p_color = "white", filename="./man/figures/logo.png")

