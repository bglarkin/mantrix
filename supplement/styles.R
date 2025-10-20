# Theme BGL small
# Used throughout
# ————————————————————

theme_bgl_s =
  theme_classic() +
  theme(
    axis.title.x = element_text(
      size = 10,
      margin = unit(c(
        t = 1,
        r = 0,
        b = 0,
        l = 0
      ), "mm")
    ),
    axis.title.y = element_text(
      size = 10,
      margin = unit(c(
        t = 0,
        r = 1,
        b = 0,
        l = 0
      ), "mm")
    ),
    axis.text.x = element_text(
      size = 9,
      color = "black",
      margin = unit(c(
        t = 1.5,
        r = 0,
        b = 0,
        l = 0
      ), "mm")
    ),
    axis.text.y = element_text(
      size = 9,
      color = "black",
      margin = unit(c(
        t = 0,
        r = 1,
        b = 0,
        l = 0
      ), "mm")
    ),
    axis.ticks.length.x = unit(0, "mm"),
    axis.ticks.length.y = unit(-1.4, "mm"),
    axis.ticks.y = element_line(color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    panel.grid = element_blank(),
    # legend.title = element_blank(),
    legend.title = element_text(size = 10, color = "black"),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  ) 

# Theme elements for supplemental
# Improve the rendering of the SVG file
# ————————————————————
theme_supplement <- theme(
  axis.title.y = element_text(size = 11, family = "serif"),
  axis.text.x = element_text(size = 10, family = "serif"),
  axis.text.y = element_text(size = 10, family = "serif")
)
