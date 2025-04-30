# Theme BGL
# ————————————————————

# theme_bgl =
#   theme_classic() +
#   theme(
#     plot.margin = unit(c(
#       t = 2,
#       r = 2,
#       b = 4,
#       l = 4
#     ), "mm"),
#     axis.title.x = element_text(
#       size = 0,
#       face = 1,
#       margin = unit(c(
#         t = 0,
#         r = 0,
#         o = 0,
#         b = 0
#       ), "mm")
#     ),
#     axis.title.y = element_text(
#       size = 12,
#       face = 1,
#       margin = unit(c(
#         t = 0,
#         r = 4,
#         o = 0,
#         b = 0
#       ), "mm")
#     ),
#     axis.text.x = element_text(
#       size = 9,
#       face = 1,
#       margin = unit(c(
#         t = 3,
#         r = 0,
#         o = 0,
#         b = 0
#       ), "mm")
#     ),
#     axis.text.y = element_text(
#       size = 9,
#       face = 1,
#       margin = unit(c(
#         t = 0,
#         r = 3,
#         o = 0,
#         b = 0
#       ), "mm")
#     ),
#     axis.ticks.length = unit(-2, "mm"),
#     strip.text = element_text(size = 12, face = 2),
#     plot.caption = element_text(size = 5, face = 2),
#     plot.title = element_text(size = 14, face = 2),
#     text = element_text(family = "Times"),
#     legend.text = element_text(size = 12, face = 1),
#     legend.title = element_text(size = 12, face = 1),
#     legend.position = "right",
#     panel.grid = element_blank()
#   )

# Theme BGL small
# Used in mantis obs plot; others where space is an issue
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
      margin = unit(c(
        t = 0,
        r = 1,
        b = 0,
        l = 0
      ), "mm")
    ),
    axis.ticks.length.x = unit(0, "mm"),
    axis.ticks.length.y = unit(-1.4, "mm"),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    # legend.title = element_blank(),
    legend.title = element_text(size = 10),
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
