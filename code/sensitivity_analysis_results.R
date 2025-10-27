#' ---
#' title: "Sensitivity Analysis Results from MANTRIX"
#' author: "Beau Larkin"
#' date: "Last updated: `r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   github_document:
#'     toc: true
#'     toc_depth: 2
#'     fig_width: 7
#'     fig_height: 4
#' ---
#' 
#' # Description
#' This script summarizes sensitivity analysis results for MANTRIX using output from the
#' [MPG Matrix tool](https://matrix.mpgranch.com). Figures 4 and 5 visualize predicted
#' percent changes in mantid and trophic guild abundance under alternative scenarios, which
#' considered mantids, their natural enemies, and abiotic conditions.
#' Results are based on paired manipulations of *Mantis religiosa* abundance, 
#' cold snap frequencies, and parasitoid wasp presence/absence.
#' 
#' # Packages and Libraries
#+ packages
packages_needed <- c("tidyverse", "colorspace", "knitr", "ggpubr", "Cairo", "rprojroot")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)
#+ root_path
root_path <- function(...) rprojroot::find_rstudio_root_file(...)
#' 
#' ## Plot Style
#+ theme
source(root_path("supplement", "styles.R"))
fig_pt_size <- 3
fig_dodgewidth <- 0.7
guild_labs <- c(
  "Invertebrate\nPredators",
  "Invertebrate\nHerbivores",
  "Pollinators",
  "Songbirds",
  "Small\nMammals"
)

#' 
#' # Sensitivity Analysis Results
#' ## Data ETL
#' Extract, transform, and load data for later use
sens_path <- root_path("data", "sensitivity_analysis_data")
sens_files <- list.files(sens_path, full.names = TRUE, pattern = "matrix_export")
sens_list <- map(sens_files, read_csv, show_col_types = FALSE)
sens_scenarios <- regmatches(sens_files, regexpr("S\\d+", sens_files))
names(sens_list) <- sens_scenarios
df_names <- c(
  "name", "rel_abund", "rel_abund_lc", "rel_abund_uc",
  "abund", "abund_lc", "abund_uc",
  "pct_change", "pct_change_lc", "pct_change_uc"
)
sens_list <- map(sens_list, function(df) {
  colnames(df) <- df_names
  df %>%
    mutate(
      across(starts_with("pct"), ~ round(.x * 100, 1)),
      name = case_match(
        name,
        "Inv. Herbivores" ~ "Invertebrate Herbivores",
        "Inv. Predators"  ~ "Invertebrate Predators",
        "Sm. Mammals"     ~ "Small Mammals",
        .default          = name
      )
    ) %>%
    filter(name %in% c("Mantids", "Invertebrate Predators", "Songbirds",
                       "Small Mammals", "Invertebrate Herbivores", "Pollinators")) %>%
    mutate(name = factor(name, levels = c(
      "Mantids", "Invertebrate Predators", "Invertebrate Herbivores",
      "Pollinators", "Songbirds", "Small Mammals"
    ), ordered = TRUE)) %>%
    select(name, starts_with("pct")) %>%
    arrange(name)
})

#' 
#' Scenario Labels
#+ scenarios
scenario_names <- data.frame(
  scenario = sens_scenarios,
  scenario_labels = c(
    "Observed-Low", "Predicted Equilibrium", "Observed-Mid", "Observed-High",
    "No Cold Snaps, No Wasps", "Four Cold Snaps, No Wasps",
    "No Cold Snaps, 6X Wasps", "Two Cold Snaps, 6X Wasps",
    "Four Cold Snaps, 6X Wasps"
  )
) %>%
  separate_wider_position(scenario, widths = c(1, s_num = 1), cols_remove = FALSE) %>%
  mutate(
    s_num = as.numeric(s_num),
    scenario_labels = fct_reorder(scenario_labels, s_num)
  )

#' 
#' # Figure 4 – Manipulated mantid abundance
#+ fig4_data
fig4_data <- bind_rows(
  list(S1 = sens_list$S1, S2 = sens_list$S2, S3 = sens_list$S3, S4 = sens_list$S4),
  .id = "scenario"
) %>%
  left_join(scenario_names, by = join_by(scenario)) %>%
  filter(name != "Mantids")

fig4_pal <- sequential_hcl(4, h = c(109, 252), c = c(100, 150, 7), l = c(86, 13), power = c(0.5, 0.9))

#+ fig4_plot
fig4 <- ggplot(fig4_data, aes(x = name, y = pct_change)) +
  geom_hline(yintercept = 0, linewidth = 0.2, linetype = "longdash") +
  geom_linerange(
    aes(ymin = pct_change_lc, ymax = pct_change_uc, group = scenario_labels),
    position = position_dodge(width = fig_dodgewidth), linewidth = 0.3
  ) +
  geom_point(
    aes(fill = scenario_labels), shape = 21, size = fig_pt_size,
    position = position_dodge(width = fig_dodgewidth)
  ) +
  scale_x_discrete(labels = guild_labs) +
  scale_fill_manual(name = "Scenarios 1−4:\nMantid Abundance", values = rev(fig4_pal)) +
  labs(x = NULL, y = "Abundance Change (Percent)") +
  theme_bgl_s +
  guides(fill = guide_legend(position = "inside")) +
  theme(legend.position.inside = c(0.86, 0.30), legend.title = element_text(hjust = 0))

#+ fig4,fig.align='center'
fig4
#' **Fig. 4** Percentage abundance change for target nodes in Scenarios 1-4, each corresponding to 
#' incrementally increased M. religiosa abundance based on the pre-invasion baseline FIW. 
#' Points show median changes and vertical lines encompass 95% confidence intervals; 
#' these are derived from a sensitivity analysis (10,000 permutations). 
#' Point color indicates scenario; the color gradient reflects increasing mantid abundance 

#+ save_fig4,echo=FALSE
ggsave(root_path("figs", "Fig4.svg"), plot = fig4, device = svg,
       width = 174, height = 80, units = "mm", bg = "transparent")

#' 
#' # Figure 5 – Manipulating abiotic conditions and natural enemies 
#' Cold snaps and parasitoid wasps effects on mantids. A two-panel plot is produced.
#+ fig5_data
fig5_data <- bind_rows(
  list(S5 = sens_list$S5, S6 = sens_list$S6, S7 = sens_list$S7,
       S8 = sens_list$S8, S9 = sens_list$S9),
  .id = "scenario"
) %>%
  left_join(scenario_names, by = join_by(scenario)) %>%
  separate_wider_delim(scenario_labels, delim = ", ", names = c("winter", "wasps"), cols_remove = FALSE)

fig5_pal <- sequential_hcl(5, h = c(109, 252), c = c(100, 150, 7), l = c(86, 13), power = c(0.5, 0.9))[
  order(c(1, 3, 2, 4, 5))
]

#' 
#' Panel 1: mantids
fig5_mantis <- fig5_data %>%
  filter(name == "Mantids") %>%
  ggplot(aes(x = name, y = pct_change)) +
  geom_hline(yintercept = 0, linewidth = 0.2, linetype = "longdash") +
  geom_linerange(
    aes(ymin = pct_change_lc, ymax = pct_change_uc, group = scenario_labels),
    position = position_dodge(width = fig_dodgewidth), linewidth = 0.3
  ) +
  geom_point(
    aes(fill = scenario_labels), shape = 21, size = fig_pt_size,
    position = position_dodge(width = fig_dodgewidth)
  ) +
  scale_fill_manual(name = "Scenario", values = fig5_pal) +
  scale_y_continuous(breaks = seq(-100, 25, 25)) +
  labs(x = NULL, y = "Abundance Change (Percent)") +
  theme_bgl_s
#' 
#' Panel 2: target nodes
fig5_targets <- fig5_data %>%
  filter(name != "Mantids") %>%
  ggplot(aes(x = name, y = pct_change)) +
  geom_hline(yintercept = 0, linewidth = 0.2, linetype = "longdash") +
  geom_linerange(
    aes(ymin = pct_change_lc, ymax = pct_change_uc, group = scenario_labels),
    position = position_dodge(width = fig_dodgewidth), linewidth = 0.3
  ) +
  geom_point(
    aes(fill = scenario_labels), shape = 21, size = fig_pt_size,
    position = position_dodge(width = fig_dodgewidth)
  ) +
  scale_x_discrete(labels = guild_labs) +
  scale_fill_manual(name = "Scenarios 5−9: Winter Severity\nand Parasitoid Wasps", values = fig5_pal) +
  labs(x = NULL, y = "Abundance Change (Percent)") +
  theme_bgl_s +
  guides(fill = guide_legend(position = "inside", ncol = 1)) +
  theme(legend.position.inside = c(0.58, 0.16), legend.title = element_text(hjust = 1))
#' 
#' Arrange panels
fig5 <- ggarrange(
  fig5_mantis + theme(legend.position = "none"),
  fig5_targets +
    scale_y_continuous(
      limits = c(-3.4, 5.4),
      breaks = c(-3, -1.5, 0, 1.5, 3, 4.5),
      labels = c("-3.0", "-1.5", "0", "1.5", "3.0", "4.5")
    ) +
    theme(
      axis.title.y = element_blank(),
      legend.position.inside = c(0.80, 0.80)
    ),
  nrow = 1,
  ncol = 2,
  labels = c("a", "b"),
  hjust = c(-6.2, -5),
  vjust = 2,
  font.label = list(size = 12, face = "plain"),
  widths = c(0.25, 0.75),
  align = "h"
)

#+ fig5,fig.align='center'
fig5
#' **Fig. 5** Percentage abundance change for a M. religiosa and b target nodes in Scenarios 5-9. 
#' Scenarios correspond to altered winter severity and presence/absence of parasitoid wasps and 
#' are based on the post-invasion baseline FIW. Points show median changes and vertical lines encompass 95% confidence 
#' intervals; these are derived from a sensitivity analysis (10,000 permutations). 
#' Point color indicates scenario; the color gradient reflects increasing mantid abundance

#+ save_fig5,echo=FALSE
ggsave(root_path("figs", "Fig5.svg"), plot = fig5, device = svg,
       width = 174, height = 110, units = "mm", bg = "transparent")

