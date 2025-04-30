Sensitivity Analysis Results from MANTRIX
================
Beau Larkin
Last updated: 30 April, 2025

- [Description](#description)
- [Packages and Libraries](#packages-and-libraries)
  - [Plot Style](#plot-style)
- [Sensitivity Analysis Results](#sensitivity-analysis-results)
  - [Data Import and Formatting](#data-import-and-formatting)
- [Figure 4 – Manipulated mantid
  abundance](#figure-4--manipulated-mantid-abundance)
- [Figure 5 – Manipulating abiotic conditions and natural
  enemies:](#figure-5--manipulating-abiotic-conditions-and-natural-enemies)

# Description

This script summarizes sensitivity analysis results for MANTRIX using
output from the [MPG Matrix tool](https://matrix.mpgranch.com). Figures
4 and 5 visualize predicted percent changes in mantid and trophic guild
abundance under alternative scenarios, which considered mantids, their
natural enemies, and abiotic conditions. Results are based on paired
manipulations of *Mantis religiosa* abundance, cold snap frequencies,
and parasitoid wasp presence/absence.

# Packages and Libraries

``` r
packages_needed <- c("tidyverse", "colorspace", "knitr", "ggpubr", "Cairo", "rprojroot")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)
```

``` r
root_path <- function(...) rprojroot::find_rstudio_root_file(...)
```

## Plot Style

``` r
source(root_path("supplement", "styles.R"))
fig_pt_size <- 3
fig_dodgewidth <- 0.7
```

# Sensitivity Analysis Results

## Data Import and Formatting

``` r
{
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
          "Inv. Predators" ~ "Invertebrate Predators",
          "Sm. Mammals" ~ "Small Mammals",
          .default = name
        )
      ) %>%
      select(name, starts_with("pct")) %>%
      arrange(name)
  })
}
```

Scenario Labels

``` r
scenario_names <- data.frame(
  scenario = names(sens_list),
  scenario_labels = c(
    "1. Obs-Mantids",
    "2. ~Mantids",
    "3. ++Mantids",
    "4. +++Mantids",
    "5. −Cold, −Wasps",
    "6. +Cold, −Wasps",
    "7. −Cold, +Wasps",
    "8. ~Cold, +Wasps",
    "9. +Cold, +Wasps"
  )
) %>%
  mutate(scenario_labels = factor(scenario_labels, ordered = TRUE))
```

# Figure 4 – Manipulated mantid abundance

``` r
fig4_data <- bind_rows(
  list(S1 = sens_list$S1, S2 = sens_list$S2, S3 = sens_list$S3, S4 = sens_list$S4),
  .id = "scenario"
) %>%
  left_join(scenario_names, by = join_by(scenario)) %>%
  mutate(
    region = case_when(
      scenario %in% c("S1", "S2") ~ "Montana estimates",
      scenario %in% c("S3", "S4") ~ "Delaware studies",
      .default = scenario
    )
  )
```

``` r
fig4_pal <- sequential_hcl(4, h = c(109, 252), c = c(100, 150, 7), l = c(86, 13), power = c(0.5, 0.9))
fig4 <- fig4_data %>%
  filter(name %in% levels(factor(c(
    "Invertebrate Predators", "Invertebrate Herbivores", "Pollinators",
    "Songbirds", "Small Mammals"
  )))) %>%
  mutate(name = factor(name, levels = c(
    "Invertebrate Predators", "Invertebrate Herbivores", "Pollinators",
    "Songbirds", "Small Mammals"
  ), ordered = TRUE)) %>%
  ggplot(aes(x = name, y = pct_change)) +
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.2) +
  geom_linerange(aes(ymin = pct_change_lc, ymax = pct_change_uc, group = scenario_labels),
                 position = position_dodge(fig_dodgewidth), linewidth = 0.3) +
  geom_point(aes(fill = scenario_labels), shape = 21, size = fig_pt_size,
             position = position_dodge(fig_dodgewidth)) +
  scale_x_discrete(labels = c(
    "Invertebrate\nPredators", "Invertebrate\nHerbivores", "Pollinators",
    "Songbirds", "Small\nMammals"
  )) +
  scale_fill_manual(name = "Scenario", values = rev(fig4_pal)) +
  labs(x = NULL, y = "Abundance Change (Percent)") +
  theme_bgl_s +
  guides(fill = guide_legend(position = "inside")) +
  theme(legend.position.inside = c(0.9, 0.2))
```

``` r
fig4
```

<img src="/Users/blarkin/Documents/Git_repositories/mantrix/supplement/sensitivity_analysis_results_files/figure-gfm/fig4-1.png" style="display: block; margin: auto;" />

# Figure 5 – Manipulating abiotic conditions and natural enemies:

cold snaps and parasitoid wasps. A two-panel plot is produced.

``` r
fig5_data <- bind_rows(
  list(S5 = sens_list$S5, S6 = sens_list$S6, S7 = sens_list$S7, S8 = sens_list$S8, S9 = sens_list$S9),
  .id = "scenario"
) %>%
  left_join(scenario_names, by = join_by(scenario)) %>%
  separate_wider_delim(scenario_labels, delim = ". ", names = c(NA, "temp"), cols_remove = FALSE) %>%
  separate_wider_delim(temp, delim = ", ", names = c("winter", "wasps"), cols_remove = TRUE) %>%
  mutate(
    winter = factor(winter, levels = c("+Cold", "Cold", "-Cold")),
    wasps = factor(wasps, levels = c("-Wasps", "+Wasps"))
  )

fig5_pal <- sequential_hcl(5, h = c(109, 252), c = c(100, 150, 7), l = c(86, 13), power = c(0.5, 0.9))[
  order(c(1, 3, 2, 4, 5))
]
```

Panel 1: mantids

``` r
fig5_mantis <- fig5_data %>%
  filter(name == "Mantids") %>%
  ggplot(aes(x = name, y = pct_change)) +
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.2) +
  geom_linerange(aes(ymin = pct_change_lc, ymax = pct_change_uc, group = scenario_labels),
                 position = position_dodge(fig_dodgewidth), linewidth = 0.3) +
  geom_point(aes(fill = scenario_labels), shape = 21, size = fig_pt_size,
             position = position_dodge(fig_dodgewidth)) +
  scale_y_continuous(breaks = seq(-100, 25, 25)) +
  scale_fill_manual(name = "Scenario", values = fig5_pal) +
  labs(x = NULL, y = "Abundance Change (Percent)") +
  theme_bgl_s
```

Panel 2: target nodes

``` r
fig5_targets <- fig5_data %>%
  filter(name %in% c(
    "Invertebrate Predators", "Songbirds", "Small Mammals",
    "Invertebrate Herbivores", "Pollinators"
  )) %>%
  mutate(name = factor(name, levels = c(
    "Invertebrate Predators", "Invertebrate Herbivores", "Pollinators",
    "Songbirds", "Small Mammals"
  ), ordered = TRUE)) %>%
  ggplot(aes(x = name, y = pct_change)) +
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.2) +
  geom_linerange(aes(ymin = pct_change_lc, ymax = pct_change_uc, group = scenario_labels),
                 position = position_dodge(fig_dodgewidth), linewidth = 0.3) +
  geom_point(aes(fill = scenario_labels), shape = 21, size = fig_pt_size,
             position = position_dodge(fig_dodgewidth)) +
  scale_x_discrete(labels = c(
    "Invertebrate\nPredators", "Pollinators", "Invertebrate\nHerbivores",
    "Songbirds", "Small\nMammals"
  )) +
  scale_fill_manual(name = "Scenario", values = fig5_pal) +
  labs(x = NULL, y = "Abundance Change (Percent)") +
  guides(fill = guide_legend(position = "inside", ncol = 2)) +
  theme_bgl_s +
  theme(legend.position.inside = c(0.72, 0.15), legend.title = element_text(hjust = 0.5))
```

Arrange panels

``` r
fig5 <- ggarrange(
  fig5_mantis + theme(legend.position = "none"),
  fig5_targets + lims(y = c(-4.8, 4.4)) + theme(axis.title.y = element_blank()),
  nrow = 1, ncol = 2, labels = c("a", "b"),
  hjust = c(-6.2, -5), vjust = 2.4,
  font.label = list(size = 12, face = "plain"),
  widths = c(0.25, 0.75), align = "h"
)
```

``` r
fig5
```

<img src="/Users/blarkin/Documents/Git_repositories/mantrix/supplement/sensitivity_analysis_results_files/figure-gfm/fig5-1.png" style="display: block; margin: auto;" />
