# Directory control for report rendering on GitHub
# Batch rendering

packages_needed <- c("rmarkdown", "tools", "rprojroot", "knitr", "fs")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)

# Path control
root_path <- function(...) rprojroot::find_rstudio_root_file(...)

# Scripts to render
scripts <- list.files(root_path("code"), pattern = "\\.R$", full.names = TRUE)
out_dir <- root_path()

# Render each script
for (script in scripts) {
  base_name <- tools::file_path_sans_ext(basename(script))
  output_name <- paste0(base_name, ".md")
  fig_folder_name <- paste0(base_name, "_files")
  fig_path_for_knitr <- file.path("supplement", fig_folder_name, fsep = "/")
  fig_source <- root_path(fig_folder_name)
  fig_target <- root_path("supplement", fig_folder_name)
  
  # Remove old _files folder in supplement if it exists
  if (dir.exists(fig_target)) unlink(fig_target, recursive = TRUE, force = TRUE)
  
  # Render with custom fig.path that points to supplement
  render_env <- new.env(parent = globalenv())
  knitr::opts_chunk$set(fig.path = fig_path_for_knitr)
  
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir,
    envir = render_env
  )
  
  # Move generated _files folder to supplement
  if (dir.exists(fig_source)) file.rename(from = fig_source, to = fig_target)
}

# Clean up intermediate HTML files
file.remove(list.files(out_dir, pattern = "\\.html$", full.names = TRUE))