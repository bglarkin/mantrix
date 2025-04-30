# Batch rendering of .R scripts to markdown reports (GitHub-friendly)
packages_needed <- c("rmarkdown", "tools", "rprojroot", "knitr")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)

# Path setup
root_path <- function(...) rprojroot::find_rstudio_root_file(...)
scripts <- list.files(root_path("code"), pattern = "\\.R$", full.names = TRUE)
out_dir <- root_path()

# Render each script
for (script in scripts) {
  script_base <- tools::file_path_sans_ext(basename(script))
  output_name <- paste0(script_base, ".md")
  fig_folder <- paste0("supplement/", script_base, "_files/")
  
  # Ensure output folder exists
  dir.create(root_path(fig_folder), recursive = TRUE, showWarnings = FALSE)
  
  # Render in custom environment with controlled fig.path
  render_env <- new.env(parent = globalenv())
  knitr::opts_chunk$set(fig.path = fig_folder)
  
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir,
    envir = render_env
  )
  
  # Post-process markdown to fix image paths
  md_path <- root_path(output_name)
  md_lines <- readLines(md_path)
  md_lines <- gsub(
    pattern = paste0(script_base, "_files/"),
    replacement = fig_folder,
    x = md_lines,
    fixed = TRUE
  )
  writeLines(md_lines, md_path)
}

# Clean up HTML junk
file.remove(list.files(out_dir, pattern = "\\.html$", full.names = TRUE))
