# Directory control for report rendering on GitHub
# Batch rendering

packages_needed <- c("rmarkdown", "tools", "rprojroot", "knitr")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)

# Path control
root_path <- function(...) rprojroot::find_rstudio_root_file(...)

# Scripts to render
scripts <- list.files(root_path("code"), pattern = "\\.R$", full.names = TRUE)

# Output path for reports
out_dir <- root_path()

# Render each script
for (script in scripts) {
  base_name <- tools::file_path_sans_ext(basename(script))
  output_name <- paste0(base_name, ".md")
  fig_folder <- paste0(base_name, "_files")
  fig_source <- root_path(fig_folder)
  fig_target <- root_path("supplement", fig_folder)
  
  # Create an isolated environment for rendering
  render_env <- new.env(parent = globalenv())
  knitr::opts_chunk$set(fig.path = fig_folder)
  
  # Render the script
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir,
    envir = render_env
  )
  
  # Move the _files folder into supplement
  if (dir.exists(fig_source)) {
    if (!dir.exists(root_path("supplement"))) dir.create(root_path("supplement"))
    file.rename(from = fig_source, to = fig_target)
  }
  
  # Adjust image paths in the .md file
  md_path <- root_path(output_name)
  if (file.exists(md_path)) {
    md_text <- readLines(md_path)
    md_text <- gsub(
      pattern = paste0("(?<=\\()(", fig_folder, "/)"),
      replacement = paste0("supplement/", fig_folder, "/"),
      x = md_text,
      perl = TRUE
    )
    writeLines(md_text, md_path)
  }
}

# Clean up intermediate HTML files
file.remove(list.files(out_dir, pattern = "\\.html$", full.names = TRUE))