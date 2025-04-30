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

# Output path for markdown files
out_dir <- root_path()

# Render each script
for (script in scripts) {
  output_name <- paste0(tools::file_path_sans_ext(basename(script)), ".md")
  
  # Use default fig.path (i.e., _files folders in root), no override
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir
  )
}

# Clean up intermediate HTML files
file.remove(list.files(out_dir, pattern = "\\.html$", full.names = TRUE))
