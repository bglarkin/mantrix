# Directory control for report rendering on Github
# Batch rendering

# Path control
library(rprojroot)
root_path <- function(...) rprojroot::find_rstudio_root_file(...)

# Scripts to render
scripts <- list.files("code", pattern = "\\.R$", full.names = TRUE)

# Output paths
out_dir <- file.path(getwd())

# Execution
for (script in scripts) {
  output_name <- paste0(tools::file_path_sans_ext(basename(script)), ".md")
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir
  )
}

# Clean up intermediate HTML files...
file.remove(list.files(".", pattern = "\\.html$", full.names = TRUE))




# Directory control for report rendering on GitHub
# Batch rendering

packages_needed <- c("rmarkdown", "tools", "rprojroot")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)

# Path control
root_path <- function(...) rprojroot::find_rstudio_root_file(...)

# Scripts to render
scripts <- list.files(root_path("code"), pattern = "\\.R$", full.names = TRUE)

# Output paths
out_dir <- root_path()

# Execution
for (script in scripts) {
  output_name <- paste0(tools::file_path_sans_ext(basename(script)), ".md")
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir
  )
}

# Clean up intermediate HTML files
file.remove(list.files(root_path(), pattern = "\\.html$", full.names = TRUE))