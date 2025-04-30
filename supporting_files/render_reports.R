# Directory control for report rendering on Github
# Batch rendering

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
