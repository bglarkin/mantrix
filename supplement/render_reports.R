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

# Render each script
for (script in scripts) {
  output_name <- paste0(tools::file_path_sans_ext(basename(script)), ".md")
  
  rmarkdown::render(
    input = script,
    output_format = "github_document",
    output_file = output_name,
    output_dir = out_dir
  )
}

# Move *_files folders into supplement/
files_folders <- list.files(out_dir, pattern = "_files$", full.names = TRUE)
dest_dir <- root_path("supplement")
if (!dir.exists(dest_dir)) dir.create(dest_dir)

for (folder in files_folders) {
  target <- file.path(dest_dir, basename(folder))
  if (dir.exists(target)) unlink(target, recursive = TRUE)
  file.rename(folder, target)
}

# Clean up intermediate HTML files
file.remove(list.files(out_dir, pattern = "\\.html$", full.names = TRUE))