# Package installer for "rstudio.sph" ENV H/EPI 570
# - Binary package installation for Ubuntu 20.04, optimized for high speed
# - To run from shell: Rscript --vanilla --no-save code/lab/package_installer.R
# - When run from the shell, expect execution time to be about 3-4 minutes.  
# Updated 2024-03-26 Brian High

# Clear workspace of all objects and unload all extra (non-base) packages.
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Set repo URL for Ubuntu 20.04 (Focal Fossa)
repo_url <- "https://packagemanager.posit.co/cran/__linux__/focal/latest"

# Force use of personal R library folder, creating as needed
lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE)
.libPaths(lib_dir, include.site = FALSE)

# Set option for HTTP User Agent to include R version information in header
# See: https://www.r-bloggers.com/2023/07/posit-package-manager-for-linux-r-binaries/
local(options(HTTPUserAgent = sprintf(
  "R/%s R (%s)",
  getRversion(),
  paste(
    getRversion(),
    R.version["platform"],
    R.version["arch"],
    R.version["os"]
  )
)))

# Set binary package repo, as binary packages install faster
local(options(repos = c(CRAN = repo_url)))

# Define a function to conditionally install packages, if needed
pkg_inst <- function(pkgs) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  res <- sapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) pak::pkg_install(pkg)
  })
}

# Install packages as needed
pkgs <- c("BiocManager", "cartogram", "corrr", "dplyr", "explore", "fst", 
          "GGally", "ggmap", "ggplot2", "here", "Hmisc", "janitor", 
          "leaflegend", "leaflet", "LowRankQP", "marginaleffects", "MetBrewer", 
          "mgcv", "pacman", "patchwork", "psych", "purrr", "raster", 
          "RColorBrewer", "readr", "remotes", "rlang", "tmap", "sf", "spdep", 
          "stringr", "Synth", "terra", "tidyr", "usethis", "viridis", 
          "weathermetrics")
pkg_inst(pkgs)

# Since rgdal is deprecated we need to install the last version in the archive.
# See: https://cran.r-project.org/web/packages/rgdal/index.html
# And: https://stackoverflow.com/questions/76868135/
options("rgdal_show_exportToProj4_warnings" = "none")
if (!requireNamespace("rgdal", quietly = TRUE)) 
  remotes::install_version("rgdal", version = "1.6-7")

# Since rgeos is deprecated we need to install the last version in the archive.
# See: https://cran.r-project.org/web/packages/rgeos/index.html
# And: https://stackoverflow.com/questions/77687036/
if (!requireNamespace("rgeos", quietly = TRUE)) 
  remotes::install_version("rgeos", version = "0.6-4")

# Install and load the tidysynth & hurricaneexposure packages from Github
if (!requireNamespace("tidysynth", quietly = TRUE)) 
  pak::pkg_install("edunford/tidysynth")
if (!requireNamespace("hurricaneexposure", quietly = TRUE)) 
  pak::pkg_install("geanders/hurricaneexposure")
if (!requireNamespace("hurricaneexposuredata", quietly = TRUE)) 
  pak::pkg_install("geanders/hurricaneexposuredata")

