# Package installer for ENV H/EPI 570
# Updated 2024-03-25 Brian High

# Force use of personal R library folder, creating as needed
lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE)
.libPaths(lib_dir, include.site = FALSE)

# Load packages, installing if needed
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_set_cranrepo("cloud.r-project.org")
pacman::p_load(
  BiocManager,
  cartogram,
  corrr,
  dplyr,
  explore,
  fst,
  GGally,
  ggmap,
  ggplot2,
  here,
  Hmisc,
  janitor,
  leaflegend,
  leaflet,
  marginaleffects,
  MetBrewer,
  mgcv,
  patchwork,
  psych,
  purrr,
  raster,
  RColorBrewer,
  readr,
  readr, 
  rlang,
  sf,
  spdep,
  stringr,
  terra,
  tidyr,
  tidyr, 
  tmap,
  usethis,
  viridis,
  weathermetrics
)

# Since rgdal is deprecated we need to install the last version in the archive.
# See: https://cran.r-project.org/web/packages/rgdal/index.html
# And: https://stackoverflow.com/questions/76868135/
pacman::p_install_version("rgdal", version = "1.6-7")
pacman::p_load(rgdal)

# Since rgeos is deprecated we need to install the last version in the archive.
# See: https://cran.r-project.org/web/packages/rgeos/index.html
# And: https://stackoverflow.com/questions/77687036/
pacman::p_install_version("rgeos", version = "0.6-4")
pacman::p_load(rgeos)

# Install and load the tidysynth & hurricaneexposure packages and dependencies
pacman::p_install_version("LowRankQP", version = "1.0.5")
pacman::p_install_version("Synth", version = "1.1-6")
pacman::p_load(LowRankQP, Synth)
pacman::p_load_gh("edunford/tidysynth")
pacman::p_load_gh("geanders/hurricaneexposure")
pacman::p_load_gh("geanders/hurricaneexposuredata")
data("hurr_tracks")
