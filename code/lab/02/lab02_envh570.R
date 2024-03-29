# Lab 02 ENV H/EPI 570
# Updated 2024-03-26 by Brian High

# Clear workspace of all objects and unload all extra (non-base) packages.
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Load packages, installing if needed
if(!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  here,
  sf,
  janitor,
  readr, 
  # rgeos,  # Note: This package has been deprecated, see below for work-around.
  viridis,
  Hmisc,
  GGally,
  patchwork,
  fst,
  psych,
  spdep,
  dplyr,
  ggplot2,
  stringr,
  purrr,
  mgcv
)

# Since rgeos is deprecated we need to install the last version in the archive.
# See: https://cran.r-project.org/web/packages/rgeos/index.html
# And: https://stackoverflow.com/questions/77687036/
if (!requireNamespace("rgeos", quietly = TRUE)) 
  remotes::install_version("rgeos", version = "0.6-4")
pacman::p_load(rgeos)

#############################################################################
# Set a seed for reproducibility
set.seed(570)

# Bring in data on Tulsa and Osage counties oil and gas wells from Enverus 
# (https://www.enverus.com/)
## Label the well's production type, replace "other" with "disposal well" 
wells <- read_csv(here("data/lab/02/tulsa_wells_no_NA_clean.csv")) %>%
  dplyr::select(-ends_with("date")) %>%
  dplyr::select(
    state,
    surface_lat_wgs84, # latitude of well
    surface_long_wgs84, # longitude of well
    well_status, 
    production_type,
    comp_year # year well was completed
  ) %>%
  mutate(production_type = recode(production_type, "other" = "disposal well"))

# Look at the data and compare to your codebook in the handout
dplyr::glimpse(wells) 

# Let's look at well status, production type, and year of completion
wells %>% group_by(well_status) %>% summarise(n = n()) 
# Very few still active
wells %>% group_by(production_type) %>% summarise(n = n()) 
# Mostly disposal wells
wells %>% group_by(comp_year) %>% summarise(n = n()) 
# Not a useful way to look, too many rows

# To-do: create a figure showing year of well completion (comp_year)
# Ideas include a bar chart, a line chart, something else?
# Example for bar chart but please feel free to improve
wells %>% ggplot(aes(comp_year)) + geom_bar()

## ADD YOURS HERE##

#############################################################################
# EJ analysis #1 (SES/race/ethnicity - modern day)
# Bringing in ses and population density data 
density <- read.fst("data/lab/02/tract_density")

ses <- read.fst("data/lab/02/tract_dem_pov.fst") %>% 
  mutate(state_fips = str_pad(statea, width=2, side="left", pad="0"),
         county_code = str_pad(countya, width=3, side="left", pad="0"),
         tract_code = str_pad(tracta, width=6, side="left", pad="0"),
         county_fips = str_c(state_fips, county_code),
         tract_fips = str_c(county_fips, tract_code),
         gisjoin = as.character(gisjoin)) %>% 
  filter(county_fips %in% c("40143", "40113")) %>% 
  dplyr::select(gisjoin, county_fips, tract_fips, starts_with("pct_")) %>%
  left_join(density, by = "gisjoin")

# Look at these data
ses <- glimpse(ses)

# Remove extra county_fips, don't need it
ses <- ses %>% dplyr::select(-county_fips)

# Descriptive statistics of tract-level SES variables, what do you observe?
ses %>% 
  dplyr::select(starts_with("pct"), density) %>%
  psych::describe(na.rm = TRUE) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>%
  #Note that we use the dplyr::selection convention here
  dplyr::select(n, mean, sd, median, min, max)

# Map of tract-level percent American Indian population 
# Bringing in a shapefile of all census tracts in Oklahoma 
# and filtering to the study area (Tulsa and Osage counties, 
# FIPS = 40143 and 40113).
tract_geo <- st_read("data/lab/02/ok_tract_2010.shp") %>%
  mutate(county_fips = str_c(STATEFP10, COUNTYFP10)) %>%
  filter(county_fips %in% c("40143", "40113")) 
# What is the projection? Look up here: https://epsg.io/102003

# Transforming coordinates to UTM 14N projection, so we can be in meters: 
# https://epsg.io/26914 
# Check which UTM zone for OK here: 
# https://pubs.usgs.gov/fs/2001/0077/report.pdf
tract_geo_utm14n <- st_transform(tract_geo, crs=26914)

# What is the tract identifier? A: GEOID10
glimpse(tract_geo_utm14n)

# What is the tract identifier in ses? A: tract_fips
glimpse(ses)

# Merging data and shapefile, recall that we merge onto the spatial file to 
# retain spatail aspect
ses_sp <- left_join(tract_geo_utm14n, ses, by = c("GEOID10" = "tract_fips"))

# Map of American Indian population -- let's walk through this code in 
# detail together
map_pct_amer_in <- ggplot(data = ses_sp) +
  geom_sf(aes(fill = pct_amer_in), lwd = 0) +
  scale_fill_viridis(name = "Percent American Indian (%)",
                     na.value = "maroon2") +
  labs(title = "Tract-level percent American Indian in Tulsa and Osage Counties") +
  theme_void(base_size = 12) + 
  # Use theme void for maps because eliminates background
  theme(legend.position = "bottom") 
map_pct_amer_in

# To-do: create a map of another census sociodemographic variable
# Try using a different color scale (change something in scale_fill_viridis)
# ADD HERE

# Count wells per census tract and look at correlation with % American Indian
# Create sf (spatial) object from lat/long coordinates
# + setting coordinate reference system for well data 
wells_sp <- st_as_sf(wells, 
                     coords = c("surface_long_wgs84", "surface_lat_wgs84"),
                     crs = 4326)

# Transforming into UTM 14N, so it's in the same projection as the census 
# tract data
wells_sp_utm14n <- st_transform(wells_sp, 26914)

# Adding count of wells per census tract back to the census tract shapefile
tract_geo_utm14n$well_count <- 
  lengths(st_intersects(tract_geo_utm14n, wells_sp_utm14n))

# Setting counts of 0 wells to NA for improved plotting
tract_geo_utm14n$well_count <- replace(tract_geo_utm14n$well_count, 
                                       tract_geo_utm14n$well_count == 0, NA)

# Creating centroid coordinates
# The st_centroid function outputs the centroid of each tract. 
# Using [[1]] and [[2]], we extract the first and second columns of that output 
# as the longitude and latitude respectively. 
tract_geo_utm14n <- tract_geo_utm14n %>% 
  mutate(lon_centroid = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat_centroid = map_dbl(geometry, ~st_centroid(.x)[[2]]))

# Plot well count on census tract file using proportional circles 
# (larger circle = more wells)
# Do you need both color and size here to show well count?
# To-do: change map to show just one of them
well_ct_map <- tract_geo_utm14n %>% 
  filter(!well_count == 0) %>%
  ggplot() + 
  geom_sf(data = ses_sp, color="black", aes(geometry = geometry), 
          lwd = 0.2, alpha = 0.8) +
  geom_sf(data = tract_geo_utm14n, aes(fill = well_count), alpha = 0.85) +
  scale_fill_viridis("Well count (n)", limits = c(1, 11500), 
                     breaks=c(500, 2500, 5000, 7500, 10000)) +
  geom_point(
    aes(x = lon_centroid, y = lat_centroid, size = well_count),
    fill = "grey", color = "black", alpha = .5) + 
  scale_size_continuous("Well count (n)", breaks=c(50, 500, 2500, 5000, 7500))+
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank())  
well_ct_map

# Overlaying wells with sociodemographic information
# Plot well count on census tract percent American Indian
overlay_map <- tract_geo_utm14n %>% 
  filter(!well_count == 0) %>%
  ggplot() + 
  geom_sf(data = ses_sp, color="black", aes(geometry = geometry, 
                                            fill = pct_amer_in), 
          lwd = 0.2, alpha = 0.8) +
  scale_fill_viridis("American Indian (%)") + 
  geom_point(
    aes(x = lon_centroid, y = lat_centroid, size = well_count),
    fill = "grey", color = "black", alpha = .5) + 
  scale_size_continuous("Wells (n)", breaks=c(50, 500, 2500, 5000, 7500))+
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank()) 
overlay_map

# Taking a closer look at Tulsa
# Notice that census tracts are much smaller and that 
# there are fewer wells overall (scale breaks change)
overlay_tulsa_map <- tract_geo_utm14n %>% 
  dplyr::filter(county_fips == 40143) %>% # Filtering to Tulsa
  dplyr::filter(!well_count == 0) %>%
  ggplot() + 
  geom_sf(data = ses_sp %>% dplyr::filter(county_fips == 40143),
          color="black", aes(geometry = geometry, fill = pct_amer_in), 
          lwd = 0.2, alpha = 0.8) +
  scale_fill_viridis("Percent American Indian (%)") + 
  scale_size_continuous("Well count (n)", breaks=c(1, 10, 50, 100, 500, 1000)) +
  geom_point(
    aes(x = lon_centroid, y = lat_centroid, size = well_count),
    fill = "grey", color = "black", alpha = .5) + 
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank()) 

overlay_tulsa_map


# 2. Regression focusing on wells built in or after 2000
# Here, we use Poisson regression to assess the association between percent 
# American Indian and well counts. Restricting study period to modern day 
# (starting in 2000)
wells_sp_utm14n_modern_day <- wells_sp_utm14n %>% 
  filter(comp_year >= 2000)

# Counting wells built on and after 2000
tract_geo_utm14n$well_count_modern_day <- 
  lengths(st_intersects(tract_geo_utm14n, wells_sp_utm14n_modern_day))

# Merging data
# Make tract_geo_utm14n not spatial so we can join
tract_geo_utm14n_nonsp <- st_drop_geometry(tract_geo_utm14n)
ct_ses <- left_join(ses_sp %>% 
                      dplyr::select(GISJOIN, starts_with("pct_"), density),
                    tract_geo_utm14n_nonsp %>% 
                      dplyr::select(GISJOIN, well_count_modern_day, 
                                    # just keeping a few variables
                                    lon_centroid, lat_centroid),
                    by = c("GISJOIN"))

# Regression 
# We run a simple Poisson model and control for population density
reg_amer_in <- glm(well_count_modern_day ~ pct_amer_in + density, 
                   family="poisson", data = ct_ses)

# Summary
summary(reg_amer_in)
exp(6.657e-02) # 1.07 <- how to interpret this value? Discuss.

# Fancier summary for glm
# Extracting the coefficient for pct_amer_in and converting to a dataframe
coeff.table = rbind(summary(reg_amer_in)$coefficients[2,1:4])
coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)

# Naming the columns of the dataframe 
names(coeff.table) <- c("coeff", "se", "wald", "p-value")

# Exponentiating the coefficient and computing the 95% confidence interval
output.table <- coeff.table %>% 
  mutate(pt.estimate = exp(coeff)) %>%
  mutate(lci = exp(coeff - 1.96*se), 
         uci = exp(coeff + 1.96*se)) %>%
  mutate(model = "pct_american_indian") %>% 
  dplyr::select(model, pt.estimate, lci, uci, "p-value")

output.table

# Let's look at the model Pearson residuals spatially
# Extracting the model residuals
residuals <- resid(reg_amer_in)

# Adding the residuals to the shapefile -- this makes them spatial
tract_geo_utm14n$resid <- residuals

# Also adding deciles of residuals for plotting
tract_geo_utm14n <- tract_geo_utm14n %>% mutate(decile_res=ntile(residuals, 10))

# Quick look at the residuals statistics 
summary(tract_geo_utm14n$resid)

# Mapping residuals 
residual_ct_map <- tract_geo_utm14n %>% 
  ggplot() + 
  geom_sf(data = tract_geo_utm14n,
          color="black", aes(geometry = geometry, fill = resid), 
          lwd = 0.2, alpha = 0.8) +
  scale_fill_viridis("Residuals") + 
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank()) 

residual_ct_map

#### Moran's I - test of global clustering of residuals
# Creating neighbor matrix - this describes which polygons are neighbors of 
# other polygons
nb <- poly2nb(tract_geo_utm14n, queen=TRUE)

# Assigning spatial weights based on neighbors 
nb_lw <- nb2listw(nb, style="B")

# Running Moran's I test -- values will range from -1 to 1 
moran.test(tract_geo_utm14n$resid, nb_lw) 
# p-value <0.05  indicating support for non-randomly distributed residuals

# We run a simple Poisson model and control for population density
# now with a spline on census tract centroid lat/long
reg_amer_in_sp <- gam(well_count_modern_day ~ pct_amer_in + density + 
                        s(lat_centroid, lon_centroid),
                  family="poisson", data = ct_ses)

# Do results change?
summary(reg_amer_in_sp)
exp(0.0732) # 1.08

# Let's look at the residuals now
# Let's look at the model Pearson residuals spatially
# Extracting the model residuals
residuals <- resid(reg_amer_in_sp)

# Adding the residuals to the shapefile -- this makes them spatial
tract_geo_utm14n$resid <- residuals

# Also adding deciles of residuals for plotting
tract_geo_utm14n <- tract_geo_utm14n %>% mutate(decile_res=ntile(residuals, 10))

# Quick look at the residuals statistics 
summary(tract_geo_utm14n$resid)

# Mapping residuals 
residual_ct_map2 <- tract_geo_utm14n %>% 
  ggplot() + 
  geom_sf(data = tract_geo_utm14n,
          color="black", aes(geometry = geometry, fill = resid), 
          lwd = 0.2, alpha = 0.8) +
  scale_fill_viridis("Residuals") + 
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank()) 

residual_ct_map2

# Plot the residual maps side by side
# Which is "better?" and why?
residual_ct_map + residual_ct_map2

#### Moran's I - test of global clustering of residuals
# Creating neighbor matrix - this describes which polygons are neighbors of 
# other polygons
nb <- poly2nb(tract_geo_utm14n, queen=TRUE)

# Assigning spatial weights based on neighbors 
nb_lw <- nb2listw(nb, style="B")

# Running Moran's I test -- values will range from -1 to 1 
moran.test(tract_geo_utm14n$resid, nb_lw) 
# What does this Morna's I and p-value indicate?

#############################################################################
# EJ analysis #2 (redlining - historical perspectives)
# Analysis of HOLC data (home owner's loan corporation) 
# Data from the Mapping Inequality Project: 
# https://dsl.richmond.edu/panorama/redlining/#loc=5/39.1/-97.217

# Bringing in HOLC boundaries
holc <- read_sf("data/lab/02/ok_tulsa_holc_utm15n.shp")
holc

# Retrieving coordinate reference system from shapefile
st_crs(holc) # crs=UTM 15N

# Let's reproject in UTM 14N for consistency with other files
holc_utm14n <- st_transform(holc, 26914)

# Creating neighborhood ID
holc_utm14n <- holc_utm14n %>% mutate(neigh_ID=row_number())

# HOLC grades proportions
holc_utm14n %>%
  tabyl(holc_grade) %>% 
  mutate(percent = round(percent*100, 2)) 

# Creating color scale
colors <- c("#006f3c","#264b96","#f9a73e","#bf212f")

# Mapping HOLC grades
holc_map <- 
  holc_utm14n %>% ggplot() + geom_sf(aes(fill=factor(holc_grade))) +
  #what does scale_fill_manual do? 
  scale_fill_manual("HOLC grade", values=colors) +
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank()) + 
  labs(title = "HOLC grade")

holc_map

# Transform wells into UTM zone 14N
wells_sp_utm14n <- st_transform(wells_sp, 26914)

# Count of wells in a given radius 
# Create 1km buffer around each community
holc_buff_1km <- st_buffer(holc_utm14n, dist = 1000)

# Count of total number of wells overall within 1km
holc_buff_1km$well_count <- 
  lengths(st_intersects(holc_buff_1km, wells_sp_utm14n))

# Add counts back to orginal HOLC polygon file
holc_buff_counts <-  holc_buff_1km %>% dplyr::select(well_count, neigh_ID)
holc_buff_counts <- st_drop_geometry(holc_buff_counts) 
holc_utm14n <- left_join(holc_utm14n, holc_buff_counts)

# Distribution of wells by HOLC grade
# Removing spatial feature to obtain a simple dataframe
holc_wells <- st_drop_geometry(holc_utm14n)

# Computing well count by quantile 
wells_holc_dist <- holc_wells %>% group_by(holc_grade) %>%
  summarise(mean_well_count = mean(well_count), 
            p25_well_count=quantile(well_count, probs=0.25),
            p50_well_count=quantile(well_count, probs=0.5),
            p75_well_count=quantile(well_count, probs=0.75)) %>% 
  knitr::kable()
wells_holc_dist #what does the p75_well_count column show us?

# Visualizing boxplot for each HOLC grade
holc_boxplot <- holc_wells %>%
  ggplot(aes(holc_grade, well_count, fill = holc_grade)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = colors) +
  labs(x = "HOLC grade", y = "Well count") +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      linetype = 'solid',
      colour = "grey"
    ),
    panel.grid.major.x = element_line(
      linewidth = 0.25,
      linetype = 'solid',
      colour = "grey"
    ),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    text = element_text(size = 16)
  )
holc_boxplot

# Maps of well distribution 
# Overall well count, you may want to pick a different color scheme 
# (fill colors)
holc_well_map <-
  holc_utm14n %>% ggplot() + 
  geom_sf(aes(fill=well_count)) +
  scale_fill_viridis("Wells (n)", option="A")+
  theme_void(base_size=14) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect=element_blank()) + 
  labs(title = "Total wells drilled")

# Use patchwork to plot side by side 
holc_map + holc_well_map

# For doctoral students only:
# To-do: use patchwork to plot one above the other instead of side by side
# ADD CODE HERE

# To-do: work with wells drileld pre/post HOLC
# HOLC maps were drawn in 1937 for Tulsa, so let's break wells into two groups: 
# pre- and post-1937
# Creating a binary variable for wells creation before HOLC
wells_sp_utm14n <- wells_sp_utm14n %>% 
  mutate(pre_holc=ifelse(comp_year<1937,1,0))
wells_sp_utm14n %>% tabyl(pre_holc) %>% 
  mutate(percent = round(percent*100, 2))

# Count of wells drilled in and before 1937 within HOLC communities
pre_holc_wells_utm14n <- wells_sp_utm14n %>% filter(pre_holc==1)
post_holc_wells_utm14n <- wells_sp_utm14n %>% filter(pre_holc==0)
holc_buff_1km$well_count_pre <- 
  lengths(st_intersects(holc_buff_1km, pre_holc_wells_utm14n))

# Type of wells by pre/post HOLC grading
# To-do, how many wells drilled pre/post HOLC; show on a figure
# ADD HERE

# Count of wells drilled after 1937 in HOLC communities
holc_buff_1km$well_count_post <- 
  lengths(st_intersects(holc_buff_1km, post_holc_wells_utm14n))

# To-do: try running a regression model to look at the association between 
# HOLC grade and wells
# ADD CODE HERE (hint: paste much from maps above and edit a bit of it)

