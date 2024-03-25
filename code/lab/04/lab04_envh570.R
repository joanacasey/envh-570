#Lab 04 ENV H/EPI 570 - Built environment
#Updated 03 May 2023 by Joan Casey

#Load packages, installing if needed
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  here,
  dplyr,
  sf,
  ggplot2,
  MetBrewer,
  RColorBrewer,
  patchwork,
  readr,
  leaflet,
  leaflegend,
  ggmap,
  raster,
  terra
  # rgdal   # Note: This package has been deprecated, see below for work-around.
)

# Since rgdal is deprecated we need to install the last version in the archive.
# See: https://cran.r-project.org/web/packages/rgdal/index.html
# And: https://stackoverflow.com/questions/76868135/
pacman::p_install_version("rgdal", version = "1.6-7")
pacman::p_load(rgdal)

##############################################################################
# PART 1 
# Bring in Seattle data with SES and trees
# What is the projection?
seattle <- st_read(here("data/lab/04/seattle_tree_ses_ct.shp"))
glimpse(seattle)

# Plot tree density
tree_density <- ggplot() +
  geom_sf(data = seattle, aes(fill = tr_dnst)) +
  theme_void(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  scale_fill_gradientn("Tree density \n(per mi2)",
                       colors = met.brewer("VanGogh3"),
                       na.value = "grey50")


# Plot median income
income <- ggplot() +
  geom_sf(data = seattle, aes(fill = MEDIAN_)) +
  theme_void(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) + scale_fill_gradientn("Median income ($)",
                           colors = met.brewer("Degas"),
                           na.value = "grey50")


tree_density + income

# Try adding figure for percent vacant housing below
ggplot() +
  geom_sf(data = seattle, aes(fill = PCT_VAC)) +
  theme_void(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) + scale_fill_gradientn("% vacant",
                           colors = met.brewer("Degas"),
                           na.value = "grey50")

#############################################################################
## PART 2: CROWD-SOURCED NOISE MEASUREMENTS
noise <-
  read_csv(here("data/lab/04/noise_envh570_seattle.csv")) 
# cool that R recognizes the time column as time of day

#Remove erroneous rows of data
noise <- drop_na(noise)

#time of day
summary(noise$time)

#how does these data look? see any problems?
summary(noise)

noise <- noise %>% mutate(longit = ifelse(longit<0, longit, -1*longit))

#make an sf object with lat/longit
noise_sf <-
  st_as_sf(noise, coords = c("longit", "latit"), crs = "EPSG:4326")

#get coordinates
coords <- st_coordinates(noise_sf)
lat = coords[, 2]
long = coords[, 1]

#Make a map of Seattle with leaflet (you can change long/lat/zoom)
m <- leaflet() %>% setView(lng = -122.38,
                           lat = 47.6,
                           zoom = 11)

m <- m %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    m,
    lng = long,
    lat = lat,
    popup = m$noise,
    label = m$noise
  )
m

#Let's make a proportional symbol map
symbols <- makeSymbolsSize(
  values = noise_sf$noise,
  shape = 'circle',
  color = 'blue',
  fillColor = 'blue',
  opacity = .5,
  baseSize = 10
)

#Making the map
m2 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    data = m,
    icon = symbols,
    lng = long,
    lat = lat
    ) %>%
  addLegendSize(
    values = noise_sf$noise,
    color = 'blue',
    fillColor = 'blue',
    opacity = .5,
    title = 'Leq noise (dBA)',
    shape = 'circle',
    orientation = 'horizontal',
    breaks = 5)
m2

#############################################################################
## PART 3: CROWD-SOURCED NOISE MEASUREMENTS + SOCIAL ENVIRONMENT 

# Change noise measurements into same projection as Seattle shapefile
noise_sf <- st_transform(noise_sf, crs = st_crs(seattle))

# Map points on the tree map
ggplot() +
  geom_sf(data = seattle, aes(fill = tr_dnst)) +
  geom_sf(data = noise_sf, aes(size=noise), color="grey", alpha=0.5) +
  theme_void(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  scale_fill_gradientn("Tree density \n(per mi2)",
                       colors = met.brewer("VanGogh3"),
                       na.value = "grey50") +
  scale_size_continuous("Noise level (dB)")

# Create a similar map with income 
##ADD ME##

#############################################################################
## PART 4: CROWD-SOURCED NOISE MEASUREMENTS VS NATIONAL NOISE MODEL
transit_noise <- raster("data/lab/04/seattle_transit.tif")

# Check out specs of the raster data
transit_noise #How many columnns and rows of data? How many total grid cells?

# Plot it
plot(transit_noise)

# Extract transit noise levels where we took noise measurements
noise_from_transit_model <- terra::extract(transit_noise, noise_sf)

# Gives us values from from the transit noise raster
# We need to add them back to the noise_sf shapefile
noise_sf <- noise_sf %>% mutate(transit_noise = noise_from_transit_model)

# What is the correlation between our measurements and the model?
ggplot(noise_sf, aes(noise, transit_noise)) + geom_point() + 
  geom_smooth() +
  theme_minimal() +
  geom_abline(linetype="dashed") + # adding a reference line +
  scale_x_continuous("Our noise measurements (dBA)", limits=c(40,80))+
  scale_y_continuous("Transit noise model (dBA)", limits=c(40,80))


# Pearson and Spearman correlation
cor.test(noise_sf$noise, noise_sf$transit_noise,
         method = "spearman")

# You do Pearson 
#ADD ME
