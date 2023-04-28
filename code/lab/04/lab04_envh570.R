#Lab 04 ENV H/EPI 570 - Built environment
#Updated 26 April 2023 by Joan Casey

#Load packages, installing if needed
if(!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  here,
  dplyr,
  sf,
  ggplot2,
  MetBrewer,
  patchwork
)

##############################################################################################################
#PART 1#
# Bring in Seattle data with SES and trees
# What is the projection?
seattle <- st_read("data/lab/04/seattle_tree_ses_ct.shp")
glimpse(seattle)

#Plot tree density
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
                       colors=met.brewer("VanGogh3"), na.value="grey50")
  

#Plot median income
income <- ggplot() +
  geom_sf(data = seattle, aes(fill = MEDIAN_)) +
  theme_void(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) + scale_fill_gradientn("Median income ($)", 
                           colors=met.brewer("Degas"), na.value="grey50")


tree_density + income

##############################################################################################################
## PART 2: CROWD-SOURCED NOISE MEASUREMENTS
noise <- read_csv("data/lab/04/noise_measurements.csv")




