#Lab 01 ENV H/EPI 570
#Updated 24 Mar 2024 by Joan Casey

#Objective: This file is meant to get you acclimated to the online RStudio environment and introduce you to basic commands to explore data.

#Load packages, installing if needed
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")
pacman::p_load(
  here,
  usethis,
  dplyr,
  readr,
  tidyr,
  rlang,
  ggplot2,
  RColorBrewer,
  tmap,
  sf,
  viridis,
  corrr,
  marginaleffects,
  spdep,
  cartogram
)

#Read in outcome data (fetal deaths = "spontaneous intrauterine death of a fetus at any time during pregnancy"), downloaded from CDC Wonder:
fetal_data <- read_csv("data/lab/01/fetal_death.csv")

# Explore the data using glimpse
glimpse(fetal_data)

# Explore the data using summary
summary(fetal_data)

# Explore the data using View
View(fetal_data)

#How many states are included in the dataset? Find out using n_distinct. Which package has the n_distinct command?
n_distinct(fetal_data$state) # 50 states + DC

# Some states do not have data for each year/race/death combo
# If table shows two entries for a state, they have counts for both Black and white pregnant people
table(fetal_data$state, fetal_data$year)

# Expand so we have a row for each year/race/state combo
# We expect a dataframe 51 states x 4 years x 2 race/ethnicities = 408
fetal_data_expanded <- fetal_data %>% tidyr::expand(state, year, mom_race_eth)
dim(fetal_data_expanded)

# Add this back to our main dataframe, we now have NA where data is not reported due to small numbers
# We do this using left_join. Please read how this verb works by typing ?left_join into the console
fetal_data_new <- left_join(
  fetal_data_expanded,
  fetal_data,
  by = c(
    "state" = "state",
    "year" = "year",
    "mom_race_eth" = "mom_race_eth"
  )
)

#Glimpse it
glimpse(fetal_data_new)

## Let's create some data visualizations:
# Look at the data in 2017 for counts of fetal deaths by race/ethnicity
# First create variable to order the y-axis by total number of fetal deaths
# You group_by state and year (i.e., calculate the total fetal deaths by state and year) then mutate to create a new variable.
fetal_data_new <- fetal_data_new %>% group_by(state, year) %>% mutate(total_fetal_deaths=sum(fetal_deaths, na.rm=T))

# What are we doing here? First, create a new variable called "mom_race_eth" that replaces "Black or African American" with "Black" for simplicity
# Then you will filter to just 2017 (only plot data for 2017)
# Next use ggplot to create a plot of fetal deaths by state
fetal_data_new %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  dplyr::filter(year == 2017) %>%
  ggplot(aes(
    x = fetal_deaths,
    y = reorder(state, total_fetal_deaths),
    color = mom_race_eth
  )) +
  geom_point() +
  theme_minimal() +
  ylab("") + xlab("Fetal deaths, N") +
  scale_color_brewer("Mom race/ethnicity", palette = "Set1")
#this sort of looks like a plot of total population, we need denominators

# Go to CDC Wonder and download total births by state 
births <- read_csv("data/lab/01/births.csv") 
glimpse(births)

#Drop births to other race/ethnicities
births <- drop_na(births)
table(births$year)

# OK, let's add total live birth counts as the denominator
fetal_data_new <- left_join(fetal_data_new, 
                            births, 
                            by = c("state" = "state", 
                                   "mom_race_eth" = "mom_race_eth", 
                                   "year" = "year", 
                                   "state_code" = "state_code"))

# Create variable scaling fetal deaths by total life births
fetal_data_new <- fetal_data_new %>% mutate(fetal_death_scaled = fetal_deaths/births*1000)

# Look at scaled prevalence
fetal_data_new %>% 
  dplyr::filter(year == 2017) %>%
  ggplot(aes(x = fetal_death_scaled, 
             y = reorder(state, fetal_death_scaled), 
             color = mom_race_eth)) + 
  geom_point() +
  theme_minimal() +
  ylab("") + 
  xlab("Fetal deaths per 1000 live births, N") +
  scale_color_brewer("Mom race/ethnicity", palette = "Set1")
#much more informative, what's happening with states with only white rate?

# Reading in a shapefile using st_read
states <- st_read("data/lab/01/US_State_Albers.shp")
head(states) 

#What is the CRS? Read about crs here: https://rspatial.org/spatial/6-crs.html
#Projected CRS = North America Albers Equal Area Conic 
#read more here: http://www.geo.hunter.cuny.edu/~jochen/gtech201/lectures/lec6concepts/Map%20coordinate%20systems/Albers%20Equal%20Area%20Conic.htm
st_crs(states)

#Add the fetal death data to the spatial file by state name (note these are different variable names in each dataset)
states <- left_join(states, fetal_data_new, by = c("STATE_NAME" = "state"))
glimpse(states)

# Plot 2017, grey shows where we have missing values
states %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  dplyr::filter(year == 2017) %>%
  ggplot() +
  geom_sf(aes(fill = fetal_death_scaled),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis_c("Fetal deaths", na.value = "grey") +
  facet_wrap( ~ mom_race_eth) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        # Hide panel borders and remove grid lines
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", legend.box = "horizontal")

#Map the number of fetal deaths per 1000 live births by race/ethnicity
# Plot all years using facet_grid, read about it using ?facet_grid
states %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  ggplot() +
  geom_sf(aes(fill = fetal_death_scaled),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis_c("Fetal deaths per 1000 live births", na.value = "grey") +
  theme_minimal(base_size = 14) +
  facet_grid(year ~ mom_race_eth) + #does using facet_grid make sense here? seems no to me as you can't see the variability in white fetal death
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", legend.box = "horizontal")

# Let's create quintiles for the legend
quantile(states$fetal_death_scaled, c(.2, .4, .6, .8), na.rm = T)
states = states %>%
  mutate(
    fetal_death_q = case_when(
      fetal_death_scaled < 4.746778 ~ 1,
      fetal_death_scaled >= 4.746778 &
        fetal_death_scaled < 5.591580 ~ 2,
      fetal_death_scaled >= 5.591580 &
        fetal_death_scaled < 7.371913 ~ 3,
      fetal_death_scaled >= 7.371913 &
        fetal_death_scaled < 10.169492 ~ 4,
      fetal_death_scaled >= 10.169492 ~ 5
    )
  )
states %>%
  mutate(mom_race_eth = replace(
    mom_race_eth,
    mom_race_eth == "Black or African American",
    "Black"
  )) %>%
  ggplot() +
  geom_sf(aes(fill = factor(fetal_death_q)),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis(
    "Fetal deaths",
    na.value = "grey",
    discrete = T,
    breaks = c(1, 2, 3, 4, 5),
    labels = c("< 4.75", "(4.75-5.59]", "(5.59-7.37]", "(7.37-10.17]", "\u2265 10.17")
  ) +
  theme_minimal(base_size = 14) +
  facet_grid(year ~ mom_race_eth) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom",
        legend.box = "horizontal")



###############END OF CLASS, IF YOU WISH TO EXPLORE CARTOGRAMS AND REGRESSION SEE BELOW#####################
#Cartograms
st_crs(states)
states <- st_transform(states, 2163) #look up what this projection is at spatialreference.org
st_crs(states)

#Create cartogram for Black and white fetal deaths in 2017
states_black <- states %>% filter(mom_race_eth == "Black or African American" & year == 2017)
states_white <- states %>% filter(mom_race_eth == "White" & year == 2017) 

states_fetaldeath <- cartogram_cont(states_black, "fetal_death_scaled", 
                                    itermax = 10, 
                                    threshold = .1)
states_fetaldeath2 <- cartogram_cont(states_white, 
                                     "fetal_death_scaled", 
                                     itermax = 10, 
                                     threshold = .1)

# Plot the cartograms
tm_shape(states_fetaldeath) + 
  tm_polygons("fetal_death_scaled", style = "jenks") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

tm_shape(states_fetaldeath2) + 
  tm_polygons("fetal_death_scaled", style = "jenks") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

#OK in this part, we will add in information on smoking
# Cigarette smoking data from the EH Tracking Network
cig <- read_csv("data/lab/01/cig_repro_age_state.csv")
glimpse(cig)

# Want overall average by state and year in the 18-44 age group
cig <- cig %>% 
  group_by(state, year) %>%
  summarise(prop_smokers = mean(proportion_smok, na.rm = T))
summary(cig$prop_smokers) # ranges from 9.3% to 33.7%

fetal_data_new <- left_join(fetal_data_new, cig, by = c("state" = "state", "year" = "year"))

# Correlation between smoking and fetal deaths
fetal_data_new %>% 
  dplyr::select(prop_smokers, fetal_death_scaled) %>% 
  corrr::correlate() 

# Add cigarette smoking to states
states <- st_read("data/lab/01/US_State_Albers.shp")
head(states)

# Add fetal death data to states shapefile
states <- left_join(states, fetal_data_new, by = c("STATE_NAME" = "state"))
head(states)

# Linear regression
# Change prop to percent
states <- states %>% mutate(percent_smokers = prop_smokers*100)
fetal_data_new <- fetal_data_new %>% mutate(percent_smokers = prop_smokers*100)

# Basic poisson model
# Check distribution of fetal_deaths
states %>% ggplot(aes(y=fetal_deaths)) + geom_histogram()

smoke_poisson <- glm(fetal_deaths ~ percent_smokers + factor(year) + factor(mom_race_eth) + offset(log(births)), 
                     data = states, family = poisson(link = "log"))
summary(smoke_poisson)
exp( 0.0052174 )

# Is there spatial autocorrelation in the data that we need to account for?
# Let's look at the residuals of our regression model
fetal_data_nomiss <-
  fetal_data_new %>% drop_na(percent_smokers, fetal_deaths, births, mom_race_eth)
fetal_data_nomiss$residuals <- smoke_poisson$residuals
fetal_data_nomiss <-
  fetal_data_nomiss %>% dplyr::select(year, state, mom_race_eth, residuals, mom_race_eth)
states = left_join(
  states,
  fetal_data_nomiss,
  by = c(
    "year" = "year",
    "STATE_NAME" = "state",
    "mom_race_eth" = "mom_race_eth"
  )
)

# Do we see a spatial pattern in the residuals?
states %>%
  ggplot() +
  geom_sf(aes(fill = residuals),
          color = "white",
          lwd = 0.1) +
  scale_fill_viridis("Residuals",
                     na.value = "grey",
                     option = "A") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", legend.box = "horizontal")
# Does it look random?

# Create a neighbor matrix, queen matrix identifying queen-type neighbors
nb <- poly2nb(states, queen = TRUE)

# Assigning weights 
lw <- nb2listw(nb, style = "S", zero.policy = TRUE)

# First polygon's weights,
lw$weights[1]

# Get Moran's I
#set.seed
set.seed(111)
MC <- moran.mc(states$fetal_death_scaled,lw, na.action = na.exclude, nsim = 500)
MC
# Reject hypothesis that there is no global spatial autocorrelation
